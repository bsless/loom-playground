(ns loom-playground.core
  (:refer-clojure
   :exclude [future-call])
  (:require
   [clojure.core.async :as async]
   [clojure.reflect :refer [reflect]])
  (:import
   [java.time Instant]
   [java.util.concurrent ThreadPoolExecutor ThreadFactory ThreadExecutor Executor Executors]
   [java.lang Continuation ContinuationScope Thread ThreadGroup Scoped]))

(comment
  (reflect java.util.concurrent.ThreadExecutor)
  (.ensureNotShutdown java.util.concurrent.ThreadExecutor)
  (reflect Continuation)
  (reflect ContinuationScope)

  (def c (ContinuationScope. "foo"))
  (reflect c)

  (Continuation/getCurrentContinuation c )

  (def sv (Scoped/forType Object))

  (.bind sv 1)
  (.get sv)

  (with-open [_ (.bind sv 2)]
    (.get sv))

  )

(defprotocol IBind
  (-bind [this v]))

(defn wrap-scope
  [^Scoped sv]
  (reify
    IBind
    (-bind [this v]
      (.bind sv v))
    clojure.lang.IDeref
    (deref [this] (.get sv))))

(defn validate-scopes
  [bindings]
  (assert (vector? bindings) "Bindings must be a vector")
  (assert (even? (count bindings)) "Must provide even number of bindings")
  (let [pairs (partition 2 bindings)
        names (map first pairs)]
    (assert (= (count names) (count (set names)))
            "Bindings must be unique when creating scope")))

(defmacro in-scope
  [bindings & body]
  (validate-scopes bindings)
  (let [pairs (partition 2 bindings)
        names (map first pairs)
        binds (mapcat
               (fn [sv v]
                 ['_ `(-bind ~sv ~v)])
               names
               (map second pairs))]
    `(with-open [~@binds]
       ~@body)))

(defmacro with-scope
  [bindings & body]
  (validate-scopes bindings)
  (let [pairs (partition 2 bindings)
        names (map first pairs)
        scopes (mapcat
                (fn [name]
                  (let [tag (resolve (:tag (meta name) 'Object))]
                    [(with-meta name {:tag 'IBind})
                     `(wrap-scope (Scoped/forType ~tag))]))
                names)]
    `(let [~@scopes]
       (in-scope ~bindings ~@body))))

(comment
  (with-scope [b 2 a 3]
    (+ @a @b))
  (with-scope [^Long b 2 ^Long a 3]
    (+ @a @b)))


(def ^ThreadFactory tf (doto (Thread/builder) .virtual .factory))

(defn ^ThreadExecutor virtual-thread-executor
  []
  (Executors/newVirtualThreadExecutor))

(defn ^ThreadExecutor with-deadline
  [^ThreadExecutor e ^long t]
  (.withDeadline e (doto (Instant/now) (.plusMillis t))))

(def ^ThreadExecutor ex (virtual-thread-executor))

(defn binding-conveyor-fn
  [f]
  (let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
    (fn
      ([]
       (clojure.lang.Var/resetThreadBindingFrame frame)
       (f))
      ([x]
       (clojure.lang.Var/resetThreadBindingFrame frame)
       (f x))
      ([x y]
       (clojure.lang.Var/resetThreadBindingFrame frame)
       (f x y))
      ([x y z]
       (clojure.lang.Var/resetThreadBindingFrame frame)
       (f x y z))
      ([x y z & args]
       (clojure.lang.Var/resetThreadBindingFrame frame)
       (apply f x y z args)))))

(defn ^:private deref-future
  ([^java.util.concurrent.Future fut]
   (.get fut))
  ([^java.util.concurrent.Future fut timeout-ms timeout-val]
   (try (.get fut timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
        (catch java.util.concurrent.TimeoutException _
          timeout-val))))

(defn future-call
  ([f]
   (future-call ex f))
  ([^Executor ex f]
   (let [f (binding-conveyor-fn f)
         fut (.submit ex ^Callable f)]
     (reify
       clojure.lang.IDeref
       (deref [_] (deref-future fut))
       clojure.lang.IBlockingDeref
       (deref
           [_ timeout-ms timeout-val]
         (deref-future fut timeout-ms timeout-val))
       clojure.lang.IPending
       (isRealized [_] (.isDone fut))
       java.util.concurrent.Future
       (get [_] (.get fut))
       (get [_ timeout unit] (.get fut timeout unit))
       (isCancelled [_] (.isCancelled fut))
       (isDone [_] (.isDone fut))
       (cancel [_ interrupt?] (.cancel fut interrupt?))))))

(defn submit* [ex f] (future-call ex f))
(defmacro submit [ex & body]
  `(future-call ~ex (^{:once true} fn* [] ~@body)))

(defmacro vfuture
  "Takes a body of expressions and yields a future object that will
  invoke the body in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant of
  deref with timeout is used. See also - realized?."
  {:added "1.1"}
  [& body] `(future-call (^{:once true} fn* [] ~@body)))

(comment
  (vfuture (println "hi")))


(defonce ^:private ^Executor thread-macro-executor
  (Executors/newVirtualThreadExecutor))

(defn thread-call
  "Executes f in another thread, returning immediately to the calling
  thread. Returns a channel which will receive the result of calling
  f when completed, then close."
  [f]
  (let [c (async/chan 1)]
    (let [binds (clojure.lang.Var/getThreadBindingFrame)]
      (.execute thread-macro-executor
                (fn []
                  (clojure.lang.Var/resetThreadBindingFrame binds)
                  (try
                    (let [ret (f)]
                      (when-not (nil? ret)
                        (async/>!! c ret)))
                    (finally
                      (async/close! c))))))
    c))

(defmacro vthread
  "Executes the body in another thread, returning immediately to the
  calling thread. Returns a channel which will receive the result of
  the body when completed, then close."
  [& body]
  `(thread-call (^:once fn* [] ~@body)))

(comment
  (let [x 1]
    (async/<!! (vthread (println "hi") x)))

  (def ^:dynamic y 2)

  (binding [y 1]
    (async/<!! (vthread (println "hi") y))))


(comment
  (with-open [e (Executors/newVirtualThreadExecutor)]
    (future-call e (fn [] (println "hello") (Thread/sleep 1000)))
    (future-call e (fn [] (println "world") (Thread/sleep 2000)))
    ))

(defmacro with-tasks
  [& tasks]
  (let [e (gensym "e__")
        tasks (for [task tasks]
                `(future-call ~e (^{:once true} fn* [] ~task)))]
    `(with-open [~e (Executors/newVirtualThreadExecutor)]
       ~@tasks)))

(comment
  (with-tasks
    [(println "hello") (Thread/sleep 1000)]
    [(println "world") (Thread/sleep 2000)]
    )

  (time
   (->> 100
        range
        (mapv (fn [i] (future (Thread/sleep (+ (rand-int 1000) 1000)) (println i))))
        (run! deref)))

  (time
   (with-open [ex (virtual-thread-executor)]
     (dotimes [i 100]
       (submit ex (Thread/sleep (+ (rand-int 1000) 1000)) (println i)))))

  ;;; Doesn't work
  (time
   (with-open [ex (with-deadline (virtual-thread-executor) 1001)]
     (submit ex (Thread/sleep (+ (rand-int 1000) 1000)) (println "hello"))))

  )
