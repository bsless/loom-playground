(ns loom-playground.continuation
  (:refer-clojure
   :exclude [future-call run!])
  (:require
   [clojure.reflect :refer [reflect]])
  (:import
   (java.lang Continuation ContinuationScope)))

(comment
  (reflect Continuation)
  (into [] (comp (filter (comp :public :flags)) (map :name)) (:members (reflect Continuation)))
  (reflect ContinuationScope)

  (def c (ContinuationScope. "foo"))
  (type c)
  (reflect c)

  (Continuation/getCurrentContinuation c )

  )

(defn ^ContinuationScope scope*
  ([]
   (scope* (str (gensym))))
  ([^String name]
   (new ContinuationScope name)))

(defn continuation*
  [scope f]
  (new Continuation scope f))

(defmacro with-continuation
  [scope & body]
  `(continuation* ~scope (fn* [] ~@body)))

(defn run! [^Continuation cont] (.run cont))
(defn done? [^Continuation cont] (.isDone cont))
(defn ^{:tag "boolean"} yield
  [^ContinuationScope scope]
  (Continuation/yield scope))

(defn ^Continuation current-continuation
  [^ContinuationScope s]
  (Continuation/getCurrentContinuation s))

(def cc current-continuation)
(reset-meta! #'cc (meta #'current-continuation))

(defmacro runc!
  [cont & body]
  `(run! (with-continuation ~cont ~@body)))

(defn call-with-current-continuation
  [^ContinuationScope scope f]
  (runc! (cc scope) (f)))

(defmacro with-scope
  [name & body]
  (let [s (str name)]
    `(let [~name (scope* ~s)]
       ~@body)))

(defmacro with-scopes
  [names & body]
  (assert (vector? names))
  (let [scopes (mapcat (fn [name] [name `(scope* ~(str name))]) names)]
    `(let [~@scopes]
       ~@body)))

(comment
  (with-scope foo
    (runc! foo (println (cc foo))))

  )

(comment

  (import 'java.util.concurrent.atomic.AtomicInteger)
  (import 'java.util.concurrent.atomic.AtomicReference)

  ;;; test1 https://github.com/openjdk/loom/blob/c8bff399b1b7413145c3effabcc38be6733ac730/test/jdk/java/lang/Continuation/Basic.java
  (with-scope *foo*
    (let [bar (fn [b] (yield *foo*) (str (inc b)))
          foo (fn [a] (inc (Integer/parseInt (bar (inc a)))))
          res (AtomicInteger. 0)
          cont (with-continuation *foo*
                 (loop [k 1
                        r 0]
                   (if (== 20 k)
                     (.set ^AtomicInteger res r)
                     (recur (inc k) (+ r (foo k))))))]
      (while (not (done? cont))
        (run! cont))
      (.get res)))
  )

(with-scope *foo*
  (let [bar (fn [b] (yield *foo*) (str (inc b)))
        foo (fn [a] (inc (Integer/parseInt (bar (inc a)))))
        res (AtomicInteger. 0)
        cont (with-continuation *foo*
               (loop [k 1
                      r 0]
                 (if (== 20 k)
                   (.set ^AtomicInteger res r)
                   (recur (inc k) (+ r (foo k))))))]
    (while (not (done? cont))
      (run! cont))
    (.get res)))
