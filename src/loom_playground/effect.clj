(ns loom-playground.effect
  (:require
   [loom-playground.scope :as s]
   [loom-playground.continuation :as c]
   )
  (:import
   (java.lang Scoped)
   (java.lang Continuation ContinuationScope)))

(def ^Scoped handlers (doto (Scoped/forType clojure.lang.IPersistentMap) (.bind {})))
(def ^Scoped cc (doto (Scoped/forType ContinuationScope) (.bind (ContinuationScope. "base"))))
(def this-ns (symbol (str *ns*)))

(defn resume
  []
  (c/yield (.get cc)))

(defmacro perform
  [k & args]
  `(let [f# (get (.get handlers) ~k)
         c# (c/with-continuation (.get cc)
              (f# resume ~@args))]
     (.run c#)))

(defmacro handle
  [mapping & body]
  `(with-open [~'_ (.bind handlers (merge (.get handlers) ~mapping))]
     (with-open [~'_ (.bind cc (ContinuationScope. ~(str (gensym))))]
       ~@body)))

(comment
  (defn die
    [resume e]
    (println "Handling the exception!"))

  (defn throws! []
    (println "Throwing the exception!")
    (perform :throw
             (Exception. "Oh no!")))

  (handle
   {:throw die}
   (throws!)))

(comment
  (defn app []
    (println (perform :state/get))
    (perform :state/put 1)
    (println (perform :state/get))
    (perform :state/put "abc")
    (println (perform :state/get)))

  (let [state (atom 0)]
    (handle {:state/put (fn [resume n]
                          (reset! state n)
                          (resume ))
             :state/get (fn [resume]
                          (resume @state))}
            (app))))
