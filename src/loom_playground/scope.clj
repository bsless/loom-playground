(ns loom-playground.scope
  (:refer-clojure :exclude [binding])
  (:import
   (java.lang  Scoped)))

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

(defmacro binding
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
  (binding [b 2 a 3]
    (+ @a @b))
  (binding [^Long b 2 ^Long a 3]
    (+ @a @b))

  (def sv (Scoped/forType String))
  (defn bar [] (println (.get sv)))
  (defn baz [] (with-open [_ (.bind sv "B")] (bar)))
  (defn foo [] (with-open [_ (.bind sv "A")] (bar) (baz) (bar)))
  (foo)
  )
