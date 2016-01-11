(ns utils.misc)

(defn impl-for
  "get multim's implementation for args"
  [multim & args]
  (let [disp (apply (.dispatchFn multim) args)]
    (get-method multim disp)))

(defn add-method [multifn dispatch-val f]
  (.addMethod multifn dispatch-val f))

(defmacro defmets [n & body]
  `(do ~@(map (fn [spec] `(defmethod ~n ~@spec))
              (partition 3 body))))

(defmacro with-dispatch [disp-val expr]
  "call a particular dispatch on a multi method"
  `((get (methods ~(first expr)) ~disp-val) ~@(next expr)))

(defn immigrate
  "Add all the public vars in a list of namespaces to the current
  namespace."
  [& namespaces]
  (doseq [ns namespaces]
    (require ns)
    (doseq [[sym v] (ns-publics (find-ns ns))]
      (merge-meta! v
                   (if (.isBound v)
                     (intern *ns* sym (var-get v))
                     (intern *ns* sym))))))

;; from clojure programming book (cemerick)
(defn scaffold
  "Given an interface, returns a 'hollow' body suitable for use with `deftype`." [interface]
  (doseq [[iface methods]
          (->> interface
               .getMethods
               (map #(vector (.getName (.getDeclaringClass %))
                             (symbol (.getName %))
                             (count (.getParameterTypes %))))
               (group-by first))]
    (println (str " " iface))
    (doseq [[_ name argcount] methods]
      (println (str " " (list name (into '[this] (take argcount (repeatedly gensym)))))))))

;define an invokable type (kotarak)
(defmacro definvokable
  [type fields & deftype-tail]
  (let [f (fields 0)
        args (repeatedly 20 gensym)
        arity (fn [n]
                (let [args (take n args)]
                  `(invoke [this# ~@args] ((. this# ~f) ~@args))))
        vararg `(invoke [this# ~@args more#]
                        (apply (. this# ~f) ~@args more#))
        apply-to `(applyTo [this# args#] (apply (. this# ~f) args#))]
    `(deftype ~type
       ~fields
       clojure.lang.IFn
       ~@(map arity (range (inc 20)))
       ~vararg
       ~apply-to
       ~@deftype-tail)))