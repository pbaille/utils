(ns utils.types)

(defn t
  ([e] (:type (meta e)))
  ([sym e] (vary-meta e assoc :type sym)))

(defn t? [sym e] (= sym (t e)))
