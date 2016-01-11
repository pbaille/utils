(ns utils.logic)

;similar to all-true?
(defn satisfies-all?
  "return true if for each pred (pred v) is true"
  [v & preds]
  (every? identity ((apply juxt preds) v)))

(defn or=
  ([expr coll] (apply or= expr coll))
  ([expr o & ors]
   (eval `(or ~@(map (fn [o] `(= ~expr ~o)) (cons o ors))))))

(defn and=
  ([expr coll] (apply and= expr coll))
  ([expr o & ors]
   (eval `(and ~@(map (fn [o] `(= ~expr ~o)) (cons o ors))))))

(defn and-not=
  ([expr coll] (apply and-not= expr coll))
  ([expr o & ors]
   (eval `(and ~@(map (fn [o] `(not= ~expr ~o)) (cons o ors))))))

(defn or-not=
  ([expr coll] (apply or-not= expr coll))
  ([expr o & ors]
   (eval `(or ~@(map (fn [o] `(not= ~expr ~o)) (cons o ors))))))

(defmacro or->
  "ex: (or-> 10 neg? (= 10))
  => true"
  [arg & exprs]
  `(or ~@(map (fn [expr]
                (if (symbol? expr)
                  `(~expr ~arg)
                  (cons (first expr) (cons arg (next expr)))))
              exprs)))

(defmacro and->
  "ex: (and-> 10 pos? (= 10))
  => true"
  [arg & exprs]
  `(and ~@(map (fn [expr]
                 (if (symbol? expr)
                   `(~expr ~arg)
                   (cons (first expr) (cons arg (next expr)))))
               exprs)))

(defmacro all-true? [d & preds]
  `(and ~@(map #(list % d) preds)))
