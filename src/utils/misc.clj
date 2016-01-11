(ns utils.misc)

(defn impl-for
  "get multim's implementation for args"
  [multim & args]
  (let [disp (apply (.dispatchFn multim) args)]
    (get-method multim disp)))

(defn add-method [multifn dispatch-val f]
  (.addMethod multifn dispatch-val f))
