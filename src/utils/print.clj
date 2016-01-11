(ns utils.print
  (:require backtick))

(defmacro def-IShow []
  (backtick/template
    (do
      (defprotocol IShow (show-me [x]))

      (defmethod print-method 'IShow [x w]
        (print-method (show-me x) w))

      (defmethod print-method 'Ishow [x w]
        (let [smm (:show-me (meta x))
              to-show (if (fn? smm) (smm x) smm)]
          (print-method to-show w)))

      (do
        (defmulti print-method*
                  (fn [x w] (cond
                              (:show-me (meta x)) 'Ishow
                              (satisfies? IShow x) 'IShow
                              :else (type x))))
        (doseq [[dv m] (.getMethodTable print-method)]
          (.addMethod print-method* dv m))
        (doseq [[dv dvs] (.getPreferTable print-method), dv' dvs]
          (.preferMethod print-method* dv dv'))
        (.bindRoot #'print-method print-method*)))))

(defn showable [to-show x]
  (vary-meta x assoc :show-me to-show))

(defmacro sfn [& body]
  `(showable '~(list* 'fn body) (fn ~@body)))

(defmacro sdefn [n & body]
  `(def ~n (sfn ~n ~@body)))
