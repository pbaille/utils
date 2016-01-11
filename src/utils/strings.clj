(ns utils.strings)

(defn dash->camel [s]
  (let [[f & n] (clojure.string/split s #"-")]
    (apply str f (map clojure.string/capitalize n))))

(defn dash->Camel [s]
  (let [s (clojure.string/split s #"-")]
    (apply str (map clojure.string/capitalize s))))

(defn camel->dash [s]
  (let [[f & n] (re-seq #"[A-Za-z][a-z]*" s)]
    (apply str (clojure.string/lower-case f)
           (map #(str "-" (clojure.string/lower-case %)) n))))
