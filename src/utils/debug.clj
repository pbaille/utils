(ns utils.debug
  (:require [com.gfredericks.debug-repl :refer [break! unbreak! unbreak!! current-locals]]))

(defn throw* [message] (throw (Exception. message)))

(defmacro succeed-or [s f]
  `(try ~s (catch Exception e# ~f)))

(defmacro !b [] `(break!))
(defmacro !u [] `(unbreak!))
(defmacro !!u [] `(unbreak!!))

(defmacro or-dr
  "try an expr and launch debug-repl if Exception"
  [expr]
  (let [line (:line (meta &form))
        file *file*]
    `(try ~expr (catch Exception _ (break! (str "file:" ~file "::" ~line))))))
