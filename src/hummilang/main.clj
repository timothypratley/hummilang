(ns hummilang.main
  (:require [hummilang.hummicore :as hl]))

(defn -main []
  (hl/read-evaluate (slurp () *in*)))

(defn repl []
  (println (hl/read-evaluate (read-line)))
  (recur))
