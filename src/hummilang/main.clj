(ns hummilang.main
  (:require [hummilang.hummicore :as hl]))

(defn -main [& args]
  (hl/read-evaluate (slurp *in*)))

(defn repl [& args]
  ;; TODO: handle multi-line inputs!
  (loop [line (read-line)]
    (when line
      (-> (hl/read-evaluate line)
        (doto (println)))
      (recur (read-line)))))
