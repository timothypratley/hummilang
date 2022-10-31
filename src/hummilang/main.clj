(ns hummilang.main
  (:require
    [clojure.string :as str]
    [hummilang.hummicore :as hl]))

(defn -main [& args]
  (hl/read-evaluate (slurp *in*)))

(defn repl [& args]
  ;; TODO: handle multi-line inputs!
  (loop [line (read-line)]
    (when-not (str/blank? line)
      (-> (hl/read-evaluate line)
        (doto (println)))
      (recur (read-line)))))
