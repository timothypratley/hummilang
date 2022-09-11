(ns hummilang.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [hummilang.parser :as hp]))

(deftest parsing
  (is (= [:forms
          [:quote [:symbol "x"]]
          [:list [:number "1"] [:number "-2.0"] [:symbol "alpha"]
           [:set [:number "0"] [:set] [:list [:number "1"] [:number "2"]] [:symbol "x"]]]]
         (hp/parse "'x (1 -2.0 alpha {0 {} (1 2) x})")))

  (is (= '((quote x) (1 -2.0 alpha #{#{} (1 2) 0 x}))
         (hp/read "'x (1 -2.0 alpha {0 {} (1 2) x})")))

  (is (= '(((1 2) (3 4) (5 6)))
         (hp/read "((1 2) (3 4) (5 6))")))

  (is (= '((ns foo) (defn f (x) (inc x)))
         (hp/read "(ns foo) (defn f (x) (inc x))"))))
