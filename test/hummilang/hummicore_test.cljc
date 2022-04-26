(ns hummilang.hummicore-test
  (:require [clojure.test :refer [deftest is testing]]
            [hummilang.hummicore :as hl]))

(deftest evaluate-test
  (testing "Basic evaluation"
    (-> 1
        (hl/evaluate)
        (= 1)
        (is "numbers should be numbers"))
    (-> '(if 1 "hummi" "not hummi")
        (hl/evaluate)
        (= "hummi")
        (is "(if test then else) should evaluate then when test is truthy"))
    (-> '(quote (this is quoted))
        (hl/evaluate)
        (= '(this is quoted))
        (is "should not invoke quoted forms"))
    (-> '(if false "hummi" "not hummi")
        (hl/evaluate)
        (= "not hummi")
        (is "(if test then else) should evaluate else when test is false"))
    (-> '(begin "one" "two" (quote (lets go)))
        (hl/evaluate)
        (= '(lets go))
        (is "(begin ...) should behave like doall but return the last thing"))
    (-> '((lambda [x]
                  (set! x 1)
                  x)
          2)
        (hl/evaluate)
        (= 1)
        (is "lambda should extend the environment, set! should assign a variable in the environment, placing a lambda in the head of a list should invoke it"))))
