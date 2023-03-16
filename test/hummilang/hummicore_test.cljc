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
    (-> '(if #{} "hummi" "not hummi")
      (hl/evaluate)
      (= "not hummi")
      (is "(if test then else) should evaluate else when test is false"))
    (-> '(do "one" "two" (quote (lets go)))
      (hl/evaluate)
      (= '(lets go))
      (is "(begin ...) should behave like doall but return the last thing"))
    (-> '((fn [x]
            (set! x 1)
            x)
          2)
      (hl/evaluate)
      (= 1)
      (is "lambda should extend the environment, set! should assign a variable in the environment, placing a lambda in the head of a list should invoke it"))
    (-> '((fn [a]
            ((fn [b]
               (list a b))
             (+ 2 a)))
          1)
      (hl/evaluate)
      (= '(1 3))
      (is "lambda should make use of the environment it is invoked in"))
    (-> '(((fn [a]
             (fn [b]
               (list a b)))
           1)
          2)
      (hl/evaluate)
      (= '(1 2))
      (is "lambda should capture the environment it is defined in"))
    #_(-> '(let [a 1]
             ((let [a 2]
                (lambda [b] (list a b)))
              3))
        (hl/evaluate)
        (= '(2 3))
        (is "let's go"))
    (-> '(apply + [1 2])
      (hl/evaluate)
      (= 3)
      (is "apply invokes a function with arguments"))
    ;; broken
    #_(-> '(+ (apply + [1 2]) 4)
      (hl/evaluate)
      (= 7))
    #_(time (dotimes [i 10000]
              (hl/evaluate '((fn [x] (+ x x)) 1000))))
    #_(time (dotimes [i 10000]
              (eval '((fn [x] (+ x x)) 1000))))
    (-> '(let [a 1]
           (+ a 2))
      (hl/evaluate)
      (= 3)
      (is "let creates a binding"))
    (-> '(cons 1 [2 3])
      (hl/evaluate)
      (= [1 2 3])
      (is "cons works"))
    (-> '(catch 1 (+ 3 5))
      (hl/evaluate)
      (= 8)
      (is "no throw"))
    (-> '(catch 1 (+ 1 (throw 1 5)))
      (hl/evaluate)
      (= 5)
      (is "throw catch"))
    (-> '(+ 3 (catch 1
                     (+ 2 (throw 1 5))))
      (hl/evaluate)
      (= 8)
      (is "throw catch"))
    (-> '(catch 2 (* 7 (catch 1 (* 3 (catch 2 (throw 1 (throw 2 5)))))))
      (hl/evaluate)
      (= (* 7 3 5))
      (is "fun"))
    (-> '(catch 2 (* 7 (throw 1 (throw 2 3))))
      (hl/evaluate)
      (->> (thrown? Exception) (is)))))

;; 100 arithmetic error
;; 200 stack overflow
;; stack is 256kb (stack overflow @ 1024)
(deftest ttt
  (-> '((fn [f n]
          (f f n))
        (fn [rec n]
          (if (<= n 2)
            n
            (+ n (rec rec (dec n)))))
        10)
    (hl/evaluate)
    (is "recursion"))
  ;; homework << can these continuations be collapsed??
  (-> '((fn [f acc n]
          (f f acc n))
        (fn [rec acc n]
          (if (<= n 2)
            n
            (+ n (rec rec acc (dec n)))))
        1
        10000)
    (hl/evaluate)
    (is "recursion")))
