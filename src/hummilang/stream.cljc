(ns hummilang.stream)

;; the interface

(defprotocol transducer
  ;; empty is unnecessary, always specify the initial value instead
  (init [this r])
  (step [this r result x])
  (complete [this r result]))

;; transducer constructors

(defn map* [f]
  (reify transducer
    (init [this r] (r))
    ;; this is map over one stream for now
    (step [this r result x]
      (r result (f x)))
    (complete [this r result]
      (r result))
    Object
    (toString [this]
      (str "<map transducer>"))))

(defn filter* [pred]
  (reify transducer
    (init [this r] (r))
    (step [this r result x]
      (if (pred x)
        (r result x)
        result))
    (complete [this r result]
      (r result))
    Object
    (toString [this]
      (str "<filter transducer>"))))

;; applicators

(defn transduce* [t r init from]
  (let [f (fn [acc x]
            (step t r acc x))
        ret (reduce f init from)]
    (complete t r ret)))

(defn into* [to t from]
  (transduce* t conj to from))

(def t1 (map* inc))
(def t2 (filter* odd?))

(into* [] t1 (range 10))
(into* [] t2 (range 10))

;; dodgy composition

(defn comp* [t1 t2]
  (reify transducer
    (init [this r]
      (r))
    (step [this r result x]
      (let [r2 (fn [result y]
                 (step t2 r result y))]
        (step t1 r2 result x)))
    (complete [this r result]
      (r result))))

(def t3 (comp* t1 t2))

(into* [] t3 (range 10))
