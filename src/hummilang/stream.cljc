(ns hummilang.stream)

;; the interface

(defprotocol Reducer
  ;; init is unnecessary; always specify the initial value instead
  (init [this r])
  (step [this r result x])
  (complete [this r result])
  (reset [this]))

;; constructors

(defn map* [f]
  (reify
    Reducer
    (init [this r] (r))
    ;; this is map over one stream for now
    (step [this r result x]
      (r result (f x)))
    (complete [this r result]
      (r result))
    (reset [this])
    Object
    (toString [this]
      (str "<map " f "> reducer"))))

(defn filter* [pred]
  (reify
    Reducer
    (init [this r] (r))
    (step [this r result x]
      (if (pred x)
        (r result x)
        result))
    (complete [this r result]
      (r result))
    (reset [this])
    Object
    (toString [this]
      (str "<filter " pred "> reducer"))))

(defn take* [n]
  ;; how to put the state be inside the Reducer itself?
  (let [i (atom 0)]
    (reify
      Reducer
      (init [this r] (r))
      (step [this r result x]
        (if (< @i n)
          (do (swap! i inc)
              (r result x))
          result))
      (complete [this r result]
        (r result))
      (reset [this]
        (reset! i 0))
      Object
      (toString [this]
        (str "<take " n "> reducer")))))

;; applicators

(defn transduce* [t r init from]
  (reset t)
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
  (reify Reducer
    (init [this r]
      (r))
    (step [this r result x]
      (let [r2 (fn [result y]
                 (step t2 r result y))]
        (step t1 r2 result x)))
    (complete [this r result]
      (r result))
    (reset [this]
      (reset t1)
      (reset t2))
    Object (toString [this] (str "<comp " t1 ", " t2 "> reducer"))))

(def t3 (comp* t1 t2))

(into* [] t3 (range 10))

(def t4 (take* 5))
(into* [] t4 (range 10))
