(ns hummilang.y-combinator)

(def fix
  (let [d (fn [w]
            (fn [f]
              (f (fn DELAY [x]
                   (((w w) f) x)))))]
    (d d)))

(defn fix
  "Y: λa.(λb(a(bb))(λb(a(bb))))"
  [f]
  ((fn [x] (x x))
   (fn [x] (f (fn DELAY [y]
                ((x x) y))))))


(defn w
  "w (omega): Mockingbird λa.aa"
  [a]
  (a a))
;; (w (fn [f] 1)) => 1
;; (w (fn foo [f] (println "f is foo:" (= f foo)) 1))
;; f is foo: true => 1
;; w enables recursion, because we could have invoked `f` inside foo.
;; (w (fn foo [f] (f f))) => stack overflow
;; but with only a single argument, the function itself, we can't have a termination condition

;; (w w) => stack overflow
;; (w w) is also known as W (Omega) because it never ends

(defn fact [rec y]
  (if (zero? y)
    1
    (* y (rec (dec y)))))

(defn curry [f]
  (fn [x]
    (fn [y]
      (f x y))))

(defn fact' [rec y]
  (if (zero? y)
    1
    (* y ((rec rec) (dec y)))))

(def cfact' (curry fact'))
((cfact' cfact') 5)
((w (curry fact')) 5)
(def wc (comp w curry))
((wc fact') 5)


(defn cfact [crec x]
  (fact (crec crec) x))
((wc cfact) 5)
;=> 120

(defn Yt [f]
  "Tim's version:"
  (wc (fn [crec x]
        (f (crec crec) x))))
((Yt fact) 5)

(defn Ytc2 [cf]
  "Tim's version of Y accepting a curry2 using only 1-arity functions:
  A function that receives a cf and recurs with it."
  (w
    ;; curried fn of crec, x
    (fn [crec]
      (fn [x]
        ;; apply cf, rec, x where rec is apply crec, crec
        ((cf (crec crec)) x)))))
(defn fact2 [rec]
  (fn [n]
    (if (zero? n)
      1
      (* n (rec (dec n))))))
((Ytc2 fact2) 5)

;; TODO: better than ignore, use an acc
(defn fact3 [rec]
  (fn p [n]
    (fn q [ignore]
      (if (zero? n)
        1
        (* n ((rec (dec n)) ignore))))))
(defn Ytc3 [cf]
  "Tim's version for cf as a curry3:"
  (w (fn a [crec]
       (fn b [x]
         (fn c [y]
           (((cf (crec crec)) x) y))))))
(((Ytc3 fact3) 5) :ignore)

(defn Ytc2 [cf]
  "Tim's version of Y accepting a curry2 using only 1-arity functions:
  A function that receives a cf and recurs with it."
  (w
    ;; curried fn of crec, x
    (fn [crec]
      (fn [x]
        ;; apply cf, rec, x where rec is apply crec, crec
        ((cf (crec crec)) x)))))
((Ytc2 fact2) 5)

;; Refactor Yt to Yb:
;; presented in reverse order because I think it makes more sense that way:
;; step 3: recognize that cf can be raised above (fn [x] ...)
;;  (fn [x] ((cf rec) x)) === (cf (fn [x] (rec x)))
;; after refactoring, verify things are still working.
(defn Ytbc2 [cf]
  (w
    ;; curried fn of crec, x
    (fn [crec]
      (cf (fn [x]
            ;; apply cf, rec, x where rec is apply crec, crec
            ((crec crec) x))))))
((Ytbc2 fact2) 5)
;=> 120

;; step 2: convert Y from defn to def (this drops cf as an input)
;; step 1: add the curry layer that accepts cf after crec and before x,
;; AND recognize that introducing that layer means we now need to change
;;(rec x) -> ((rec cf) x)
(def Ytbc2
  ;; call a fn on itself, do it now
  (w
    ;; curried fn of crec, cf, x
    ;; when applied with itself, it sets up a recursion that accepts cf.
    (fn [crec]
      (fn [cf]
        ;; do an iteration
        (cf (fn [x]
              ;; apply cf, rec, x where rec is now apply crec, crec apply cf
              (((crec crec) cf) x)))))))
((Ytbc2 fact2) 5)

;; Done!

(def Ytc2b
  "Create a recursion that will accept cf"
  ;; apply to itself
  (w
    ;; when create is applied with itself, it sets up a recursion that accepts cf.
    ;; create is a curried fn of c2 (create, but passed as an arg), cf (fact)
    (fn create [c2]
      (fn recursion [cf]
        ;; apply cf, rec
        (cf (fn rec [x]
              ;; apply rec, cf, x where rec is apply c2, c2, (ie: create, create)
              (((c2 c2) cf) x)))))))
((Ytc2b fact2) 5)
;=> 120


(def Ytbc2
  (w
    ;; curried fn of crec, x
    (fn [creatc]
      (fn [cf]
        (cf (fn rec [x]
              ;; apply create, cf, x where rec is apply crec, crec
              (((creatc creatc) cf) x)))))))

(def Ytbc2
  (w
    ;; curried fn of crec, x
    (fn [creatc]
      (fn [cf]
        (cf (fn rec [x]
              ;; apply create, cf, x where rec is apply crec, crec
              (((creatc creatc) cf) x)))))))
((Ytbc2 fact2) 5)


(def Ytc2b
  "Create a recursion that will accept cf"
  ;; apply to itself
  (w
    ;; when create is applied with itself, it sets up a recursion that accepts cf.
    ;; create is a curried fn of c2 (create, but passed as an arg), cf (fact)
    (fn create [c2]
      (fn recursion [cf]
        ;; apply cf, rec
        (cf (fn rec [x]
              ;; apply rec, cf, x where rec is apply c2, c2, (ie: create, create)
              (((c2 c2) cf) x)))))))
((Ytc2b fact2) 5)
;=> 120

(def Yb
  "book, double curried but still not enough curry
  Y: ??"
  (let [d (fn [w]
            (fn [f]
              (f (fn [x]
                   (((w w) f) x)))))]
    (d d)))
((Yb (curry fact)) 5)



(defn Yw [f]
  "Why paper:
  Y: λf.(λxλy(f(xx)y))(λxλy(f(xx)y))"
  (let [g (fn [x]
            (fn [y]
              (f (x x) y)))]
    (g g)))
((Yw fact) 5)

(defn Yj
  "Jonathan, not enough curry
  Y: λa.(λb(a(bb))(λb(a(bb))))"
  [a]
  (w (fn [b]
       (a (fn [c]
            ((b b) c))))))
((Yj (curry fact)) 5)

(def Yb
  "book, double curried but still not enough curry
  Y: ??"
  (let [d (fn [w]
            (fn [f]
              (f (fn [x]
                   (((w w) f) x)))))]
    (d d)))
((Yb (curry fact)) 5)



(defn W
  "W (Omega): λa.(aa)(aa) -- double w"
  [a]
  ((a a)
   (a a)))
;; (W (fn [a] (fn [b] 1))) => 1
;; not sure why we care about W

;; too eager because arguments are evaluated first
(defn Y [a]
  ((fn [b] (a (b b)))
   (fn [b] (a (b b)))))
;;(Y fact) => stack overflow

;; (b b) => (fn [c] ((b b) c))
;; a function => the same function
;; BUT, the latter does not call (b b) until it needs it
(defn Y [a]
  (w (fn [b]
       (a (fn [c]
            ((b b) c))))))
((Y fact2) 5)
;=> 120
((fact2 (Y fact2)) 5)
;=> 120
((fact2 (fact2 (Y fact2))) 5)
;=> 120
((fact2 (fact2 (fact2 (Y fact2)))) 5)
;=> 120
(= (Y fact2) (fact2 (Y fact2)))
;=> 120
(def ff (fn [x]
          (fn [y]
            1)))
((ff (ff (Y ff))) 2)

(def Ycat (Y (fn [x] cat)))

(def t (partition-all 2))
(into [] (t (t (t (t t)))) (range 20))
(into [] (cat (cat (cat (cat cat)))) [[1 2] [3 4] [5 6]])
(def tcat (comp t cat))
(into [] (cat (t (tcat (tcat tcat)))) (range 20))
(def tf (filter odd?))
(into [] (tf (tf tf)) (range 20))

(defn Y
  "Y: λa.(λb(a(bb))(λb(a(bb))))"
  [a]
  (w (fn [b] (a (fn [c]
                  ((b b) c))))))

(defn Y [a]
  (let [d (fn [b] (a (fn [c]
                       ((b b) c))))]
    (d d)))

(def Y
  (w (fn [b]
       (fn [z]
         (z (fn [c]
              (((b b) z) c)))))))


(def Y
  (let [d (fn [w]
            (fn [f]
              (f (fn [x]
                   (((w w) f) x)))))]
    (d d)))



(defn fact* [rec y]
  (if (zero? y)
    1
    (* y (rec rec (dec y)))))

(defn w* [w' f & args]
  (apply f (partial w' w' f) args))

(defn w' [f & args]
  (apply w* w* f args))

(w' fact 5)
;=> 120
