(ns hummilang.hummicore
  (:require [clojure.edn :as edn]))

(declare evaluate)

(defmacro ^:private wrong [message expression environment]
  `(throw (ex-info ~message {:environment ~environment
                             :expression  ~expression})))

(defn ^:private parse [s]
  (edn/read-string s))

(defn evaluate-program [[head & tail] environment]
  (if tail
    (do (evaluate head environment)
        (recur tail environment))
    (evaluate head environment)))

;; TODO: this is permanently destructive! Should only shadow globals
(defn ^:private extend-environment [environment variables values]
  (merge environment (zipmap variables values)))

(defn ^:private make-function [bindings body definition-environment]
  ^{:name "lambda"}
  (fn [values current-environment]
    (let [extended (merge definition-environment
                          (extend-environment current-environment bindings values))]
      (evaluate-program body extended))))

;; TODO: why does the book propose this?
(defn ^:private make-closure [function definition-environment]
  (fn [values current-environment]
    (function values definition-environment)))

(defn invoke [function arguments environment]
  #_(prn 'INVOKE (some-> function meta :name) arguments)
  (if (fn? function)
    (function arguments environment)
    (wrong (str "Not a function: " (pr-str function))
           function
           nil)))

(defn ^:private primitive-fn [function-name f]
  ^{:name function-name}
  (fn [values current-environment]
    (apply f values)))

(def true# 1)
(def false# 0)
(def uninitialized# 'uninitialized)

(defn ^:private primitive-boolean-fn [function-name f]
  ^{:name function-name}
  (fn [values current-environment]
    (if (apply f values)
      true#
      false#)))

(defn ^:private new-environment []
  {'t     true#
   'f     false#
   'cons  (primitive-fn 'cons cons)
   'list  (primitive-fn 'list list)
   'apply ^{:name 'apply} (fn [[f args] environment]
                            (invoke f args environment))
   '+     (primitive-fn '+ +)
   '=     (primitive-boolean-fn '= =)
   '<     (primitive-boolean-fn '< <)})

(def global-environment (atom (new-environment)))

(defn read-evaluate [s]
  (-> (parse s)
      (evaluate @global-environment)))

(defn ^:private atom? [expression]
  (not (list? expression)))

(defn ^:private lookup [sym environment]
  (get environment sym))

(defn ^:private update! [variable value environment]
  (when (not (symbol? variable))
    (wrong (str "Not a symbol:" variable)
           variable environment))
  (swap! environment assoc variable value)
  value)

(defn ^:private primitive? [expression]
  (or (number? expression)
      (string? expression)
      (keyword? expression)
      (boolean? expression)
      (char? expression)
      (coll? expression)))

(defn ^:private evaluate-list [expressions environment]
  (reduce
    (fn [acc expression]
      (conj acc
            (evaluate expression environment)))
    []
    expressions))

(defn evaluate
  ([expression] (evaluate expression (new-environment)))
  ([expression environment]
   (if (atom? expression)
     (cond (symbol? expression) (lookup expression environment)
           (primitive? expression) expression
           :else (wrong (str "Expression is not a list, primitive or variable: " expression)
                        expression environment))
     (let [[function & arguments] expression]
       (case function
         let (let [[bindings & body] arguments]
               (evaluate-program
                 body
                 (extend-environment
                   environment
                   (for [binding bindings]
                     (if (symbol? binding)
                       binding
                       (first binding)))
                   (for [binding bindings]
                     (if (symbol? binding)
                       uninitialized#
                       (evaluate (last binding) environment))))))
         quote (first arguments)
         if (let [[test then else] arguments]
              (if (evaluate test environment)
                (recur then environment)
                (recur else environment)))
         begin (evaluate-program arguments environment)
         set! (let [[variable assignment] arguments
                    value (evaluate assignment environment)]
                (update! variable value environment))
         lambda (let [[args & body] arguments]
                  (make-function args body environment))
         (invoke (evaluate function environment)
                 (evaluate-list arguments environment)
                 environment))))))
