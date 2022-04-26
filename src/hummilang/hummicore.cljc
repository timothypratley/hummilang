(ns hummilang.hummicore
  (:require [clojure.edn :as edn]))

(declare evaluate)

(defn ^:private wrong [message expression environment]
  (throw (ex-info message {:environment environment
                           :expression expression})))

(defn ^:private parse [s]
  (edn/read-string s))

(defn ^:private new-environment []
  (atom {}))

(defn read-evaluate [s]
  (-> (parse s)
      (evaluate (new-environment))))

(defn ^:private atom? [expression]
  (not (list? expression)))

(defn ^:private lookup [sym environment]
  (get @environment sym))

(defn ^:private update! [variable value environment]
  (when (not (symbol? variable))
    (wrong (str "Not a symbol:" variable)
           variable environment))
  ;; TODO: should check this for set! but not for extend-environment
  #_
  (when (not (contains? @environment variable))
    (wrong (str "No such binding: " variable)
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
      (cons (evaluate expression environment)
            acc))
    ()
    expressions))

(defn invoke [function arguments]
  (if (fn? function)
    (function arguments)
    (wrong (str "Not a function: " function)
           function
           nil)))

(defn evaluate-program
  ([program] (evaluate-program program (new-environment)))
  ([[head & tail] environment]
   (if tail
     (do (evaluate head environment)
         (recur tail environment))
     (evaluate head environment))))

;; TODO: this is permanently destructive! Should only shadow globals
(defn ^:private extend-environment [environment variables values]
  (doseq [[binding value] (map vector variables values)]
    (update! binding value environment))
  environment)

(defn ^:private make-function [bindings body environment]
  (fn [values]
    (let [extended (extend-environment environment bindings values)]
      (evaluate-program body extended))))

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
         quote (first arguments)
         if (let [[test then else] arguments]
              (if (evaluate test environment)
                (recur then environment)
                (recur else environment)))
         begin (evaluate-program arguments environment)
         set! (let [[variable assignment] arguments
                    value (evaluate assignment environment)]
                (update! variable value environment))
         lambda (let [[args body] arguments]
                  (make-function args body environment))
         (invoke (evaluate function environment)
                 (evaluate-list arguments environment)))))))
