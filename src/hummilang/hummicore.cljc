(ns hummilang.hummicore
  (:require [hummilang.parser :as hp]))

(declare evaluate)
(declare update!)
(declare evaluate-do)
(declare evaluate-args)
(declare resume)

(defn ^:private wrong [message expression env cont]
  (throw (ex-info (str message ": " (pr-str expression))
                  {:type       'exception
                   :env        env
                   :cont       cont
                   :expression expression})))

;; TODO: this is permanently destructive! Should only shadow globals
(defn ^:private extend-environment [env variables values]
  (merge env (zipmap variables values)))

(defn True [a b] a)
(defn False [a b] b)
(def false-values #{False #{} 0 nil})
(defn boolify [x]
  (if (contains? false-values x)
    False
    True))
(def uninitialized# 'uninitialized)
(def empty-do-value nil)

(defn ^:private primitive? [expression]
  (or (number? expression)
      (string? expression)
      (keyword? expression)
      (boolean? expression)
      (char? expression)
      (coll? expression)))

(defn ^:private primitive-fn [function-name f arity]
  {:type  'primitive
   :name  function-name
   :arity arity
   :f     (fn [args env cont]
            (if (or (nil? arity) (= arity (count args)))
              (resume cont (apply f args))
              (wrong (str "Incorrect arity " function-name) args env cont)))})

(defn invoke [f args env cont]
  (case (:type f)
    function (let [env (extend-environment (:env f) (:args f) args)]
               (evaluate-do (:body f) env cont))
    primitive ((:f f) args env cont)
    (if (:cont f)
      (if (= 1 (count args))
        (resume f (first args))
        (wrong "Continuations expect one argument" args env cont))
      (wrong "Not a function" f env cont))))

(defn ^:private new-environment []
  {'True    True
   'False   False
   'cons    (primitive-fn 'cons cons 2)
   'list    (primitive-fn 'list list nil)
   'apply   {:type 'primitive
             :name 'apply
             :f    (fn [[f & args] env cont]
                     (resume cont (invoke f (concat (butlast args) (last args))
                                          env cont)))}
   'call/cc {:type 'primitive
             :name 'call/cc
             :f    (fn [args env cont]
                     (if (= 1 (count args))
                       (invoke (first args) (list cont) env cont)
                       (wrong "Incorrect arity call/cc" args env cont)))}
   ;; TODO: these shouldn't be primitives
   '+       (primitive-fn '+ + 2)
   '=       (primitive-fn '= = 2)
   '<       (primitive-fn '< < 2)})

(def global-environment (atom (new-environment)))

(defn read-evaluate [s]
  (-> (hp/read s)
    (into '(do))
    (evaluate)))

(defn ^:private atom? [expression]
  (not (list? expression)))

(defn catch-lookup [cont tag throwcont]
  (case (:type cont)
    labeled-cont (if (= tag (:tag cont))
                   (evaluate (:form throwcont)
                             (:env throwcont)
                             (assoc throwcont
                               :type 'throwing-cont
                               :tag tag
                               :throwcont (:cont cont)))
                   (catch-lookup (:cont cont)
                                 tag
                                 throwcont))
    bottom-cont (wrong "No associated catch" tag cont throwcont)
    (if (:cont cont)
      (catch-lookup (:cont cont) tag throwcont)
      (wrong "Not a continuation" (:type cont) cont throwcont))))

(defn resume [cont value]
  (case (:type cont)
    if-cont (evaluate (if (contains? false-values value)
                        (:else cont)
                        (:then cont))
                      (:env cont)
                      (:cont cont))
    set!-cont (update! (:variable cont) value (:env cont) (:cont cont))
    do-cont (evaluate-do (:tail cont) (:env cont) (:cont cont))
    evfn-cont (evaluate-args (:args cont) (:env cont)
                             {:type 'apply-cont
                              :env  (:env cont)
                              :cont (:cont cont)
                              :f    value})
    arg-cont (evaluate-args (:args cont) (:env cont)
                            {:type  'gather-cont
                             :env   (:env cont)
                             :cont  (:cont cont)
                             :value value})
    gather-cont (resume (:cont cont) (cons (:value cont) value))
    apply-cont (invoke (:f cont) value (:env cont) (:cont cont))
    bottom-cont ((:f cont) value)
    catch-cont (evaluate-do (:body cont) (:env cont) {:type 'labeled-cont
                                                      :cont (:cont cont)
                                                      :tag  value})
    throw-cont (catch-lookup cont value cont)
    throwing-cont (resume (:throwcont cont) value)
    (wrong "Unknown continuation" (:type cont) (:env cont) (:cont cont))))

(defn evaluate-args [args env cont]
  (if (seq args)
    (evaluate (first args) env {:type 'arg-cont
                                :env  env
                                :cont cont
                                :args (rest args)})
    (resume cont ())))

(defn evaluate-do [[head & tail] env cont]
  (if head
    (if (first tail)
      (evaluate head env {:type 'do-cont
                          :env  env
                          :cont cont
                          :tail tail})
      (evaluate head env cont))
    (resume cont empty-do-value)))

(def not-found (reify))

(defn ^:private lookup [sym env cont]
  (let [value (get env sym not-found)]
    (if (= value not-found)
      (wrong "Unknown variable" sym env cont)
      (resume cont value))))

(defn ^:private update! [variable value env cont]
  (when (not (symbol? variable))
    (wrong "Not a symbol" variable env cont))
  (swap! env assoc variable value)
  (resume cont value))

(defn ^:private evaluate-fn [[args & body] definition-environment cont]
  (resume cont {:type 'function
                :env  definition-environment
                :cont cont
                :args args
                :body body}))

(defn evaluate-let [[bindings & body] env cont]
  (let [names (for [binding bindings]
                (if (symbol? binding)
                  binding
                  (first binding)))
        ;; TODO: just make a map instead
        values (for [binding bindings]
                 (if (symbol? binding)
                   uninitialized#
                   (evaluate (last binding) env cont)))]
    (evaluate-do body
                 (extend-environment env names values)
                 cont)))

(defn evaluate-if [[test then else] env cont]
  (evaluate test env {:type 'if-cont
                      :env  env
                      :cont cont
                      :test test
                      :then then
                      :else else}))

(defn evaluate-set! [[variable assignment] env cont]
  (evaluate assignment env {:type     'set!-cont
                            :env      env
                            :cont     cont
                            :variable variable}))

(defn evaluate-apply [f args env cont]
  (evaluate f env {:type 'evfn-cont
                   :env  env
                   :cont cont
                   :args args}))

(defn evaluate-catch [[tag & body] env cont]
  (evaluate tag env {:type 'catch-cont
                     :env  env
                     :cont cont
                     :body body}))

(defn evaluate-throw [[tag form] env cont]
  (evaluate tag env {:type 'throw-cont
                     :env  env
                     :cont cont
                     :form form}))

(defn evaluate
  ([expression]
   (evaluate expression @global-environment {:type 'bottom-cont
                                             :f    identity}))
  ([expression env cont]
   (if (atom? expression)
     (cond (symbol? expression) (lookup expression env cont)
           (primitive? expression) (resume cont expression)
           :else (wrong "Expression is not a list, primitive or variable"
                        expression env cont))
     ;; TODO: if these were multi-methods, it would be more extensible...
     (let [[head & tail] expression]
       (case head
         let (evaluate-let tail env cont)
         quote (resume cont (first tail))
         if (evaluate-if tail env cont)
         do (evaluate-do tail env cont)
         set! (evaluate-set! tail env cont)
         fn (evaluate-fn tail env cont)
         catch (evaluate-catch tail env cont)
         throw (evaluate-throw tail env cont)
         (evaluate-apply head tail env cont))))))
