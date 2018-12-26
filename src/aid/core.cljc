(ns aid.core
  (:refer-clojure :exclude [defcurried])
  (:require [cats.context :as ctx]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [cats.monad.maybe :as maybe]
            [aid.unit :as unit]
            #?@(:clj [[clojure.test :as test]
                      [potemkin]]))
  #?(:cljs (:require-macros [aid.core :refer [build
                                              case-eval
                                              casep
                                              defcurried]])))

(defn call-pred
  ([_]
   true)
  ([pred expr]
   (pred expr)))

#?(:clj
   (do (defmacro casep
         [x & clauses]
         `(condp call-pred ~x
            ~@clauses))

       (defmacro case-eval
         [x & clauses]
         `(condp = ~x
            ~@clauses))))

#?(:clj
   (do (defn gensymize
         ;This function works around java.lang.ExceptionInInitializerError
         ;(eval (list map (partial + 1) [0]))
         ;CompilerException java.lang.ExceptionInInitializerError
         ;(eval (list map (def x (partial + 1)) [0]))
         ;=> (1)
         ;(eval (list map inc [0]))
         ;=> (1)
         ;(eval (list map (fn [x] (+ 1 x)) [0]))
         ;=> (1)
         [x]
         (-> (intern *ns* (gensym) x)
             str
             (subs 2)
             symbol))

       (defmacro functionize
         ;If operator is a list, then it returns a value, which can be passed arround.
         [operator]
         (casep operator
           test/function? operator
           list? operator
           `(fn [& more#]
              (->> (map gensymize more#)
                   (cons '~operator)
                   eval))))

       ;This definition is harder to read.
       ;This definition doesn't use functionize.
       (defmacro build
         [operator & fs]
         (potemkin/unify-gensyms
           `(fn [& more##]
              ;TODO use flip
              (~operator ~@(map (fn [f##]
                                  `(apply ~f## more##))
                                fs)))))

       ;This defintion is not compatible with ClojureScript
       ;(defmacro build
       ;  [operator & fs]
       ;  `(comp (partial apply (functionize ~operator))
       ;         (juxt ~@fs)))
       ))

(defn funcall
  ([f]
   (f))
  ([f & more]
   (apply f more)))

#?(:clj
   (do (defn get-required-arity
         [f]
         (-> (exc/try-or-recover (-> f
                                     .getRequiredArity
                                     maybe/just)
                                 (fn [_]
                                   (exc/success unit/unit)))
             m/join))

       (def get-non-variadic-arities
         (comp (partial map (comp alength
                                  (functionize .getParameterTypes)))
               (partial filter (comp (partial = "invoke")
                                     (functionize .getName)))
               (functionize .getDeclaredMethods)
               class))

       (def get-arities
         (build (comp distinct
                      maybe/cat-maybes
                      cons)
                get-required-arity
                (comp (partial map maybe/just)
                      get-non-variadic-arities)))

       (def get-currying-arity
         (comp (partial max 2)
               (partial apply min)
               get-arities))))

(defn curry
  #?(:clj ([f]
           (curry (get-currying-arity f) f)))
  ([arity f]
   (fn [& outer-more]
     (let [n (count outer-more)]
       (case-eval arity
         n (apply f outer-more)
         (curry (- arity n)
                (fn [& inner-more]
                  (apply f (concat outer-more inner-more)))))))))

#?(:clj
   (do (defmacro curriedfn
         [bindings & body]
         `(curry ~(count bindings)
                 (fn ~bindings
                   ~@body)))

       (defmacro defcurried
         [function-name bindings & body]
         `(def ~function-name
            (curriedfn ~bindings
                       ~@body)))))

(defn flip
  [f]
  (fn
    ([x]
     (fn [y & more]
       (apply f y x more)))
    ([x y & more]
     (apply f y x more))))

(def nop
  (constantly unit/unit))

#?(:clj (defmacro defpfmethod
          [multifn dispatch-val f]
          `(defmethod ~multifn ~dispatch-val
             [& x#]
             (apply ~f x#))))

;TODO delete this function when it's added to cats.core
(defn <$
  [a fa]
  (m/<$> (constantly a) fa))

(defn lift-a
  [f]
  (fn [& more]
    (apply m/<*>
           (m/pure (ctx/infer (first more)) (curry (count more) f))
           more)))

;TODO delete this function after cats.core is fixed
(defn ap
  [m1 m2]
  ;TODO use >>= and <$>
  (m/mlet [x1 m1
           x2 m2]
          (m/return (x1 x2))))

;TODO delete this definition after cats.monad.maybe is fixed
(def nothing
  (maybe/nothing))

(defn maybe*
  [expr]
  (casep expr
    nil? nothing
    (maybe/just expr)))

#?(:clj
   (do (defmacro maybe-if
         [test then]
         `(maybe* (if ~test
                    ~then)))

       (defmacro maybe-if-not
         [test then]
         `(maybe* (if-not ~test
                    ~then)))))

(defcurried if-then-else
  [if-function then-function else-function x]
  ((build if
          if-function
          then-function
          else-function)
    x))

(defcurried if-then
  [if-function then-function else]
  (if-then-else if-function
                then-function
                identity
                else))

(defcurried if-else
  [if-function else-function then]
  (if-then-else if-function
                identity
                else-function
                then))
