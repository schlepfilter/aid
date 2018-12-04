(ns aid.core
  (:require [cats.context :as ctx]
            [cats.core :as m]
            [cats.monad.exception :as exc]
            [cats.monad.maybe :as maybe]
            [cats.protocols :as p]
            [aid.unit :as unit]
    #?@(:clj
        [
            [clojure.test :as test]
            [potemkin]]))
  #?(:cljs (:require-macros [aid.core :refer [case-eval casep mlet]])))

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
       ;TODO use case
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

;TODO delete this function after cats.context is fixed
(defn infer
  "Given an optional value infer its context. If context is already set, it
  is returned as is without any inference operation."
  {:no-doc true}
  ([]
   (when (nil? ctx/*context*)
     (ctx/throw-illegal-argument "No context is set."))
   ctx/*context*)
  ([v]
   (cond
     (satisfies? p/Contextual v)
     (p/-get-context v)
     :else
     (ctx/throw-illegal-argument
       (str "No context is set and it can not be automatically "
            "resolved from provided value")))))

;TODO delete this function after cats.context is fixed
(defn <>
  [& more]
  (with-redefs [cats.context/infer infer]
    (apply m/<> more)))

;TODO delete this function after cats.context is fixed
(defn mempty
  [& more]
  (with-redefs [cats.context/infer infer]
    (apply m/mempty more)))

;TODO delete this function after cats.context is fixed
(defn <$>
  [& more]
  (with-redefs [cats.context/infer infer]
    (apply m/<$> more)))

;TODO delete this function after cats.context is fixed
(defn pure
  [& more]
  (with-redefs [cats.context/infer infer]
    (apply m/pure more)))

;TODO delete this function after cats.context is fixed
(defn <*>
  [& more]
  (with-redefs [cats.context/infer infer]
    (apply m/<*> more)))

;TODO delete this function after cats.context is fixed
(defn return
  [& more]
  (with-redefs [cats.context/infer infer]
    (apply m/return more)))

;TODO delete this function after cats.context is fixed
(defn >>=
  [& more]
  (with-redefs [cats.context/infer infer]
    (apply m/>>= more)))

;TODO delete this function after cats.context is fixed
(defn =<<
  [& more]
  (with-redefs [cats.context/infer infer]
    (apply m/=<< more)))

;TODO delete this function after cats.context is fixed
(defn join
  [& more]
  (with-redefs [cats.context/infer infer]
    (apply m/join more)))

;TODO delete this macro after cats.context is fixed
#?(:clj (defmacro lift-m
          [& more]
          `(with-redefs [cats.context/infer infer]
             (m/lift-m ~@more))))

;TODO delete this macro after cats.context is fixed
#?(:clj (defmacro mlet
          [& more]
          `(with-redefs [cats.context/infer infer]
             (m/mlet ~@more))))

;TODO delete this macro after cats.context is fixed
#?(:clj (defmacro ->=
          [& more]
          `(with-redefs [cats.context/infer infer]
             (m/->= ~@more))))

;TODO delete this function
(defn lift-a*
  [x ys]
  (casep ys
         empty? x
         (recur (<*> x (first ys)) (rest ys))))

(defn lift-a
  [f]
  (fn [& more]
    ;TODO use apply <*>
    (lift-a* (<$> (curry (count more) f) (first more)) (rest more))))

;TODO delete this function after cats.core is fixed
(defn ap
  [m1 m2]
  ;TODO use >>= and <$>
  (mlet [x1 m1
         x2 m2]
        (return (x1 x2))))

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
