(defn inc-first [nums]
  (if (first nums)
    ; If there's a first number, build a new list with cons
    (cons (inc (first nums))
          (inc-first (rest nums)))
    ; If there's no first number, just return an empty list
   ))



(defn inc-first [nums]
  (if (first nums)
    ; If there's a first number, build a new list with cons
    (cons (inc (first nums))
          (rest nums))
    ; If there's no first number, just return an empty list
    (list)))


(inc-first [])

(inc-first [1 2 3])

(defn expand [f x]
  (lazy-seq (cons x (expand f (f x)))))

(take 10 )

(iterate)


(take 5 (iterate (fn [x] (str x "o")) "y"))

(take 5 (repeatedly (fn [] "1")))


(map (fn [n vehicle] (str "I've got " n " " vehicle "s"))
     [0 200 9]
     ["car" "train" "kiteboard"])

(map (fn [index element] (str index ". " element))
            (iterate inc 0)
            ["erlang" "ruby" "haskell"])


(interpose [1 2 3] (take 4 (repeat ",")))
("," [1 2 3] "," [1 2 3] ",")

(interleave [1 2 3] (take 3 (repeat ",")))
(1 "," 2 "," 3 ",")


(defn my-group-by [f coll]
  (reduce
   (fn [ret x]
     (let [k (f x)]
       (assoc ret k (conj (get ret k []) x))))
   {}
   coll))


(defn my-map [f coll]
  (reduce (fn [ret x]
           (conj ret (f x)))
          []
          coll))

([pred coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (when (pred (first s))
          (cons (first s) (take-while pred (rest s)))))))


(defn my-take-while [pred coll]
  (when-let [s (seq coll)]
    (when (pred (first s))
      (cons (first s) (take-while pred (rest s))))))



(my-take-while pos? [2 1 0 -1 0 1 2]) 

(def infseq (map inc (iterate inc 0)))



(reduce + (take 1000 (map (fn [[a b]] (* a b))
                          (partition 2 1 (filter odd? (iterate inc 0))))))

(->> 0
     (iterate inc)
     (filter odd?)
     (partition 2 1)
     (map (fn [[a b]] (* a b)))
     (take 1000)
     (reduce +))


(defn is-palindroom? [s]
  (= s (apply str (reverse s))))



(defn my-filter [pred coll]
  (when-let [s (seq coll)]
    (let [f (first coll) r (rest coll)]
      (if (pred f)
        (cons f (filter pred r))
        (filter pred r)))))


(filter prime?  (iterate inc 0))







(def prime-numbers
  ((fn f [x]
     (cons x
           (lazy-seq
            (f (first
                (drop-while
                 (fn [n]
                   (some #(zero? (mod n %))
                         (take-while #(<= (* % %) n) prime-numbers)))
                 (iterate inc (inc x))))))))
   2))

(def prime-numbers
  ((fn f [x]
     (cons x
           (lazy-seq
            (f (first
                (drop-while
                 (fn [n]
                   (some #(zero? (mod n %))
                         (take-while #(<= (* % %) n) prime-numbers)))
                 (iterate inc (inc x))))))))
   2))




(def prime-numbers
  ((fn f [x]
     (cons x
           (lazy-seq
            (first
             (drop-while
              (fn []
                (some #(zero? (mod n %))
                      (take-while #(<= (* % %) n) prime-numbers)))
              (iterate inc (inc x))))))))
  2)



(defmacro ignore
  [expr]
  nil)

(defmacro rev [fun & args]
  (cons fun (reverse args)))

(let [x 2] `(inc x))
(let [x 2] `(inc ~x))

(let [x 2] (list 'clojure.core/inc x))

`(foo ~@[1 2 3])



(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
      `(let [or# ~x]
         (if or# or# (or ~@next)))))


(pprint (macroexpand '(or a b c d)))


(let*
 [or__4469__auto__ a]
 (if or__4469__auto__ or__4469__auto__ (clojure.core/or b c d)))


`(let [x# 2] x#)

(def x 0)


(macroexpand )

(eval `(let [~'x 2] ~'x))



(pprint (clojure.walk/macroexpand-all
                 '(or (mossy? stone) (cool? stone) (wet? stone))))





(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
      `(let [or# ~x]
         (if or# or# (or ~@next)))))

(let*
 [or__4469__auto__ (mossy? stone)]
 (if
  or__4469__auto__
  or__4469__auto__
  (let*
   [or__4469__auto__ (cool? stone)]
   (if or__4469__auto__ or__4469__auto__ (wet? stone)))))


(if true
  1
  2)

(defmacro my-while [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))


(def x 0)

(my-while (< x 5)
          (prn x)
          (def x (inc x)))

(pprint (macroexpand '(my-while (< x 5)
                         (prn x)
                         (def x (inc x)))))


(pprint (source loop))



(defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  {:added "1.0", :special-form true, :forms '[(loop [bindings*] exprs*)]}
  [bindings & body]
    (assert-args
      (vector? bindings) "a vector for its binding"
      (even? (count bindings)) "an even number of forms in binding vector")
    (let [db (destructure bindings)]
      (if (= db bindings)
        `(loop* ~bindings ~@body)
        (let [vs (take-nth 2 (drop 1 bindings))
              bs (take-nth 2 bindings)
              gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
              bfs (reduce1 (fn [ret [b v g]]
                            (if (symbol? b)
                              (conj ret g v)
                              (conj ret g v b g)))
                          [] (map vector bs vs gs))]
          `(let ~bfs
             (loop* ~(vec (interleave gs gs))
               (let ~(vec (interleave bs gs))
                 ~@body)))))))




(source interleave)


(defmacro cond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time.  If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn't evaluate any of the
  other tests or exprs. (cond) returns nil."
  {:added "1.0"}
  [& clauses]
    (when clauses
      (list 'if (first clauses)
            (if (next clauses)
                (second clauses)
                (throw (IllegalArgumentException.
                         "cond requires an even number of forms")))
            (cons 'clojure.core/cond (next (next clauses))))))

(macroexpand '(cond
                (> 3 2) 
                :else 'ype))

(if (> 3 2)
  :else
  (clojure.core/cond (quote ype)))

(source condp)





(defmacro condp
  {:added "1.0"}
  [pred expr & clauses]
  (let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (fn emit [pred expr args]
               (let [[[a b c :as clause] more]
                       (split-at (if (= :>> (second args)) 3 2) args)
                       n (count clause)]
                 (cond
                  (= 0 n) `(throw (IllegalArgumentException. (str "No matching clause: " ~expr)))
                  (= 1 n) a
                  (= 2 n) `(if (~pred ~a ~expr)
                             ~b
                             ~(emit pred expr more))
                  :else `(if-let [p# (~pred ~a ~expr)]
                           (~c p#)
                           ~(emit pred expr more)))))]
    `(let [~gpred ~pred
           ~gexpr ~expr]
       ~(emit gpred gexpr clauses))))




(pprint (macroexpand '(condp <= wind-speed
                 70 :F5
                 58 :F4
                 49 :F3
                 42 :F2
                 :F1)))

(let*
 [pred__12369 <= expr__12370 wind-speed]
 (if
  (pred__12369 70 expr__12370)
  :F5
  (if
   (pred__12369 58 expr__12370)
   :F4
   (if
    (pred__12369 49 expr__12370)
    :F3
    (if (pred__12369 42 expr__12370) :F2 :F1))))))

(defmacro condp
  {:added "1.0"}
  [pred expr & clauses]
  (let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (fn emit [pred expr args]
               (let [[[a b c :as clause] more]
                       (split-at (if (= :>> (second args)) 3 2) args)
                       n (count clause)]
                 (cond
                  (= 0 n) `(throw (IllegalArgumentException. (str "No matching clause: " ~expr)))
                  (= 1 n) a
                  (= 2 n) `(if (~pred ~a ~expr)
                             ~b
                             ~(emit pred expr more))
                  :else `(if-let [p# (~pred ~a ~expr)]
                           (~c p#)
                           ~(emit pred expr more)))))]
    `(let [~gpred ~pred
           ~gexpr ~expr]
       ~(emit gpred gexpr clauses))))

(pprint (source case))



























(defmacro case 
  [e & clauses]
  (let [ge (with-meta (gensym) {:tag Object})
        default (if (odd? (count clauses)) 
                  (last clauses)
                  `(throw (IllegalArgumentException. (str "No matching clause: " ~ge))))]
    (if (> 2 (count clauses))
      `(let [~ge ~e] ~default)
      (let [pairs (partition 2 clauses)
            assoc-test (fn assoc-test [m test expr]
                         (if (contains? m test)
                           (throw (IllegalArgumentException. (str "Duplicate case test constant: " test)))
                           (assoc m test expr)))
            pairs (reduce1
                   (fn [m [test expr]]
                     (if (seq? test)
                       (reduce1 #(assoc-test %1 %2 expr) m test)
                       (assoc-test m test expr)))
                   {} pairs)
            tests (keys pairs)
            thens (vals pairs)
            mode (cond
                   (every? #(and (integer? %) (<= Integer/MIN_VALUE % Integer/MAX_VALUE)) tests)
                   :ints
                   (every? keyword? tests)
                   :identity
                   :else :hashes)]
        (condp = mode
          :ints
          (let [[shift mask imap switch-type] (prep-ints tests thens)]
            `(let [~ge ~e] (case* ~ge ~shift ~mask ~default ~imap ~switch-type :int)))
          :hashes
          (let [[shift mask imap switch-type skip-check] (prep-hashes ge default tests thens)]
            `(let [~ge ~e] (case* ~ge ~shift ~mask ~default ~imap ~switch-type :hash-equiv ~skip-check)))
          :identity
          (let [[shift mask imap switch-type skip-check] (prep-hashes ge default tests thens)]
            `(let [~ge ~e] (case* ~ge ~shift ~mask ~default ~imap ~switch-type :hash-identity ~skip-check))))))))



(defn my-sum [numbers]
  (if-let [f (first numbers)]
    (+ f (my-sum (next numbers)))
    0))

(defn my-sum
  ([nums] (my-sum 0 nums))
  ([ret nums]
   (if-let [f (first nums)]
     (recur (+ ret f) (next nums))
     ret)))


(for [n (range 10)]
  n)



(pprint (source for))

(defn integers [x]
  (lazy-seq
   (cons x (integers (inc x)))))

(def xs (integers 0))

(pprint (source lazy-seq))

(for [x [1 2 3]
      y [:a :b]]
  [y x])

(for [x     (range 5)
      y     (range 5)
      :when (and (even? x) (odd? y))
      :let [z (inc x)]
     ]
  [x y z])

([0 1 1] [0 3 1] [2 1 3] [2 3 3] [4 1 5] [4 3 5])

([0 1 1] [0 3 1] [2 1 3] [2 3 3])

(pprint (clojure.walk/macroexpand-all 
         '(->> (range 10) (filter odd?) (reduce +))))


(source ->)x


(defmacro ->  
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

;; problems

(defn schedule
  [hour]
  (condp >= hour
    8 :breakfast
    12 :dinner
    18 :supper))

(defmacro id
  [f & args]
  `(~f ~@args))
(def logging-enabled nil)

(defmacro log
  [key]
  `(if logging-enabled
     (prn :hi)))



