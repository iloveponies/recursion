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




