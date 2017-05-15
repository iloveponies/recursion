(ns recursion)

(defn product [coll]
  ;; more concise
  ;;   (reduce (fn [a b] (* a b)) 1 coll))
  (if (empty? coll)
    1
    (let [a (first coll)
          rest (rest coll)]
      (if rest
        (* a (product rest))
        a))))

(defn singleton? [coll]
  ;; more complexity just for the sake of reduce
  ;; (simpler would be better)
  ;; (= 1 (reduce (fn [a b] (inc a)) 0 coll))
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  ;; using loop-recur
  ;; (loop [[val & rest] coll]
  ;;  (if rest
  ;;    (recur rest)
  ;;    val))
  (let [[a & rest] coll]
    (if rest
      (my-last rest)
      a)))

(defn max-element [a-seq]
  (let [[a & rest] a-seq]
    (if rest
      (max a (max-element rest))
      a)))

(defn seq-max [seq-1 seq-2]
  (first (sort-by count #(>= %1 %2) [seq-1 seq-2])))

(defn longest-sequence [a-seq]
  (let [[a & rest] a-seq]
    (if rest
      (seq-max a (longest-sequence rest))
      a)))

(defn my-filter [pred? a-seq]
  (into (empty a-seq)
        (let [a (first a-seq)
              rest (rest a-seq)]
          (if (empty? rest)
            (if (pred? a) (list a) '())
            (if (pred? a)
              (cons a (my-filter pred? rest))
              (my-filter pred? rest))))))

(defn sequence-contains? [elem a-seq]
  (let [a (first a-seq)
        rest (rest a-seq)]
    (if (empty? rest)
      (= a elem)
      (if (= a elem)
        true
        (sequence-contains? elem rest)))))

(defn my-take-while [pred? a-seq]
  (try
    (let [a (first a-seq)
             rest (rest a-seq)]
         (if (empty? rest)
           (if (pred? a) (list a) '())
           (if (pred? a)
             (if (pred? (first rest))
               (cons a (my-take-while pred? rest))
               (list a))
             '())))
    (catch Exception e '())))

(defn my-drop-while [pred? a-seq]
  ;; compositional solution
  ;;(keep-indexed
  ;;  (fn [a b] (when (> a (dec (count (my-take-while pred? a-seq)))) b))
  ;;  a-seq)

  ;; basic recursion
  (let [a (first a-seq)
        rest (rest a-seq)
        try-pred? #(try (pred? %) (catch Exception e false))]
    (if (empty? rest)
      (if (try-pred? a) '() (list a))
      (if (try-pred? a)
        (my-drop-while pred? rest)
        a-seq))))

(defn seq= [a-seq b-seq]
  (let [[a & a-rest] a-seq
        [b & b-rest] b-seq]
    (if (= (count a-seq) (count b-seq))
      (if (and (empty? a-rest)
               (empty? b-rest))
        (= a b)
        (if (= a b)
          (seq= a-rest b-rest)
          false))
      false)))

(defn my-map [f seq-1 seq-2]
  (let [[val-1 & rest-1] seq-1
        [val-2 & rest-2] seq-2]
    (if (or (not rest-1) (not rest-2))
      (if (not-any? nil? [val-1 val-2])
        (list (apply f [val-1 val-2]))
        '())
      (cons (apply f [val-1 val-2]) (my-map f rest-1 rest-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (= 1 n) 1
        :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (or (neg? how-many-times) (zero? how-many-times)) '()
    (= 1 how-many-times) (list what-to-repeat)
    (pos? how-many-times) (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (let [n (dec up-to)]
      (if (zero? n)
        (list n)
        (cons n (my-range n))))))

(defn tails [a-seq]
  (let [[_ & rest] a-seq]
    (if (nil? rest)
      (if (empty? a-seq)
        '()
        (list (seq a-seq) '()))
      (cons (seq a-seq) (tails rest)))))

(defn inits [a-seq]
  (let [rest (pop a-seq)]
    (if (empty? rest)
      (list (seq a-seq) '())
      (cons (seq a-seq) (inits rest)))))

(defn rotations [a-seq]
  ;; composing with core fns
  ;; (I gave up :p)
  (if (empty? a-seq)
    (list '())
    (let [n (count a-seq)]
      (-> (partition n 1 (apply concat (repeat n a-seq)))
        set
        seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [[a & rest] a-seq]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper
        (update-in freqs [a] #(if (nil? %) 1 (inc %)))
        rest))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [map-entry (first a-map)
        rest (rest a-map)
        v (key map-entry)
        n (val map-entry)]
    (if (empty? rest)
      (repeat n v)
      (concat (repeat n v) (un-frequencies rest)))))

(defn my-take [n coll]
  (let [[a & rest] coll]
    (cond (<= n 0) '()
          (not rest) (list a)
          :else (cons a (my-take (dec n) rest)))))


(defn my-drop [n coll]
  (let [[_ & rest] coll]
    (cond (<= n 0) coll
          (not rest) '()
          :else (my-drop (dec n) rest))))

(defn halve [a-seq]
  (if (empty? a-seq)
    []
    (let [n (int (/ (count a-seq) 2))]
      [(my-take n a-seq) (my-drop n a-seq)])))

(defn seq-merge [a-seq b-seq]
  (let [[b & rest-b] b-seq]
    (cond
      (empty? b-seq) a-seq
      (empty? a-seq) b-seq
      :else (let [index (.lastIndexOf a-seq b)]
              (if (= -1 index)
                  ;; not found
                  (let [prefix-coll (my-take-while #(< % b) a-seq)]
                    (seq-merge (concat prefix-coll (list b) (my-drop (count prefix-coll) a-seq)) rest-b))
                  ;; found
                  (seq-merge (concat (my-take index a-seq) (list b) (my-drop index a-seq)) rest-b))))))

(defn merge-sort [a-seq]
  (cond (empty? a-seq) a-seq
        (= 1 (count a-seq)) a-seq
        :else (let [[a b] (halve a-seq)]
                (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (cond
    (empty? a-seq) (do (prn a-seq) a-seq)
    :else (let [monotonic-coll (vec (last (filter #(or (apply < %) (apply > %)) (filter not-empty (sort-by count (inits a-seq))))))]
            (cons monotonic-coll (split-into-monotonics (vec (drop (count monotonic-coll) a-seq)))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

