(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (let [rest-coll (rest coll)]
    (if (empty? rest-coll)
      (first coll)
      (my-last rest-coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first-element (first a-seq)]
      (if (pred? first-element)
        (cons first-element (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (let [up-to-minus-1 (- up-to 1)]
      (cons up-to-minus-1 (my-range up-to-minus-1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map (fn [n _] (concat (drop n a-seq) (take n a-seq)))
         (range 0 (count a-seq))
         a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key (first a-seq)
          current-value (get freqs key)
          new-value (if (nil? current-value)
                      1
                      (inc current-value))
          new-freqs (assoc freqs key new-value)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (flatten (concat (map (fn [[k v]] (repeat v k)) a-map))))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (== n 0))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [first-half-count (int (/ (count a-seq) 2))]
    [(my-take first-half-count a-seq)
     (my-drop first-half-count a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [first-a (first a-seq)
        first-b (first b-seq)]
    (cond
     (nil? first-a) b-seq
     (nil? first-b) a-seq
     (< first-a first-b) (cons first-a (seq-merge (rest a-seq) b-seq))
     :else (cons first-b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (empty? (rest a-seq)))
    a-seq
    (let [[fst, snd] (halve a-seq)]
      (seq-merge (merge-sort fst) (merge-sort snd)))))

(defn monotonic-helper? [pred a-seq]
  (let [[fst snd _] (take 2 a-seq)]
    (if (or (nil? fst) (nil? snd))
      true
      (and (pred fst snd) (monotonic-helper? pred (rest a-seq))))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (let [[fst snd _] (take 2 a-seq)]
      (if (< fst snd)
        (monotonic-helper? < a-seq)
        (monotonic-helper? > a-seq)))))

(defn split-into-monotonics [a-seq]
  (let [prefixes (inits a-seq)
        longest-monotonic (first (drop-while (complement monotonic?) prefixes))
        count-longest (count longest-monotonic)]
    (if (== count-longest 0)
      longest-monotonic
      (cons longest-monotonic
            (split-into-monotonics (drop count-longest a-seq))))))

(defn permutations [a-set]
  (let [permutations-helper
        (fn [n-th a-seq]
          (let [n-th-elem (nth a-seq n-th)
                rest-set (disj (set a-seq) n-th-elem)]
            (mapv (fn [x] (into [n-th-elem] x)) (permutations rest-set))))]
    (cond
     (empty? a-set) '([])
     (== (count a-set) 1) [[(first a-set)]]
     (== (count a-set) 2) (let [fst (first a-set)
                                snd (second a-set)]
                            [[fst snd] [snd fst]])
     :else (apply concat (mapv
                          (fn [x] (permutations-helper x (seq a-set)))
                          (range 0 (count a-set)))))))

(defn powerset-helper [elem a-set]
  (map (fn [x] (conj x elem)) a-set))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [first-elem (first a-set)
          rest-set (disj (set a-set) first-elem)
          powerset-rest (powerset rest-set)]
      (clojure.set/union
       powerset-rest
       (powerset-helper first-elem powerset-rest)))))

