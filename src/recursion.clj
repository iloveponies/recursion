(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
    (boolean (seq coll)) ;; idiomatic for collection emptiness
    (empty? (rest coll))))

(defn my-last [coll]
  (let [fst (first coll)
        rst (rest  coll)]
    (if (empty? rst)
      fst
      (my-last rst))))

(defn max-element [a-seq]
  (let [fst (first a-seq)
        rst (rest  a-seq)]
    (if (empty? rst)
      fst
      (max fst (max-element rst)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1 seq-2))

(defn longest-sequence [a-seq]
  (let [fst (first a-seq)
        fst-count (count (first a-seq))
        rst (rest a-seq)]
    (if (empty? rst)
      fst
      (let [longest-sequence-rest (longest-sequence rst)]
        (if (> fst-count (count longest-sequence-rest))
          fst
          longest-sequence-rest)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [my-filter-others (my-filter pred? (rest a-seq))]
      (if (pred? (first a-seq)) 
        (cons (first a-seq) my-filter-others)
        my-filter-others))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (or (= elem (first a-seq))
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (or 
        (empty? a-seq)
        (not (pred? (first a-seq))))
    []
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (let [fst-a (first a-seq)
        fst-b (first b-seq)
        rst-a (rest a-seq)
        rst-b (rest b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) true
      (or  (empty? a-seq) (empty? b-seq)) false
      :else (and 
              (= fst-a fst-b)
              (seq= rst-a rst-b)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (if (= how-many-times 1)
      [what-to-repeat]
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)))))

(defn my-range [up-to]
  (cond 
    (<= up-to 0) ()
    (= up-to 1)  [0]
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn take-until 
  ([m-seq]
    (take-until m-seq nil))
  ([m-seq stop-at]
    (cond
      (empty? m-seq) ()
      (= (first m-seq) stop-at) (seq [(first m-seq)])
      :else (cons (first m-seq) (take-until (rest m-seq) stop-at)))))

(defn inits [a-seq]
  (cons () 
        (map
          (fn [element] (take-until a-seq element))
          a-seq)))

(defn tails [a-seq]
  (let [reversed (reverse a-seq)]
  (cons ()
        (map
          (fn [element] (reverse (take-until reversed element)))
          a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (seq [()])
  (map
    (fn [element]
      (concat (rest (my-drop-while #(not (= % element)) a-seq))
              (take-until a-seq element)))
    a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [el (first a-seq)
          cnt (get freqs el)
          new-cnt (if cnt (inc cnt) 1)]
      (my-frequencies-helper (assoc freqs el new-cnt) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [k (get (first a-map) 0)
          v (get (first a-map) 1)]
      (concat (un-frequencies (rest a-map))
              (repeat v k)))))

(defn my-take [n coll]
  (cond 
    (or (empty? coll) (<= n 0)) '() 
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '() 
    (= n 0) (seq coll)
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [m (int (/ (count a-seq) 2))]
    [(my-take m a-seq) (my-drop m a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '() 
    (and (not-empty a-seq) (not-empty b-seq)) (let [a1 (first a-seq)
                                                     b1 (first b-seq)]
                                                  (cond
                                                    (<= a1 b1) (cons a1 (seq-merge (rest a-seq) b-seq))
                                                    :else      (cons b1 (seq-merge a-seq (rest b-seq)))))
    (not-empty a-seq) (cons (first a-seq)
                            (seq-merge (rest a-seq) b-seq))
    (not-empty b-seq) (cons (first b-seq)
                            (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [halves (halve a-seq)
          left   (get halves 0)
          right  (get halves 1)]
      (cond
        (and (empty? left) (empty? right)) a-seq
        (and (not-empty left) (not-empty right)) (seq-merge (merge-sort left) (merge-sort right))
        (not-empty left) (merge-sort left) 
        (not-empty right) (merge-sort right)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    (set a-seq)
    (let [monotonic? (fn [a-seq] 
                       (or (apply <= a-seq) (apply >= a-seq)))
          split-monotonic (fn [prefix suffix]
                            (if (monotonic? prefix)
                              (list prefix suffix)
                              (recur (butlast prefix)
                                     (cons (last prefix) suffix))))
       [prefix suffix] (split-monotonic a-seq '())]
     (cons prefix (split-into-monotonics suffix)))))

(defn permutations [a-set]
  (let [ perms (fn [[ f & r] ]
                 (map (fn [e] (cons f e)) (permutations r)))]
    (cond
      (empty? a-set)
        (cons a-set a-set)
      (= 1 (count a-set))
        (list a-set)
      :else
        (apply concat (map perms (rotations a-set))))))

(defn powerset-inner [a-set acc]
  (if (empty? a-set)
    acc
      (powerset-inner (rest a-set) 
                      (concat acc
                              (map
                                #(conj % (first a-set))
                                acc)))))
(defn powerset [a-set]
  (powerset-inner a-set #{#{}}))

