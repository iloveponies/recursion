(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (= (take 1 coll) (take 2 coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
         seq-1
         seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (not (pred? (first a-seq))) '()
        :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (not (= (first a-seq) (first b-seq))) false
        :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (= n 1) 1
        :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [inits-reversed (tails (reverse a-seq))]
    (reverse (map reverse inits-reversed))))

(defn rotate-helper [a-seq rotations]
  (if (>= 1 rotations)
    [a-seq]
    (cons a-seq (rotate-helper (concat (rest a-seq) (vector (first a-seq))) (dec rotations)))))

(defn rotations [a-seq]
  (rotate-helper a-seq (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-freqs (if (contains? freqs elem)
                       (assoc freqs elem (inc (get freqs elem)))
                       (assoc freqs elem 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map repeat (vals a-map) (keys a-map))))

(defn my-take-helper [n new-coll coll]
  (if (or (zero? n)
          (empty? coll))
    new-coll
    (cons (first coll) (my-take-helper (dec n) new-coll (rest coll)))))

(defn my-take [n coll]
  (my-take-helper n [] coll))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (or (empty? a-seq)
           (empty? b-seq))
    (concat a-seq b-seq)
    (if (> (first a-seq) (first b-seq))
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))))


(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn operator-helper [a-seq]
  (let [first-term (first a-seq)
        rest-seq (drop-while (fn [x] (= x first-term)) a-seq)]
    (if (empty? rest-seq)
      =
      (if (<= first-term (first rest-seq))
        <=
        >=))))

(defn longest-monotonic [a-seq mono-seq operator]
  (if (empty? a-seq)
    (seq mono-seq)
    (if (empty? mono-seq)
      ; add first num in a-seq to mono-seq
      (let [operator (operator-helper a-seq)]
        (longest-monotonic (drop 1 a-seq) [(first a-seq)] operator))
      (if (operator (last mono-seq) (first a-seq))
        (longest-monotonic (drop 1 a-seq) (conj mono-seq (first a-seq)) operator)
        (seq mono-seq)))))

(defn monotonic-helper [a-seq mono-seqs]
  (if (empty? a-seq)
    mono-seqs
    (let [longest-mono-seq (longest-monotonic a-seq [] nil)
          rest-seq (drop (count longest-mono-seq) a-seq)
          new-mono-seqs (cons longest-mono-seq mono-seqs)]
      (monotonic-helper rest-seq new-mono-seqs))))

(defn split-into-monotonics [a-seq]
  (if (<= (count a-seq) 2)
    [a-seq] ; sequences with two or fewer elements are always monotonic
    (reverse (monotonic-helper a-seq []))))

(defn permutations-helper [a-set perms-seq]
  (if (empty? a-set)
    [perms-seq]
    (for [elem a-set
          solution (permutations-helper (disj a-set elem) (conj perms-seq elem))]
        solution)))

(defn permutations [a-set]
  (permutations-helper (into #{} a-set) []))

(defn powerset-helper [a-set all-sets]
  (if (empty? a-set)
    #{all-sets}
    (for [elem a-set
          solution (powerset-helper (disj a-set elem) (conj all-sets a-set))]
      solution)))

(defn powerset [a-set]
  (conj (apply clojure.set/union (powerset-helper (into #{} a-set) #{})) #{}))
