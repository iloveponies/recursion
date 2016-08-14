(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq)
               (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
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
    (and (empty? a-seq)
         (empty? b-seq)) true
    (or (empty? a-seq)
        (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))


(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1)
        (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n
       (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1) '()
    (= how-many-times 1) (list what-to-repeat)
    :else (cons what-to-repeat
                (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (< up-to 1) '()
    (= up-to 1) '(0)
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list ())
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list ())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [add-to-map (fn [elem a-map]
          (if (contains? a-map elem)
            (assoc a-map elem (inc (get a-map elem)))
            (assoc a-map elem 1)))]
      (add-to-map (first a-seq)
                  (my-frequencies-helper freqs (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [print-keys (fn [a-seq]
                     (repeat (second a-seq)
                             (first a-seq)))]
    (if (empty? a-map)
      ()
      (concat (print-keys (first a-map))
              (un-frequencies (rest a-map))))))

(defn my-take-helper [n coll taken-coll]
  (if (or (= (count taken-coll) n)
          (empty? coll))
    taken-coll
    (my-take-helper n
                    (rest coll)
                    (conj taken-coll (first coll)))))

(defn my-take [n coll]
  (reverse (my-take-helper n coll '())))

(defn my-drop-helper [n coll k]
  (if (= k n)
    coll
    (my-drop-helper n (rest coll) (inc k))))

(defn my-drop [n coll]
  (my-drop-helper n coll 0))

(defn halve [a-seq]
  (let [mid-point (int (/ (count a-seq) 2))]
    (vector (my-take mid-point a-seq) (my-drop mid-point a-seq))))

(defn seq-merge-helper [a-seq b-seq merged]
  (cond
    (and (empty? a-seq)
         (empty? b-seq)) []
    (empty? a-seq) (concat merged b-seq)
    (empty? b-seq) (concat merged a-seq)
    :else (if (<= (first a-seq) (first b-seq))
            (seq-merge-helper (rest a-seq) b-seq (conj merged (first a-seq)))
            (seq-merge-helper a-seq (rest b-seq) (conj merged (first b-seq))))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq []))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (seq-merge (merge-sort (first (halve a-seq)))
               (merge-sort (second (halve a-seq))))))

(defn monotonic+? [a-seq]
  (cond
    (empty? a-seq) true
    (singleton? a-seq) true
    (<= (first a-seq) (second a-seq)) (monotonic+? (rest a-seq))
    :else false))

(defn monotonic-? [a-seq]
  (cond
    (empty? a-seq) true
    (singleton? a-seq) true
    (>= (first a-seq) (second a-seq)) (monotonic-? (rest a-seq))
    :else false))

(defn monotonic? [a-seq]
  (or (monotonic+? a-seq)
      (monotonic-? a-seq)))

(defn split-into-monotonics-helper [a-seq monotonics]
  (let [mono-inits (take-while monotonic? (reverse (inits a-seq)))
        n (count mono-inits)]
    (if (monotonic? a-seq)
      (list a-seq)
      (cons (first (drop (dec n) mono-inits))
            (split-into-monotonics-helper (my-drop (dec n) a-seq) monotonics)))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper a-seq '()))

(defn permutations-helper-helper [element a-set]

  "helper function to put an element between each elems in list,
  including at the start and the end
  example: :a (1 2) => (:a 1 2) (1 :a 2) (1 2 :a)"

  (let [put-elem-to-ith (fn [elem a-list]
                          (fn [i]
                            (concat (take i a-list)
                                    (cons elem
                                          (drop i a-list)))))]
    (map (put-elem-to-ith element a-set) (range (inc (count a-set))))))

(defn permutations-helper [a-set perms]
  (let [helper (fn [elem]
                 (fn [a-list]
                   (permutations-helper-helper elem a-list)))]
    (cond
      (empty? a-set) '(())
      (singleton? a-set) (list (list (first a-set)))
      :else (apply concat (map (helper (first a-set)) (permutations-helper (rest a-set) perms))))))

(defn permutations [a-set]
  (permutations-helper a-set '()))

(defn powerset [a-set]
  (set
    (clojure.set/union
      (map set
        (apply concat
          (map inits
            (permutations a-set)))))))

