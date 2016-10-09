(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (let [r (rest coll)]
    (and (not (empty? coll)) (empty? r))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [f (first a-seq)
          max-of-rest (max-element (rest a-seq))]
      (cond
        (nil? f) max-of-rest
        (nil? max-of-rest) f
        (> f max-of-rest) f
        :else max-of-rest))))

(defn seq-max [seq-1 seq-2]
  (let [len-1 (count seq-1)
        len-2 (count seq-2)]
    (if (> len-1 len-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [f (first a-seq)
          len-f (count f)
          longest-r (longest-sequence (rest a-seq))
          len-r (count longest-r)]
      (if (> len-f len-r)
        f
        longest-r))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (pred? f)
        (cons f (my-filter pred? r))
        (my-filter pred? r)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (not (pred? (first a-seq))) '()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))


(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [helper (fn h-f [res-seq rem-elem cur-seq]
                 (if (empty? rem-elem)
                   res-seq
                   (h-f (cons cur-seq res-seq)
                        (rest rem-elem)
                        (concat (rest cur-seq) (list (first cur-seq))))))]
    (if (empty? a-seq)
       '(())
       (helper '() a-seq a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          rest-seq (rest a-seq)]
      (if (contains? freqs elem)
        (my-frequencies-helper (assoc freqs elem (inc (get freqs elem))) rest-seq)
        (my-frequencies-helper (assoc freqs elem 1) rest-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[e c] (first a-map)]
      (concat (my-repeat c e) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (empty? coll) '()
    (> n 0) (cons (first coll) (my-take (dec n) (rest coll)))
    :else '()))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (> n 0) (my-drop (dec n) (rest coll))
    :else coll))

(defn halve [a-seq]
  (let [half-len (int (/ (count a-seq) 2))]
    [(my-take half-len a-seq) (my-drop half-len a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (<= a b) (cons a (seq-merge (rest a-seq) b-seq))
     :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (singleton? a-seq) (list (first a-seq))
    :else (let [[fh sh] (halve a-seq)]
            (seq-merge (merge-sort fh) (merge-sort sh)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonics? (fn [a-seq]
                        (or (empty? a-seq)
                            (singleton? a-seq)
                            (apply <= a-seq)
                            (apply >= a-seq)))
          first-mono-seq (last (take-while monotonics? (inits a-seq)))
          n (count first-mono-seq)]
      (cons first-mono-seq (split-into-monotonics (drop n a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (mapcat (fn [e]
              (let [ss (disj (set a-set) e)]
                (map (fn [s] (cons e s))
                     (permutations ss))))
            a-set)))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [e (first a-set)
          ss (disj (set a-set) e)
          pss (powerset ss)]
      (clojure.set/union pss
             (map (fn [s] (conj s e)) pss)))))

