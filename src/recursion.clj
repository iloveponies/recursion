(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (nil? (my-last (rest coll))) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (nil? (max-element (rest a-seq))) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
   (last (sort-by count [seq-1 seq-2])))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) '()
   :else (if (pred? (first a-seq))
           (cons (first a-seq) (my-filter pred? (rest a-seq)))
           (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (= (first a-seq) elem) true
   (empty? a-seq) false
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) ()
   :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   :else (and (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1 (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (< how-many-times 1) ()
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0) '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [result (rest (map concat (tails a-seq) (inits a-seq)))]
    (if (empty? result) (seq ['()]) result)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (let [fe (first a-seq) new-freqs (assoc freqs fe (inc (or (get freqs fe) 0)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[key, freq]] (repeat freq key)) a-map)))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0)) '() (concat [(first coll)] (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n) coll (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [sp (int (/ (count a-seq) 2))]
    (seq [(my-take sp a-seq) (my-drop sp a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq))
   (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

;there is a bug here, when I wake up and drink my coffee will fix it.
;fixed!

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) '()
   (= (count a-seq) 1) a-seq
   :else (let [[l r] (halve a-seq)]
           (seq-merge (merge-sort l) (merge-sort r)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn rec-powerset [pas fut]
  (if (empty? fut) #{pas}
   (clojure.set/union
    (rec-powerset (conj pas (first fut)) (rest fut))
    (rec-powerset pas (rest fut)))))

(defn powerset [a-set]
  (rec-powerset #{} a-set))
