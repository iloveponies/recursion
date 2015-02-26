(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))
;; (product [1 2 4])
;; (product (cons 1 (cons 2 (cons 4 []))))
;; (* 1 (product (cons 2 (cons 4 []))))
;; (* 1 (* 2 (product (cons 4 []))))
;; (* 1 (* 2 (* 4 (product []))))
;; (* 1 (* 2 (* 4 1)))
;; (* 1 (* 2 4))
;; (* 1 8)
;; 8

(defn singleton? [coll]
  (== 1 (count coll)))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (defn helper [largest coll]
    (if (empty? coll)
      largest
      (helper (max largest (first coll)) (rest coll))))
  (if (empty? a-seq)
    nil
    (helper (first a-seq) (rest a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (defn helper [longest coll]
    (if (empty? coll)
      longest
      (helper (seq-max longest (first coll)) (rest coll))))
  (if (empty? a-seq)
    nil
    (helper (first a-seq) (rest a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    (not (pred? (first a-seq))) (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (or (and (empty? a-seq) (not (empty? b-seq)))
        (and (not (empty? a-seq)) (empty? b-seq))) false
    (or (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== 0 n)
    0
    (if (== 0 k)
      1
      (* n (power n (- k 1))))))

(defn fib [n]
  (cond 
    (== 0 n) n
    (== 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (not (number? up-to)) '()
    (<= up-to 0) '()
    ;;(== 0 up-to) '(0)
    :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [ranges (range 0 (count a-seq))
          drops (map (fn [x] (drop x a-seq)) ranges)
          takes (map (fn [x] (take x a-seq)) ranges)]
      (map concat drops takes))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper (update-in freqs [(first a-seq)] inc) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (concat (repeat (first (rest (first a-map))) (first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (== 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (== 0 n) (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (if (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq)))
               (merge-sort (first (rest (halve a-seq)))))))

(defn split-into-monotonics [a-seq]
  (defn ordered-seq [orig-seq tail-seq order]
    (if (or (empty? tail-seq)
            (empty? orig-seq))
      '()
      (concat [(first orig-seq)] (if (order (first orig-seq) (first tail-seq))
                                   (ordered-seq (rest orig-seq) (rest tail-seq) order)
                                   '()))))
  (if (empty? a-seq)
    '()
    (if (> (count a-seq) 2)
      (let [seq-le (ordered-seq a-seq (rest a-seq) <=)
            temp-seq-le (drop (count seq-le) seq-le)
            seq-ge (ordered-seq temp-seq-le (rest temp-seq-le) >=)
            temp-seq-ge (drop (count seq-ge) seq-ge)]
        (if (not (empty? seq-le))
          (cons seq-le (split-into-monotonics (drop (count seq-le) a-seq)))
          (cons seq-ge (split-into-monotonics (drop (count seq-ge) a-seq)))))
      (cons a-seq '()))))

(defn permutations [a-set]
  (defn helper [prefix suffix]
    (if (empty? suffix)
      prefix
      (flatten (map (fn [i] (helper (concat prefix (nth suffix i))
                                    (concat (take i suffix)
                                            (drop (inc i) suffix))))
                    (range 0 (count suffix))))))
  (defn take-counts [a-list]
    (if (empty? a-list)
      a-list
      (cons (take (count a-set) a-list)
            (take-counts (drop (count a-set) a-list)))))
  (if (empty? a-set)
    '(())
    (take-counts (helper '() (map list a-set)))))

(defn powerset [a-set]
  [:-])

  ;; public static void permutation(String str) { 
  ;;     permutation("", str); 
  ;; }
  
  ;; private static void permutation(String prefix, String str) {
  ;;     int n = str.length();
  ;;     if (n == 0) System.out.println(prefix);
  ;;     else {
  ;;         for (int i = 0; i < n; i++)
  ;;             permutation(prefix + str.charAt(i), str.substring(0, i) + str.substring(i+1, n));
  ;;     }
  ;; }    

  ;; above Java code copied from
  ;; http://stackoverflow.com/questions/4240080/generating-all-permutations-of-a-given-string
