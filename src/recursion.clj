(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (not (empty? coll))
    (empty? (rest coll))
    false))

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
       (if (singleton? (rest a-seq))
        (max (first a-seq) (second a-seq))
        (max (first a-seq) (max-element (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1)(count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (if (singleton? (rest a-seq))
        (seq-max (first a-seq)(second a-seq))
        (seq-max (first a-seq)(longest-sequence (rest a-seq)))))))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq)(my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))


(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq)(my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq)(empty? b-seq)) true
   (or (empty? a-seq)(empty? b-seq)) false
   (= (first a-seq)(first b-seq)) (seq= (rest a-seq)(rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
    (if (or (empty? seq-1)(empty? seq-2))
      ()
      (cons (f (first seq-1)(first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== 0 n) 0
   (== 1 n) 1
   :else
     (+ (fib (- n 1))(fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== 0 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq) (tails (rest a-seq)))))


(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))



(defn rotate [b-seq n]
  (if (< 0 n)
    (cons (seq b-seq)
          (rotate (seq (conj (apply vector (rest b-seq)) (first b-seq))) (dec n)))
    '()))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotate a-seq (count a-seq))))

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn without-duplicates [item coll]
  (remove  (fn [x] (= item  x)) coll))

(defn my-frequencies-helper [freqs a-seq]
  (let [freq-elem (cons (first a-seq)(cons (count-elem-helper 0 (first a-seq) a-seq) '()))]
    (let [rm-dup (fn [item coll] (remove (fn [x] (= item x)) coll))]
      (if (empty? a-seq)
        {}
      (conj {(first freq-elem)(second freq-elem)}
            (my-frequencies-helper {} (rm-dup (first freq-elem) a-seq)))))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    {}
    (concat (repeat (second (first a-map))(first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (== 0 n)(empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (<= (count coll) n)
    '()
    (if (== 0 n)
      (seq coll)
      (my-drop (dec n)(rest coll)))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq)(my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
 (if (empty? a-seq)
   (if (empty? b-seq)
     '()
     b-seq)
   (if (empty? b-seq)
      a-seq
      (if (<(first a-seq)(first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    '()
    (if (singleton? a-seq)
      a-seq
      (seq-merge (merge-sort (first (halve a-seq)))
                 (merge-sort (second (halve a-seq)))))))

;_________________________split-into-monotonics_____________________________________________

(defn one-mono-seq-length [coll length]
  (if (empty? coll)
    length
    (if (singleton? coll)
      (inc length)
      (if (< (second coll) (first coll))
        (inc length)
        (one-mono-seq-length (rest coll) (inc length))))))


 (defn split-into-monotonics [a-seq]
   (let [monolen (one-mono-seq-length a-seq 0)]
   (if (empty? a-seq)
     '()
      (cons (my-take monolen a-seq)(split-into-monotonics (my-drop monolen a-seq))))))

;___________________________________________________________________________________________

 (defn pair [ a-coll b-coll]
 (map a-coll b-coll))

(defn coll-merge [a-coll b-coll]
  (apply conj a-coll b-coll))
;  (apply a-coll b-coll)))

(defn permutations [a-set]
  (cond
   (empty? a-set) '()
   (singleton? a-set) a-set
   (singleton? (rest a-set)) (cons a-set (reverse a-set))
   :else    (pair (permutations (first (halve a-set)))
                  (permutations (second (halve a-set))))))

(defn swap-adjacent [coll pos]
  (if (empty? coll)
    []
    (if (> pos (count coll))
      []
      (if (== 0 pos)
        (if (singleton? coll)
          (vector (first coll)[])
          (apply vector (second coll) (first coll)(nthrest coll 2)[]))
        (apply vector (first coll) (swap-adjacent (rest coll) (dec pos)))))))



(defn call-swap [coll i]
    (if (empty? coll)
      '() 
      (if (< i (count coll))
     (cons (seq coll) (seq (call-swap (swap-adjacent coll i) (inc i))))
     (cons vector coll '()))))

;_______________________________________________________powerset____________________________

(defn to-bin [number bit-count]
  (if (== bit-count 0)
    '()
    (cons (bit-test number (dec bit-count)) (to-bin number (dec bit-count)))))



(defn bin-range [bit-count]
  (let [conv-to-bin (fn [x] (to-bin x bit-count))]
    (map conv-to-bin (range 0 (power 2 bit-count)))))


(defn map-truthy [original bin-row]
  (if (empty? original)
    []
   (cons (if (first bin-row)(first original) nil)
          (map-truthy (rest original) (rest bin-row)))))
   ; (filter bin-row original)))

(defn map-one [original bin-row]
 (apply hash-set (filter identity (apply concat (filter (fn [x](not (empty? x))) [(map-truthy original bin-row)])))))

(defn map-one2 [original bin-row]
  (apply hash-set (map first (filter second (map hash-set bin-row original)))))

(defn powerset2 [coll]
   (apply hash-set (map (fn [x] (map-one2 coll x))(bin-range (count coll)))))
(defn plus [x y]
  (+ x y 5))

(defn powerset [coll]
   (apply hash-set (map (fn [x] (map-one coll x))(bin-range (count coll)))))
