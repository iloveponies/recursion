(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  (if
    (and (not (nil? (first coll)))
         (nil? (second coll)))
    true false))


(defn my-last [coll]
  (if (= (second coll) nil)
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  (if (nil? (second a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))




(defn seq-max [seq-1 seq-2]
  (let [length1 (count seq-1)
        length2 (count seq-2)]
    (if (> length1 length2)
      seq-1
      seq-2)))


(defn longest-sequence [a-seq]
  (if (nil? (second a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))


(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq))
     (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))



(defn sequence-contains? [elem a-seq]
  (let [x (first a-seq)]
    (cond
     (= elem x) true
     (= nil (second a-seq)) false
     :else (sequence-contains? elem (rest a-seq)))))



(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (not (pred? (first a-seq))) ()
   :else (if (pred? (first a-seq))
           (cons (first a-seq) (my-take-while pred? (rest a-seq)))
           (my-take-while pred? (rest a-seq)))))



(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) ()
  (if (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (not (== (count a-seq) (count b-seq))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))


(defn my-map-helper [f seq-1 seq-2 current-map]
  (cond
     (or
      (nil? (first seq-1))
      (nil? (first seq-2)))
     current-map
   :else
   (my-map-helper f
                  (rest seq-1)
                  (rest seq-2)
                  (conj current-map
                        (f (first seq-1)
                           (first seq-2))))))


(defn my-map [f seq-1 seq-2]
  (my-map-helper f seq-1 seq-2 []))







(defn power [n k]
  (if (zero? k) 1
    (*
       n
       (power n (dec k)))))

(defn fib [n]
  (cond

   (== n 0) 0
   (== n 1) 1

   :else (+
          (fib (dec n))
          (fib (dec (dec n))))))



(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) ()
    (concat
       (list what-to-repeat)
       (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if
   (== up-to 0) ()
     (reverse (sort (concat
      (list (dec up-to))
      (my-range (dec up-to)))))))



(defn tails [a-seq]
  (if (empty? a-seq) (conj a-seq [])
    (cons a-seq (tails (rest a-seq)))))


(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [a-seq tail-length]
  (let [init
        (filter (fn[x]
                  (== (count x)
                      (- (count a-seq) tail-length)))
                (inits a-seq))
        tail
        (filter (fn [x]
                  (== (count x)
                      tail-length))
                (tails a-seq))]
    (cond
     (== tail-length 0) init
     :else (concat
            (map concat tail init)
            (rotations-helper a-seq (dec tail-length))))))

(defn rotations [a-seq]
  (if (== (count a-seq) 0) [[]]
  (rotations-helper a-seq (dec (count a-seq)))))




(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
  (let [poistettava (first a-seq)]
    (my-frequencies-helper (assoc freqs poistettava
                            (count (filter
                                    (fn[x] (= x poistettava))
                                    a-seq)))
                           (remove (fn[x] (= x poistettava)) a-seq)))))



(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies-helper [a-map freq-seq]
  (if (empty? a-map) freq-seq
    (un-frequencies-helper (dissoc
                            a-map
                            (key (first a-map)))

                           (concat
                            freq-seq
                            (repeat (val (first a-map)) (key (first a-map)) )))))


(defn un-frequencies [a-map]
  (un-frequencies-helper a-map []))


(defn my-take-helper [n coll taken]
  (if (or
       (== n 0)
       (empty? coll))
    taken

    (my-take-helper
     (dec n)
     (rest coll)
     (conj taken (first coll)))))



(defn my-take [n coll]
  (my-take-helper n coll []))


(defn my-drop-helper [n coll taken]
  (if (or
       (<= n 0)
       (empty? coll))
    taken

    (my-drop-helper
     (dec n)
     (butlast coll)
     (conj taken (last coll)))))


(defn my-drop [n coll]
  (reverse (my-drop-helper (- (count coll) n) coll [])))




(defn halve [a-seq]
  (let [takelen (int (/ (count a-seq) 2))
        droplen takelen]
    (concat
     (list (my-take takelen a-seq))
     (list (my-drop droplen a-seq)))))


(defn seq-merge-helper [a-seq b-seq merge-seq]
  (cond
   (empty? a-seq) (concat merge-seq b-seq)
   (empty? b-seq) (concat merge-seq a-seq)
   (> (first a-seq) (first b-seq)) (seq-merge-helper
                                   (rest b-seq)
                                   a-seq
                                   (conj merge-seq (first b-seq)))
   :else (seq-merge-helper
          b-seq
          (rest a-seq)
          (conj merge-seq (first a-seq)))))




(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq []))



(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1) a-seq
  (let [puolikkaat (halve a-seq)]
    (seq-merge
     (merge-sort (first puolikkaat))
     (merge-sort (second puolikkaat))))))



(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])












