(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (let [vector-of-collection (vec coll)]
    (cond
      (empty? vector-of-collection) false
      (empty? (drop 1 vector-of-collection)) true
      :else false)))

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
      (max-element (cons (max (first a-seq) (second a-seq)) (drop 2 a-seq))))))

(defn seq-max [seq-1 seq-2]
    (let [length-of-seq-1 (count seq-1)
          length-of-seq-2 (count seq-2)]
            (if (> length-of-seq-1 length-of-seq-2)
              seq-1
              seq-2)))

 (defn longest-sequence [a-seq]
   (let [length-of-coll (count a-seq)
         longer-of-first-two (seq-max (first a-seq) (second a-seq))
         hh (fn find-longest [seq-a]
              (if (= 2 (count seq-a))
                (seq-max (first seq-a) (second seq-a))
                (find-longest (assoc (into [] (rest seq-a)) 0 (seq-max (first seq-a) (second seq-a))))))]
     (cond
       (= 0 length-of-coll) nil
       (= 1 length-of-coll) (first a-seq)
       (= 2 length-of-coll) longer-of-first-two
       (< 2 length-of-coll) (hh a-seq))))

 (defn my-filter [pred? a-seq]
      (if (empty? a-seq)
        a-seq
        (if (pred? (first a-seq))
          (cons (if pred? (first a-seq)
                  (first a-seq))
                (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
      false
      (if (= elem (first a-seq))
        true
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [recurse (fn list-where-true [seq-a list-of-succesfuls]
                    (if (pred? (first seq-a))
                      (list-where-true (rest seq-a) (conj list-of-succesfuls (first seq-a)))
                      (reverse list-of-succesfuls)))]
    (if (empty? a-seq)
      ()
      (recurse a-seq ()))))

(defn my-drop-while [pred? a-seq]
   (let [drop-until (fn list-undropped [seq-a undropped]
                      (if (pred? (first seq-a))
                        (list-undropped (rest seq-a) (rest undropped))
                        undropped))]
     (if (empty? a-seq)
       ()
       (drop-until a-seq a-seq))))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (cond
      (or (and (empty? a-seq) (not (empty? b-seq) )) (and (empty? b-seq) (not (empty? a-seq)))) false
      (and (= (first a-seq) (first b-seq))) (seq= (rest a-seq) (rest b-seq))
      :else false)))

(defn my-map [f seq-1 seq-2]
  (let [map-f-over-seqs-1-and-2 (fn mapping-function
                                  [function-to-be-mapped seq-a seq-b return-list]
                                  (if (or (empty? seq-a) (empty? seq-b))
                                    (reverse return-list)
                                    (mapping-function
                                     function-to-be-mapped
                                     (rest seq-a)
                                     (rest seq-b)
                                     (conj return-list (function-to-be-mapped (first seq-a) (first seq-b))))))]

    (map-f-over-seqs-1-and-2 f seq-1 seq-2 ())))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (let [repeat-this (fn repeating-function [repeats-left f collection-of-repeated-things] (cond
                          (< repeats-left 0) ()
                          (= repeats-left 0) collection-of-repeated-things
                          :else (repeating-function
                                  (dec repeats-left)
                                  what-to-repeat
                                  (conj collection-of-repeated-things what-to-repeat))))]

    (repeat-this how-many-times what-to-repeat ())))

(defn my-range [up-to]
    (if (= up-to 0)
      ()
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
   (let [tail-function (fn tails-f
                         [sequence-to-tail
                          length-of-seq
                          iteration-counter
                          tail]
                           (cond
                             (empty? sequence-to-tail) 0
                               (= length-of-seq iteration-counter) tail
                               :else (tails-f
                                       sequence-to-tail
                                       length-of-seq
                                       (inc iteration-counter)
                                       (cons (drop iteration-counter sequence-to-tail) tail))))]
     (if (empty? a-seq)
       [a-seq]
       (reverse (cons () (tail-function a-seq (count a-seq) 0 ()))))))

(defn inits [a-seq]
   (let [init-function (fn init-f
                         [sequence-to-init
                          length-of-seq
                          iteration-counter
                          init]
                           (cond
                             (empty? sequence-to-init) 0
                               (= length-of-seq iteration-counter) init
                               :else (init-f
                                       sequence-to-init
                                       length-of-seq
                                       (inc iteration-counter)
                                       (cons (drop-last iteration-counter sequence-to-init) init))))]
     (if (empty? a-seq)
       [a-seq]
         (cons () (init-function a-seq (count a-seq) 0 ())))))

(defn rotations [a-seq]
    (let [get-rotations (fn rot
                          [sequence-to-rotate
                           length-of-seq
                           counter-of-iterations
                           collection-of-rotations]
                           (cond
                             (empty? sequence-to-rotate) (into () (vector ()))
                             (=  length-of-seq counter-of-iterations) collection-of-rotations
                             :else (rot
                                      (cons
                                        (last sequence-to-rotate)
                                        (take (dec length-of-seq)
                                              sequence-to-rotate))
                                      length-of-seq
                                      (inc counter-of-iterations)
                                      (cons (cons (last sequence-to-rotate) (take (dec length-of-seq) sequence-to-rotate)) collection-of-rotations))))]

      (get-rotations a-seq (count a-seq) 0 ())))

(defn my-frequencies-helper [freqs a-seq]

  (let [different-items (into () (set a-seq))
        amount-of-differents (count different-items)
        get-list-of-differents (fn get-list [amount-of-differents
                                             iterator
                                             seq-a
                                             collection-of-differents]
                                               (if (= amount-of-differents iterator)
                                                 seq-a
                                                 (get-list
                                                   amount-of-differents
                                                   (inc iterator)
                                                   (assoc seq-a (nth different-items iterator) 0)
                                                    different-items)))

        list-of-different-items (get-list-of-differents
                                    amount-of-differents
                                    0
                                    freqs
                                    different-items)

        get-frequencies (fn recourse [number-of-differents
                                      iterator
                                      seq-1
                                      list-of-differents
                                      collection-of-freq-pairs]
                                       (if (= number-of-differents iterator)
                                         collection-of-freq-pairs
                                         (recourse
                                           number-of-differents
                                           (inc iterator)
                                           seq-1
                                           list-of-differents
                                           (assoc
                                             collection-of-freq-pairs
                                             (nth list-of-differents iterator)
                                             (count (filter (fn [x] (= x (nth list-of-differents iterator))) a-seq))))))]

    (get-frequencies amount-of-differents 0 a-seq different-items list-of-different-items)))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [de-freq (fn recourse [counter
                              iterator
                              lista
                              numbers
                              items]
                                (if (= counter iterator)
                                  lista
                                  (recourse
                                    counter
                                    (inc iterator)
                                    (conj lista
                                      (repeat
                                        (nth numbers iterator)
                                        (nth items iterator)))
                                      numbers
                                      items)))]

    (flatten (de-freq (count (keys a-map)) 0 () (vals a-map) (keys a-map)))))

(defn my-take [n coll]
  (drop-last (- (count coll) n) coll))

(defn my-drop [n coll]
  (if (<= (- (count coll) n) 0)
    ()
    (take-last (- (count coll) n) coll)))

(defn halve [a-seq]
  (let [half-of-seq-length (int (/ (count a-seq) 2))
        first-half (my-take half-of-seq-length a-seq)
        last-half (my-drop half-of-seq-length a-seq)]
    (vector first-half last-half)))

(defn seq-merge [a-seq b-seq]
  (let [combine (fn [acc a-seq b-seq]
                  (if (and (empty? b-seq) (empty? a-seq))
                    acc
                    (cond
                      (empty? a-seq) (recur (conj acc (first b-seq)) a-seq (rest b-seq))
                      (empty? b-seq) (recur (conj acc (first a-seq)) (rest a-seq) b-seq)
                      (<= (first a-seq) (first b-seq)) (recur (conj acc (first a-seq)) (rest a-seq) b-seq)
                      (>= (first a-seq) (first b-seq)) (recur (conj acc (first b-seq)) a-seq (rest b-seq)))))]

    (reverse (combine () a-seq b-seq))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (singleton? a-seq) a-seq
    :else (seq-merge
            (merge-sort (first (halve a-seq)))
            (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
