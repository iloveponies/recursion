(ns recursion)
;E1
(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;E2 ...

;E3
(defn singleton? [coll]
    (cond
     (empty? coll) false
     (empty? (rest coll)) true
     :else false))

;E4
(defn single-or-empty [coll]
  (or (singleton? coll) (empty? coll)))

(defn my-last [coll]
  (if (single-or-empty coll)
    (first coll)
    (my-last (rest coll))))
;E5
(defn max-element [a-seq]
  (if (single-or-empty a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

;E6
(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))
;E7
(defn longest-sequence [a-seq]
  (if (single-or-empty a-seq)
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))



;E8
(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else  (my-filter pred? (rest a-seq))))

;E9
(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

;E10
(defn my-take-while [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond (empty? a-seq) ()
          (pred? f) (cons f (my-take-while pred? r))
          :else '() )))


;E11
(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))
;E12
(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (not (= (first a-seq) (first b-seq))) false
        :else (seq= (rest a-seq) (rest b-seq))))
;E13
(defn mmtool [f a b]
  (seq [(f (first a) (first b) )]))

(defn my-map [f a-seq b-seq]
   (cond (or (empty? a-seq) (empty? b-seq)) []
         (or (singleton? a-seq) (singleton? b-seq)) (mmtool f a-seq b-seq)
         :else (cons (f (first a-seq) (first b-seq))
                     (my-map f (rest a-seq) (rest b-seq)))))

;E14
(defn power [n k]
  (cond (= 0 k) 1
        (= 1 k) n
        (< k 0) (/ (power n (- k)))
        :else (* n (power n (- k 1)))))


;E15
(defn fib [n]
  (cond (= 0 n) 0
        (<= n 2) 1
        :else (+ (fib (- n 2)) (fib (- n 1)))))

;E16
(defn my-repeat [n x]
   (cond  (<= n 0) []
          (= n 1) [x]
          :else (cons x (my-repeat (- n 1) x))))

;E17
(defn my-range [n]
  (let [x (- n 1)]
   (cond  (< n 1) []
          (= n 1) [x]
          :else (cons x (my-range x)))))

;E18
(defn tails [a-seq]
  (let [ s (seq a-seq) ]
    (cond (empty? a-seq) '([])
          :else (cons s (tails (rest a-seq))))
  ))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

;E19
(defn rotations [a-seq]
  (if (empty? a-seq) '([])
    (let [i (inits a-seq)
          t (reverse (tails a-seq))
          fi (first i)
          ft (first t)]
      (rest (my-map concat t i)))))
(rotations '(1 2))

;E20
(defn my-frequencies-helper [freqs a-seq]
  (if (or (nil? a-seq) (empty? a-seq)) freqs
  (let [w (first a-seq)
        r (rest  a-seq)
        c (if (contains? freqs w) (inc (get freqs w)) 1)
        nfreqs (assoc freqs w c)]
    (my-frequencies-helper nfreqs r))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

;E21
(defn un-frequencies [a-map]
  (let [x a-map
        y (first x)
        z (rest x)
        f (if (or (nil? x) (empty? x)) [] (my-repeat (val y) (key y)) )]
    (cond (or (nil? x) (empty? x)) []
          (singleton? x) f
          :else (concat f (un-frequencies z)))
    ; f
  ))

;E22
(defn my-take [n coll]
  (let [x (first coll)
        r (rest coll)]
    (cond  (<= n 0) []
           (or (empty? coll) (nil? coll)) []
           (= n 1) [x]
           :else (cons x (my-take (- n 1) r)))))
; E23
(defn my-drop [n coll]
  (let [x (first coll)
        r (rest coll)]
    (cond  (or (empty? coll) (nil? coll)) []
           (= n 0) coll
           :else (my-drop (- n 1) r))))
; E24
(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

;E25
(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)
        ar (rest a-seq)
        br (rest b-seq)]
    (cond (or (empty? a-seq) (nil? a-seq)) b-seq
          (or (empty? b-seq) (nil? b-seq)) a-seq
          (< a b) (cons a (seq-merge ar b-seq))
          :else (cons b (seq-merge a-seq br)))))



;E26
(defn merge-sort [a-seq]
  (if (or (nil? a-seq) (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a)
                 (merge-sort b)))))
;E27
(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) ()
    (let [init (rest (reverse (inits a-seq)))
          a    (last (my-take-while (fn [x] (= (last x) (max-element x))) init))
          b    (last (my-take-while (fn [x] (= (first x) (max-element x))) init))
          c    (seq-max a b)
          n    (count c)
          r    (my-drop n a-seq)]
      (concat [c] (split-into-monotonics r))
      )))

;E28
(defn permutations
  [a-set]
  (cond
   (empty? a-set) [()]
   (vector? a-set)
   (if (= 1 (count a-set))
     (list [(a-set 0)])
     (loop [i 0
            permutaatiot '()]
       (if (= i (count a-set))
         permutaatiot
         (let [muut (into [] (concat (my-take i a-set) (my-drop (inc i) a-set)))
               muu-muut (permutations muut)
               uudet-muut (map #(conj % (a-set i)) muu-muut)]
           (recur (inc i) (into permutaatiot uudet-muut))))))
   :else (permutations (into [] a-set))))


;E29
(defn powerset [a-set]
  (if (empty? a-set)
    '(())
    (let [ps (powerset (rest a-set))]
    (concat ps
            (map #(conj % (first a-set)) ps)))))


