(ns recursion)

(defn product [coll]
  (let [[f & r] (seq coll)]
    (if (empty? coll) 1 (* f (product r)))))

(defn singleton? [coll]
  (let [[f & r] (seq coll)]
    (and ((complement empty?) coll)
         (empty? r))))

(defn my-last [coll]
  (let [[f & r] (seq coll)]
   (cond 
     (empty? coll) nil
     (singleton? coll) f 
     :else (my-last r))))

(defn max-element [a-seq]
  (let [[f & r]  (seq a-seq)] 
    (cond 
      (empty? a-seq) nil 
      (singleton? a-seq) f 
      :else (max f (max-element r)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (let [[f & r] (seq a-seq)]
    (cond
      (empty? a-seq) nil 
      (singleton? a-seq) f 
      :else (seq-max f (longest-sequence r)))))

(defn my-filter [pred? a-seq]
  (let [[f & r] (seq a-seq)]
    (cond
      (empty? a-seq) () 
      (pred? f) (cons f (my-filter pred? r))
      :else (my-filter pred? r))))

(defn sequence-contains? [elem a-seq]
  (let [[f & r] (seq a-seq)]
    (cond
      (empty? a-seq) false 
      (= f elem) true
      :else (sequence-contains? elem r))))

(defn my-take-while [pred? a-seq]
  (let [[f & r] (seq a-seq)]
    (cond
      (empty? a-seq) ()
      (pred? f) (cons f (my-take-while pred? r))
      :else ())))

(defn my-drop-while [pred? a-seq]
  (let [[f & r] (seq a-seq)]
    (cond 
      (empty? a-seq) ()
      (pred? f) (my-drop-while pred? r)
      :else (seq a-seq))))

(defn seq= [a-seq b-seq]
  (let [[fa & ra] (seq a-seq)
        [fb & rb] (seq b-seq)]
    (cond 
      (not= (count a-seq) (count b-seq)) false
      (and (empty? a-seq) (empty? b-seq)) true
      (= fa fb) (seq= ra rb)
      :else false)))

(defn my-map [f seq-1 seq-2]
  (let [[f1 & r1] (seq seq-1)
        [f2 & r2] (seq seq-2)]
    (cond 
      (or (nil? f1) (nil? f2)) ()
      :else (cons (f f1 f2) (my-map f r1 r2)))))

(defn power [n k]
  (if (> k 0) (* n (power n (dec k))) 1))

(defn fib [n]
  (cond 
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0) 
    (cons 
      what-to-repeat 
      (my-repeat 
        (dec how-many-times) 
        what-to-repeat))
    ()))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons 
      (dec up-to) 
      (my-range (dec up-to)))
    ()))

(defn tails [s]
  (if (seq s) 
    (cons (seq s) (tails (rest s))) 
    '(())))

(defn inits [s]
  (if (seq s) 
    (cons (seq s) (inits (reverse (rest (reverse s)))))
    '(())))

(defn rotations [s]
  (let [i (rest (reverse (inits s)))
        t (rest (tails s))]
    (if (seq s) 
      (map concat t i) 
      '(()))))

(defn counter [n el coll]
  (let [c (if (= el (first coll)) (inc n) n)]
    (if (seq coll)
      (counter c el (rest coll))
      n)))

(defn my-frequencies-helper [freqs a-seq]
  (let [el (first a-seq)
        f (if (contains? freqs el) 
            freqs
            (assoc freqs el (counter 0 el a-seq)))]
    (if (seq a-seq)
      (my-frequencies-helper f (rest a-seq)) 
      freqs)))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [res a-map]
 (let [[k v] (first a-map)
       r (rest a-map)
       d (concat res (repeat v k))]
    (if (empty? a-map)
      res
      (un-frequencies-helper d r))))
 
(defn un-frequencies [a-map]
  (un-frequencies-helper () a-map))
  
(defn my-take [n [f & r :as c]]
  (if (= n 0)
    ()
    (cons f (my-take 
              (dec (min n (count c))) 
              r))))

(defn my-drop [n [f & r :as c]]
  (cond 
    (>= n (count c)) ()
    (= n 0) (seq c) 
    :else (my-drop (dec n) r)))

(defn halve [a-seq]
  (let [len (count a-seq)
        l (int (/ len 2))
        r (- (- len l) (mod len 2))]
    [(my-take l a-seq) (my-drop r a-seq)]))

(defn ins [k [f & r :as s]]
  (if (seq s)
    (if (seq r)
      (if (> k f)
        (cons f (ins k r))
        (concat [k f] r))
      (if (> k f)
        [f k]
        [k f]))
    [k]))

(defn seq-merge [[f & r :as a-seq] b-seq]
  (let [nb (ins f b-seq)]
    (if (seq r)
      (seq-merge r nb)
      nb)))

(defn merge-sort [a-seq]
  (let [[a b] (halve a-seq)]
    (if (< (count a-seq) 2) 
      a-seq
      (seq-merge 
        (merge-sort a) 
        (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (let [ins (rest (reverse (inits a-seq)))
        raw (map #(if (and (apply < %) (> (count %) 1)) % nil) ins)
        mon (last (filter seq raw))
        nxt (drop (count mon) a-seq)]
    (if (seq mon) 
      (concat [mon] (split-into-monotonics nxt))
      (if (seq nxt) [nxt] ()))))

(defn mix [n m k]
  (let [l (take k m)
        r (drop k m)
        nxt (concat l [n] r)]
    (if (= k 0)
      (conj () nxt)
      (cons nxt (mix n m (dec k))))))

(defn permutations [[f & r :as s]]
  (if (empty? s) 
    '(())
    (concat [] (mapcat #(mix f % (count r)) (permutations r)))))

(defn p-helper [[f & r :as s]]
  (let [res (if (seq r) (p-helper r) {})] 
    (if (empty? s) 
      [#{}] 
      (concat [#{} #{f}] (map #(conj % f) res) res))))

(defn powerset [a-set]
  (set (p-helper (seq a-set))))
