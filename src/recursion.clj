(ns recursion)

(defn product [coll]
  (if (empty? coll)
   1
   (* (first coll)
      (product (rest coll)))))
  

(defn singleton? [coll]
  (cond
    (empty? coll) false
      (empty? (rest coll)) true
      :else false))


(defn my-last [coll]
  (if (empty? (rest coll))
   (first coll)
   (my-last (rest coll))))
  

(defn max-element [a-seq]
  (if (empty? a-seq)
   nil
   (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
 (let [c1 (count seq-1)
       c2 (count seq-2)]
  (if (< c2 c1)
   seq-1
   seq-2)))


(defn longest-sequence [a-seq]
 (let [f (first a-seq)
       s (second a-seq)]
  (if (empty? (rest a-seq))
   (first a-seq)
   (longest-sequence (rest (assoc (vec a-seq) 1 (vec (seq-max f s))))))))

(defn my-filter [pred? a-seq]
 (if (empty? a-seq)
  (seq '())
  (if (pred? (first a-seq))
   (cons (first a-seq) (my-filter pred? (rest a-seq)))
   (reduce  conj [] (seq(my-filter pred? (rest a-seq)))))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) '()
    (pred? (first a-seq)) (cons (first a-seq) 
                           (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   (not (pred? (first a-seq))) a-seq))
  

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) 
                                     (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= 0 k) 1
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond 
   (>= 0 how-many-times) '()
   :else (cons what-to-repeat 
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (>= 0 up-to) '()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
       (cond 
        (empty? a-seq) (conj a-seq '())
        :else (conj (tails (rest a-seq)) a-seq)))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotate [n a-seq]
 (cond
    (>= 0 n) '()
    :else (cons (concat (rest a-seq) [(first a-seq)])
           (rotate (dec n) (concat (rest a-seq) [(first a-seq)])))))
 

(defn rotations [a-seq]
 (let [c (count a-seq)]
  (cond
   (= 0 c) (cons a-seq '())
   :else (rotate c a-seq))))
      
(defn my-frequencies-helper [freqs a-seq]
 (let [f (first a-seq)]
  (if (empty? a-seq)
   freqs
   (my-frequencies-helper(assoc freqs (first a-seq) (inc(get freqs (first a-seq) 0)))
    (rest a-seq)))))
   
(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn my-un-frequencies-helper [unfreqs a-map]
 (let [[k v] (first a-map)]
  (if (empty? a-map)
   unfreqs
   (my-un-frequencies-helper (concat unfreqs (repeat v k))
     (rest a-map)))))
  
(defn un-frequencies [a-map]
  (my-un-frequencies-helper {} a-map))

(defn my-take [n coll]
  (let [f (first coll)
        r (rest coll)
        decn (dec n)]
   (cond 
    (= 0 n) '()
    (empty? coll) '()
    :else (cons f (my-take decn r)))))

(defn my-drop [n coll]
  (let [r (rest coll)
        decn (dec n)]
   (cond 
    (= 0 n) coll
    (empty? coll) '()
    :else (my-drop decn r))))

(defn halve [a-seq]
  (let [c (count a-seq)
        h (int(/ c 2))
        fhalf (my-take h a-seq)
        shalf (my-drop h a-seq)]
   (vector fhalf shalf)))

(defn seq-merge [a-seq b-seq]
  (let [fa (first a-seq)
        fb (first b-seq)
        ra (rest a-seq)
        rb (rest b-seq)]
       (cond 
        (and (empty? a-seq) (empty? b-seq)) '()
        (empty? a-seq) (cons fb (seq-merge a-seq rb))
        (empty? b-seq) (cons fa (seq-merge ra b-seq))
        (<= fa fb) (cons fa (seq-merge ra b-seq))
        :else (cons fb (seq-merge a-seq rb)))))
  

(defn merge-sort [a-seq]
  (let [[fh sh] (halve a-seq)
        cf (count fh)
        cs (count sh)]
   (cond
    (and (empty? fh) (empty? sh)) '()
    (and (= 1 cf) (= 1 cs)) (seq-merge fh sh)
    (= 1 cf) (seq-merge fh (merge-sort sh))
    (= 1 cs) (seq-merge (merge-sort fh) sh)
    :else (seq-merge (merge-sort fh) (merge-sort sh)))))
  




(defn mon [pred a-seq]
 (loop [seq a-seq
        cur []]
       (cond
        (empty? seq) [seq cur]
        (and (not(empty? cur)) (pred (last cur) (first seq))) [seq cur]
        :else (recur (rest seq) (conj cur (first seq))))))
   
 

(defn next-monotonic [a-seq]
 (let [[seq desc] (mon < a-seq)
       [seq incr] (mon >= a-seq)
       c1 (count desc)
       c2 (count incr)]
      (if (<= c1 c2)
       [seq incr]
       [seq desc])))

(defn split-into-monotonics [a-seq]
  (loop [seq a-seq
         mon []]
        (cond 
         (empty? seq) (if (> (apply + (mapv count mon)) (count a-seq))
                       (reverse (drop 1 (reverse mon)))
                       mon)        
         :else (let [[new-seq mono] (next-monotonic seq)]
                (recur new-seq (conj mon mono)))))) 
         
        
(defn permutations [a-set])


(defn powerset [a-set]
  [:-])

