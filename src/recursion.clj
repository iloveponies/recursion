(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll)      
      nil
    (empty? (rest coll)) 
      (first coll)
    :else              
      (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)     
      nil
    (singleton? a-seq) 
      (first a-seq)
    :else              
      (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq)     
      nil
    (singleton? a-seq) 
      (first a-seq)
    :else              
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)        
      a-seq
    (pred? (first a-seq)) 
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else                 
      (my-filter pred? (rest a-seq))))


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         
      false
    (= (first a-seq) elem) 
      true
    :else                  
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        
      a-seq
    (pred? (first a-seq)) 
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else                 
      ()))


(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) 
      a-seq
    (pred? (first a-seq)) 
      (my-drop-while pred? (rest a-seq))
    :else 
      a-seq))

(defn seq= [a-seq b-seq]
  (let [do-seq= (fn [a b]
                  (cond
                    (and (empty? a) (empty? b))
                      true
                    (= (first a) (first b))
                      (recur (rest a) (rest b))
                   :else
                     false))]
    (if (== (count a-seq) (count b-seq))
      (do-seq= a-seq b-seq)
      false)))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    () 
    (cons 
      (f (first seq-1) (first seq-2)) 
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (== n 0) 0
    (== k 0) 1
    :else    (* n (power n (dec k)))))
      

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (<= how-many-times 0) ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))
    

(defn my-range [up-to]
  (if 
    (== up-to 0) ()
    (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [do-inits (fn [s acc]
                   (if (empty? s)
                     (cons [] acc)
                     (recur (rest s) (cons (reverse s) acc))))]
    (do-inits (reverse a-seq) [])))

(defn rotations [a-seq]
  (let [do-rotations (fn [s acc num-rotations]
                       (if (== 0 num-rotations)
                         acc
                         (recur 
                           (concat (rest s) [(first s)])
                           (concat  acc (list s))
                           (dec num-rotations))))]
    (if (empty? a-seq)
      [[]] 
      (do-rotations a-seq () (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem     (first a-seq)
          old-freq (get freqs elem 0)]
      (my-frequencies-helper 
        (assoc freqs elem (inc old-freq)) 
        (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    () 
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (== n 0)
    coll
    (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
  (let [length (count a-seq)
        s      (int (/ length 2))]
    (list 
      (my-take s a-seq) 
      (my-drop s a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
  (cond
    (empty? a-seq) 
      b-seq
    (empty? b-seq)
      a-seq
    (<= a b)
      (cons a (seq-merge (rest a-seq) b-seq))
    :else 
      (cons b (seq-merge a-seq (rest b-seq))))))  

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (singleton? a-seq)
      a-seq
    :else
      (let [[h1 h2] (halve a-seq)]
        (seq-merge
          (merge-sort h1)
          (merge-sort h2)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [a-seq-inits (drop 1 (inits a-seq)) ; drop the empty init
          monotonic-lt (last (take-while #(apply < %) a-seq-inits))
          monotonic-gt (last (take-while #(apply > %) a-seq-inits))
          monotonic (if (> (count monotonic-lt) (count monotonic-gt))
                      monotonic-lt
                      monotonic-gt)]
      (concat 
        [monotonic]
        (split-into-monotonics (drop (count monotonic) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

