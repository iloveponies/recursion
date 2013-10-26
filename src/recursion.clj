(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
  )
)

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)
  )
)

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (= (my-last(rest coll)) nil)
      (first coll)
      (my-last (rest coll))
    )
  )
)

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (if (<= (max (first a-seq) (max-element (rest a-seq))) (first a-seq))
        (first a-seq)
        (max-element (rest a-seq))
      )
    )
  )
)

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1)
)

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (= (seq-max (first a-seq) (longest-sequence (rest a-seq))) (first a-seq))
      (first a-seq)
      (longest-sequence (rest a-seq))
    )
  )
)

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
    )
  )
)

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)            false
    (= (first a-seq) elem)    true
    :else                     (sequence-contains? elem (rest a-seq))
  )
)

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)         a-seq
   (pred? (first a-seq))  (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else                  ()
  )
)

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)             a-seq
    (pred? (first a-seq))      (my-drop-while pred? (rest a-seq))
    :else                      a-seq
   )
)

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))  true
    (and (or (empty? a-seq) (empty? b-seq))
         (or ((complement empty?) a-seq) ((complement empty?) b-seq))
    )                                    false
    (= (first a-seq) (first b-seq))      (seq= (rest a-seq) (rest b-seq))

    :else                                false
   )
)

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))
  )
)

(defn power [n k]
  (cond
    (= k 0)       1
    :else         (* n (power n (dec k)))
  )
)

(defn fib [n]
  (cond
    (== n 0)       0
    (== n 1)       1
    :else          (+ (fib (dec n)) (fib (dec(dec n))))
  )
)

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1)     ()
    :else                    (cons what-to-repeat(my-repeat (dec how-many-times) what-to-repeat) )
  )
)

(defn my-range [up-to]
  (cond
    (<= up-to 0)       ()
    :else              (cons (dec up-to) (my-range (dec up-to)))
  )
)

(defn tails [a-seq]
  (cond
   (empty? a-seq)      (cons () a-seq)
   :else               (cons (seq a-seq) (tails (rest a-seq)))
  )
)


(defn inits [a-seq]
  (cond
    (empty? a-seq)      (cons () a-seq)
    :else               (rest(cons () (reverse(map reverse (tails(reverse a-seq))))))
  )
)

(defn rotation-helper [ n a-seq]
  (let [ rot (fn [seq-1] (reverse(rest(reverse(cons(last a-seq) a-seq)))))]
    (cond
      (empty? a-seq)               (cons () ())
      (== (inc n) (count a-seq))   (cons (rot a-seq) ())
      :else                         (cons (rot a-seq) (rotation-helper (inc n) (rot a-seq)))
    )
  )
)

(defn rotations [a-seq]
  (rotation-helper 0 a-seq)
)

(defn my-frequencies-helper [freqs a-seq]
  (let [is? (fn [x] (fn [y] (= x y)))]
    (cond
      (empty? a-seq)      freqs
      :else               (assoc (my-frequencies-helper freqs (filter (complement (is? (first a-seq))) a-seq))
                                 (first a-seq)
                                 (count(filter (is? (first a-seq)) a-seq))
                          )
    )
  )
)

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
)

(defn un-frequencies [a-map]
  (cond
    (empty? a-map)     []
    :else              (concat (repeat (val(first a-map)) (key(first a-map)))
                               (un-frequencies(rest a-map)))
  )
)

(defn my-take [n coll]
  (cond
    (empty? coll)    coll
    (< 0 n)          (cons (first coll) (my-take (dec n) (rest coll)))
    :else            ()

  )
)

(defn my-drop [n coll]
  (cond
    (< 0 n)           (my-drop (dec n) (rest coll))
    :else             coll
  )
)

(defn halve [a-seq]

  (vec(cons (my-take (int (/ (count a-seq) 2)) a-seq)
      (cons (my-drop (int (/ (count a-seq) 2)) a-seq) [])))
)

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)           b-seq
   (empty? b-seq)           a-seq
   (<= (first a-seq) (first b-seq))
                            (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else                    (cons (first b-seq) (seq-merge (rest b-seq) a-seq))
  )
)



(defn merge-sort [a-seq]
  (cond
    (< (count a-seq) 2)  a-seq
    :else                (seq-merge(merge-sort(halve a-seq)) (merge-sort(halve a-seq)))
  )
)

(defn merge-sort [a-seq]
  (cond
    ;(empty? a-seq)       ()
    (< (count a-seq) 2)  a-seq
    :else                (seq-merge(merge-sort(first(halve a-seq))) (merge-sort(last(halve a-seq))))
  )
)

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq)
          (apply >= a-seq)
       )
    true
    false
  )
)

(defn -last [a-seq]
  (rest(reverse(rest(reverse(cons(last a-seq) a-seq)))))
)

(defn index-of-first-monotonic [a-seq]
  (cond
    (monotonic? (get(vec(inits a-seq)) (count a-seq)))   (count a-seq)
    :else                              (index-of-first-monotonic (-last a-seq))
  )
)

(defn cut [a-seq n]
  (if (= 0 n)
    a-seq
    (cut (rest a-seq) (dec n))
  )
)

(defn split-into-monotonics [a-seq]
  (cond
    (monotonic? a-seq)     (cons (seq a-seq) ())
    :else                  (cons (get (vec(inits a-seq)) (index-of-first-monotonic a-seq))
                                 (split-into-monotonics(cut a-seq (index-of-first-monotonic a-seq))))
  )
)


(declare const-size-permu)


(defn add-to-sequence [x]
  (fn [a-seq] (cons x a-seq))
)

 ; ROTATE FUNCTIONS
 ;----------------------------
(defn rotate-> [ a-seq n]
  (if (= 0 n)
      a-seq
      (rotate-> (cons (last a-seq) (-last a-seq)) (dec n))
  )
)
(defn rotate<- [a-seq n]
  (rotate-> a-seq (- (count a-seq) n))
)
; -----------------------------

(defn remove-index [n a-seq]
  (rotate<-(-last(rotate-> a-seq(-(count a-seq) n 1)))(-(count a-seq) n 1))
)

(defn permutationss [a-set]
  (cond
    (empty? a-set)                (cons () ())
    (= (count a-set) 1)           (cons (seq a-set) ())
    (= (count a-set) 2)           (cons (reverse(seq a-set)) (cons (seq a-set) ()))
    ;:else                         (map (add-to-sequence (first a-set)) (permutations (rest a-set)))
     :else                         (const-size-permu a-set 0)
  )
)
(defn const-size-permu [a-seq n]
  (cond
    (>= n (count a-seq))           ()                                                   ;{ok}
    :else                          (concat (map (add-to-sequence (get a-seq n))        (permutationss(vec(remove-index n a-seq))))
                                           (const-size-permu a-seq (inc n) ))
                                           ;(map (add-to-sequence (get a-seq (inc n))   (permutations(remove-index (inc n)))))
  )

)

(defn permutations [a-set]
  (permutationss(vec a-set))
)


(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}
  )
)


(defn powerset [a-set]

)










