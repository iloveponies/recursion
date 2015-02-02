;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(use 'recursion :reload)

(defn find-ascending-pair-index [v & [i]]
  (if (empty? v)
    nil
    (let [index (or i (dec (count v)))]
      (cond
       (= index 0) nil
       (< (v (dec index)) (v index )) (dec index)
       :else (find-ascending-pair-index v (dec index))))))

(find-ascending-pair-index [ 1 2 3 ])


(defn find-last-greater-than-index [v index & [i]]
  (let [reference (v index)
        try-index (or i (dec (count v)))]
    (cond
     (< reference (v try-index)) try-index
     :else (find-last-greater-than-index v index (dec try-index)))))

(find-last-greater-than-index [ 3 1 2 ] 1)

(defn swap [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))

(swap [1 2 3] 0 2)

(defn reverse-from-index [v index]
  (vec (concat (subvec v 0 index) (reverse (subvec v index)))))

(reverse-from-index [1 2 3] 1)

(defn perms-helper [v]
  (cond
   (empty? v) v
   (empty? (rest v)) v
   :else (let [asc-pair-idx (find-ascending-pair-index v)]
           (if (not asc-pair-idx)
             v
             (let [lgt-idx (find-last-greater-than-index v asc-pair-idx)]
               (cons v (perms-helper (reverse-from-index (swap v asc-pair-idx lgt-idx) (inc asc-pair-idx)))))))))

(perms-helper [1])
(rest [1])
(empty? (rest [1]))

(defn permutations [a-set]
  (perms-helper (vec (merge-sort a-set))))

(permutations #{1 2 3})

