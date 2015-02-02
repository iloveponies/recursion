;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(use 'recursion :reload)

(permutations #{})

(find-ascending-pair-index [1 2])

(if (not 0)
  1
  2
  )

(find-last-greater-than-index [1 2] 0)

(def v [1 2])
(def asc-pair-idx 0)
(def lgt-idx 1)

(reverse-from-index
 (swap v asc-pair-idx lgt-idx)
 (inc asc-pair-idx))

(find-ascending-pair-index [2 1])

(concat [1 2] [2 1])

'(())

(powerset #{1})

(add-set-copy-with-element-added #{1} 2)
(subsets #{1 2})

(clojure.set/union
 #{#{} #{:y} #{:z} #{:y :z}}
 (map
  (fn
    [subset]
    (add-set-copy-with-element-added
     subset
     :x))
  #{#{} #{:y} #{:z} #{:y :z}}))

(subsets #{:a :b :c})

(map
 (fn
   [subset]
   (add-set-copy-with-element-added
    subset
    :x))
 #{#{} #{:y} #{:z} #{:y :z}})


(powerset #{1 2 3})