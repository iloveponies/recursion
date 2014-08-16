(use 'recursion :reload)

(defn my-frequencies2 "count frequencies of different elements"
                 ([a-seq] (my-frequencies2 a-seq {}))
                 ([a-seq freqs]
                  (if (empty? a-seq)
                    freqs
                    (recur (next a-seq) (freqs)))))



(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(def fs {:a 0 :b 2 :c 4} )

(inc (get fs :c 0)) nil)

(update-in {:freqs fs} [ :freqs :c] inc)

(assoc fs :c (inc (get fs :c 0)))

(def users [{:name "James" :age 26}  {:name "John" :age 43}])

(update-in users [1 :age] inc)


(doc update-in)
