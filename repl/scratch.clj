(use 'recursion :reload)

(defn ps [a-set]
  (let [sset (if (set? a-set) a-set (set a-set))]
    (if (empty? sset)
      #{#{}}
      (apply conj #{} sset
             (mapcat ps (sub1s sset))))))


(powerset #{}) ;=> #{#{}}
(powerset #{1})
(powerset #{2 3})
(powerset #{1 2 3}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}
(powerset #{5 8 6 7})

(def t1 #{1 2 3})

(sub1s t1)
(mapcat sub1s (sub1s t1))

(conj #{} 1 2 3 3 4 5)
(set? #{3})
(conj #{} #{1 2})
(conj #{} #{2 3} #{#{3 8 7}} #{#{4 7 8}})

(clojure.set/union #{#{2 3}} #{#{4 5}} );(sub1s t1)) ; (sub1s t1))

(apply conj #{} #{2 3} #{4 5} #{} t1 (sub1s t1))

(powerset #{})      ;=> #{#{}}
(= (powerset #{1 2 4}) #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}})
