(use 'recursion :reload)

(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}
(my-frequencies [1 nil 3 1 4 5 1 nil 5 '() 5 {}])
