
;test case 1
(setq test1-1 (list (list '~ (make-compound :op 'hasfinal :args '(?x))) (list '~ (make-compound :op 'take :args '(?y ?x))) (list '~ (make-compound :op 'ishappy :args '(?y)))))
(setq test1-2 (list (list '~ (make-compound :op 'iseasy :args '(?x))) (make-compound :op 'take :args (list (make-compound :op 'f :args '(?x)) '?x))))
(setq test1-3 (list (make-compound :op 'ishappy :args (list (make-compound :op 'f :args '(?x))))))
(setq test-kb1 (list test1-1 test1-2 test1-3))
(setq test-nq1 (list (make-compound :op 'iseasy :args '(?w)) (make-compound :op 'hasfinal :args '(?w))))


;test case 2
(setq test2-1 (list (make-compound :op 's :args '(*x)) (make-compound :op 'm :args '(*x))))
(setq test2-2 (list (list '~ (make-compound :op 'm :args '(*y))) (list '~ (make-compound :op 'l :args '(*x2 rain)))))
(setq test2-3 (list (list '~ (make-compound :op 's :args '(*z))) (make-compound :op 'l :args '(*z snow))))
(setq test2-4 (list (list '~ (make-compound :op 'l :args '(ellen *u))) (list '~ (make-compound :op 'l :args '(tony *u)))))
(setq test2-5 (list (make-compound :op 'l :args '(tony snow))))
(setq test-kb2 (list test2-1 test2-2 test2-3 test2-4 test2-5))
(setq test-nq2 (list (list '~ (make-compound :op 'm :args '(ellen))) (make-compound :op 's :args '(ellen))))


;test case 3
(setq test3-1 (list (make-compound :op 'animal :args '(?w)) (make-compound :op 'loves :args '(?v ?x))))
(setq test3-2 (list (list '~ (make-compound :op 'loves :args '(?x ?u))) (make-compound :op 'loves :args '(?v ?x))))
(setq test3-3 (list (list '~ (make-compound :op 'loves :args '(?y ?x))) (list '~ (make-compound :op 'animal :args '(?z))) (list '~ (make-compound :op 'kills :args '(?x ?z)))))
(setq test3-4 (list (list '~ (make-compound :op 'animal :args '(?x))) (make-compound :op 'loves :args '(jack ?x))))
(setq test3-5 (list (make-compound :op 'kills :args '(jack tuna)) (make-compound :op 'kills :args '(curiosity tuna))))
(setq test3-6 (list (make-compound :op 'cat :args '(tuna))))
(setq test3-7 (list (list '~ (make-compound :op 'cat :args '(?x))) (make-compound :op 'animal :args '(?x))))
(setq test-kb3 (list test3-1 test3-2 test3-3 test3-4 test3-5 test3-6 test3-7))
(setq test-nq3 (list (list '~ (make-compound :op 'kills :args '(curiosity tuna)))))



;test case 4 
(setq test4-1 (list (make-compound :op 'ws :args '(*x)) (make-compound :op 'sd :args '(*x))))
(setq test4-2 (list (list '~ (make-compound :op 'ws :args '(*z))) (make-compound :op 'likes :args '(*z Warm))))
(setq test4-3 (list (list '~ (make-compound :op 'likes :args '(Laura *w))) (list '~ (make-compound :op 'likes :args '(Jacob *w)))))
(setq test4-4 (list (make-compound :op 'likes :args '(Jacob Warm))))
(setq test-kb4 (list test4-1 test4-2 test4-3 test4-4))
(setq test-nq4 (list (list '~ (make-compound :op 'sd :args '(*v))) (make-compound :op 'ws :args '(*v))))

;test case 5
(setq test5-1 (list (list '~ (make-compound :op 'e :args '(*x))) (make-compound :op 'v :args '(*x)) (make-compound :op 's :args (list '*x (make-compound :op 'f :args '(*x))))))
(setq test5-2 (list (list '~ (make-compound :op 'e :args '(*x))) (make-compound :op 'v :args '(*x)) (make-compound :op 'c :args (list (make-compound :op 'f :args '(*x))))))
(setq test5-3 (list (make-compound :op 'p :args '(c))))
(setq test5-4 (list (make-compound :op 'e :args '(c))))
(setq test5-5 (list (list '~ (make-compound :op 's :args '(c *y))) (make-compound :op 'p :args '(*y))))
(setq test5-6 (list (list '~ (make-compound :op 'p :args '(*z))) (list '~ (make-compound :op 'v :args '(*z)))))
(setq test-kb5 (list test5-1 test5-2 test5-3 test5-4 test5-5 test5-6))
(setq test-nq5 (list (list '~ (make-compound :op 'p :args '(*w))) (list '~ (make-compound :op 'c :args '(*w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;test cases for snf;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;test case 1
(setq test-snf1 (list '(exist *x) (list '|| (make-compound :op 'p :args '(*x)) (make-compound :op 'q :args '(*x)))))

;test case 2
(setq test-snf2 (list '(all *x) (list '=> (make-compound :op 'p :args '(*x)) (list '~ (make-compound :op 'q :args '(*x))))))

;test case 3
(setq test-snf3 (list '(all *x) '(exist *y) (list '=> (list '& (make-compound :op 'p :args '(*x)) (make-compound :op 'q :args '(*y))) (list '~ (make-compound :op 'r :args '(*x))))))

;test case 4
(setq test-snf4 (list '(exist *x) '(all *y) (list '=> (make-compound :op 'p :args '(*x)) (list '|| (make-compound :op 'q :args '(*x)) (list '~ (make-compound :op 'r :args '(*x *y)))))))

;test case 5
(setq test-snf5 (list '(all *x) (list '=> (make-compound :op 'p :args '(*x)) (list '(exist *y) (list (make-compound :op 'r :args '(*y)))))))
