


(define (mystery numTerms augEnd f g)
   (inspect (+ augEnd (mys-iter 1 numTerms f g)))
   )

(define (mys-iter a b f g)
;   (inspect a)
;   (inspect b)
;   (inspect (g a))
;   (inspect (f a))
;   (pause)
   (if (= a b) (real (/ (f a) (g a)))

      (/ (f a) (+ (g a) (mys-iter (+ a 1) b f g)))))

(define (g a)
   (cond ((= a 1) 1)
         ((= (% (- a 1) 3) 0) (+ a (/ a 3)))
         (else 1)))

(inspect (g 1))
(inspect (g 2))
(inspect (g 3))
(inspect (g 4))
(inspect (g 5))
(inspect (g 6))
(inspect (g 7))
(inspect (g 8))
(inspect (g 9))
(inspect (g 10))
(inspect (g 11))
(inspect (g 12))
(inspect (g 13))
(inspect (g 14))
(inspect (g 15))
(inspect (g 16))
(inspect (g 17))
(inspect (g 18))
(inspect (g 19))









;(inspect (mystery 2516 2 (lambda (n) 1) (lambda (n) n)))

(inspect (mystery 1000 1 (lambda (n) 1) g))


