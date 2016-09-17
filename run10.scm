

;(define (r d)
;   (if (= d 0) (* (+ d 1) (sqrt (+ d 6)))
;      (inspect (* (+ d 1) (sqrt (+ d 5 (r (- d 1))))))))

;(define (r d)
;   (* c (sqrt (+ 5 c (r-iter (

;(define (r-iter c d term)
;   (inspect d)
;   (inspect c)
;   (inspect term)
;   (pause)
;   (if (> c d) term
;      (* (+ c 1) (sqrt (+ c 6 (r-iter (+ c 1) d term)))))
;   )

;(define (r d)
;   (r-iter 0 d 0))

(define (r d)
   (if (= d 0) 6 
               (sqrt (+ 
                        (r 
                           (- d 1))
                        (* 
                           (+ d 1) 
                           (sqrt
                              (+ 6 d)))))))

;(inspect (r 0))
(inspect (r 1))
(inspect (r 2))
(inspect (r 3))
(inspect (r 4))
(inspect (r 6))
(inspect (r 30))
(inspect (r 1000))
(inspect (r 2625))
;(inspect (r 100000))
;(inspect (r 1000000))
;(inspect (r 10000000))
;(inspect (r 100000000))
;(inspect (r 1000000000))
;(inspect (r 10000000000))

