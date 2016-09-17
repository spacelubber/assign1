
(define (sqrt-iter guess x)
   (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
         x)))

(define (improve guess x)
   (average guess (/ x guess)))

(define (average x y)
   (/ (+ x y) 2))

(define (good-enough? guess x)
   (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
   (sqrt-iter 1.0 x))

(define (square x ) 
   (* x x))

(inspect (sqrt 9))
(inspect (sqrt (+ 100 37)))


(define (rt3-iter guess x)
   (if (good-enough3? guess x)
      guess
      (sqrt3-iter (improve3 guess x)
         x)))

(define (improve3 guess x)
   (average guess (/ x (square guess))))

(define (average x y)
   (/ ((+ x y) 3))

(define (good-enough3? guess x)
   (< (abs (- (square guess) x)) 0.001))

(define (rt3 x)
   (rt3-iter 1.0 x))

(define (square x ) 
   (* x x))

(inspect (sqrt 9))
(inspect (sqrt (+ 100 37)))
