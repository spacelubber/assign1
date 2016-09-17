

(define (root5 radicand)
   (root5-iter 1 radicand))

(define (root5-iter guess x)
   (if (good-enough? guess x)
      guess
      (root5-iter (improve guess x) x)))

(define (improve guess x)
   (average (* 4 guess) (/ x (toTheFourth guess))))

(define (toTheFourth x)
   (* x x x x))

(define (average x y) 
   (/ (+ x y) 5))

(define (good-enough? guess x)
   (< (abs (- (* guess guess guess guess guess) x)) 0.001))

(define (square x) (* x x)) 

(inspect (root5 32))
(inspect (root5 243))
