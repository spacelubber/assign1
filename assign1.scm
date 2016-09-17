
(define (author) 
   (println "AUTHOR: William Scott Carl wscarl@crimson.ua.edu")
   )

(define (exprTest # $expr target)
   (define result (catch (eval $expr #)))
   (if (error? result)
      (println $expr " is EXCEPTION: " (result'value)
         " (it should be " target ")")
      (println $expr " is " result 
         " (it should be " target ")")
      )
   )

; what if you used an outer cond to decide between e or 
(define (min5 a b c d e)
   (define (f x y) (if (< x y) x y))
   (f (f (f a b) (f c d)) e))


(define pi 3.141592653589793238462643383279502884197169)

(define (cym val)
   ; calls cyan, yellow, magenta then returns their values as string with #
   (string+ "#" (hex (cyan val)) (hex (yellow val)) (hex (magenta val))); add hex calls
   )

(define (cyan val)
   ; unModVal = cos((val*200/pi))
   ; return unModVal *255
   (int (* 255 (cos (/ (* (real val) pi) 200))))
   )

(define (yellow val)
   ; unModVal = -sin(x) + 1
   (int (* 255 (+ 1 (* -1 (sin (* pi .01 val))))))
   )

(define (magenta val)
   ; unModVal = cos(x) + 1 
   (int (/ (* 255 (+ 1 (cos (/ (* 3 pi (real val)) 200)))) 2))
   )

(define (run3) 
   (exprTest (cym 0) "#FFFFFF")
   (exprTest (cym 100) "#00FF7F") 
   )

(define hexConv "0123456789ABCDEF")

(define (hex function)
   ; this function calls cyan, yellow, or magenta and then converts the return value to hex
   ; and returns a 2 digit string of that hex
   (string+ (getElement hexConv (int (/ function 16))) 
            (getElement hexConv (int (% function 16))))
   )

(define (root5 radicand)
   (root5-iter 1 radicand)
   )

(define (root5-iter guess x)
   (if (good-enough? guess x)
       guess
       (root5-iter (improve guess x) x)))

(define (improve guess x) 
   (average (* 4 guess) (/ x (toTheFourth guess))))

(define (toTheFourth x)
   (* x x x x))

(define (average x y) (/ (+ x y) 5))

(define (good-enough? guess x)
   (< (abs (- (* guess guess guess guess guess) x)) 0.001))

(define (square x) (* x x))
 
; this is pascals triangle
(define (bico i j)
   (cond
      ((= i 1) 1)
      ((= j 0) 1)
      ((= j i) 1)
      (else (+ (bico (- i 1) j )
               (bico (- i 1) (- j 1))))
      )
   )

(define (curry f a)
   (define (lambda b)
      (define (lambda c)
         (define (lambda d)
            (f a b c d))))
   )

(define (zorp i f) 
   (define (zorp-iter count z1 z2 z3)
      (if (= (- i 2) count) z3 
         (zorp-iter (+ count 1) z2 z3 (+ z3 (/ (* (- z3 z2) (- z3 z2)) (+ z1 z3 (* -2 z2)))))
         )
      )
   (if (< i 3) (f i)
      (zorp-iter 0 (f 0) (f 1) (f 2))
      )
   )

(define (zorp-iter count z1 z2 z3)
   (if (= i count) z3 
      (zorp-iter (+ count 1) z2 z3 (+ z3 (/ (* (- z3 z2) (- z3 z2)) (+ z1 z3 (* -2 z2)))))
      )
   )

(define (egypt/ dividend divisor)
   ; takes two arguments and returns quotient. iterative process. no multiplication or division
   ; hint: define double and halve which dont use banned operators. halve must run sub-linear
   (egypt/iter1 1 divisor dividend)
   )

(define (egypt/iter1 a b c)
;   (inspect a)
;   (inspect b)
;   (inspect c)
   (if(> b c) (egypt/iter2 a b c 0)
      (egypt/iter1 (double a) (double b) c)
      )
   )

(define (egypt/iter2 a b c d)
;   (inspect a)
;   (inspect b)
;   (inspect c)
;   (inspect d)
;   (pause)
   (if (< a 1) d
   (if (<= b c) (egypt/iter2 (halve a) (halve b) (- c b) (+ d a)) 
         (egypt/iter2 (halve a) (halve b) c d)
      )
   ))

; halve function is broken. recursion doesn't work, no eligible base case??
(define (halve a)
;   (doubleForHalve 1 a 1)
   (/ a 2)
   )

(define (doubleForHalve a b sink)
   (inspect a)
   (inspect b)
   (inspect sink)
   (pause)
   (cond 
      ((= b 0) sink)
      ((> (double a) b) (doubleForHalve 1 (- b a) sink)
      (else (doubleForHalve (double a) b (double sink)))
      ))
   )

(define (double a)
      (+ a a)
   )

(define (mys-iter counter countTo firstFunc secFunc)
   (if (= counter countTo) (real (/ (secFunc counter) (secFunc counter)))
      (/ (real (firstFunc counter)) 
         (real (+ (secFunc counter) 
                  (mys-iter (+ counter 1) countTo firstFunc secFunc))))))

(define (mystery numTerms augEnd f g)
   (+ augEnd (real (mys-iter 1 numTerms f g))))


(define (run1) 
   (println 
"And and my-and behave differently when using the function to check
if a divisor is 0 before attempting to divide by 0. For example, when using and,listing (if (= a 0) #f #t) before (< a (/ x a)) yields the result #f whereas using my-and yield a mathException. This is because the and function is a special form that resolves its arguments in a definite order. my-and is a regularly defined function, and regular functions do not have a rule guiding the order of their parameter resolution")
   )
 
(define (run2)
   (exprTest (min5 1 2 3 4 5) 1)
   (exprTest (min5 0 3 4 5 -1) -1)
   (exprTest (min5 10000 20000 30000 0 5999) 0))

(define (run3) 
   (exprTest (cym 0) "#FFFFFF")
   (exprTest (cym 100) "#00FF7F") 
   )

(define (run4) 
   (exprTest (root5 32) 2)
   (exprTest (root5 243) 3)
   )

(define (run5) 
   (exprTest (bico 4 2) 6)
   (exprTest (bico 5 2) 10)
   (exprTest (bico 2 2) 1)
   (exprTest (bico 4 3) 4)
   )

(define (run6) 
   (define (f a b c d) (+ a b c d))

   (exprTest ((((curry f 1) 2) 3) 4) (f 1 2 3 4))
   )

(define (run7) 
   (exprTest (zorp 0 (lambda (n) (+ (^ n 3) (^ n 2) n))) 0)
   (exprTest (zorp 2 (lambda (n) (+ (^ n 3) (^ n 2) n))) 14)
   (exprTest (zorp 3 (lambda (n) (+ (^ n 3) (^ n 2) n))) 29)
   (exprTest (zorp 4 (lambda (n) (+ (^ n 3) (^ n 2) n))) 85)
   )

(define (run8) 
   (exprTest (egypt/ 1960 56) 35)
   )

;(define (mystery numTerms augend (genNum n) (genDenom n)))

(define (run9) 
   (exprTest (mystery 1000 2 (lambda (n) 1) (lambda (n) n)) 2.6977746580)
   (println "The mysterious number z is 1.6487212707, or the square root of e")
   )

(define (ramanujan depth))
(define (iramanujan depth))

(define (run10) 
   
   )

;(author)
;(run1)
;(run2)
;(run3)
;(run4)
;(run5)
;(run6)
;(run7)
;(run8)
;(run9)
;(run10)


(println "assignment 1 loaded!")
