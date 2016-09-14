
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
   (if (< e 
          (if (< (if (< a b) a b) 
                 (if (< c d) c d))
              (if (< a b) a b)
              (if (< c d) c d)))
       e 
       (if (< (if (< a b) a b) 
              (if (< c d) c d)) 
           (if (< a b) a b) 
           (if (< c d) c d)
          )
      )
   )

(define (min5 a b c d e)
   (if (< e a b c d) e 
       (if (< (if (< a b) a b) 
              (if (< c d) c d)) 
           (if (< a b) a b) 
           (if (< c d) c d)
          )
      )
   )



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
   (int (* 255 (+ 1 (* -1 (sin (/ (* (real val) pi) 100))))))
   )

(define (magenta val)
   ; unModVal = cos(x) + 1 
   (int (/ (* 255 (+ 1 (cos (/ (* 3 pi (real val)) 200)))) 2))
   )

(define hexConv "0123456789ABCDEF")

(define (hex function)
   ; this function calls cyan, yellow, or magenta and then converts the return value to hex
   ; and returns a 2 digit string of that hex
   (string+ (getElement hexConv (int (/ function 16))) 
            (getElement hexConv (int (% function 16))))
   )

(define (root5 radicand)
   (define (sqrt-iter guess x)
      (if (good-enough? guess x)
          guess
          (sqrt-iter (imrpove guess x)x)))
   (define (improve guess x) 
      (average guess (/ x guess)))
   (define (average x y) (/ (+ x y) 2))
   (define (Good-enough? guess x)
      (< (abs (- (square guess) x)) 0.001))
   (define (sqrt x) (sqrt-iter 1.0 x))
   )

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
   (f a )
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

(define (run1) 
   (println "And and my-and behave differently when using the function to check")
   ; whether a variable is 0 before using it to divide. For example, 
   ; when and is given (if (= a 0) #f #t) before (< a (/ x a))), the result is #f
   ; whereas when the same parameters are given to my-and, the return is 
   ; <exception mathException> instead. This is because and is a special form that has
   ; a definite order in which it resolves its parameters (from leftmost to rightmost)
   ; whereas since my-and is simply a function, there is not rule governing which parameter
   ; will be resolved first
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

;(define (run6) 
   
;   )

(define (run7) 
   (exprTest (zorp 0 (lambda (n) (+ (^ n 3) (^ n 2) n))) 0)
   (exprTest (zorp 2 (lambda (n) (+ (^ n 3) (^ n 2) n))) 14)
   (exprTest (zorp 3 (lambda (n) (+ (^ n 3) (^ n 2) n))) 29)
   (exprTest (zorp 4 (lambda (n) (+ (^ n 3) (^ n 2) n))) 85)
   )

(define (egypt/ divisor dividend)
   ; takes two arguments and returns quotient. iterative process. no multiplication or division
   ; hint: define double and halve which dont use banned operators. halve must run sub-linear
   )

(define (run8) 
   
   )

;(define (mystery numTerms augend (genNum n) (genDenom n)))

(define (run9) 
   (exprTest (mystery 3 2 (lambda (n) 1) (labmda (n) n)) "no clue")
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
(run5)
;(run6)
;(run7)
;(run8)
;(run9)
;(run10)


(println "assignment 1 loaded!")
