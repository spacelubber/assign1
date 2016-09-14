
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

(define pi 3.14159)

(define (cym val)
   ; calls cyan, yellow, magenta then returns their values as string with #
   (string+ "#" ); add hex calls
   (println (hex (cyan val)) (hex (yellow val)) (hex (magenta val)))
   )

(define (cyan val)
   ; unModVal = cos((val*200/pi))
   ; return unModVal *255
   (int (* 255 (cos (/ (* (real val) pi) 200))))
   )

(define (yellow val)
   ; unModVal = -sin(x) + 1
   (int (* 255 (+ 1 (* -1 (sin (real val))))))
   )

(define (magenta val)
   ; unModVal = cos(x) + 1 
   (int (/ (* 255 (+ 1 (cos (real val)))) 2))
   )

(define (hex function)
   ; this function calls cyan, yellow, or magenta and then converts the return value to hex
   ; and returns a 2 digit string of that hex
   (string+ (fmt "%-2d" function))
   )

(define (run3) 
   (exprTest (cym 0) "#000000")
   (exprTest (cym 100) "#FFFFFF") 
   )

(define (run4) 
   (exprTest (root5 32) 2)
   (exprTest (root5 243) 3)
   )

(define (bico i j))

(define (run5) 
   (exprTest (bico 4 2) 6)
   )

(define (curry a b c d))

(define (run6) 
   
   )

(define (zorp i f))

(define (run7) 
   
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

(author)
(run1)
(run2)
(run3)


(println "assignment 1 loaded!")
