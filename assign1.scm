
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

(define (run3) 
   (exprTest () "still ignore") 
   )

(define (run4) 
   (exprTest (root5 32) 2)
   (exprTest (root5 243) 3)
   )

(define (run5) 
   
   )

(define (run6) 
   
   )

(define (run7) 
   
   )

(define (run8) 
   
   )

(define (run9) 
   
   )

(define (run10) 
   
   )

(author)
(run1)
(run2)
(run3)


(println "assignment 1 loaded!")
