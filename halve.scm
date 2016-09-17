


(define (halve num)
   (halve-iter num 1 0)
   )
  
(define (halve-iter num dub store)
   (if (> (+ dub dub) num) (halve-iter (- num dub) 1 dub)
   (halve-iter num (+ dub dub) store)))

(inspect (halve 12))
