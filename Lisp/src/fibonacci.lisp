;;; @author Stephen Hilliard (c) 2015.
;;; @developer Stephen Hilliard (c) 2015.

;;; The functions program
;;; Fibonacci sequence to recursion
(define (fib n)
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (else
        (+ (fib (- n 1))
           (fib (- n 2))))))

(define (fib2 n)
    (let loop
      ((m 0)
       (k 1)
       (count n))
      (if (= count 0)
        m
        (loop k (+ m k) (- count 1)))))

(define (fib3 n)
    (define (fib-iter a b u v count)
      (cond ((= count 0) b)
     ((even? count)
             (fib-iter a
         b
         (+ (square u) (square v))
         (+ (square v) (* 2 u v))
         (/ count 2)))
            (else (fib-iter (+ (* b v) (* a v) (* a u))
                            (+ (* b u) (* a v))
                            u
                            v
                            (- count 1)))))
    (fib-iter 1 0 0 1 n))