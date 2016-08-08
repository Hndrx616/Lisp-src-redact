;;; @author Stephen Hilliard (c) 2015.
;;; @developer Stephen Hilliard (c) 2015.

;;; The functions program
;;; Factorial Function:
(define (fact n)
    (if (zero? n)
      1
      (* n (fact (- n 1)))))

(define (helper n acc)
    (if (zero? n)
      acc
      (helper (- n 1) (* acc n))))
  (define (fact2 n)
    (helper n 1))

  ;;; Tail Calls
  (+ 3 (add1 2))

  (if (x > y)
    (fact x)
    (fact y))

  (add1 (if (x > y)
          (fact x)
          (fact y)))

;;; Recursion
(define (list-sum lis)
   (if (null? lis)
       0
       (+ (car lis)
          (list-sum (cdr lis)))))

;;; a tail-recursive list summing procedure
(define (list-sum lis)
   (let loop ((lis lis)
              (sum-so-far 0))
      (cond ((null? lis)
             sum-so-far)
            (else
             (loop (cdr lis)
                   (+ sum-so-far (car lis)))))))

;;; Definition of list-sum
(define (length lis)
   (if (null? lis)
       0
       (+ 1 (length (cdr lis)))))

;;; length procedure
(define (length lis)
   (let loop ((lis lis)
              (length-so-far 0))
      (if (null? lis)
          len-so-far
          (loop (cdr lis)
                (+ (car lis) length-so-far)))))

;;; Function Sum the Elements of a List
(define (list-sum lis)
   (if (null? lis)
       0
       (+ (car lis)
          (list-sum (cdr lis)))))
          (list-sum '(10 15 20 25))

(define (list-prod lis)
   (if (null? lis)
       1
       (+ (car lis)
          (list-prod (cdr lis)))))
          (list-prod '(2 3 4 5))

(define (reduce fn base-value lis)
   (if (null? lis)
       base-value
       (fn (car lis)
           (reduce fn base-value (cdr lis)))))

(define (list-copy lis)
   (reduce cons '() lis))
(define (append list1 list2)
   (reduce cons list2 list1))

;;; Function Make-Reducer
(define (make-reducer fn base-value)
   (lambda (lis)
      (reduce fn base-value lis)))

(define (make-reducer fn base-value)
   (letrec ((reduce (lambda (lis)
                       (if (null? lis)
                           base-value
                           (fn (car lis)
                               (reduce (cdr lis)))))))
       reduce)) ;; return new closure of local procedure