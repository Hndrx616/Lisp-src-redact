;;; @author Stephen Hilliard (c) 2015.
;;; @developer Stephen Hilliard (c) 2015.

;;; The functions program
;; filter, implemented recursively

(define (filter function lst)
	(cond   ((null? lst)
	                '())  ;; if the list is empty, return an empty list
	        ((function (car lst))
	                (cons (car lst) (filter function (cdr lst))))
                              ;; if function returns true, cons car with
                              ;; the result of calling filter on the cdr of this list
	        (else
	                (filter function (cdr lst)))))
                              ;; otherwise, just call filter on the cdr of this list

(define (quicksort lst)
    (cond ((or (null? lst) (= (length lst) 1)) lst) ;; if the list is empty or of length 1, return the list
        (else
            (let ((r (random (length lst))))
                    (append (quicksort (filter (lambda (x) (< x (list-ref r lst))) lst))
                              ;;take every term of the list with a value less than that found at index r

                        (append (filter (lambda (x) (= x (list-index r lst))) lst)
                              ;;take every term of the list with a value equal to that found at index r

                        (quicksort (filter (lambda (x) (> x (list-index r lst))) lst))))))))
                              ;;take every term of the list with a value less than that found at index r

(define (quicksort2 order lst)
    (cond ((or (null? lst) (= (length lst) 1)) lst) ;; if the list is empty or of length 1, return the list
        (else
            (let ((r (random (length lst))))
                    (append (quicksort (filter (lambda (x) (order x (list-ref r lst))) lst))
                              ;;take every term of the list with a value less than that found at index r

                        (append (filter (lambda (x) (= x (list-index r lst))) lst)
                              ;;take every term of the list with a value equal to that found at index r

                         (quicksort (filter (lambda (x) ((and (not (order x)) (not (= x))) (list-index r lst))) lst))))))))
                              ;;take every term of the list with a value less than that found at index r