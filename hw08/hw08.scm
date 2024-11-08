(define (ascending? s) (or (null? s)
                           (null? (cdr s))
                           (and (<= (car s) (car (cdr s))) (ascending? (cdr s)))))

(define (my-filter pred s) (cond ((null? s) '()) ; if the list is empty, reutrn an empty list
                                 ((pred (car s)) ; if the predicate is true
                                  (cons (car s) (my-filter pred (cdr s)))) ; include it to result
                                 (else (my-filter pred (cdr s))))) ; otherwise, skip it

(define (interleave lst1 lst2) (cond ((null? lst1) lst2)
                                     ((null? lst2) lst1)
                                     ((and (null? lst1) (null? lst2)) '())
                                     (else (append (list (car lst1) (car lst2)) (interleave (cdr lst1) (cdr lst2))))
                                     ))

(define (no-repeats s) (if (null? s) '()
                         (cons (car s)
                               (no-repeats (filter (lambda (x) (not (= x (car s)))) (cdr s))
                         ))))
