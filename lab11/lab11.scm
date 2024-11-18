(define (if-program condition if-true if-false)
  (list `if condition if-true if-false))

(define (square n) (* n n))

(define (pow-expr base exp) (cond ((= exp 0) 1)
                                  ((= exp 1) `(* ,base 1))
                                  ((even? exp) `(square ,(pow-expr base (/ exp 2))))
                                  (else `(* ,base (square ,(pow-expr base (quotient exp 2)))))))

(define-macro (repeat n expr)
  `(repeated-call ,n (lambda () ,expr)))

; Call zero-argument procedure f n times and return the final result.
(define (repeated-call n f)
  (if (= n 1)
      (f) ; Base case: Call f once and return its result
      (begin (f) (repeated-call (- n 1) f)))) ; Recur with n - 1
