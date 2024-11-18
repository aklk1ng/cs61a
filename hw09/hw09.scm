(define (curry-cook formals body) 
  (if (null? (cdr formals))
    `(lambda (,(car formals)), body)
    `(lambda (,(car formals)) ,(curry-cook (cdr formals) body))))

(define (curry-consume curry args)
  (if (null? args) curry
    (let ((next-curry (curry (car args))))
    (curry-consume next-curry (cdr args)))))

(define-macro (switch expr options)
  (switch-to-cond (list 'switch expr options)))

(define (switch-to-cond switch-expr)
  (cons 'cond
        (map (lambda (option)
               (cons `(equal? ,(car (cdr switch-expr)), (car option)) (cdr option)))
             (car (cdr (cdr switch-expr))))))