(display (equal? '(this is a list) '(this is a list)))
(newline)

(display (equal? '(this is a list) '(this (is a) list)))
(newline)

(define (equal? list1 list2)
    (or
        (eq? list1 list2)
        (and
            (pair? list1)
            (pair? list2)
            (equal? (car list1) (car list2))
            (equal? (cdr list1) (cdr list2))
        )
    )
)

(display (equal? '(this is a list) '(this is a list)))
(newline)

(display (equal? '(this is a list) '(this (is a) list)))
(newline)
