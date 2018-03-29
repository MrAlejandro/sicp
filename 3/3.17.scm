; wrong procedure
(define (count-pairs x)
    (if (not (pair? x))
        0
        (+
            (count-pairs (car x))
            (count-pairs (cdr x))
            1
        )
    )
)

(define (count-pairs x)
    (define encountered '())
    (define (iter x)
        (if (or (not (pair? x)) (memq x encountered))
            0
            (begin
                (set! encountered (cons x encountered))
                (+
                    (iter (car x))
                    (iter (cdr x))
                    1
                )
            )
        )
    )
    (iter x)
)

(define three '(a b c))
(display (count-pairs three))
(newline)

(define common-pair '(a))
(define four (cons common-pair (cons common-pair 'b)))
(display (count-pairs four))
(newline)

(define common-pair '(a))
(define second (cons common-pair common-pair))
(define seven (cons second second))
(display (count-pairs seven))
(newline)
