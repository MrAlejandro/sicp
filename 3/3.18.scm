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

; checks if list has recursive links
(define (is-recursive x)
    (define (iter x parents)
        (if (not (pair? x))
            #f
            (if (memq x parents)
                #t
                (or
                    #f
                    (iter (car x) (cons x parents))
                    (iter (cdr x) (cons x parents))
                )
            )
        )
    )
    (iter x '())
)

(define (count-pairs x)
    (if (is-recursive x)
        "Recursive links"
        (let
            ((encountered '()))
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
        
    )
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

(define third-pair '(e f))
(define second-pair (cons 'c third-pair))
(define first-pair (cons 'a second-pair))
(set-cdr! third-pair first-pair)

(define recursive (cons first-pair (cons second-pair third-pair)))
(display (count-pairs recursive))
(newline)
