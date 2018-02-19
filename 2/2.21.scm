(define (map proc items)
    (if (null? items)
        '()
        (cons
            (proc (car items))
            (map proc (cdr items))
        )
    )
)

(define (square-list items)
    (map (lambda (x) (* x x)) items)
)

(display (square-list (list 1 2 3 4)))
(newline)

(define (square-list items)
    (if (null? items)
        '()
        (let
            ((first (car items)))
            (cons (* first first) (square-list (cdr items)))
        )
    )
)

(display (square-list (list 1 2 3 4)))
(newline)
