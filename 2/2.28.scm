(define (fringe lst)
    (define (iter result lst)
        (if (null? lst)
            result
            (let
                (
                    (first (car lst))
                    (rest (cdr lst))
                )
                (if (pair? first)
                    (iter (append result (fringe first)) rest)
                    (iter (append result (list first)) rest)
                )
            )
        )
    )
    (iter '() lst)
)

(define x (list (list 1 2) (list 3 4)))
(display (fringe x))
(newline)
(display (fringe (list x x)))
(newline)
