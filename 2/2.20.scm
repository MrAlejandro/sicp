(define (same-parity . lst)
    (define filter (if (odd? (car lst)) odd? even?))
    (define (iter lst result)
        (if (null? lst)
            result
            (let
                (
                    (first (car lst))
                    (rest (cdr lst))
                )
                (if (filter first)
                    (iter rest (append result (list first)))
                    (iter rest result)
                )
            )
        )
    )
    (iter (cdr lst) (list (car lst)))
)

(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))
