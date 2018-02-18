(define (cons a b)
    (* (expt 2 a) (expt 3 b))
)

(define (car z)
    (define (check z power)
        (if (= (mod z 2) 0)
            (check (/ z 2) (+ power 1))
            power
        )
    )
    (check z 0)
)

(define (cdr z)
    (define (check z power)
        (if (= (mod z 3) 0)
            (check (/ z 3) (+ power 1))
            power
        )
    )
    (check z 0)
)

(display (car (cons 7 9)))
(newline)
(display (cdr (cons 7 9)))
