(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))
        )
    )
    (iter initial sequence)
)

(display (accumulate / 1 (list 1 2 3))) ; 3/2 = 1.5
(newline)
(display (fold-left / 1 (list 1 2 3))) ; 1/6 = 0.16666
(newline)

(display (accumulate list '() (list 1 2 3))) ; (1 (2 (3 ())))
(newline)
(display (fold-left list '() (list 1 2 3))) ; (((() 1) 2) 3)
(newline)
