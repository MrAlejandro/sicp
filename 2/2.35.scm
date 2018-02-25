(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(define (count-leaves x)
    (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))
    )
)

(display (count-leaves (list 1 2 3 4 (list 5 6 7 8) 9 10 (list 11 12))))
(newline)

(define (count-leaves t)
    (accumulate 
        +
        0
        (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)
    )
)

(display (count-leaves (list 1 2 3 4 (list 5 6 7 8) 9 10 (list 11 12))))
(newline)
