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

(define (reverse sequence)
    (accumulate (lambda (x y) (append y (list x))) '() sequence)
)

(display (reverse (list 1 (list "test") 2 3 4 5 6)))
(newline)

(define (reverse sequence)
    (fold-left (lambda (x y) (append (list y) x)) '() sequence)
)

(display (reverse (list 1 (list "test") 2 3 4 5 6)))
(newline)
