(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(display (map (lambda (x) (* x x)) (list 1 2 3 4 5)))
(newline)

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence)
)

(display (map (lambda (x) (* x x)) (list 1 2 3 4 5)))
(newline)

(display (append (list 1 2 3 4) (list 5 6 7 8)))
(newline)

(define (append seq1 seq2)
    (accumulate cons seq2 seq1)
)

(display (append (list 1 2 3 4) (list 5 6 7 8)))
(newline)

(display (length (list 1 2 3 4 (list 5 6))))
(newline)

(define (length sequence)
    (accumulate 
        (lambda (x y)
            (if (null? x)
                (+ 0 y)
                (+ 1 y)
            )
        )
        0
        sequence
    )
)

(display (length (list 1 2 3 4 (list 5 6))))
(newline)
