(define (make-accumulator acc)
    (lambda (term)
        (set! acc (+ acc term))
        acc
    )
)

(define A (make-accumulator 5))
(display (A 10))
(newline)
(display (A 10))
