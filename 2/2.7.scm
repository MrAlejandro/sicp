(define (make-interval a b)
    (cons a b)
)

(define (lower-bound i)
    (min (car i) (cdr i))
)

(define (upper-bound i)
    (max (car i) (cdr i))
)

(define (add-interval x y)
    (make-interval
        (+ (lower-bound x) (lower-bound y))
        (+ (upper-bound x) (upper-bound y))
    )
)

(add-interval
    (make-interval -7 9)
    (make-interval -8 10)
)
