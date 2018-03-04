(define (element-of-set? x set)
    (cond
        ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))
    )
)

(define (adjoin-set x set)
    (cons x set)
)

(define (remove-from-set x set)
    (cond
        ((null? set) set)
        ((equal? x (car set)) (cdr set))
        (else (cons (car set) (remove-from-set x (cdr set))))
    )
)

(define (intersection-set set1 set2)
    (cond
        ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
        (cons
            (car set1)
            (intersection-set (cdr set1) (remove-from-set (car set1) set2))
        ))
        (else (intersection-set (cdr set1) set2))
    )
)

(define (union-set set1 set2)
    (if (null? set1)
        set2
        (union-set (cdr set1) (cons (car set1) set2))
    )
)

(display (intersection-set (list 1 2 3 4 2) (list 5 2 2 3 4)))
