(define (element-of-set? x set)
    (cond
        ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))
    )
)

(define (intersetion-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let
            (
                (x1 (car set1))
                (x2 (car set2))
            )
            (cond
                ((= x1 x2) 
                    (cons
                        x1
                        (intersetion-set (cdr set1) (cdr set2))
                    )
                )
                ((< x1 x2) (intersetion-set (cdr set1) set2))
                ((< x2 x1) (intersetion-set set1 (cdr set2)))
            )
        )
    )
)

(define (adjoin-set x set)
    (if (null? set)
        (list x)
        (let 
            ((first (car set)))
            (cond
                ((= x first) set)
                ((< x first) (cons x set))
                (else (cons first (adjoin-set x (cdr set))))
            )
        )
    )
)

(define (union-set set1 set2)
    (if (null? set1)
        set2
        (union-set (cdr set1) (adjoin-set (car set1) set2))
    )
)

(union-set (list 1 3 5 7) (list 4 6 8 10))

(define (union-set set1 set2)
    (cond
        ((null? set1) set2)
        ((null? set2) set1)
        (else
            (let
                ((x1 (car set1)) (x2 (car set2)) (rest1 (cdr set1)) (rest2 (cdr set2)))
                (cond
                    ((= x1 x2) (union-set rest1 set2))
                    ((< x1 x2) (cons x1 (union-set rest1 set2)))
                    (else (cons x2 (union-set set1 rest2)))
                )
            )
        )
    )
)

(display (union-set (list 1 3 5 7) (list 4 6 8 10)))
(newline)
