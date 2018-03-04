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

(adjoin-set 2 (list 1 2 3 4 5))
