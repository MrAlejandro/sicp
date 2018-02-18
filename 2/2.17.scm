(define (last-pair list)
    (let 
        (
            (cut-list (cdr list))
        )
        (if (null? cut-list)
            (car list)
            (last-pair cut-list)
        )
    )
)
