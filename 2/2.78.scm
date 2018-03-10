(define (attach-tag type-tag contents)
    (if (eq? 'scheme-number type-tag)
        contents
        (cons type-tag contents)
    )
)

(define (type-tag datum)
    (cond
        ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Unknown type"))
    )
)

(define (contents datum)
    (cond
        ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Unknown type"))
    )
)
