(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
           "Not enouth money"
        )
    )
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance
    )
    (define (dispatch pass m)
        (cond
            ((not (eq? pass password)) (lambda (arg) "Wrong password"))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (display "Unknown call"))
        )
    )
    dispatch
)

(define acc (make-account 100 'say-and-pass))
((acc 'say-and-pass1 'withdraw) 40)
