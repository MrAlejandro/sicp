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
    (define (make-joint new-pass)
        (dispatch new-pass)
    )
    (define (dispatch orig-pass)
        (lambda (pass m)
            (cond
                ((not (eq? pass orig-pass)) (lambda (arg) "Wrong password"))
                ((eq? m 'make-joint) make-joint)
                ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (lambda (arg) "Unknown call"))
            )
        )
    )
    (dispatch password)
)

(define peter-acc (make-account 100 'say-and-pass))
(display ((peter-acc 'say-and-pass 'withdraw) 40))
(newline)

(define (make-joint account orig-pass new-pass)
    ((account orig-pass 'make-joint) new-pass)
)

(define paul-acc (make-joint peter-acc 'say-and-pass 'test))

(display ((paul-acc 'test 'withdraw) 40))
(newline)
