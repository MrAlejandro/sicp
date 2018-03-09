(define (make-from-mag-ang r a)
    (define (dispatch op)
        (cond
            ((eq? op 'real-path) (* r (cos a)))
            ((eq? op 'imag-path) (* r (sin a)))
            ((eq? op 'magnitude) r)
            ((eq? op 'angle) a)
            (else (erros "Unknown operation" op))
        )
    )
    dispatch
)
