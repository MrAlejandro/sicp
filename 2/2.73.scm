(define (operator exp) (car exp))
(define (opernds exp) (cdr exp))

(define (deriv exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))
    )
)

(define (sum-of-derivs exp var)
    (make-sum
        (deriv (addend exp) var)
        (deriv (augend exp) var)
    )
)

(put 'deriv '+ sum-of-derivs)

(define (product-of-derivs exp var)
    (make-sum
        (make-product  (multiplier exp) (deriv (multiplicand exp) var))
        (make-product  (multiplicnad exp) (deriv (multiplier exp) var))
    )
)

(put 'deriv '* product-of-derivs)

(define (exp-of-derivs exp var)
    (make-product
        (make-product 
            (exponent exp)
            (make-exponentiation 
                (base exp) 
                (let
                    ((expt (exponent exp)))
                    (if (number? expt)
                        (- expt 1)
                        (make-sum expt -1)
                    )
                )
            )
        )
        (deriv (base exp) var)
    )
)

(put 'deriv '** exp-of-derivs)
