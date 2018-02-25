(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(define (first-items lst)
    (cond
        ((null? lst) lst)
        (else (pair? (car lst)) (cons (car (car lst)) (first-items (cdr lst))))
    )
)

;(display (first-items s))
;(newline)

(define (but-first-items lst)
    (cond
        ((null? lst) lst)
        (else (pair? (car lst)) (cons (cdr (car lst)) (but-first-items (cdr lst))))
    )
)

;(display (but-first-items s))
;(newline)
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (first-items seqs))
              (accumulate-n op init (but-first-items seqs))
        )
    )
)

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(display (accumulate-n + 0 s))
(newline)
