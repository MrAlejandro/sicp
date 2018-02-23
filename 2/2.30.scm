(define test-list (list 1 (list 2 (list 3 4) 5 (list 6 7))))
(display test-list)
(newline)

(define (square x) (* x x))
(define (square-tree lst)
    (define (iter result lst)
        (cond
            ((null? lst) result)
            ((pair? (car lst)) (iter (append result (list (iter '() (car lst)))) (cdr lst)))
            (else (iter (append result (list (square (car lst)))) (cdr lst)))
        )
    )
    (iter '() lst)
)

(display (square-tree test-list))
(newline)

; using map
(define (square-tree lst)
    (map
        (lambda (x)
            (if (pair? x)
                (square-tree x)
                (* x x)
            )
        )
        lst
    )
)

(display (square-tree test-list))
(newline)

(define (square x) (* x x))
(define (square-tree lst)
    (if (null? lst)
        lst
        (let
            (
                (first (car lst))
                (squared-rest (square-tree (cdr lst)))
            )
            (if (pair? first)
                (append (list (square-tree first)) squared-rest)
                (append (list (square first)) squared-rest)
            )
        )
    )
)
(display (square-tree test-list))
(newline)
