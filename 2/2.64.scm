
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) 
    (list entry left right)
)

(define (element-of-set? x set)
    (cond
        ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
            (element-of-set? x (left-branch set))
        )
        ((> x (entry set))
            (element-of-set? x (right-branch set))
        )
    )
)

(define (adjoin-set x set)
    (cond
        ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
            (make-tree 
                (entry set)
                (adjoin-set x (left-branch set))
                (right-branch set)
            )
        )
        ((> x (entry tree))
            (make-tree
                (entry set)
                (left-branch set)
                (adjoin-set x (right-branch set))
            )
        )
    )
)


(define first
    (make-tree
        7
        (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
        (make-tree 9 '() (make-tree 11 '() '()))
    )
)
(display first)
(newline)

(define second
    (make-tree
        3
        (make-tree 1 '() '())
        (make-tree
            7
            (make-tree 5 '() '())
            (make-tree 9 '() (make-tree 11 '() '()))
        )
    )
)
(display second)
(newline)

(define third
    (make-tree
        5
        (make-tree 3 (make-tree 1 '() '()) '())
        (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))
    )
)
(display third)
(newline)

(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append 
            (tree->list-1 (left-branch tree))
            (cons 
                (entry tree)
                (tree->list-1 (right-branch tree))
            )
        )
    )
)

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list
                (left-branch tree)
                (cons
                    (entry tree)
                    (copy-to-list (right-branch tree) result-list)
                )
            )
        )
    )
    (copy-to-list tree '())
)

(display (tree->list-1 first)) ; (1 2 5 7 9 11)
(newline)
(display (tree->list-2 first)) ; (1 2 5 7 9 11)
(newline)

(display (tree->list-1 second)) ; (1 2 5 7 9 11)
(newline)
(display (tree->list-2 second)) ; (1 2 5 7 9 11)
(newline)

(display (tree->list-1 second)) ; (1 2 5 7 9 11)
(newline)
(display (tree->list-2 second)) ; (1 2 5 7 9 11)
(newline)

(display (tree->list-1 third)) ; (1 2 5 7 9 11)
(newline)
(display (tree->list-2 second)) ; (1 2 5 7 9 11)
(newline)
