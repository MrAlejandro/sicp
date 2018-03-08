(define (make-leaf symbol weight)
    (list 'leaf symbol weight)
)

(define (leaf? object)
    (eq? (car object) 'leaf)
)

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)
    )
)

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)
    )
)

(define (make-code-tree left right)
    (list 
        left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
    )
)

(define (adjoin-set x set)
    (cond
        ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))
    )
)

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let 
            ((pair (car pairs)))
            (adjoin-set
                (make-leaf
                    (car pair)
                    (cadr pair)
                )
                (make-leaf-set (cdr pairs))
            )
        )
    )
)

(define (choose-branch bit branch)
    (cond
        ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "Wrong bit prowided" bit))
    )
)

(define (decode bits tree)
    (define (decode-iter bits current-branch)
        (if (null? bits)
            '()
            (let
                ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons
                        (symbol-leaf next-branch)
                        (decode-iter (cdr bits) tree)
                    )
                    (decode-iter (cdr bits) next-branch)
                )
            )
        )
    )
    (decode-iter bits tree)
)

(define sample-tree
    (make-code-tree
        (make-leaf 'A 4)
        (make-code-tree
            (make-leaf 'B 2)
            (make-code-tree 
                (make-leaf 'D 1)
                (make-leaf 'C 1)
            )
        )
    )
)

(display sample-tree) ; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
(newline)

(define smaple-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;(display (decode smaple-message sample-tree)) ; (A D A B B C A) 
;(newline)

(define (encode-symbol symbol tree)
    (define (iter tree result)
        (cond
            ((null? tree) #f)
            ((leaf? tree)
                (let
                    ((current-symbol (symbol-leaf tree)))
                    (if (eq? current-symbol symbol)
                        result
                        #f
                    )
                )
            )
            (else
                (let 
                    ((left-result (iter (left-branch tree) (append result '(0)))))
                    (if (not (eq? left-result #f))
                        left-result
                        (let
                            ((right-result (iter (right-branch tree) (append result '(1)))))
                            (if (not (eq? right-result #f))
                                right-result
                                (error "NO VALID SYMBOL FOUND")
                            )
                        )
                    )
                )
            )
        )
    )
    (iter tree '())
)

(define (encode message tree)
    (if (null? message)
        '()
        (append
            (encode-symbol (car message) tree)
            (encode (cdr message) tree)
        )
    )
)

;(display (encode '(A D A B B C A) sample-tree))
;(newline)


(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs))
)

(define (successive-merge leafs)
    (define (iter leafs)
        (cond
            ((null? leafs) '())
            ((= (length leafs) 2) (make-code-tree (cadr leafs) (car leafs)))
            ((> (length leafs) 2) (make-code-tree (car leafs) (iter (cdr leafs))))
            (else (make-code-tree (car leafs) (make-leaf #f 0)))
        )
    )
    (iter (reverse leafs))
)

(define sample-tree
    (make-code-tree
        (make-leaf 'A 4)
        (make-code-tree
            (make-leaf 'B 2)
            (make-code-tree 
                (make-leaf 'D 1)
                (make-leaf 'C 1)
            )
        )
    )
)
(generate-huffman-tree '((C 1) (B 2) (D 1) (A 4)))
