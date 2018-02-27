(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(define (flatmap proc seq)
    (accumulate append '() (map proc seq))
)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
  
(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
    (cons (list new-row k) rest-of-queens)
)

(define (safe? k positions)
    (if (or (= k 1) (= k 0))
        #t
        (let 
            (
                (row (car (car positions)))
                (col (cadr (car positions)))
            )
            (null?
                (filter  
                    (lambda (bool) (not bool))
                    (map 
                        (lambda (test-pair)
                            (cond
                                ((= (car test-pair) row) #f)
                                ((= (cadr test-pair) col) #f)
                                ((= (car test-pair) (+ (cadr test-pair) (- row col))) #f)
                                ((= (car test-pair) (- (+ row col) (cadr test-pair))) #f)
                                (else #t)
                            )
                        )
                        (cdr positions)
                    )
                )
            )
        )
    )
)

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (display positions) (newline) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map 
                            (lambda (new-row)
                                (adjoin-position new-row k rest-of-queens)
                            )
                            (enumerate-interval 1 board-size)
                        )
                    )
                    (queen-cols (- k 1))
                )
            )
        )
    )
    (queen-cols board-size)
)

(display (queens 8))
