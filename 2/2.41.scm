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

(define (result-sum-triples n s)
    (filter 
        (lambda (triple) (= (accumulate + 0 triple) s))
        (flatmap 
            (lambda (i)
                (flatmap
                    (lambda (j)
                        (map 
                            (lambda (k) (list i j k))
                            (enumerate-interval 1 n)
                        )
                    )
                    (enumerate-interval 1 n)
                ) 
            )
            (enumerate-interval 1 n)
        )
    )
)

(display (result-sum-triples 5 7))
(newline)
