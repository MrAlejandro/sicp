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

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (square x) (* x x))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

 (define (smallest-divisor n) 
   (find-divisor n 2)) 

(define (find-divisor n test-divisor) 
   (cond ((> (square test-divisor) n) n) 
         ((divides? test-divisor n) test-divisor) 
         (else (find-divisor n (+ test-divisor 1))))) 
  
 (define (divides? a b) 
   (= (mod b a) 0)) 

(define (prime? n)
    (= n (smallest-divisor n))
)

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
            (flatmap
                (lambda (i) 
                    (map 
                        (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))
                    )
                )
                (enumerate-interval 1 n)
            )
        )
    )
)

(display (prime-sum-pairs 6))
(newline)

(define (unique-pairs n)
    (define (iter result i j)
        (cond
            ((> i n) result)
            ((< j i) (iter (append result (list (list i j))) i (+ j 1)))
            (else (iter result (+ i 1) 1))
        )
    )
    (iter '() 2 1)
)

;(display (unique-pairs 6))
;(newline)

(define (prime-sum-pairs n)
    (map 
        make-pair-sum
        (filter prime-sum? (unique-pairs n))
    )
)

(display (prime-sum-pairs 6))
(newline)
