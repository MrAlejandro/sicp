(define (make-point x y)
    (cons x y)
)

(define (x-point x) 
    (car x)
)

(define (y-point x) 
    (cdr x)
)

(define (make-segment start end)
    (cons start end)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)

(define (midpoint-segment segment)
    (define (average x y) (/ (+ x y) 2))
    (let
        (
            (start (start-segment segment))
            (end (end-segment segment))
        )
        (make-point
            (average (x-point start) (x-point end))
            (average (y-point start) (y-point end))
        )
    )
)

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display " , ")
    (display (y-point p))
    (display ")")
)

(define sgmt1 (make-segment (make-segment 1 5) (make-point 7 2)))
(print-point (midpoint-segment sgmt1))
(define sgmt2 (make-segment (make-point -6 2) (make-segment -2 5)))
(print-point (midpoint-segment sgmt2))
(define sgmt3 (make-segment (make-point -1 -1) (make-segment -4 -8)))
(print-point (midpoint-segment sgmt3))
(define sgmt4 (make-segment (make-point 3 -3) (make-segment 8 1)))
(print-point (midpoint-segment sgmt4))
