(define (make-vect x y)
    (cons x y)
)

(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
    (cons (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)))
)

(define (sub-vect v1 v2)
    (cons (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)))
)

(define (scale-vect v scale)
    (cons (* (xcor-vect v) scale) (* (ycor-vect v) scale))
)

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))
(scale-vect v1 3)
