(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
    (empty-queue? queue)
    (error "Queue is empty")
    (car (front-ptr queue))
)

(define (insert-queue! queue item)
    (let
        ((new-pair (cons item '())))
        (cond 
            ((empty-queue? queue)
                (set-front-ptr! queue new-pair)
                (set-rear-ptr! queue new-pair)
            )
            (else
                (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
            )
        )
        queue
    )
)

(define (delete-queue! queue)
    (cond
        ((empty-queue? queue)
            (error "Queue is empty")
        )
        (else
            (set-front-ptr! queue (cdr (front-ptr queue)))
            queue
        )
    )
)

(define q1 (make-queue))
(display q1)
(newline)
(insert-queue! q1 'a)
(insert-queue! q1 'b)
;(delete-queue! q1)
;(delete-queue! q1)

(display q1)
(newline)

(define (print-queue queue)
    (if (pair? queue)
        (display (front-ptr queue))
        (display '())
    )
    (newline)
)

(print-queue q1)
