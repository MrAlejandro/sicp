(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (make-queue)
    (let
        ((front-ptr '()) (rear-ptr '()))
        (define (set-front-ptr! value)
            (set! front-ptr value)
        )
        (define (set-rear-ptr! value)
            (set! rear-ptr value)
        )
        (define (empty-queue?) 
            (null? front-ptr)
        )
        (define (insert-queue! item) 
            (let
                ((new-pair (cons item '())))
                (cond 
                    ((null? front-ptr) 
                        (set-front-ptr! new-pair)
                        (set-rear-ptr! new-pair)
                    )
                    (else
                        (set-cdr! rear-ptr new-pair)
                        (set-rear-ptr! new-pair)
                    )
                )
                (cons front-ptr rear-ptr)
            )
        )
        (define (delete-queue! item) 
            (cond
                ((null? front-ptr)
                    (error "Queue is empty")
                )
                (else
                    (set-front-ptr! (cdr front-ptr))
                    (cons front-ptr rear-ptr)
                )
            )
        )
        (define (print-queue) 
            (if (pair? front-ptr)
                (display front-ptr)
                (display '())
            )
            (newline)
        )
        (define (dispatch m)
            (cond
                ((eq? m 'insert-queue) insert-queue!)
                ((eq? m 'delete-queue) (delete-queue!))
                ((eq? m 'print-queue) (print-queue))
                (else (lambda (x) (display "Unknown operation")))
            )
        )
        dispatch
    )
)

(define (insert-queue! queue item)
    ((queue 'insert-queue) item)
)

(define (delete-queue! queue item)
    (queue 'delete-queue)
)

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)


(define (print-queue queue)
    (queue 'print-queue)
)

(print-queue q1)
