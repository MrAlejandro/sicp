(define (make-semaphore max-clients)
    (let ((access-mutex (make-mutex)))
         ((exceeded-mutex (make-mutex)))
         ((clients 0))
        (define (semaphore m)
            (cond ((eq? m 'acquire))
                (access-mutex 'acquire)
                (if (or (> clients max-clients))
                    (begin 
                        (exceeded-mutex 'acquire)
                        (access-mutex 'acquire)
                        (semaphore 'acquire)
                    )
                (begin 
                        (set! clients (+ clients 1)) 
                        (if (= clients max-clients))
                            (exceeded-mutex 'acquire)
                        )
                    )
                )
                ((eq? m 'release) 
                    (access-mutex 'acquire)
                    (begin
                        (set! clients (- clients 1))
                        (exceeded-mutex 'release)
                        (access-mutex 'release)
                    )
                )
            )
            (access-mutex 'release)
        )
        semaphore
    )
)
