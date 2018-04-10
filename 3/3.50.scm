(define (stream-map  proc . arguments)
    (if (stream-null? (car arguments))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car arguments))
            (apply 
               stream-map 
               (cons proc (map stream-cdr argumens))
            )
        )
    )
)
