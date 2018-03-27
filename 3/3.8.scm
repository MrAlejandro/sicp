(define f
    (let
        ((cnt 0))
        (lambda (x) 
            (set! cnt (+ cnt x))
            (- cnt x)
        )
    )
)

(+ (f 0) (f 1))
