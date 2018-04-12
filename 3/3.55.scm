(define (partial-sum stream)
    (add-streams stream (cons-stream 0 (partial-sum stream)))
)

(partial-sum integers)
