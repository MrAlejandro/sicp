(define  (mul-streams s1 s2)
    (stream-map * s1 s2)
)

(define factorials (cons-stram 1 (mul-streams factorials (stream-cdr integers))))
