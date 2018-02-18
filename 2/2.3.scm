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

; Segment rect implementation
(define (make-segment-rect left top)
    (cons left top)
)

(define (segment-rect-side rect)
    (car rect)
)

(define (segment-rect-base rect)
    (cdr rect)
)
; End segment rect implementation

; Point rect implementation
(define (make-point-rect p1 p-common p2)
    (cons (make-segment p1 p-common) (make-segment p-common p2))
)

(define (point-rect-get-side rect)
    (car rect)
)

(define (point-rect-get-base rect)
    (cdr rect)
)

(define point-rect (make-point-rect (make-point 2 3) (make-point 2 7) (make-point 10 7)))
;(newline)
;(display (point-rect-get-base point-rect))
; End point rect implementation

; Calculates line lenght by its points 
(define (get-point-side-length p1 p2)
    (let 
        (
            (x-len (- (x-point p1) (x-point p2)))
            (y-len (- (y-point p1) (y-point p2)))
        )
        (sqrt (+ (* x-len x-len) (* y-len y-len)))
    )
)
;(display (get-point-side-length (make-point 2 7) (make-point 10 7)))

; Calculates segment line lenght
(define (get-segment-side-lenght segment)
    (get-point-side-length (start-segment segment) (end-segment segment))
)

; Calculates rect area 
(define (rect-area rect get-side-length get-base-length)
    (* (get-side-length rect) (get-base-length rect))
)

; Calculates suqare perimeter
(define (rect-perimeter rect get-side-length get-base-length)
    (+ (* (get-side-length rect) 2) (* (get-base-length rect) 2))
)

(define side (make-segment (make-point 2 3) (make-point 2 7)))
(define base (make-segment (make-point 2 7) (make-point 10 7)))
(define segment-rect (make-segment-rect side base))

; calculate segment rect area
(display 
    (rect-area 
        segment-rect
        (lambda (rect)
            (get-segment-side-lenght (segment-rect-side rect))
        )
        (lambda (rect)
            (get-segment-side-lenght (segment-rect-base rect))
        )
    )
)

(newline)

; calculate segment rect area
(display 
    (rect-perimeter 
        segment-rect
        (lambda (rect)
            (get-segment-side-lenght (segment-rect-side rect))
        )
        (lambda (rect)
            (get-segment-side-lenght (segment-rect-base rect))
        )
    )
)

(newline)

(display 
    (rect-area 
        point-rect
        (lambda (rect)
            (get-segment-side-lenght (point-rect-get-base rect))
        )
        (lambda (rect)
            (get-segment-side-lenght (point-rect-get-side rect))
        )
    )
)

(newline)

(display 
    (rect-perimeter 
        point-rect
        (lambda (rect)
            (get-segment-side-lenght (point-rect-get-base rect))
        )
        (lambda (rect)
            (get-segment-side-lenght (point-rect-get-side rect))
        )
    )
)
