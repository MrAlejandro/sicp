(define false #f)
(define true #t)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;: (probe "Celsius temp" C)
;: (probe "Fahrenheit temp" F)
;: (set-value! C 25 'user)
;: (set-value! F 212 'user)
;: (forget-value! C 'user)
;: (set-value! F 212 'user)


(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)                                      ; p. 291: Only those values that were set by this adder are actually lost. See (forget-my-value) in (make-connector)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))                                        ; a connector may have had a value that was not originally set by the adder; these values may need to be propagated back through the adder
  (define (me request)                                          ; actually, after Exercise 3.35, i'm not sure i understand this ^^^^^
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (display "Unknown request -- ADDER"))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))           ; div by 0 is ruled out by first case
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (display "Unknown request -- MULTIPLIER"))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (display "Unknown request -- CONSTANT"))              ; p. 292: will reject ANY incoming messages defensively
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)                                  ; prints a message about the setting or unsetting of the designated connector
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (display "Unknown request -- PROBE"))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (display "Contradiction"))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (display "Unknown operation -- CONNECTOR"
                         ))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define C (make-connector))
(define F (make-connector))

(celsius-fahrenheit-converter C F)

;(probe "Celcius" C)
;(probe "Farengheit" F)

;(set-value! C 25 'user)
;(set-value! F 212 'user)
;(forget-value! C 'user)
;(set-value! F 212 'user)

(define a (make-connector))
(define b (make-connector))
(define avg (make-connector))

(define (averager a b avg)
    (let ((d (make-connector))
          (s (make-connector)))
        (adder a b s)
        (multiplier s d avg)
        (constant 0.5 d)
    )
)

(averager a b avg)
(probe "Average" avg)

(set-value! a 1 'user)
(set-value! b 5 'user)

(define a (make-connector))
(define b (make-connector))

(define (squarer a b)
    (define (process-new-value)
        (if (has-value? b)
            (if (< (get-value b) 0)
                (display "square is less than 0")
                (set-value! a (sqrt (get-value b)) me)
            )
            (if (has-value? a)
                (let
                    ((term (get-value a)))
                    (set-value! b (* term term) me)
                )
            )
        )
    )
    (define (process-forget-value)
        (forget-value! a me)
        (forget-value! b me)
        (process-new-value)
    )
    (define (me request)
        (cond 
            ((eq? request 'I-have-a-value) (process-new-value))
            ((eq? request 'I-lost-my-value) (process-forget-value))
            (else (display "Wrong request"))
        )
    )
    (connect a me)
    (connect b me)
    me
)

(squarer a b)
(probe "Term" a)
(probe "Square" b)

(set-value! a 3 'user)
