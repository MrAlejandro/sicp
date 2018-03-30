(define (make-table same-key?)
    (let ((local-table (list '*table*)))
        (define (assoc key records)
            (cond
                ((null? records) #f)
                ((same-key? key (caar records)) (car records))
                (else (assoc key (cdr records)))
            )
        )
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            #f
                        )
                    )
                    
                )
            )
        )
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable 
                                (cons 
                                    (list key-1 (cons key-2 value))
                                    (cdr locla-table)
                                )
                            )
                        )
                    )
                    (set-cdr! local-table
                        (cons
                            (list key-1 (cons key-2 value))
                            (cdr local-table)
                        )
                    )
                )
                'OK
            )
        )
        (define (dispatch m)
            (cond 
                ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation"))
            )
        )
        dispatch
    )
)

(define operation-table (make-table 
        (lambda (numeric-key table-key) 
            (let ((delta (- numeric-key table-key)))
                (let ((delta (if (< delta 0) (* delta -1) delta)))
                    (if (> delta 0.0001)
                        #t
                        #f
                    )
                )
            )
        )
    )
)
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 1 1 "test")
(get 1.001 1.001)
