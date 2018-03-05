(define (lookup given-key set-of-records)
    (if (null? set-of-records)
        #f
        (let
            ((current-key (key (entry set-of-records))))
            (cond
                ((= current-key given-key) (entry set-of-records))
                ((> current-key given-key) (lookup given-key (left-branch set-of-records)))
                ((< current-key given-key) (lookup given-key (right-brnach set-of-records)))
            )
        )
    )
)
