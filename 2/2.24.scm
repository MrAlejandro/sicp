(define lst1 (list 1 3 (list 5 7) 9))
(display (car (cdr (car (cdr (cdr lst1))))))
(newline)

(define lst2 (list (list 7)))
(display (car (car lst2)))
(newline)

(define lst3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst3)))))))))))))
(newline)
