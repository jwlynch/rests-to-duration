(define (disp-list l)
  (cond 
    ((null? l) '())
    (#t 
      (display (car l))
      (display " ")
      (disp-list (cdr l))
    )
  )
)

(define (disp-result-items result)
  (cond
    ((null? result) '())
    (#t
       (display "  ")
       (disp-list (car result))
       (display "\n")
       (disp-result-items (cdr result))
    )
  )
)

