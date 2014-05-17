(define (mk-cl init)
  (let 
    (
      (state init)
      (group '())
    )
    (lambda ()
      (let
        ((prev '()))
        (cond
          ((pair? group)
            (set! prev (car group))
            (set! group (cdr group))
            prev
          )
          ((pair? state)
            (cond 
              ((pair? (car state))
                (set! group (car state))
                (set! state (cdr state))
              
                (set! prev (car group))
                (set! group (cdr group))
              
                prev
              )
              (else
                (set! prev (car state))
                (set! state (cdr state))
              
                prev
              )
            )
          )
          (else
            '()
          )
        )
      )
    )
  )
)

