;; makes a streamer for a grouped list (as output by rests-to-groupings)
;;
;; returns list:
;; (item in-group-p last-in=group-p)

;; selectors

(define note car)
(define in-group? cadr)
(define last-in-group? caddr)

(define (mk-cl init)
  (let 
    (
      (state init)
      (group '())
    )
    (lambda ()
      (let ;; binding for local var
        ((prev '()))
        (cond
          ((pair? group)
            (set! prev (car group))
            (set! group (cdr group))
            
            (list prev #t (null? group))
          )
          ((pair? state)
            (cond 
              ((pair? (car state))
                (set! group (car state))
                (set! state (cdr state))
              
                (set! prev (car group))
                (set! group (cdr group))
              
                (list prev #t (null? group))
              )
              (else
                (set! prev (car state))
                (set! state (cdr state))
              
                (list prev #f #f)
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

