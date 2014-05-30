;; makes a streamer for a grouped list (as output by rests-to-groupings)
;;
;; returns list:
;; (item in-group-p last-in=group-p)

;; data structure streamout

;constructor
(define (mk-streamout note in-group? last-in-group?)
  (list note in-group? last-in-group?)
)

; selectors
(define streamout-note car)
(define streamout-in-group? cadr)
(define streamout-last-in-group? caddr)

(define (mk-note-streamer note-list)
  (let 
    (
      (state note-list)
      (group '())
    )
    (lambda ()
      (let ;; binding for local var
        ((prev '()))
        (cond
          ((pair? group)
            (set! prev (car group))
            (set! group (cdr group))
            
            (mk-streamout prev #t (null? group))
          )
          ((pair? state)
            (cond 
              ((pair? (car state))
                (set! group (car state))
                (set! state (cdr state))
              
                (set! prev (car group))
                (set! group (cdr group))
              
                (mk-streamout prev #t (null? group))
              )
              (else
                (set! prev (car state))
                (set! state (cdr state))
              
                (mk-streamout prev #f #f)
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

