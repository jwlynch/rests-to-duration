;; makes a streamer for a grouped list (as output by rests-to-groupings)
;;
;; returns next note from list

(define (mk-list-streamer the-list)
  (let
    ((state the-list))
    
    (lambda ()
      (let
        ((prev '()))
        
        (cond
          ((null? state)
            '()
          )
          (else
            (set! prev (car state))
            (set! state (cdr state))
            
            prev
          )
        )          
      )
    )
  )
)

