
(load "groupings-streamer.scm")

;; some lilypond note defs

;;(define (pitch note) )

;;(define (duration note) )

(define (groupings-to-durations measure pattern)
  (let*
    (
      (streamer (mk-cl measure))
      (next (streamer))
    )
  
    (groupings-to-durations-parser
      streamer
      pattern
      next
      0
      ""
    )
  )
)

(define 
  (groupings-to-durations-parser 
    streamer 
    pattern 
    next
    group-dur-accum
    group-note
  )
  
  (cond
    ((null? next)
      '()
    ) 
    (else
      (cons 
        (note next)
        (groupings-to-durations-parser
          streamer
          pattern
          (streamer)
          0
          ""
        )
      )
    )
  )
)

;; we will need a func that tries the measure on the pattern to see if it fits