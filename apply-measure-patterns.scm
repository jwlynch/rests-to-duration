
(load "groupings-streamer.scm")

;; some lilypond note defs

;;(define (pitch note) )

;;(define (duration note) )

(define (apply-measure-pattern measure pattern)
  (let*
    (
      (streamer (mk-cl measure))
      (next (streamer))
    )
  
    (apply-measure-pattern-parser
      streamer
      pattern
      next
      0
      ""
    )
  )
)

(define 
  (apply-measure-pattern-parser 
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
        (apply-measure-pattern-parser
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