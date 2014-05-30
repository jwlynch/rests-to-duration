
(load "note-streamer.scm")

;; data structure note
; constructors
(define (mk-rest duration)
  (list 'r duration)
)
(define (mk-note duration pitch octave)
  (list 'n duration pitch octave)
)

;selectors
(define note-type car)
(define note-duration cadr)
(define note-pitch caddr)
(define note-octave cadddr)

;recognizer
(define (rest? note)
  (eq? (note-type note) 'r)
)

(define (apply-measure-pattern measure pattern)
  (let*
    (
      (streamer (mk-note-streamer measure))
      (next (streamer))
      (pat-el-one-note? (pair? (car pattern)))
      (pat-el (if pat-el-one-note? (caar pattern) (car pattern)))
      (pat-rest (cdr pattern))
    )
  
    (apply-measure-pattern-parser
      streamer
      pattern
      next
      0
      ""
      pat-el
      pat-rest
      pat-el-one-note?
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
    pattern-element
    pattern-spool
    pattern-el-one-note?
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
          pattern-element
          pattern-spool
          pattern-el-one-note?
        )
      )
    )
  )
)

;; we will need a func that tries the measure on the pattern to see if it fits