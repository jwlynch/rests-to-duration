
(load "list-streamer.scm")
(load "note.scm")

(define (apply-measure-pattern measure pattern)
  (let*
    (
      (streamer (mk-list-streamer measure))
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
        next
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