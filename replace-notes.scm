(load "note.scm")

(define (replace-notes note-list measure)
  (cond
    ((null measure)
      '()
    )
    ((note-isrest? (car measure))
      (cons
        (car measure)
        (replace-notes note-list (cdr measure))
      )
    )
    (else
      (let*
        (
          (dur (note-duration (car measure)))
          (more-notes? (not (null? note-list)))
          (pitch (if nore-notes? (note-pitch (car note-list)) '()))
          (octave (if nore-notes? (note-octave (car note-list)) '()))
          (repl-note-result )
        )
        
        (if nore-notes? 
          (cons
            (mk-note duration pitch octave)
            (replace-notes (cdr note-list) (cdr measure))
          )
          (replace-notes note-list (cdr measure))
        )
      )
    )
  )
)

