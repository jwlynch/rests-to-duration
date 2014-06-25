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
          (pitch (if more-notes? (note-pitch (car note-list)) '()))
          (octave (if more-notes? (note-octave (car note-list)) '()))
        )
        
        (if more-notes? 
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

