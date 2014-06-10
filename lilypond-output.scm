;; This will do the final translation from the note.scm form to the form suitable for lilypond input.

;; this will be "hard-wired" for simple quadruple 4/4 time

(define (lilypond-output measure)
  (cond
    ((null? measure) '())
    ((eq? (car measure) 'TIE) "~")
    ((note-isrest? (car measure) 
    )
    ((note-isnote? (car measure)
    )
    (else
      (lilypond-output (cdr measure))
    )
  )
)