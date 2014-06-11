;; This will do the final translation from the note.scm form to the form suitable for lilypond input.

(load "note.scm")
(load "note-durations.scm")

(define (pitch-octave pitch octave)
  (let 
    (
      (pitch-str
        (cond
          ((string? pitch) pitch)
          ((symbol? pitch) (symbol->string pitch))
          (else "error-pitch-type")
        )
      )
    )

    (cond
      ((> octave 0)
        (string-append pitch-str (make-string octave #\'))
      )
      ((= octave 0)
        (string-append pitch-str)
      )
      ((< octave 0)
        (string-append pitch-str (make-string (- octave) #\,))
      )
    )
  )
)

;; this will be "hard-wired" for simple quadruple 4/4 time

(define (lilypond-output measure)
  (cond
    ((null? measure) '())
    ((eq? (car measure) 'TIE) 
      (cons
        "~"
        (lilypond-output (cdr measure))
      )
    )
    ((note-isrest? (car measure))
      ;; hardwire: no dots on rests
      (append
        (map
          (lambda (dur) 
            (let*
              (
                (denom (denominator dur))
                (denom-str (number->string denom 10))
              )
              
              (string-append "r" denom-str)
            )
          )
          (split-duration-pow2 (note-duration (car measure)))
        )
      
        (lilypond-output (cdr measure))
      )
    )
    ((note-isnote? (car measure))
      (append
        (map
          (lambda (bitrun) 
            (let*
              (
		(n (car measure))
		(pitch (pitch-octave (note-pitch n) (note-octave n)))
                (denom (denominator (bitrun-basicdur bitrun)))
                (denom-str (number->string denom 10))
                (dots (make-string (bitrun-numdots bitrun) #\.))
              )
              
              (string-append pitch denom-str dots)
            )
          )
          (split-duration-dots (note-duration (car measure)))
        )
        
        (lilypond-output (cdr measure))
      )
    )
    (else ;; none recognized, just skip it
      (lilypond-output (cdr measure))
    )
  )
)

