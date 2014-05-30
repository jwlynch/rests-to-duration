(load "note.scm")

(define rest-8th (mk-rest 1/8))
(define note-8th (mk-note 1/8 'g 1))

(define 
  sample 
  (list
    rest-8th
    rest-8th
    note-8th
    rest-8th
    rest-8th
    note-8th
    note-8th
    note-8th
  )
)

;; initial assumptions: all incoming notes/rests same duration, all have 
;; explicit duration as digits. A note/rest can be split by looking at the
;; digits at the end, collect all the digits, what remains is the note or rest.
;;
;; this collects things to be consolidated into inner lists, so
;; (r8 d8 d8 r8 r8 d8 d8 d8) would become (r8 d8 (d8 d8 d8) d8 d8 d8).
;; Notice how it left the leading rest. Another example:
;; (r8 r8 r8 d8 d8 r8 r8 d8) => ((r8 r8 r8) d8 (d8 d8 d8) d8), so it groups
;; the leading rests and does not turn them into duration, it turns rests that
;; follow attacks into grouped duration.

;; WARNING this assumes each item in the measure list has the SAME duration. if not,
;; Weird Things will happen.

;; we're going to define our own note, as a list with one of two forms:
;;
;; (r <duration>) is a rest, and duration being a number (usually an exact fraction)
;;                with 1 meaning the size of a whole note, 1/2 meaning dur of half note, 
;;                etc.
;;
;; (n <duration> <lettername> <octave number>) 
;;                with n meaning a note, <duration> as above, <lettername> being a
;;                chromatic note between c and b, <octave number refers to which octave
;;                and a 1 means close to the middle of the piano, (n 1/8 c 1) is middle C.

(define (rest? note) 
  (eq? (car note) 'r)
)

(define (mk-longer-note what how-many)
  (cond
    ((rest? what)
      (let
        ((duration (cadr what)))
        
        (list 'r (* duration how-many))
      )
    )
    (else
      (let
        (
          (duration (cadr what))
          (lettername (caddr what))
          (octave-number (cadddr what))
        )
        
        (list 'n (* duration how-many) lettername octave-number)
      )
    )
  )
)

(define (rests-to-durations measure)
  (rests-to-durations-parser
    measure
    #t
    '()
    0
    '()
  )
)

(define debug-p #f)

;; debug 
(define (debug t a b c d e)
  (cond 
    (debug-p
      (display t)
      (display ": measure=")
      (display a)
      (display "; lead-rest-p=")
      (display b)
      (display "; what-were-collecting=")
      (display c)
      (display "; how-many=")
      (display d)
      (display "; saved-duration=")
      (display e)
      (newline)
    )
  )
)

; (debug "tag" measure lead-rest-p what-were-collecting how-many saved-duration)

(define (rests-to-durations-parser 
          measure 
          lead-rest-p 
          what-were-collecting 
          how-many
          saved-duration
        )
  (cond
    ((null? measure) 
      (debug "end" measure lead-rest-p what-were-collecting how-many saved-duration)
      (let
        ((ender
          (if (null? saved-duration)
            '()
            (cons saved-duration '())
          )
        ))
        (if (eq? how-many 0)
          ender
          (cons (mk-longer-note what-were-collecting how-many) ender)
        )
      )
    )
    ((rest? (car measure))
      (debug "rest" measure lead-rest-p what-were-collecting how-many saved-duration)
      (if lead-rest-p
        (rests-to-durations-parser 
          (cdr measure) 
	  #f 
	  (car measure)
	  (+ 1 how-many)
	  saved-duration
        )
        (if (null? saved-duration)
          (rests-to-durations-parser 
            (cdr measure) 
	    #f
	    what-were-collecting
	    (+ 1 how-many)
	    saved-duration
          )
          (rests-to-durations-parser 
            (cdr measure) 
	    #f
	    saved-duration
	    (+ 2 how-many)
	    '()
          )
        )
      )
    )
    (#t ; attack.
      (debug "attack" measure lead-rest-p what-were-collecting how-many saved-duration)
      (cond
        ((> how-many 0) ; a collecting was pending
          (cons 
            (mk-longer-note what-were-collecting how-many) 
            (rests-to-durations-parser 
              (cdr measure) 
              #f
              '()
              0
              (car measure)
            )
          )
        )
        (#t
          (cond
            ((null? saved-duration)
              (rests-to-durations-parser
                (cdr measure)
                #f
                '()
                0
                (car measure)
              )
            )
            (#t
              (cons
                saved-duration
                (rests-to-durations-parser
                  (cdr measure)
                  #f
                  '()
                  0
                  (car measure)
                )
              )
            )
          )
        )
      )
    )
  )
)

