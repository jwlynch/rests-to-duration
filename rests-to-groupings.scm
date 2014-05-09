(define sample '(r8 d8 d8 r8 r8 d8 d8 d8))

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

(define (split-note note-sym)
  (let ((n (symbol->string note-sym)))
    (let ((m (string-match "([^0-9]*)([0-9]*)" n)))
      (let ((note (match:substring m 1))
            (dur  (match:substring m 2)))
        (list note dur)
      )
    )
  )
)

(define (rest? note-sym) 
  (let
    ((note-list (split-note note-sym)))
    (equal? (car note-list) "r")
  )
)

(define (mk-grouping-helper what how-many)
  (cond
    ((eq? how-many 0) '())
    (#t (cons what (mk-grouping-helper what (- how-many 1))))
  )
)

(define (mk-grouping what how-many)
  (cond
    ((eq? how-many 1) what)
    (#t (mk-grouping-helper what how-many))
  )
)

(define (rests-to-groupings measure)
  (rests-to-groupings-parser
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

(define (rests-to-groupings-parser 
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
          (cons (mk-grouping what-were-collecting how-many) ender)
        )
      )
    )
    ((rest? (car measure))
      (debug "rest" measure lead-rest-p what-were-collecting how-many saved-duration)
      (if lead-rest-p
        (rests-to-groupings-parser 
          (cdr measure) 
	  #f 
	  (car measure)
	  (+ 1 how-many)
	  saved-duration
        )
        (if (null? saved-duration)
          (rests-to-groupings-parser 
            (cdr measure) 
	    #f
	    what-were-collecting
	    (+ 1 how-many)
	    saved-duration
          )
          (rests-to-groupings-parser 
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
            (mk-grouping what-were-collecting how-many) 
            (rests-to-groupings-parser 
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
              (rests-to-groupings-parser
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
                (rests-to-groupings-parser
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

