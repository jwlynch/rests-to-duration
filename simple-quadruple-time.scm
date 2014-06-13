;; simple quadruple time has as its main feature, the concept of equal division, where it's possible
;; to see the downbeat of beat 3 as a note, a note tied from a privious note, or a rest. having said
;; that, there are exceptions to equal division, four possibilities which are actually mandatory, and
;; an additional possibility -- syncopation over the split point -- which is optional, and should 
;; depend on whether the syncopation is important enough to the material to be written that way.
;;
;; here, we will implement the mandatory exceptions only.

(load "apply-measure-patterns.scm")
(load "measure-pattern-test.scm")
(load "note.scm")
(load "list-streamer.scm")

(define equal-division '(1/2 1/2))

(define exception-one 
  '(
     ((1))
     (1/4 (3/4))
     ((3/4) 1/4)
     (1/4 (1/2) 1/4)
   )
)

(define (chk-exception-one measure)
  (chk-exception-one-star measure exception-one)
)

(define (chk-exception-one-star measure ex-list)
  (cond
    ((null? ex-list) '())
    (else
      (let
        (
          (applied-measure (apply-measure-pattern measure (car ex-list)))
        )
        
        (if (measure-pattern-test applied-measure (car ex-list))
          applied-measure
          (chk-exception-one-star measure (cdr ex-list))
        )
      )
    )
  )
)

(define dbg-c16s? #t)
(define
  (dbg-c16s 
    tag
    note
    pattern-value
    result
  )
  
  (cond
    (dbg-c16s?
      (display "tag: ") (display tag)
      (display "; note: ") (display note)
      (display "; pattern-value: ") (display pattern-value)
      (display "; result: ") (display result) (newline)
      
    )
  )
)
;; (dbg-c16s "tag" note pattern-value result)

(define 
  (chk-16ths-star 
    measure-streamer
    note
    pattern-value
    result
  )
  
  (cond
    ((null? note) ;; no more notes left in measure
      (dbg-c16s "null note" note pattern-value result)
      (and result (= pattern-value 0))
    )
    ((= pattern-value 0) ;; collected notes to fill duration in pattern-value
      (dbg-c16s "pattern value is 0" note pattern-value result)
      result
    )
    ((> pattern-value 0) ;; more duration to fill
      (dbg-c16s "pattern value greater than 0" note pattern-value result)
      (let*
        (
          (dur (note-duration note))
          (new-pattern-value (- pattern-value dur))
          (shorter-than-8th? (> (denominator dur) 8))
        )
        
        (chk-16ths-star 
          measure-streamer
          (measure-streamer)
          new-pattern-value
          (or shorter-than-8th? result) ;; in case a prev note was shorter
        )
      )
    )
    (else ;; this shouldn't happen if measure processed properly
      (dbg-c16s "pattern value less than 0 (shouldn't happen)" note pattern-value result)
      'error-pattern-negative
    )
  )
)

;; takes a measure processed with equal-division pattern and returns
;; a list of 2 bools, first true if any 16ths on the left, and second
;; true if any 16ths on the right.
(define (chk-eq-div-16ths measure)
  (let
    (
      (measure-streamer (mk-list-streamer measure))
      (result #f)
    )
    
    (map
      (lambda (pattern-value)
        (chk-16ths-star 
          measure-streamer 
          (measure-streamer)
          pattern-value 
          result
        )
      )
      equal-division
    )
  )
)

;; do this (if 4/4 time) after rests-to-durations

(define (simple-quadruple measure)
  (let 
    (
      (measure-exceptions-tried (chk-exception-one measure))
    )
    
    (if (null? measure-exceptions-tried)
      (apply-measure-pattern measure equal-division)
      measure-exceptions-tried
    )
  )
)

