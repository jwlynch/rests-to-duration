;; simple quadruple time has as its main feature, the concept of equal division, where it's possible
;; to see the downbeat of beat 3 as a note, a note tied from a privious note, or a rest. having said
;; that, there are exceptions to equal division, four possibilities which are actually mandatory, and
;; an additional possibility -- syncopation over the split point -- which is optional, and should 
;; depend on whether the syncopation is important enough to the material to be written that way.
;;
;; here, we will implement the mandatory exceptions only.

(load "apply-measure-patterns.scm")
(load "measure-pattern-test.scm")

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

