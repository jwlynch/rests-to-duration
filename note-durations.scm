;; part of converstion to actual notes. takes durations and splits them into powers of 2 or runs 
;; of admacent 1 bits, needed for multi-dotted notes.

(define (dec->bin num)   (number->string (string->number num 10) 2))

(define (int->binstr int)
  (number->string int 2)
)

(define (duration-list numerator)
  (reverse (string->list (int->binstr numerator)))
)

(define (split-duration-star duration-list denom pwr2)
  (cond
    ((null? duration-list) '())
    ((char=? #\1 (car duration-list))
      (cons (/ pwr2 denom)
            (split-duration-star (cdr duration-list) denom (* 2 pwr2))
      )
    )
    (else ( split-duration-star (cdr duration-list) denom (* 2 pwr2) ))
  )
)

(define (split-duration-pow2 duration)
  (let*
    (
      (numer (numerator duration))
      (denom (denominator duration))
      (duration-list (duration-list numer))
    )
    
    (reverse (split-duration-star duration-list denom 1))
  )
)

;; TODO: rewrite this to find serieses of 1-bits in duration list
;; (at the moment it's the same as the above)
(define 
  (split-duration-adj-one-bits-star 
    duration-list 
    denom 
    pwr2
    last-was-1?
    num-ones
    first-one
    total-of-run
  )
  
  (cond
    ((null? duration-list) 
      (if last-was-1? ;; then add element to the list
        (cons
          (list total-of-run first-one (- num-ones 1)) ;; (dur note numdots)
          '()
        )
        '()
      )
    )
    ((char=? #\1 (car duration-list)) ;; collect this pwr of 2
      (split-duration-adj-one-bits-star 
        (cdr duration-list) 
        denom 
        (* 2 pwr2)
        #t
        (+ num-ones 1)
        (if (not last-was-1?) (/ pwr2 denom) first-one)
        (+ total-of-run (/ pwr2 denom))
      )
    )
    (else 
      (let
        ((split-output
          (split-duration-adj-one-bits-star 
            (cdr duration-list) 
            denom 
            (* 2 pwr2) 
            #f
            0
            0
            0
          )
        ))
        
        (if last-was-1?
	  (cons
	    (list total-of-run first-one (- num-ones 1)) ;; (dur note numdots)
	    split-output
	  )
	  split-output
        )
      )
    )
  )
)

(define (split-duration-dots duration)
  (let*
    (
      (numer (numerator duration))
      (denom (denominator duration))
      (duration-list (duration-list numer))
    )
    
    (reverse 
      (split-duration-adj-one-bits-star 
        duration-list 
        denom
        1
        #f
        0
        0
        0
      )
    )
  )
)

