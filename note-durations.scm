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
  (let
    (
    )
    
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