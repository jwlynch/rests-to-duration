;; data structure note
; constructors
(define (mk-rest duration)
  (list 'r duration)
)
(define (mk-note duration pitch octave)
  (list 'n duration pitch octave)
)

;selectors
(define note-type car)
(define note-duration cadr)
(define note-pitch caddr)
(define note-octave cadddr)

;recognizer
(define (note-isrest? note)
  (eq? (note-type note) 'r)
)

