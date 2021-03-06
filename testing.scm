(load "note.scm")
(load "rests-to-durations.scm")
(load "apply-measure-patterns.scm")
(load "simple-quadruple-time.scm")

(define 
  sample-1
  (list
    (mk-rest 1/8)
    (mk-rest 1/8)
    (mk-note 1/8 'a 1)
    (mk-rest 1/8)
    (mk-rest 1/8)
    (mk-note 1/8 'b 1)
    (mk-note 1/8 'c 1)
    (mk-note 1/8 'd 1)
  )
)

(define 
  sample-2
  (list
    (mk-rest 1/8)
    (mk-rest 1/8)
    (mk-rest 1/8)
    (mk-rest 1/8)
    (mk-rest 1/8)
    (mk-note 1/8 'b 1)
    (mk-note 1/8 'c 1)
    (mk-note 1/8 'd 1)
  )
)

(define samp-1 (rests-to-durations sample-1))
(define samp-2 (rests-to-durations sample-2))
(define result-1 (simple-quadruple samp-1))
(define result-2 (simple-quadruple samp-2))

