(load "note.scm")
(load "rests-to-durations.scm")
(load "apply-measure-patterns.scm")

(define 
  sample 
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

(define samp-1 (rests-to-durations sample))
(define result (apply-measure-pattern samp-1 '(1/2 1/2))

