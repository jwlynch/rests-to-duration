(define (measure-pattern-test measure pattern)
  (let*
    (
      (measure-streamer (mk-list-streamer measure))
      (pattern-streamer (mk-list-streamer pattern))
      (note-or-rest (measure-streamer))
      (pattern-value (pattern-streamer))
    )
    
    (measure-pattern-test-parser
      measure-streamer
      note-or-rest
      pattern-streamer
      pattern-value
      #t
    )
  )
)

(define 
  (measure-pattern-test-parser
    measure-streamer
    note-or-rest
    pattern-streamer
    pattern-value
    bool-result
  )
  
  (cond
    ((null? note-or-rest)
      (if (and (= pattern-value 0) (null? pattern-value)) ;; not possible, fixit
        bool-result
        #f
      )
    )
    ((= 0 pattern-value)
      (and
        bool-result
        (measure-pattern-test-parser
          measure-streamer
          note-or-rest
          pattern-streamer
          (pattern-streamer)
          bool-result
        )
      )
    )
    (else
      (and
        bool-result
        (measure-pattern-test-parser
          measure-streamer
          (measure-streamer)
          pattern-streamer
          (- pattern-value (note-duration note-or-rest))
          bool-result
        )
      )
    )
  )
)