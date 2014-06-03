(define (measure-pattern-test measure pattern)
  (let*
    (
      (measure-streamer )
      (pattern-streamer )
    )
    
    (measure-pattern-test-parser
      measure-streamer
      pattern-streamer
      pattern-value
      #t
    )
  )
)

(define 
  (measure-pattern-test-parser
    measure-streamer
    pattern-streamer
    pattern-value
    bool-result
  )
)