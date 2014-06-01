
(load "list-streamer.scm")
(load "note.scm")

(define (apply-measure-pattern measure pattern)
  (let*
    (
      (measure=streamer (mk-list-streamer measure))
      (next (measure=streamer))
      
      (pattern-streamer (mk-list-streamer pattern))
      (pat-el (pattern-streamer))
      (pat-el-one-note? (pair? pat-el))
      (pattern-element (if pat-el-one-note? (car pat-el) pat-el))
    )

    (cond
      ((null? measure)
        'EMPTY_MEASURE
      )
      ((null? pattern-element)
        'EMPTY_PATTERN
      )
      (else
        (apply-measure-pattern-parser
          measure=streamer
          next
          0
          ""
          pattern-element
          pattern-streamer
          pat-el-one-note?
        )
      )
    )
  )
)

(define 
  (apply-measure-pattern-parser 
    measure=streamer 
    next
    group-dur-accum
    group-note
    pattern-element
    pattern-streamer
    pattern-el-one-note?
  )
  
  (cond
    ((null? next)
      '()
    )
    ((null? pattern-element)
      '()
    )
    ((< (note-duration next) pattern-element)
      (cons 
        next
        (apply-measure-pattern-parser
          measure=streamer
          (measure=streamer)
          0
          ""
          (- pattern-element (note-duration next))
          pattern-streamer
          pattern-el-one-note?
        )
      )
    )
    ((= (note-duration next) pattern-element)
      (let*
        (
          (pat-el (pattern-streamer))
          (pat-el-one-note? (pair? pat-el))
          (pattern-element (if pat-el-one-note? (car pat-el) pat-el))
        )
        
        (cons 
          next
          (apply-measure-pattern-parser
            measure=streamer
            (measure=streamer)
            0
            ""
            pattern-element
            pattern-streamer
            pat-el-one-note?
          )
        )
      )
    )
    ((> (note-duration next) pattern-element)
      (let*
        (
          (isrest? (note-isrest? next))
          (orig-duration (note-duration next))
          (remainder (- orig-duration pattern-element))
          (duration (- orig-duration remainder))
          (pitch (if isrest? '() (note-pitch next)))
          (octave (if isrest? '() (note-octave next)))
          (after-tie-note (if isrest? (mk-rest remainder) (mk-note remainder pitch octave)))

          (pat-el (pattern-streamer))
          (pat-el-one-note? (pair? pat-el))
          (pattern-element (if pat-el-one-note? (car pat-el) pat-el))
        )
        
        (cons 
          (if isrest?
            (mk-rest duration)
            (mk-note duration pitch octave)
          )
          (let
            ((apply-out
              (apply-measure-pattern-parser
                measure=streamer
                after-tie-note
                0
                ""
                pattern-element
                pattern-streamer
                pat-el-one-note?
              )
            ))
            
            (if isrest?
              apply-out
              (cons 'TIE apply-out)
            )
          )
        )
      )
    )
  )
)

;; we will need a func that tries the measure on the pattern to see if it fits