(load "note.scm")
(load "list-streamer.scm")

(define (measure-pattern-test measure pattern)
  (let*
    (
      (measure-streamer (mk-list-streamer measure))
      (pattern-streamer (mk-list-streamer pattern))
      (note-or-rest 'dummynote)
      (pattern-value 0) ;; trigger new pattern section
      (pat-val-one-note? #f)
    )
    
    (measure-pattern-test-parser
      measure-streamer
      note-or-rest
      pattern-streamer
      pattern-value
      pat-val-one-note?
      #t
    )
  )
)

(define deb-p #f)
(define 
  (deb-mptp
    tag
    note-or-rest
    pattern-value
    pat-val-one-note?
    bool-result
  )
  
  (cond
    (deb-p
      (display (string-append tag ": note-or-rest: "))
      (display note-or-rest)
      (display "; pattern value: ")
      (display pattern-value )
      (display (string-append "; one note? " (if pat-val-one-note? "yes" "no") "; result: "))
      (display (string-append (if bool-result "true\n" "false\n") ))
    )
  )
)
;;   (deb-mptp "tag" note-or-rest pattern-value pat-val-one-note? bool-result)


(define 
  (measure-pattern-test-parser
    measure-streamer
    note-or-rest
    pattern-streamer
    pattern-value
    pat-val-one-note?
    bool-result
  )
  
  (cond
    ((null? pattern-value) ;; reached end of pattern
      bool-result
    )
    ((= pattern-value 0) ;; collected all notes in section corresponding to this pattern value
      (let*
        (
          (nxt-note (measure-streamer))
          (note (if (eq? nxt-note 'TIE) (measure-streamer) nxt-note))
          
          (pat-el (pattern-streamer))
          (pat-el-one-note? (pair? pat-el))
          (pattern-value (if pat-el-one-note? (car pat-el) pat-el))
        )

        (cond
	  ((null? note) bool-result)
          ((not pat-el-one-note?)
            (measure-pattern-test-parser
              measure-streamer
              note
              pattern-streamer
              pattern-value
              pat-el-one-note?
              bool-result
            )
          )
          ((note-isrest? note) #f)
          ((not (= pattern-value (note-duration note))) #f)
          (else ;; the one note filled the pattern value, so get next pattern val and next note
            (measure-pattern-test-parser
              measure-streamer
              '()
              pattern-streamer
              0 ;; signal next pattern section
              pat-val-one-note?
              bool-result
            )            
          )
        )
      )
    )
    (else ;; more notes or rests in section
      (let*
        (
          (its-duration (note-duration note-or-rest))
          (new-pattern-value (- pattern-value its-duration))
        )
        
        (measure-pattern-test-parser
          measure-streamer
          (measure-streamer)
          pattern-streamer
          new-pattern-value
          pat-val-one-note?
          bool-result
        )
      )
    )
  )
)

