;; this defines bitrun data structure, made out of a list
;;
;; used for note durations to indicate the basic note and the number of dots to follow

;; constructor
(define (mk-bitrun total base-dur num-dots)
  (list 'bitrun total base-dur num-dots)
)

;; recognizer
(define (is-bitrun it)
  (cond
    ((not (pair? it)) #f)
    ((not (eq? (car it) 'bitrun)) #f)
    (else #t)
  )
)

;; selectors
(define bitrun-total cadr)
(define bitrun-basicdur caddr)
(define bitrun-numdots cadddr)

