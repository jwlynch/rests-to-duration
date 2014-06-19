(load "tool-attribs.scm")

(load (string-append prefix "arrangement.scm"))
(load (string-append prefix "rests-to-durations.scm"))
(load (string-append prefix "simple-quadruple-time.scm"))
(load (string-append prefix "result-writing.scm"))
(load (string-append prefix "note.scm"))
(load (string-append prefix "lilypond-output.scm"))

(define arrs (arrangements 16 (list (list (mk-note 1/16 'g 0) 7) (list (mk-rest 1/16) 9))))

(define legatoized (map rests-to-durations arrs))

(define quadized (map simple-quadruple legatoized))

(define result (map lilypond-output quadized))

(display "\\version \"2.14.2\"\n\n")

(display "{\n")
(display "  \\clef bass\n\n")

(disp-result-items result)

(display "}\n")

