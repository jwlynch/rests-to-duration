(load "arrangement.scm")
(load "rests-to-durations.scm")
(load "simple-quadruple-time.scm")
(load "result-writing.scm")
(load "note.scm")
(load "lilypond-output.scm")
(load "result-writing.scm")

(define arrs (arrangements 8 (list (list (mk-note 1/8 'g 0) 3) (list (mk-rest 1/8) 5))))

(define legatoized (map rests-to-durations arrs))

(define quadized (map simple-quadruple legatoized))

(define result (map lilypond-output quadized))

(display "\\version \"2.14.2\"\n\n")

(display "{\n")
(display "  \\clef bass\n\n")

(disp-result-items result)

(display "}\n")

