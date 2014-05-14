;; these docs by me, remove and arrangements by Pascal J. Bourguignon <pjb@informatimago.com>
;; arrangements produces things like 5 attacks in 8 eight notes
;; the n is how many containers, the kinds breaks down into a list of lists
;; and each sublist starts with the thing to repeat, such as an 8th note on g, and 
;; the last item is how many of them to place in the containers.
;;
;; example of kinds for 6 attacks )8th note on g) and 2 rests, '((g8 6) (r8 2))
;;
;; so, for measures of 8 8th notes or rests, and 6 attacks per bar,
;;
;; (arrangements 8 '((g8 6) (r8 2))) and you can quote the g8 and r8 if you want.

(define (remove item list)
  (cond
    ((null? list) list)
    ((eqv? item (car list)) (cdr list))
    (else (cons (car list) (remove item (cdr list))))))

(define (arrangements n kinds)
  (cond
    ((null? kinds)  '())
    ((= 0 n)        '(()))
    ((= 1 n)        (map (lambda (kind) (list (car kind))) kinds))
    (else
     (apply append
            (map (lambda (kind)
                   (let* ((token  (car kind))
                          (ntok   (cadr kind))
                          (rkinds (if (= 1 ntok)
                                      (remove kind kinds)
                                      (cons (list token (- ntok 1))
                                            (remove kind kinds)))))
                     (map (lambda (subarr)
                            (cons token subarr))
                       (arrangements (- n 1) rkinds))))
              kinds)))))
