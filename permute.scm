(define first car)
(define rest cdr)
(define empty? null?)
(define empty '())

(define (permutations s)
  (cond ((empty? s) empty)
        ((empty? (rest s)) (list s))
        (else
         (let splice ((l '()) (m (first s)) (r (rest s)))
           (append
            (map (lambda (x) (cons m x))
                 (permutations (append l r)))
            (if (empty? r)
                empty
                (splice (cons m l) (car r) (cdr r))))))))
                
