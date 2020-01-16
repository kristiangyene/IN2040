;1

;;Trekker ut elementet 42
;1f)
(car (cdr '(0 42 #t bar))) ;42

;1g)
(car (cdr (car '((0 42) (#t bar))))) ;42

;1h)
(car (car (cdr '((0) (42 #t) (bar))))) ;42

;1i)
;definerer bar for at programmet skal kunne kjøre:
(define bar 'bar)
;Bare ved bruk av list:
(list (list 0 42) (list #t bar))
;Bare ved bruk av cons:
(cons (cons 0 (cons 42 '())) (cons (cons #t (cons bar '())) '()))


;2

;2a
;Sender rekursivt inn en liste med alle elementer unntatt én og teller + 1.
(define (length2 lists)
  (define (iterate lists count)
    (if (null? lists)
        count
        (iterate (cdr lists) (+ 1 count))))
  (iterate lists 0))
(length2 '())
(length2 '("towel" "a" "bring" "always")) ;4


;2b
(define (rev-list elements newlist)
    (if (null? elements)
        newlist
        (rev-list (cdr elements) (cons (car elements) newlist))))
(rev-list '("towel" "a" "bring" "always") '()) ;("always" "bring" "a" "towel")

#|Har valgt å bruke vanlig rekursjon med tanke på at det er det jeg er vant til synes er enklest, men i denne oppgaven
hadde det nok vært mest effektivt å bruke halerekursjon. Det brukes mye mer minne ettersom prosedyren holder på alle
verdiene helt til den har gått igjennom hele listen.|#


;2c
(define (all? predicate? lists)
  (if (> (car lists) 10)
      #f
      (if (equal? (length2 lists) 0)
          #t
          (if (not (predicate? (car lists)))
              #f
              (if (equal? (length2 lists) 1)
                  #t
                  (all? predicate? (cdr lists)))))))
(all? odd? '(1 3 5 7 9)) ;#t
;Litt usikker på om det var dette som var meningen å implementere når listeargumentet er høyere enn 10


;2d
(define (nth index lists)
  (if (= index 0)
      (car lists)
      (nth (- index 1) (cdr lists))))
(nth 2 '(47 11 12 13)) ;12


;2e
;Tar inn en index og en liste og returnerer nærmeste index for tallet som sendes inn.
(define (where index lists)
  (define (iterate lists count)
    (if (null? lists)
        #f
    (if (equal? (car lists) index)
        count
        (iterate (cdr lists)
              (+ 1 count)))))
  (iterate lists 0))
(where 3 '(1 2 3 3 4 5 3)) ;2


;2f
;Bygd på prosedyren fra forelesning. Kan velge hvilken prosedyre man ønsker som argument.
;I dette tilfellet legges verdiene som har samme index i listene sammen. Dersom en av de har flere verdier enn
;den andre så stopper den når den ene lista er tom.
(define (map2 proc lists1 lists2)
  (if (not (or (null? lists1) (null? lists2)))
      (cons (proc (car lists1) (car lists2))
            (map2 proc (cdr lists1) (cdr lists2)))
      '()))
(map2 + '(1 2 3 4) '(3 4 5)) ;(4 6 8)


;2g
;Bruker prosedyren over til å beregne gjennomsnittverdiene til indexene
(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5)) ;(2 3 4)


;2h
;Tar inn et predikat(f.eks even?) som argument og lager et predikat som tar inn to argumenter.
(define (both? predikat)
  (lambda (arg1 arg2) (and (predikat arg1) (predikat arg2))))

(map2 (both? even?) '(1 2 3) '(3 4 5)) ;(#f #t #f)


;2i
;Tar inn en prosedyre som argument og returnerer en ny prosedyre
(define (self proc)
  (lambda (x) (proc x x)))

((self +) 5) ;10