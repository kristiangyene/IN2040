;;1a
;;Prosedyre som teller antall ganger en prosedyre blir kalt på.
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))

(c1) ;->1
(c1) ;->2
(c1) ;->3
count ;-42
(c2) ;->1
(newline)


;;1b (se vedlagt pdf-fil: tegneoppg_2b_1b.pdf)


;;2a
;Implementere en stack(LIFO) som har prosedyrer: push!, pop!, stack.

(define (make-stack list)
  (let ((stck list))
  (define (push! elements) ;;Push appender argumentene
    (set! stck (append (reverse elements) stck))) ;;reverserer elementene for å oppnå LIFO
  (define (pop!) ;;Pop tar bort første elementet
    (if (not (null? stck))
        (set! stck (cdr stck))))
  (define (dispatch message . args) ;;Message passing
    (cond ((eq? message 'push!) (push! args))
          ((eq? message 'pop!) (pop!))
          ((eq? message 'stack) stck)))
  dispatch))


(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack) ;->(bar)
(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack) ;->(4 3 2 1)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack) ;->(baz zip zap bah bar)
(newline)


;;2b
;;Mer generelt grensesnitt.
(define (stack stck)
  (stck 'stack))

(define (pop! stck)
  (stck 'pop!))

(define (push! stck . args)
  (apply stck 'push! args))

(pop! s1)
(stack s1) ;->(zip zap bah bar)
(push! s1 'foo 'faa)
(stack s1) ;->(faa foo zip zap bah bar)
(newline)

;;3a (se vedlagt pdf-fil: tegneoppg_2b_3a.pdf)
;;Øverste diagram: etter kallet på define
;;Nederste diagram: etter kallet på set-cdr!

#|
Grunnen til at vi får verdiene vi gjør er at kallet på set-cdr! setter celle d sin peker til å
være (cdr bar), altså 'd -> ('b 'c 'd 'e). Men pga kallet blir dette en syklisk, uendelig liste
som utelater celle 'e. Listen kan visualiseres slik: ('a 'b 'c 'd 'b 'c 'd 'b 'c .. ). 
|#


;;3b (se vedlagt pdf-fil: tegneoppg_2b_3b.pdf)
;;Øverste diagram: før kallet på set-car!
;;Nederste diagram: etter kallet på set-car!

#|
før kallet: ((a towel) a towel)
etter kallet: ((42 towel) 42 towel)
Grunnen til at bah evaluerer til verdien den gjør er at både (car bah) og (cdr bah) peker på celle 'a. Dersom
en av de blir endret til 42 så gjør begge det og det er nettopp det som skjer her.
|#


;testkode for resten
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
(set-car! (car bah) 42)

;;3c
;;Predikat som tester om en liststruktur er syklisk eller ikke. Implementasjon av Floyd's cycle-finding algoritme.
;;Algoritmen sender to pekere i forskjellige hastigheter gjennom sekvensen av verdier. Dersom de lander på samme
;;verdi, er det en cycle.
(define (cycle? list)
  (define (check-iter fast slow)
    (cond
      ((or (null? fast) (null? (cdr fast)) (null? (cddr fast))) #f)
      ((eq? slow fast) #t)
      (else (check-iter (cddr fast) (cdr slow)))))
  (check-iter list (cdr list)))


(cycle? '(hey ho)) ;->#f
(cycle? '(la la la)) ;->#f
(cycle? bah) ;->#f
(cycle? bar) ;->#t
(newline)


;;3d
#|
I følge forelesning 04.09 så kan en liste i scheme defineres som: kjeder av cons-par der siste elementet er
den tomme lista. Predikatet list? sjekker dermed om det siste elementet i listen er den tomme listen '(). Siden
bar er en sirkulær, uendelig liste vil den aldri komme frem til den tomme listen og blir dermed ikke
regnet som en liste.
 
Siden sirkuløre lister strengt tatt ikke er ekte lister kunne predikatet i oppg 3c også
blitt forkortet til kun:
(define (cycle? list)
  (not (list? list)))
|#

;;3e

(define (top ring)
  (ring 'top))

(define (left-rotate! ring)
  (ring 'left-rotate!))

(define (right-rotate! ring)
  (ring 'right-rotate!))

(define (insert! ring element)
  (ring 'insert! element))

(define (delete! ring)
  (ring 'delete!))



(define (make-ring list)
  (define (connect items)
    (if (null? (cdr items))
        (set-cdr! items list)
        (connect (cdr items))))
  (connect list) ;Setter peker til den første cellen.
  (define (top)
    (car list))
  (define (left-rotate!)
    (if (not (null? list)) ;Må sjekke om den ikke er null.
        (set! list (cdr list)))
    (top))
  (define (right-rotate!)
    (define (iter items)
      (if (eq? (cdr items) list)
          (set! list items)
          (iter (cdr items))))
    (if (not (null? list))
        (iter list))
    (top))
  (define (insert! elements)
    (right-rotate!)
    (set-cdr! list (cons elements (cdr list)))
    (left-rotate!))
   (define (delete!)
    (right-rotate!)
    (set-cdr! list (cddr list))
    (left-rotate!))
  (define (dispatch message . args) ;Message passing
    (cond ((eq? message 'top) (top))
          ((eq? message 'left-rotate!) (left-rotate!))
          ((eq? message 'right-rotate!) (right-rotate!))
          ((eq? message 'insert!) (insert! (car args)))
          ((eq? message 'delete!) (delete!))))
  dispatch)


(define r1 (make-ring '(1 2 3 4)))
(define r2 (make-ring '(a b c d)))
(top r1) ;->1
(top r2) ;->a
(right-rotate! r1) ;->4
(left-rotate! r1) ;->1
(left-rotate! r1) ;->2
(delete! r1) ;->3
(left-rotate! r1) ;->4
(left-rotate! r1) ;->1
(left-rotate! r1) ;->3
(insert! r2 'x) ;->x
(right-rotate! r2) ;->d
(left-rotate! r2) ;->x
(left-rotate! r2) ;->a
(top r1) ;->3
