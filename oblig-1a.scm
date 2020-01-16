;;Oppgave 1

#|a) Uttrykket legger sammen innerste parentes, og ganger det med 5. Det 
evalueres til 30.
(* (+ 4 2) 5)

b) Gir feilmelding(syntax-error). Om det skal være parenteser, skal det 
første elementet være en prosedyre. Her er det kun en paramenter(5).
(* (+ 4 2) (5))

c) Gir feilmelding(syntax-error). Prosedyren kan ikke stå mellom argumentene, 
den skal stå først i listen.
(* (4 + 2) 5)

d) Utrykket definerer variabelen bar med 44 / 2 som mer 22. Bar evaluerer til 
verdien den er definert som.
(define bar (/ 44 2)) 
bar

e) Uttrykket trekker fra 11 på den tidligere definerte variabelen bar som har 
verdien 22. Det evalueres til 11.
(- bar 11)

f) Uttrykket ganger sammen alle verdiene i den innerste parentesen, og dermed 
deler det på bar. 264 / 22 = 12. Det evalueres til 12.
(/ (* bar 3 4 1) bar)
|#

;;Oppgave2

;;a)
#|Alle  bruker konnektiver som and, or og if som er special forms. Dette betyr 
at de kan avvike evalueringsregelen, og trenger ikke å evalurere alle 
argumentene. Den første Evaluerer "paff!". Siden det har uttrykket or så vil den 
evaluere til den første verdien som er #t. I dette tilfelle er det "paff!". 
Siden det er en special form trenger den ikke å evaluere det siste argumentet 
som har feil syntaks.
(or (= 1 2)
    "paff!"
    "piff!"
    (zero? (1 - 1)))

Den andre evalurer #f. Siden det har uttrykket and så vil den evaluere #f 
dersom en av argumentene er #f. På samme måte som den første så vil den ikke 
evaluere det siste argumentet.
(and (= 1 2)
    "paff!"
    "piff!"
    (zero? (1 - 1)))

Den tredje evalurer "poff!". Siden testen er #t vil den kun evaluere det neste 
argumentet. Siden if er special form trenger den ikke å evalure siste argument 
selv om det har noe udefinert.
(if (positive? 42)
    "poff!"
    (i-am-undefined))
|#

;;b)
(define (sign tall)
  (cond ((< tall 0) -1)
        ((> tall 0) 1)
        ((= tall 0) 0)))


(define (sign tall)
    (if(< tall 0) -1
       (if(> tall 0) 1 0)))


;;c)
(define (sign tall)
  (or
   (and (> tall 0) 1)
   (and (< tall 0) -1)
   0))


;;Oppgave 3

;;a)
(define (add1 tall)
  (+ tall 1))


(define (sub1 tall)
  (- tall 1))



;;b)
(define (plus tall1 tall2)
  (cond ((= tall1 0) tall2)
        ((= tall2 0) tall1)
        (else (plus (add1 tall1) (sub1 tall2)))))



;;c)
#|Selve prosedyren er rekursiv siden den kaller på seg selv, men det betyr ikke 
at den må ha et opphav til en rekursiv prosess. Prosedyren har ingen ventede 
kall, og dette gjør at både produktet(tall1) og telleren(tall2) samtidig endres
fra et trinn(kall på prosedyren) til det neste i følge regelen jeg har definert:
  tall1 + 1 -> tall1
  tall2 - 1 -> tall2
Helt til en av de blir 0. Dette er da en iterative prosessen som kan 
visualiseres slik:
  (plus 2 3)
  (plus 3 2)
  (plus 4 1)
  (plus 5 0)
  5

Et eksempel på en slik prosedyre med en rekursiv prosess er:|#
(define(plus2 tall1 tall2)
  (cond ((= tall1 0) tall2)
        ((= tall2 0) tall1)
        (else (add1 (plus tall1 (sub1 tall2))))))
      
#|Denne prosessen kan visualiseres slik:
  (plus 2 3)
  (+ 1 (plus 2 2))
  (+ 1 (+ 1 (plus 2 1)))
  (+ 1 (+ 1 (+ 1 (plus 2 0))))
  (+ 1 (+ 1 (+ 1 2)))
  (+ 1 (+ 1 3))
  (+ 1 4)
  5
I prosedyren er det flere ventede kall på add1. Alt regnes ut rekursivt i 
den første prosedyre og den første prosessen er aktiv gjennom hele evalueringen.
|#

;;d)
(define (power-close-to b n)
  (define (power-iter e)
     (if (> (expt b e) n)
        e
       (power-iter (+ 1 e))))
     (power-iter 1))
#|Hjelpe-prosedyren kan forenkles ved å fjerne argumentene den tar inn, b og n.
Dette er fordi prosedyren power-close-to allerede tar inn disse argumentene og 
siden hjelpeprosedyren blir definert internt vil den også ha tilgang til disse 
argumentene. Man slipper dermed å ta inn de samme argumentene på begge 
prosedyrene.|#

;;e
(define (fib n)
  (define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
   
#|Det er ikke mulig å forenkle hjelpemetoden. Først så tenkte jeg at siden
fib sender inn sitt eget argument n inn i hjelpemetoden som et argument for 
count så kunne det vært det samme slik at jeg kunne fjernet count som et 
argument i hjelpemetoden og heller brukt n der også. Men siden count endrer seg 
og sendes videre i et rekursivt kall, så trengs det et eget argument.|#
