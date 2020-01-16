(load "evaluator.scm")


;;1a)
;;Tester:
(set! the-global-environment (setup-environment))

(mc-eval '(define (foo cond else)
    (cond ((= cond 2) 0)
          (else (else cond)))) the-global-environment) ;->ok
(mc-eval '(define cond 3) the-global-environment) ;->ok
(mc-eval '(define (else x)(/ x 2)) the-global-environment) ;->ok
(mc-eval '(define (square x) (* x x)) the-global-environment) ;->ok

;;Videre forklarte tester:

; De to første fungerer egentlig bare som vanlige prosedyrekall. Den første returnerer 0 fordi evaluatoren bruker 'cond' og
; 'else' som både special forms og som verdiene som blir sendt med som parametere. Mc-eval kaller på prosedyren 'eval-special-form'
; dersom exp er en special form. Denne prosedyren håndterer dette ved å behandle det cond-uttrykket som et if-uttrykk som gjør
; at det er mulig å ha flere 'cond' og 'else' i 'foo'.


; Det som skjer i denne er at: cond = 2. foo evaluerer 0 dersom cond = 2. 0 blir evaluert.
(mc-eval '(foo 2 square) the-global-environment) ;->0

; Det som skjer i denne er at: cond = 4. foo evaluerer 0 dersom cond = 2. Det er den ikke så else(square) blir dermed utført på cond
; som evaluerer 16.
(mc-eval '(foo 4 square) the-global-environment) ;->16


; Det som skjer i denne er at: den globale omgivelsen endres ikke når cond-uttrykket blir evaluert. Dette betyr at 'cond' og 'else'
; beholder de verdiene som de allerede har fått i de tidligere definisjonene (se tidligere tester).
; cond = 3. foo evaluerer 0 dersom cond = 2. Det er den ikke så else(tidligere definert) blir dermed utført på cond
; som evaluerer 2.
(mc-eval '(cond ((= cond 2) 0)
         (else (else 4))) the-global-environment) ;->2

;;NOTE: pga endring i syntax for if i 3b så blir får disse feilmelding. Har ikke funnet en god måte for å unngå dette.
(newline)




;;2a)
;;Har lagt til primitiver der kommentaren sier det:

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
;;      her kan vi legge til flere primitiver.
        (list '1+
	      (lambda (x) (+ x 1))) ;;Legger til '1+'
	(list '1-
	  (lambda (x) (- x 1))) ;; Legger til '1-'
        ))


;;Tester:
;;(read-eval-print-loop)

;;; MC-Eval input:
;(1+ 2)

;;; MC-Eval value:
;->3

;;; MC-Eval input:
;(1- 2)

;;; MC-Eval value:
;->1



;;2b)
; Setter den globale omgivelsen til en ny omgivelse som extend-environment evaluerer. Denne består av en ny ramme der variablene i listen(name)
; er bundet til de tilsvarende verdiene i listen(proc).

(define (install-primitive! name proc)
  (set! the-global-environment (extend-environment (list name) (list (list 'primitive proc)) the-global-environment)))


;;Tester:
(install-primitive! 'test (lambda (x) (+ x x)))
;(read-eval-print-loop)

;;; MC-Eval input:
;(test 4)

;;; MC-Eval value:
;->8





;;3a)
;; Legger inn 'and' og 'or' som special forms i evaluatoren. Må legge til/endre på ulike prosedyrer.

;;Lagt til nye definisjoner
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))


;;Endringer i eval-special-form
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
;;      her kan vi legge til flere evalueringer av special forms.
        ((and? exp) (eval-and exp env)) ;;Legger til 'and'
        ((or? exp) (eval-or exp env)))) ;;Legger til 'or'


;;Endringer i special-form?
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
;;      her kan vi legge til flere special forms.
        ((and? exp) #t) ;;Legger til 'and'
        ((or? exp) #t) ;;Legger til 'or'
        (else #f)))
        

;;Lagt til prosedyre som evaluerer et 'and'-uttrykk
(define (eval-and exp env)
  (cond ((false? (mc-eval (cadr exp) env)) #f)
        ((null? (cddr exp)) (mc-eval (cadr exp) env))
        (else (eval-and (cons (car exp) (cddr exp)) env))))

;;Lagt til prosedyre som evaluerer et 'or'-uttrykk
(define (eval-or exp env)
  (cond ((true? (mc-eval (cadr exp) env)) (mc-eval (cadr exp) env))
        ((null? (cddr exp)) #f)
        (else (eval-or (cons (car exp) (cddr exp)) env))))


;;Tester:
;(read-eval-print-loop)

;;; MC-Eval input:
;(and 4 5 3)
;;; MC-Eval value:
;->3

;;; MC-Eval input:
;(or #f 4 2 '())
;;; MC-Eval value:
;->4




;;3b)
;;Syntaks for ny if med vilkårlig mange elsif-grener og else som obligatorisk.

;;Endret på definisjonen av if
(define (if? exp) (tagged-list? (cddr exp) 'then))


;;Endret på eval-if
(define (eval-if exp env)
  (let ((res (mc-eval (if-predicate exp) env)))
    (cond ((tagged-list? exp 'else) res) ;;Evaluer om den treffer else
          ((true? res) (mc-eval (cadddr exp) env))
          (else (eval-if (cddddr exp) env)))))


;;Tester:
;(read-eval-print-loop)
;;; MC-Eval input:
;(if (= 3 1)
;    then 1
;    elsif (= 2 1)
;    then 2
;    elsif (= 2 2)
;    then 3
;    else 4)
;;; MC-Eval value:
;->3




;;3c)
;;Legger til støtte for 'let' i evaluatoren.

;;Lagt til ny definisjon
(define (let? exp) (tagged-list? exp 'let))


;;Endringer i eval-special-form
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
;;      her kan vi legge til flere evalueringer av special forms.
        ((and? exp) (eval-and exp env)) ;;Legger til 'and'
        ((or? exp) (eval-or exp env)) ;;Legger til 'or'
       ((let? exp) (eval-let exp env)))) ;;Legger til 'let'



;;Endringer i special-form?
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ;;Lagt til
        ((and? exp) #t)
        ;;Lagt til
        ((or? exp) #t)
        ;;Lagt til
        ((let? exp) #t)
        (else #f)))


;;Lagt til prosedyre som evaluerer 'let'-uttrykk
(define (eval-let exp env) (mc-eval (let->lambda exp env) env))

;;Syntaktisk transformasjon til lambda-applikasjon
(define (let->lambda exp env)
  (append (list (let-build exp)) (map cadr (cadr exp))))

;;Hjelpeprosedyre for let->lambda
(define (let-build exp)
  (list 'lambda (map car (cadr exp)) (caddr exp)))



;;Tester:
;(read-eval-print-loop)

;;; MC-Eval input:
;(let ((a 1)
;      (b 2))
;     (+ a b))

;;; MC-Eval value:
;->3



;;3d)
;;Små endringer, trenger hjelpefunksjon.
(define (let->lambda exp env)
  (append (list (let-build exp)) (get-expr-pair exp)))

;;Små endringer, trenger hjelpefunksjon.
(define (let-build exp)
  (append (list 'lambda (get-var-pair exp)) (cdr (member 'in exp))))

;;Hjelpefunksjon for let->lambda.
(define (get-expr-pair exp)
    (cons (cadddr exp)
          ;;Må sjekke om det er flere 'and'
          (if (equal? (car (cddddr exp)) 'and)
              (get-expr-pair (cddddr exp))
              '())))

;;Hjelpefunksjon for let-build.
(define (get-var-pair exp)
    (cons (cadr exp)
          ;;Må sjekke om det er flere 'and'
          (if (equal? (car (cddddr exp)) 'and)
              (get-var-pair(cddddr exp))
              '())))



;;Tester:
;(read-eval-print-loop)

;;; MC-Eval input:
;(let x = 2 and
;         y = 3 in
;      (display (cons x y))
;      (+ x y))
;->(2 . 3)
;;; MC-Eval value:
;->5


;;NOTE: fikk dessverre ikke tid til å prøve meg på bonusoppgavene pga andre innleveringer. Kommer uansett til
;;å prøve meg på disse før eksamen.