;;Oblig3b, inf2810, v16
;;Gruppe: Anders Jakob Sivesind og Torjus Dahle.

;;(require r5rs)
(load "evaluator.scm")

;;(1 : Bli kjent med evaluatoren)
;;(1a)
;;(set! the-global-environment (setup-environment))
;;(define tekst '(define (foo cond else)
;;                   (cond ((= cond 2) 0)
;;                         (else (else cond)))))
;;(mc-eval tekst the-global-environment)
;;(mc-eval '(define cond 3) the-global-environment)
;;(mc-eval '(define (else x)(/ x 2)) the-global-environment)
;;(mc-eval '(define (square x) (* x x)) the-global-environment)

;; så teste:
;;(mc-eval '(foo 2 square) the-global-environment)
;;=> 0
;;Grunnen til at vi får returnert 0 her er at evaluatoren evaluerer
;;cond til forskjellige ting underveis i uttrykket.
;;cond vil først evalueres til special formen cond, og deretter
;;evalueres til 2.
;;Det samme skjer med else; den første elsen som evalueres
;;evaluerer til "den egentlige" else, mens de påfølgende
;;vil evaluere til prosedyren else er bundet til, nemlig square.

;;Vi får altså:
;;(cond ((= 2 2) 0)
;;      (else (square 2)))

;;(mc-eval '(foo 4 square) the-global-environment)
;;=> 16
;;Det samme skjer som i tilfellet over, men her slår else-tilfellet til isteden.
;;(cond ((= 4 2) 0)
;;      (else (square 4)))

;;(mc-eval '(cond ((= cond 2) 0)
;;                  (else (else 4))) the-global-environment)
;;=> 2
;;Her "settes" den omdefinerte else inn for den ene av dem.
;;Utrykket som evalueres blir:
;;(cond ((= 3 2) 0)
;;      (else ((lambda (x) (/ x 2)) 4)))

;;(2 : Primitiver / innebygde prosedyrer)
;;(2a)
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
	(list '1+
	      (lambda (n) (+ n 1)))
	(list '1-
	  (lambda (n) (- n 1)))
	;;      her kan vi legge til flere primitiver.
        ))
;;M� initialisere etter � ha lagt til primitiver:
(set! the-global-environment (setup-environment))
;;(2b)

;; <<<<<<<<<<< Gammel kode kommentert vekk, slett! >>>>>>>>
;;(define (install-primitive! name proc)
;;  (define (iter primitive-procedures)
;;    (if (null? (cdr primitive-procedures))
;;	(set-cdr! primitive-procedures (cons (list name proc) '()))
;;	(iter (cdr  primitive-procedures))))
;;  (iter primitive-procedures))
;;  
;;(install-primitive! 'asdf (lambda () 5))
;;primitive-procedures
;;(mc-eval  '(1- 5) the-global-environment)

;; Prosedyre som lar oss installere primitive prosedyrer
;; direkte gjennom REPL
(define (install-primitive! name proc)
  (set! the-global-environment (extend-environment (list name) (list (list 'primitive proc)) the-global-environment)))

;;(mc-eval 'asdf the-global-environment)
;; (mc-eval '(asdf) the-global-environment)
;; ;;(read-eval-print-loop)
;; (mc-eval '(1+ 5) the-global-environment)

;;(3a)
;; Implementer and og or som special forms i språket:
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ;;Legger til and og or:
        ((and? exp) #t)
        ((or? exp) #t)
        (else #f)))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

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
        ;;and og or:
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))))

(define (eval-and exp env)
  (cond
    ((false? (mc-eval (special-predicate exp) env)) #f)
    ((null? (special-next exp)) (mc-eval (special-predicate exp) env))
    (else (eval-and (cons (special-name exp) (special-next exp)) env))))
      
(define (eval-or exp env)
  (cond
    ((true? (mc-eval (special-predicate exp) env)) (mc-eval (special-predicate exp) env))
    ((null? (special-next exp)) #f)
    (else (eval-or (cons (special-name exp) (special-next exp)) env))))

(define (special-name exp) (car exp))
(define (special-predicate exp) (cadr exp))
(define (special-consequent exp) (caddr exp))
(define (special-next exp) (cddr exp))

;;(3b)
;; Ny syntaks for if (med vilkårlig mange elsif, men else er obligatorisk).
;; Har denne formen:
;;(if <test1>
;;then <utfall1>
;;elsif <test2>
;;then <utfall2>
;;else <utfall3>)
(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))


(define (if-next exp) (cddddr exp))
(define (if-else exp) (cadr exp))
(define (then-consequent exp) (cadddr exp))

(define (else? exp) (tagged-list? exp 'else))

(define (eval-if exp env)
  (cond
    ((else? exp) (mc-eval (if-else exp) env))
    ((true? (mc-eval (if-predicate exp) env)) (mc-eval (then-consequent exp) env))
    (else (eval-if (if-next exp) env))))

;;(3c)
;; Støtte for let, basert på syntaktisk transformasjon til lambda-applikasjon.
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ;;Legger til and og or:
        ((and? exp) #t)
        ((or? exp) #t)
        ;;Legger til let:
        ((let? exp) #t)
        (else #f)))

(define (let? exp) (tagged-list? exp 'let))

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
        ;;and og or:
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ;;let:
        ((let? exp) (mc-eval (let->lambda exp env) env))))

(define (let->lambda exp env)
  (append (list (make-lambda exp)) (let-exps exp)))

(define (make-lambda exp)
  (list 'lambda (let-vars exp) (let-rest exp)))

(define (let-bindings exp) (cadr exp))
(define (let-vars exp) (map car (let-bindings exp)))
(define (let-exps exp) (map cadr (let-bindings exp)))
(define (let-rest exp) (caddr exp))

;;(3d)
;; Let alternativ syntaks:

;;(let <var1> = <exp1> and
;;<var2> = <exp2> and
;;...
;;<varn> = <expn> in
;;<body>)
(define (let->lambda exp env)
  (append (list (make-lambda exp)) (let-exps exp)))

(define (make-lambda exp)
  (append (list 'lambda (let-vars exp)) (let-rest exp)))

(define (let-bindings exp) (cadr exp))

(define (let-vars exp)
  (let ((next (cddddr exp)))
    (cons (cadr exp)
          (if (equal? (car next) 'and)
              (let-vars next)
              '()))))

(define (let-exps exp)
  (let ((next (cddddr exp)))
    (cons (cadddr exp)
          (if (equal? (car next) 'and)
              (let-exps next)
              '()))))

(define (let-rest exp)
  (cdr (member 'in exp)))


;;      Forsøk på while      <<<<<< IKKE FERDIG >>>>>>>
;;(3e)
;; while-prosedyre implementert i scheme
;; Bruk:
;; (while <predikat> <kode-til-å-utføre>)

;;;; Eksempel på bruk:
;;(mc-eval '(define a 0) the-global-environment)
;;(mc-eval '(while (< a 10) (set! a (+ a 1))) the-global-environment)
;;(mc-eval 'a the-global-environment)

;;Legger til ny(e) primitiver først
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
	(list '1+
	      (lambda (n) (+ n 1)))
	(list '1-
	  (lambda (n) (- n 1)))
        (list '> >)
        (list '< <)
	;;      her kan vi legge til flere primitiver.
        ))


(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ;;Legger til and og or:
        ((and? exp) #t)
        ((or? exp) #t)
        ;;Legger til let:
        ((let? exp) #t)
        ;;legger til while:
        ((while? exp) #t)
        (else #f)))

(define (while? exp) (tagged-list? exp 'while))

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
        ;;and og or:
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ;;let:
        ((let? exp) (mc-eval (let->lambda exp env) env))
        ;;while:
        ((while? exp) (mc-eval (while->lambda exp env) env))))


;;Idé:
(define (while pred code)
  (display a)
  (if (false? pred)
      '()
      (begin
        code
        (while pred code))))

;; Som derivert uttrykk:
(define (while->lambda exp env)
  (let ((return (append (list (make-lambda-from-while exp)) (while-exps exp))))
    (display return)
    (display "\n")
  return))
;; ........

(define (make-lambda-from-while exp)
  (display (list 'lambda (while-vars exp) (while-rest exp)))
  (list 'lambda (while-vars exp) (while-rest exp)))

(define (while-vars exp)
  (list 'a 'b))
(define (while-exps exp)
  (list (cadr exp) (caddr exp)))
(define (return-null)
  0)

(define (while-rest exp)
  (list 'if 'a (list 'begin 'b (list ''while '(while-rest exp)))
        (list return-null)))


;; Har iallefall fått til evig løkke, det er jo noe ... :(



(set! the-global-environment (setup-environment))