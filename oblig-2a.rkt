;;1a)
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car arg)
  (arg (lambda (x y) x)))

(p-car (p-cons "foo" "bar")) ;foo


(define (p-cdr arg)
  (arg (lambda (x y) y)))

(p-cdr (p-cons "foo" "bar")) ;bar

(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar")))) ;foo

;;1b)

(define foo 42)

((lambda (foo x)
   (= x foo)
   (if (= x foo)
       'same
       'different))
 foo 5) ;different


#|TODO ----- oversett til lambda uttrykk

(let ((bar foo)
           (baz 'towel))
       (let ((bar (list bar baz))
             (foo baz))
         (list foo bar)))
|#


;;1c)
(define (infix-eval exp)
  ((cadr exp) (car exp) (caddr exp)))


;TESTS
(define foo (list 21 + 21))
(define baz (list 21 list 21)) 
(define bar (list 84 / 2))
(infix-eval foo)
(infix-eval baz)
(infix-eval bar)



;;1d)
#|Når jeg kjører kallet får jeg feilmeldingen "application: not a procedure;
 expected a procedure that can be applied to arguments given: /".

Dette hender ved at anførselstegnet som setter sammen listen i bah gjør at "/" ikke tolkes som
en prosedyre, men et tegn. Et annet eksempel med anførselstegn kan være '(* 3 4) som blir (list '* '3 '4).
Man burde helst alltid bruke list når man vil at argumentet skal evalueres.
|#


(load "huffman.scm")


;;2a)
#|hvis dekode-1 ikke hadde vært en hjelpemetode så hadde vi ikke hatt tilgang til det originale treet(noe vi trenger), fordi current-branch'
endrer seg hele tiden. Hvis `decode' skulle oppnådd det samme direkte måtte den ha tatt tre parametre, og bli kalt på f.eks. med (decode bits tree current-branch).
|#

;;2b)
(define (hale_decode bits tree)
  (define (iterate output bits current-branch)
    (if (not (null? bits))
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (iterate (append output (cons (symbol-leaf next-branch) '())) (cdr bits) tree)
              (iterate output (cdr bits) next-branch)))
        output))
  (iterate '() bits tree))


;;2c)
(decode sample-code sample-tree) ;→(ninjas fight ninjas by night)
(hale_decode sample-code sample-tree) ;→(ninjas fight ninjas by night)




#|TODO ----- gjøre ferdig
;;2d)
(define (encode seq tree)
  (define (iterate symbol current-branch)
    (if (leaf? current-branch)
        '()
        (let ((left  (left-branch current-branch))
              (right (right-branch current-branch)))))))          
        
(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree) ;→(ninjas fight ninjas by night)|#




;;2e)
(define (grow-huffman-tree sympairs)
  (define (iterate nodes)
    (if (not (null? (cdr nodes)))
        (iterate (adjoin-set (make-code-tree (car nodes) (cadr nodes)) (cddr nodes))) ;Setter inn venstre,høyre node
        (car nodes)))
  (iterate (make-leaf-set sympairs)))

;Ikke ferdig med encode så brukte sample code som test: samplecode = '(0 1 0 0 1 1 1 1 1 0)
(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode sample-code codebook) ;(d f b b)


;;2f)

(define message '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 2) (ambush 2) (defeat 1) (the 5) (sword 4)
                   (by 12) (assassin 1) (river 2) (forest 1) (wait 1) (poison 1)))
(grow-huffman-tree message)




          





        