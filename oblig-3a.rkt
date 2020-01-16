(load "prekode3a.scm") ;Prekode hentet fra emnesiden.


;;1a/1b)
;;Tar en prosedyre som argument og returnerer en ny memoisert versjon av prosedyren.
(define (mem message proc)
  
  (define (memoize proc)
     (define cache (make-table))
     (lambda args
       (or (lookup args cache) ;Søker etter element i cachen
          (let ((value (apply proc args)))
            (insert! args value cache)
            value))))
  
   (define (unmemoize proc)
     (define cache (make-table))
     (or (lookup proc cache) ;returnerer den opprinnelige prosedyren (nest siste kall)
        proc))
   
  (define (dispatch message proc)
    (cond ((eq? 'memoize message) (memoize proc))
          ((eq? 'unmemoize message) (unmemoize proc))))
  (dispatch message proc))

;;Tester
(set! fib (mem 'memoize fib))
(fib 3) ;->2
(fib 3) ;->2
(fib 2) ;->1
(fib 4) ;->3
(set! fib (mem 'unmemoize fib))
(fib 3) ;->2
(newline)


;;1c)
;;Forskjellen på kallene er at test-proc blir satt som prosedyren med set!, mens mem-fib blir definert som det.
;;Dette er også grunnen til at de ikke fungerer på samme måte. mem-fib får ikke memoisert prosedyren, men lager
;;en ny prosedyre for hvert kall på mem. Dermed er det kun det første tallet som blir lagret i cachen, og ikke
;;de andre.



;;1d)
;;Hjelpeprosedyre som håndterer navngitte argumenter.
(define (help name args)
   (cond ((null? args) #f)
          ((equal? name (car args)) (cadr args))
          (else (help name (cddr args)))))


(define (greet . args)
  (let ((time (or (help 'time args) "day")))
    (let ((title (or (help 'title args) "friend")))
      
      (display "good ") (display time)
      (display " ") (display title)
      (newline))))



;;Tester
(greet) ;->good day friend
(greet 'time "evening") ;->good evening friend
(greet 'title "sir" 'time "morning") ;->good morning sir
(greet 'time "afternoon" 'title "dear") ;->good afternoon dear
(newline)



;;2a)
;;Konvertere liste til stream
(define (list-to-stream list)
    (if (null? list)
        the-empty-stream
    (cons-stream (car list) (list-to-stream (cdr list)))))

;;Konvertere stream til liste
;;TODO: må fullføres
#|(define (stream-to-list stream . args)
  (define (iter stream count)
    (cond
    ((stream-null? stream) '())|#


;;Tester
(list-to-stream '(1 2 3 4 5)) ;->(1 . #<promise>)
;(stream-to-list (stream-interval 10 20)) ;->(10 11 12 13 14 15 16 17 18 19 20)
(show-stream nats 15) ;->1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ...
;(stream-to-list nats 10) ;->(1 2 3 4 5 6 7 8 9 10
(newline)



;;2b)
;;Generalisert versjon av stream-map som tar én eller flere strømmer som argument.
(define (stream-map proc . argstreams)
  (if (null? argstreams)
      the-empty-stream 
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map 
                          (cons proc (map stream-cdr argstreams))))))


;;Usikker på basistilfellet.



;;2c)
;;Problemet her kallet på memq. Siden kallet er i en uendelig strøm så får vi aldri et resultat fra prosedyren.
;;Den vil bare kjøre i en uendelig loop.



;;2d)
;;Prosedyre som fjerner andre forekomster av et symbol i en strøm.
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (stream-filter (lambda (x) (not (eq? x (stream-car stream))))
                                  (remove-duplicates (stream-cdr stream))))))


;;Tester
(show-stream (remove-duplicates (list-to-stream '(11 4 3 3 3 4 2 1 5 3 9)))) ;->11 4 3 2 1 5 9
(newline)


;;2e)
(define x (stream-map show (stream-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
(newline)

;;Det som printest ut er tallene: 0, 1, 2, 3, 4, 5, 5, 6, 7, 7 nedover.
;;Dette skjer ved bruk av utsatt evaluering. 0 printes fra da x blir definert. 1-5 blir printet fra
;;første kall på stream-ref (0 blir ikke printet her siden cachen vet at den allerede har blitt det).
;;5 blir evalurt av hele uttrykket siden 5 ligger i den definerte x. Andre kallet på stream-ref skjer på
;;samme måte som første. De første elementene blir ikke printet(pga memoisering).


;;2f)
;Prosedyre som multipliserer to strømmer.
(define (mul-streams stream1 stream2)
  (stream-map * stream1 stream2))


;;2g)
;;Strøm der det n’te elementet tilsvarer verdien av n!.
(define factorials
  (cons-stream 1 (mul-streams factorials nats)))


;;Tester
(stream-ref factorials 5) ;->120