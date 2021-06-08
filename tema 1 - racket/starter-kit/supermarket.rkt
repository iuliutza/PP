#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))


; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ C minutes)
  (match C
    [(counter index tt queue)
     (counter index (+ tt minutes) queue)]))


; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic
(define (min-tt counters)
  (if(null? (cdr counters))
     (cons (counter-index (car counters)) (counter-tt (car counters)))
     (compare (cons (counter-index (car counters)) (counter-tt (car counters))) (min-tt (cdr counters)))
     ))


(define (compare lst1 lst2)
  (cond
    ((< (cdr lst1) (cdr lst2) )  lst1)
    ((> (cdr lst1) (cdr lst2) )  lst2)
    (else
     (if (< (car lst1) (car lst2))
         lst1
         lst2))))
  
; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (match C
    [(counter index tt queue)
     (counter index (+ n-items (counter-tt C)) (append (counter-queue C) (list (cons name n-items))))]))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește


(define (search-and-add lst index minutes)
  (cond
    ((equal? index (counter-index (car lst))) (tt+ (car lst) minutes))
    (else (search-and-add (cdr lst) index minutes))))

(define (append-client-to-counter name n-items lst)
  (define counter1 (min-tt lst))
  (cond
    ((equal? (car counter1) (counter-index (car lst))) (add-to-counter (car lst) name n-items))
    (else (append-client-to-counter name n-items (cdr lst)))))

(define (sit name n-items C1 C2 C3 C4)
  (define lst1 (list C1 C2 C3 C4))
  (define lst2 (list C2 C3 C4))
    
  (if (<= n-items ITEMS)
      (append-client-to-counter name n-items lst1)
      (append-client-to-counter name n-items lst2)))

(define (update-counters counter1 counters)
  (cond
    ((null? counters) null)
    ((equal? (counter-index (car counters)) (counter-index counter1)) (cons counter1 (update-counters counter1 (cdr counters))))
    (else (cons (car counters) (update-counters counter1 (cdr counters))))))

(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (define lista (list C1 C2 C3 C4))
  
  
  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes)(let* ([new-c (update-counters  (search-and-add lista index minutes) lista)]
                                           [c1 (car new-c)]
                                           [c2 (cadr new-c)]
                                           [c3 (caddr new-c)]
                                           [c4 (cadddr new-c)]) (serve (cdr requests) c1 c2 c3 c4))]
                                        
      

        
        [(list name n-items) (let* ([new-c (update-counters  (sit name n-items  C1 C2 C3 C4) lista)]
                                           [c1 (car new-c)]
                                           [c2 (cadr new-c)]
                                           [c3 (caddr new-c)]
                                           [c4 (cadddr new-c)]) (serve (cdr requests) c1 c2 c3 c4))])))
