#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)
(define-struct counter (status index tt et queue) #:transparent)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define (empty-counter index)
  (make-counter #f index 0 0 empty-queue))

(define (update f counters index)
  (cond
    ((null? counters) null)
    ((equal? index (counter-index (car counters))) (append (list (f (car counters))) (update f (cdr counters) index)))
    (else (cons (car counters) (update f (cdr counters) index)))))

(define (tt+ minutes)
  (λ (C) (match C
           [(counter status index tt et queue)
            (counter status index (+ tt minutes) et queue)])))
(define (et+ minutes)
  (λ (C) (match C
           [(counter status index tt et queue)
            (counter status index tt (+ et minutes) queue)])))


(define (remove-first-from-counter C)   
  (cond
    ;left right sunt goale 
    ((queue-empty? (counter-queue C))
     (match C
       [(counter status index tt et queue)
        (counter status index 0 0 empty-queue)]))
    ;left mai are un singur elem
    ((and (equal? (queue-size-l (counter-queue C)) 1) (zero? (queue-size-r (counter-queue C))))
     (match C
       [(counter status index tt et queue)
        (counter status index 0 0 (dequeue queue))]))
    ;sunt 2 sau mai multe elem in left
    (else
     (match C
       [(counter status index tt et queue)
        (counter status index (- tt et) (cdr (stream-first (queue-left(dequeue queue))))  (dequeue queue))]))))

(define (pass-time-through-counter minutes)
  (λ (C)
    (if (< (- (counter-tt C) minutes) 0)
        (match C
          [(counter status index tt et queue)
           (counter status index 0 0 empty-queue)])
        (match C
          [(counter status index tt et queue)
           (counter status index (- tt minutes) (- et minutes) queue)]))))

(define (add-to-counter name items)     
  (λ (C)                               
    (if (queue-empty? (counter-queue C))
        (match C
          [(counter status index tt et queue)
           (counter status index (+ items (counter-tt C)) (+ items (counter-et C)) (enqueue (cons name items) (counter-queue C)))])
        (match C
          [(counter status index tt et queue)
           (counter status index (+ items (counter-tt C)) et (enqueue (cons name items) (counter-queue C)))]))))

(define (min-thingy lst1 lst2)
  (cond
    ((< (cdr lst1) (cdr lst2) )  lst1)
    ((> (cdr lst1) (cdr lst2) )  lst2)
    (else
     (if (< (car lst1) (car lst2))
         lst1
         lst2))))

(define (min-tt counters)
  (cond
    ((null? (cdr counters))  (cons (counter-index (car counters)) (counter-tt (car counters))))
    ((equal? (counter-status (car counters)) #f) (min-thingy (cons(counter-index (car counters)) (counter-tt (car counters))) (min-tt (cdr counters))))
    (else (min-tt (cdr counters)))))

(define (min-et counters)
  (cond
    ((null? (cdr counters)) (cons (counter-index (car counters)) (counter-et (car counters))))
    (else (min-thingy (cons (counter-index (car counters)) (counter-et (car counters))) (min-et (cdr counters))))))
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
(define (add-delay lst index minutes)
  (let* ([new-lst (update (tt+ minutes) lst index)]) (update (et+ minutes) new-lst index)))

;calculeaza media aritmetica
(define (find-average counters)
  (/ (foldr (lambda (x y) (+ x y)) 0 (map (λ (C) (counter-tt C)) counters)) (length counters)))

;adauga case pana cand se indeplineste conditia de medie
(define (add-counters counters average all-counters)
  (if (> (find-average all-counters) average)
      (add-counters (append counters (list (empty-counter (add1 (length all-counters))))) average (append all-counters (list (empty-counter (add1 (length all-counters))))))
      counters))

;sorteaza clientii dupa indexul casei
(define (sort-clients lst )
  (map (λ (el)  el) (sort lst #:key car >)))

;sorteaza clientii dupa et-ul lor
(define (sort-clients-et lst)
  (map (λ (el)  (cdr el)) (sort lst #:key car >)))

;functie ajutatoare, scoate toti clientii care pot iesi in nr de minute dat de la un counter
(define (exit minutes lst C)
  (cond
    ((queue-empty? (counter-queue C)) (cons lst (list (match C
                                                        [(counter status index tt et queue)
                                                         (counter status index 0 0 queue)]))))
    ((< minutes (counter-et C)) (cons lst (list (match C
                                                  [(counter status index tt et queue)
                                                   (counter status index  (- tt minutes) (- et minutes) queue)]))))
    (else (exit (- minutes (counter-et C)) (append lst  (list (cons (counter-et C)(cons (counter-index C) (car (top (counter-queue C))))))) (remove-first-from-counter C)))))
                                         
;daca nr de minute date de comanda serve sunt mai mici decat et se apeleaza pass-time-through-counter
;altfel se merge pe fiecare counter si se scot toti clientii care pot iesi in timpul dat de serve
(define (add-clients-to-list counters lst minutes new-counters)
  (cond
    ((null? counters) (cons  lst (reverse new-counters)))
    ((< minutes (counter-et (car counters)))  (add-clients-to-list (cdr counters) lst minutes (append (list ((pass-time-through-counter minutes) (car counters))) new-counters)))
    (else (add-clients-to-list (cdr counters) (append (car (exit minutes '() (car counters))) lst) minutes (append (cdr (exit minutes '() (car counters))) new-counters)))))

;lista mare a tuturor counterelor se desparte din nou in fast counters si slow counters
(define (split lst f-lst s-lst n index)
  (cond
    ((null? lst) (cons (reverse f-lst) (reverse s-lst)))
    ((<= index n) (split (cdr lst) (append (list (car lst)) f-lst) s-lst (- n 1) (add1 index)))
    (else (split (cdr lst) f-lst (append (list (car lst)) s-lst) (- n 1) (add1 index)))))

;functia care formeaza o pereche dintre coada si indexul unui counter
(define (make-pretty counters)
  (if (null? counters)
      null
      (if (queue-empty? (counter-queue (car counters)))
          (make-pretty (cdr counters))
          (append (list (cons (counter-index (car counters)) (counter-queue (car counters)))) (make-pretty (cdr counters))))))

;functia care inchide casa cu indexul x
(define (close-counter index counters)
  (cond
    ((null? counters) counters)
    ((equal? index (counter-index (car counters))) (append (list (match (car counters)
                                                                   [(counter status index tt et queue)
                                                                    (counter #t index 0 0 empty-queue)])) (close-counter index (cdr counters))))
    (else (append (list (car counters)) (close-counter index (cdr counters))))))



(define (serve requests fast-counters slow-counters ) 
  (serve-helper requests  fast-counters slow-counters '()))

;functia helper pentru serve-ul mare
(define (serve-helper requests fast-counters slow-counters lst) ;cause we always love a new lst
  (define big-list (append fast-counters slow-counters))
  (if (null? requests)
      (cons (reverse lst) (append (make-pretty fast-counters) (make-pretty slow-counters)))
      (match (car requests)
        [(list 'ensure average)(let* ([new-s-counters (add-counters slow-counters average big-list)])
                                 (serve-helper (cdr requests) fast-counters new-s-counters lst))]
        [(list 'close index) (let* ([new-f-counters (close-counter index fast-counters)]
                                    [new-s-counters  (close-counter index slow-counters)])
                               (serve-helper (cdr requests) new-f-counters new-s-counters lst))]
        
        [(list name n-items) (if (> n-items ITEMS)
                                 (let* ([new-s-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))]) (serve-helper (cdr requests) fast-counters new-s-counters lst))
                                 (let* ([new-f-counters (update (add-to-counter name n-items) fast-counters (car (min-tt big-list)))]
                                        [new-s-counters (update (add-to-counter name n-items) slow-counters (car (min-tt big-list)))])
                                   (serve-helper (cdr requests) new-f-counters new-s-counters lst)))]
        
        [(list 'delay index minutes)(let* ([new-f-counters (add-delay fast-counters index minutes)]
                                           [new-s-counters (add-delay slow-counters index minutes)])
                                      (serve-helper (cdr requests) new-f-counters new-s-counters lst))]


        [x (let* ([new-list (add-clients-to-list big-list '() x '())]
                  [clients-list (car new-list)]
                  [new-counters (cdr new-list)]
                  [new-f-counters (car (split new-counters '() '() (length fast-counters) 0))]
                  [new-s-counters (cdr (split new-counters '() '() (length fast-counters) 0))])
             (serve-helper (cdr requests) new-f-counters  new-s-counters (append (sort-clients-et (sort-clients clients-list)) lst)))])))

