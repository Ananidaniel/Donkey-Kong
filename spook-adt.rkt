#lang racket

(#%require "variabelen-en-procedures.rkt")

(#%provide spook-adt)

;;;;;;;;;;;;;;;;;
;;   Spook-adt   ;;
;;;;;;;;;;;;;;;;;

(define (spook-adt positie)

  ;!  Startposities  !;

  (define start-positie-x (positie 'get-x))
  (define start-positie-y (positie 'get-y))

  ;!  Spook-status  !;

  (define spook-status 'op-platforme)

  (define (verander-spook-status nieuwe-status)
    (set! spook-status nieuwe-status))

  ;!  Spook-richting  !;

  (define spook-richting 'rechts)
  
  (define (verander-spook-richting nieuwe-status)
    (set! spook-richting nieuwe-status))

  (define (reset-spook-richting)
    (set! spook-richting 'rechts))

  ;!  Spook-limiet?  !;

  (define bots? 'nee)

  (define (verander-bots nieuwe-bots)
    (set! bots? nieuwe-bots))

  ;!  Spook-bots?  !;

  (define (spook-mario-bots? mario-adt)
    (bots
     positie
     mario-adt
     mario-adt
     spook-interval
     spook-interval
     spook-interval
     spook-interval))

  ;!  Teken-functie  !;
  
  (define (teken! adt)
    ((adt 'teken-spook!) dispatch-spook-adt))

  (define (dispatch-spook-adt msg)
    (cond ((eq? msg 'get-x) (positie 'get-x))
          ((eq? msg 'get-y) (positie 'get-y))
          ((eq? msg 'set-x!) (positie 'set-x!))
          ((eq? msg 'set-y!) (positie 'set-y!))
          ((eq? msg 'voet) (+ (positie 'get-y) spook-voet))
          ((eq? msg 'dikte) (+ (positie 'get-x) spook-dikte))
          ((eq? msg 'startpositie-x) start-positie-x)
          ((eq? msg 'startpositie-y) start-positie-y)
          ((eq? msg 'richting) spook-richting)
          ((eq? msg 'status) spook-status)          
          ((eq? msg 'verander-status) verander-spook-status)
          ((eq? msg 'verander-richting) verander-spook-richting)
          ((eq? msg 'reset-richting) (reset-spook-richting))
          ((eq? msg 'bots?) bots?)
          ((eq? msg 'verander-bots) verander-bots)
          ((eq? msg 'spook-mario-bots?) spook-mario-bots?)
          ((eq? msg 'teken!) teken!)
          (else (error " adt: dispatch-spook-adt --  file: spook-adt -- error:" msg))))
  dispatch-spook-adt)