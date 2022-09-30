#lang racket

(#%require "variabelen-en-procedures.rkt")

(#%provide ton-adt)

;;;;;;;;;;;;;;;;;
;;   Ton-adt   ;;
;;;;;;;;;;;;;;;;;

(define (ton-adt positie)

  ;!  Startposities  !;

  (define start-positie-x (positie 'get-x))
  (define start-positie-y (positie 'get-y))

  ;!  Ton-status  !;

  (define ton-status 'op-platforme)

  (define (verander-ton-status nieuwe-status)
    (set! ton-status nieuwe-status))

  (define (reset-ton-status)
    (set! ton-status 'op-platforme))

  ;!  Ladder-status  !;

  (define ladder-status 'nee)

  (define (verander-ladder-status nieuwe-status)  
    (set! ladder-status nieuwe-status))

  ;!  Ton-richting  !;

  (define ton-richting 'rechts)
  
  (define (verander-ton-richting nieuwe-status)
    (set! ton-richting nieuwe-status))

  (define (reset-ton-richting)
    (set! ton-richting 'rechts))

  ;!  Ton?  !;

  (define ton?-status 'nee)

  (define (verander-ton?-status nieuwe-status)
    (set! ton?-status nieuwe-status))
 
  (define (ton? mario-adt)
    (bots positie
          mario-adt
          mario-adt
          ton-dikte
          ton-bots?-interval
          ton?-interval
          nul))
    
  ;!  Ton-bots?  !;

  (define (ton-bots? mario-adt)
    (bots
     positie
     mario-adt
     mario-adt
     ton-bots?-interval
     ton-bots?-interval
     ton-bots?-interval
     ton-bots?-interval))

  ;!  Teken-functie  !;
  
  (define (teken! adt)
    ((adt 'teken-ton!) dispatch-ton-adt))

  (define (dispatch-ton-adt msg)
    (cond ((eq? msg 'get-x) (positie 'get-x))
          ((eq? msg 'get-y) (positie 'get-y))
          ((eq? msg 'set-x!) (positie 'set-x!))
          ((eq? msg 'set-y!) (positie 'set-y!))
          ((eq? msg 'voet) (+ (positie 'get-y) ton-voet))
          ((eq? msg 'dikte) (+ (positie 'get-x) ton-dikte))
          ((eq? msg 'startpositie-x) start-positie-x)
          ((eq? msg 'startpositie-y) start-positie-y)
          ((eq? msg 'status) ton-status)
          ((eq? msg 'verander-status) verander-ton-status)
          ((eq? msg 'ladder-status) ladder-status)
          ((eq? msg 'verander-ladder-status) verander-ladder-status)
          ((eq? msg 'reset-status) (reset-ton-status))
          ((eq? msg 'richting) ton-richting)
          ((eq? msg 'verander-richting) verander-ton-richting)
          ((eq? msg 'reset-richting) (reset-ton-richting))
          ((eq? msg 'ton?) ton?)
          ((eq? msg 'ton?-status) ton?-status)
          ((eq? msg 'verander-ton?-status) verander-ton?-status)
          ((eq? msg 'ton-bots?) ton-bots?)
          ((eq? msg 'teken!) teken!)
          (else (error " adt: dispatch-ton-adt --  file: ton-adt -- error:" msg))))
  dispatch-ton-adt)