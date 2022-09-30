#lang racket

(#%require "variabelen-en-procedures.rkt")

(#%provide bonus-adt)

;;;;;;;;;;;;;;;;;
;; Positie-adt ;;
;;;;;;;;;;;;;;;;;

(define (bonus-adt positie)

  ;!  Bonus-status  !;

  (define bonus-status 'nee)

  (define (verander-bonus-status nieuwe-status)
    (set! bonus-status nieuwe-status))

  ;!  Bonus-bots?  !;

  (define (bonus-bots? mario-adt)
    (and (tussen (positie 'get-x)
                 (mario-adt 'get-x)
                 (+ (positie 'get-x) ton-bots?-interval))
         (tussen (- (positie 'get-y) ton-bots?-interval)
                 (mario-adt 'get-y)
                 (+ (positie 'get-y) ton-bots?-interval))))

  ;!  Teken-functie  !;
  
  (define (teken-bonus1! adt)
    ((adt 'teken-bonus1!) dispatch-bonus-adt))
  
  (define (teken-bonus2! adt)
    ((adt 'teken-bonus2!) dispatch-bonus-adt))
  
  (define (teken-bonus3! adt)
    ((adt 'teken-bonus3!) dispatch-bonus-adt))
  
  (define (dispatch-bonus-adt msg)
    (cond ((eq? msg 'get-x) (positie 'get-x))
          ((eq? msg 'get-y) (positie 'get-y))
          ((eq? msg 'set-x!) (positie 'set-x!))
          ((eq? msg 'set-y!) (positie 'set-y!))          
          ((eq? msg 'bonus-status) bonus-status)
          ((eq? msg 'verander-bonus-status) verander-bonus-status)
          ((eq? msg 'bonus-bots?) bonus-bots?)
          ((eq? msg 'teken-bonus1!) teken-bonus1!)
          ((eq? msg 'teken-bonus2!) teken-bonus2!)
          ((eq? msg 'teken-bonus3!) teken-bonus3!)
          (else (error "adt: dispatch-bonus-adt -- file: bonus-adt -- error:" msg))))
  dispatch-bonus-adt)