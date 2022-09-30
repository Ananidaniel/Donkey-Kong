#lang racket

(#%require "variabelen-en-procedures.rkt")

(#%provide mario-adt)

;;;;;;;;;;;;;;;;
;;  Mario-adt ;;
;;;;;;;;;;;;;;;;

(define (mario-adt positie)

  ;!  Startposities  !;

  (define start-positie-x (positie 'get-x))
  (define start-positie-y (positie 'get-y))

  ;!  Spring-status  !;

  (define spring-status 'nee)

  (define (verander-spring-status nieuwe-status)  
    (set! spring-status nieuwe-status))

  ;!  Ladder-status  !;

  (define ladder-status 'nee)

  (define (verander-ladder-status nieuwe-status)  
    (set! ladder-status nieuwe-status))

  ;!  Mario-status  !;

  (define mario-status 'op-platforme)

  (define (verander-mario-status nieuwe-status)
    (set! mario-status nieuwe-status))

  ;!  Leven-status  !;
  
  (define leven-status 3)

  (define (verander-leven-status)
    (when (not (eq? leven-status 0))
      (cond ((eq? leven-status 1)    
             (set! leven-status 0))
            (else (set! leven-status (- leven-status 1))))))

  (define (reset-leven-status)
    (set! leven-status 3))

  ;!  Teken-functie  !;
    
  (define (teken! mario-adt)
    ((mario-adt 'teken-mario!) dispatch-mario-adt))
  
  (define (dispatch-mario-adt msg)
    (cond ((eq? msg 'get-x) (positie 'get-x))
          ((eq? msg 'get-y) (positie 'get-y))
          ((eq? msg 'set-x!) (positie 'set-x!))
          ((eq? msg 'set-y!) (positie 'set-y!))
          ((eq? msg 'voet) (+ (positie 'get-y) mario-voet))
          ((eq? msg 'dikte) (+ (positie'get-x) mario-dikte))
          ((eq? msg 'startpositie-x) start-positie-x)
          ((eq? msg 'startpositie-y) start-positie-y)
          ((eq? msg 'spring-status) spring-status)
          ((eq? msg 'verander-spring-status) verander-spring-status)
          ((eq? msg 'ladder-status) ladder-status)
          ((eq? msg 'verander-ladder-status) verander-ladder-status)
          ((eq? msg 'status) mario-status)          
          ((eq? msg 'verander-status) verander-mario-status)
          ((eq? msg 'leven) leven-status)
          ((eq? msg 'verander-leven-status) (verander-leven-status))
          ((eq? msg 'reset-leven-status) (reset-leven-status))         
          ((eq? msg 'teken!) teken!) 
          (else (error "adt: dispatch-mario-adt -- file: mario-adt -- error:" msg))))
  dispatch-mario-adt)