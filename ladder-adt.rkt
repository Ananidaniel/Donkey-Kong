#lang racket

(#%require "variabelen-en-procedures.rkt")

(#%provide ladder-adt)

;;;;;;;;;;;;;;;;
;; Ladder-adt ;;
;;;;;;;;;;;;;;;;

(define (ladder-adt positie)

  ;!  Ladder?  !;

  (define (ladder? adt)
    (bots positie
          adt
          adt
          ladder-horizontaal-interval
          ladder-horizontaal-interval
          (+ mario-lengte platforme-dikte)
          mario-lengte))

  ;!  Teken-functie  !;
               
  (define (teken-level-1! ladder-adt)
    ((ladder-adt 'teken-ladder-level-1!) dispatch-ladder-adt))

   (define (teken-level-2! ladder-adt)
    ((ladder-adt 'teken-ladder-level-2!) dispatch-ladder-adt))

  (define (dispatch-ladder-adt msg)
    (cond ((eq? msg 'get-x) (positie 'get-x))
          ((eq? msg 'get-y) (positie 'get-y))
          ((eq? msg 'set-x!) (positie 'set-x!))
          ((eq? msg 'set-y!) (positie 'set-y!))
          ((eq? msg 'ladder?) ladder?)
          ((eq? msg 'teken-level-1!) teken-level-1!)
          ((eq? msg 'teken-level-2!) teken-level-2!)
          (else (error "adt: dispatch-ladder-adt -- file: ladder-adt -- error:" msg))))
  dispatch-ladder-adt)