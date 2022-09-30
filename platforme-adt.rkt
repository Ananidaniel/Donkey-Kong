#lang racket

(#%require "variabelen-en-procedures.rkt")

(#%provide platforme-adt)

;;;;;;;;;;;;;;;;;;;
;; Platforme-adt ;;
;;;;;;;;;;;;;;;;;;;

(define (platforme-adt positie)

  ;!  Platforme?  !;

  (define (platforme? adt adt-voet)
    (and (tussen (- (positie 'get-x) mario-dikte)
                 (adt 'get-x)
                 (+ (positie 'get-x) platforme-lengte))         
         (tussen (- (positie 'get-y) platforme-interval-verticaal)
                 adt-voet
                 (+ (positie 'get-y) platforme-interval-verticaal))))
  
  ;!  Teken-functie  !;

  (define (teken-platforme-level-1! adt)
    ((adt 'teken-platforme-level-1!) dispatch-platforme-adt))

  (define (teken-platforme-level-2! adt)
    ((adt 'teken-platforme-level-2!) dispatch-platforme-adt))

  (define (teken-kleinplatforme-level-1! adt)
    ((adt 'teken-kleinplatforme-level-1!) dispatch-platforme-adt))

  (define (teken-kleinplatforme-level-2! adt)
    ((adt 'teken-kleinplatforme-level-2!) dispatch-platforme-adt))

  (define (dispatch-platforme-adt msg)
    (cond ((eq? msg 'get-x) (positie 'get-x))
          ((eq? msg 'get-y) (positie 'get-y))
          ((eq? msg 'set-x!) (positie 'set-x!))
          ((eq? msg 'set-y!) (positie 'set-y!))
          ((eq? msg 'platforme?) platforme?)
          ((eq? msg 'teken-platforme-level-1!) teken-platforme-level-1!)
          ((eq? msg 'teken-platforme-level-2!) teken-platforme-level-2!)
          ((eq? msg 'teken-kleinplatforme-level-1!) teken-kleinplatforme-level-1!)
          ((eq? msg 'teken-kleinplatforme-level-2!) teken-kleinplatforme-level-2!)
          (else (error "adt: dispatch-platforme-adt -- file: platforme-adt -- error:" msg))))
  dispatch-platforme-adt)