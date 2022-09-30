#lang racket

(#%require "variabelen-en-procedures.rkt")

(#%provide afbeelding-adt)

;;;;;;;;;;;;;;;;;;;;
;; afbeelding-adt ;;
;;;;;;;;;;;;;;;;;;;;

(define (afbeelding-adt positie)

  ;!  Teken-functie  !;
  
  (define (teken-help! adt)
    ((adt 'teken-help!) dispatch-afbeelding-adt))
  
  (define (teken-barells! adt)
    ((adt 'teken-barells!) dispatch-afbeelding-adt))

  (define (teken-oil-barell! adt)
    ((adt 'teken-oil-barell!) dispatch-afbeelding-adt))
  
  (define (teken-kong! adt)
    ((adt 'teken-kong!) dispatch-afbeelding-adt))
  
  (define (teken-pauline! adt)
    ((adt 'teken-pauline!) dispatch-afbeelding-adt))

  (define (teken-zwarte-schrem! adt)
    ((adt 'teken-zwarte-schrem!) dispatch-afbeelding-adt))

  (define (dispatch-afbeelding-adt msg)
    (cond ((eq? msg 'get-x) (positie 'get-x))
          ((eq? msg 'get-y) (positie 'get-y))
          ((eq? msg 'set-x!) (positie 'set-x!))
          ((eq? msg 'set-y!) (positie 'set-y!))
          ((eq? msg 'teken-help!) teken-help!)
          ((eq? msg 'teken-barells!) teken-barells!)
          ((eq? msg 'teken-oil-barell!) teken-oil-barell!)
          ((eq? msg 'teken-kong!) teken-kong!)
          ((eq? msg 'teken-pauline!) teken-pauline!)
          ((eq? msg 'teken-zwarte-schrem!) teken-zwarte-schrem!)
          (else (error "adt: dispatch-afbeelding-adt -- file: afbeelding-adt -- error:" msg))))
  dispatch-afbeelding-adt)