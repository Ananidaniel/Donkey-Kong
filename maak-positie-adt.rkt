#lang racket
(#%provide maak-positie-adt)

(define (maak-positie-adt x-coordinaat y-coordinaat)
  
  (define get-x x-coordinaat)
  (define get-y y-coordinaat)
  
  (define (set-x! nieuwe-x)
    (set! get-x nieuwe-x))

  (define (set-y! nieuwe-y)
    (set! get-y nieuwe-y))

  (define (dispatch-maak-positie-adt msg)
    (cond ((eq? msg 'get-x) get-x)
          ((eq? msg 'get-y) get-y)
          ((eq? msg 'set-x!) set-x!)
          ((eq? msg 'set-y!) set-y!)
          (else (error "adt: dispatch-maak-positie-adt -- file: maak-positie-adt -- error:" msg))))
  dispatch-maak-positie-adt)