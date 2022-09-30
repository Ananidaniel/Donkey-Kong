#lang racket

(#%require (only racket/base eof))
(#%provide opslag-score-adt)

; Credit: Noah Van Es (more info on https://github.com/noahvanes)
; Spel: ZombieRun

;;;;;;;;;;;;;;;;;;;;
;;    data-adt    ;;
;;;;;;;;;;;;;;;;;;;;

(define (opslag-score-adt)
  
  (define path "score")
  
  (define (initialize)
    (let ((output-port (open-output-file path #:exists 'can-update)))
      (close-output-port output-port)))
  
  (define (get-path) path)
  
  (define (get-content)
    (let* ((input-port (open-input-file path))
           (content (read input-port)))
      (close-input-port input-port)
      content))
  
  (define (set-content! content)
    (let ((output-port (open-output-file path #:exists 'replace)))
      (write content output-port)
      (close-output-port output-port)))
  
  (define (update-content! content-updater)
    (let ((content (get-content)))
      (set-content! (content-updater content))))
  
  (define (clear-content!)
    (let ((output-port (open-output-file path #:exists 'replace)))
      (close-output-port output-port)))
  
  (define (empty?)
    (eq? (get-content) eof))
  
  (initialize)

  (define (dispatch-opslag-score-adt msg)
    (cond ((eq? msg 'neem-score) (get-content))
          ((eq? msg 'slaag-score) set-content!)
          (else (error "adt: dispatch-opslag-score-adt -- file: opslag-score-adt -- error:" msg))))

  dispatch-opslag-score-adt)