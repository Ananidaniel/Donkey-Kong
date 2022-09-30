#lang racket

;;;;;;;;;;;;;;;;;;
;;  Variabelen  ;;
;;;;;;;;;;;;;;;;;;

(#%provide px-venster-hoogte
           px-venster-breedte
           px-element-breedte
           px-element-hoogte
           ton-punten
           timer
           bonus-1-punten
           bonus-2-punten
           bonus-3-punten
           nul
           ;!  Ton-adt  !;
           ton-voet
           ton-dikte
           ton?-interval
           ton-bots?-interval
           scherm-onderaan
           ;!  Spook-adt  !;
           spook-voet
           spook-dikte
           spook-interval
           begin-spook
           eind-spook
           ;!  Ladder-adt  !;
           ladder-horizontaal-interval
           platforme-dikte
           mario-lengte
           ;!  Mario-adt  !;
           mario-voet
           mario-dikte
           ;!  Platforme-adt  !;
           platforme-lengte
           platforme-interval-verticaal
           ;!  Beweeg-adt  !;
           horizontale-verplaating
           verticaal-verplaatsing
           spring-verplaatsing)

(define px-venster-hoogte  700)
(define px-venster-breedte 700)
(define px-element-breedte (/ px-venster-hoogte 70))
(define px-element-hoogte (/ px-venster-breedte 70))

(define ton-punten 100)
(define bonus-1-punten 100)
(define bonus-2-punten 300)
(define bonus-3-punten 500)
(define timer 120)

;!  Ton-adt  !;

(define ton-voet 0.030)
(define ton-dikte 0.030)
(define ton?-interval 0.070) 
(define ton-bots?-interval 0.040)
(define scherm-onderaan 1)
(define nul 0)

;!  Spook-adt  !;

(define spook-voet 0.50)
(define spook-dikte 0.030)
(define spook-interval 0.04)
(define begin-spook 0.1)
(define eind-spook 0.8)

;!  Ladder-adt  !;

(define ladder-horizontaal-interval 0.010)
(define platforme-dikte 0.025)   
(define mario-lengte 0.055)

;!  Mario-adt  !;

(define mario-voet 0.060)
(define mario-dikte 0.050)

;!  Platforme-adt  !;

(define platforme-lengte 0.750)
(define platforme-interval-verticaal 0.010)

;!  Beweeg-adt  !;

(define horizontale-verplaating 0.0100)
(define verticaal-verplaatsing 0.0050)
(define spring-verplaatsing 0.0500)

;;;;;;;;;;;;;;;;;;
;;  Procedures  ;;
;;;;;;;;;;;;;;;;;;

(#%provide visie
           tussen           
           bots)

(define (visie status) ;een display procedure
  (display "[")(display status)(display "]"))

(define (tussen klein x groot)
  (and (< klein x)(< x groot)))

  (define (bots positie adt1 adt2 lim1 lim2 lim3 lim4)
    (and (tussen (- (positie 'get-x) lim1)
                 (adt1 'get-x)
                 (+ (positie 'get-x) lim2))         
         (tussen (- (positie 'get-y) lim3)
                 (adt2 'get-y)
                 (+ (positie 'get-y) lim4))))