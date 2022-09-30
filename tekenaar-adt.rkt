#lang racket
(#%require "graphics.rkt"
           "opslag-score-adt.rkt")
(#%provide maak-teken-adt)

(define (maak-teken-adt titel pixels-verticaal pixels-horizontaal)

  ;;;;;;;;;;;;;;;;;;;;
  ;;  Configuratie  ;;
  ;;;;;;;;;;;;;;;;;;;;

  (define venster (make-window  pixels-horizontaal pixels-verticaal titel))
  
  ((venster 'set-background!) "black")

  ;!  Layers  !;
  
  (define platforme-layer (venster 'make-layer))
  (define ladder-layer (venster 'make-layer))
  (define bonus1-layer (venster 'make-layer))
  (define bonus2-layer (venster 'make-layer))
  (define bonus3-layer (venster 'make-layer))
  (define ton-layer (venster 'make-layer))
  (define spook-layer (venster 'make-layer))
  (define help-layer (venster 'make-layer))
  (define barells-layer (venster 'make-layer))
  (define kong-layer (venster 'make-layer))
  (define pauline-layer (venster 'make-layer))
  (define zwarte-layer (venster 'make-layer))
  (define mario-layer (venster 'make-layer))  

  ;!  Tiles  !;
  
  (define bonus1-tile (make-bitmap-tile "sprites/bonus1.png" "sprites/bonus1_mask.png"))
  (define bonus2-tile (make-bitmap-tile "sprites/bonus2.png" "sprites/bonus2_mask.png"))
  (define bonus3-tile (make-bitmap-tile "sprites/bonus3.png" "sprites/bonus3_mask.png")) 
  (define barells-tile (make-bitmap-tile "sprites/barells.png" "sprites/barells_mask.png"))
  (define pauline-tile (make-bitmap-tile "sprites/pauline.png" "sprites/pauline_mask.png"))
  (define zwarte-scherm-tile (make-bitmap-tile "sprites/zwarte-scherm.png"))
  (define mario-tile (make-bitmap-tile "sprites/marioRight.png" "sprites/marioRight_mask.png"))

  (define kong-tiles (make-tile-sequence (list
                                          (make-bitmap-tile "sprites/Kong1.png" "sprites/Kong1_mask.png")
                                          (make-bitmap-tile "sprites/Kong3.png" "sprites/Kong3_mask.png"))))
  
  (define oil-barell-tiles (make-tile-sequence (list
                                                (make-bitmap-tile "sprites/OilBarrel1.png" "sprites/OilBarrel1_mask.png")
                                                (make-bitmap-tile "sprites/OilBarrel2.png" "sprites/OilBarrel2_mask.png"))))

  (define help-tiles (make-tile-sequence (list
                                          (make-bitmap-tile "sprites/help-sign.png" "sprites/help-sign_mask.png")
                                          (make-bitmap-tile "sprites/empty.png" "sprites/empty_mask.png"))))
  
  ;;;;;;;;;;;;;;;;;;;;
  ;; Teken functies ;;
  ;;;;;;;;;;;;;;;;;;;;
  
  ;!  teken-adt!  !;
  
  (define (teken-adt! adt tile)    
    (let ((adt-x (* pixels-horizontaal (adt 'get-x)))
          (adt-y (* pixels-verticaal (adt 'get-y))))
      
      ((tile 'set-x!) adt-x)
      ((tile 'set-y!) adt-y)))

  (define (teken-bonus1! adt)
    (teken-adt! adt bonus1-tile))

  (define (teken-bonus2! adt)
    (teken-adt! adt bonus2-tile))

  (define (teken-bonus3! adt)
    (teken-adt! adt bonus3-tile))

  (define (teken-zwarte-schrem! adt)
    (teken-adt! adt zwarte-scherm-tile))
  
  (define (teken-barells! adt)
    (teken-adt! adt barells-tile))
  
  (define (teken-pauline! adt)
    (teken-adt! adt pauline-tile))

  (define (teken-mario! adt)
    (teken-adt! adt mario-tile))
  
  ;!  teken-sequente!  !;
  
  (define (teken-sequent! adt tiles)    
    (let ((adt-x (* pixels-horizontaal (adt 'get-x)))
          (adt-y (* pixels-verticaal (adt 'get-y))))
      
      ((tiles 'set-x!) adt-x)
      ((tiles 'set-y!) adt-y)
      (tiles 'set-next!)))

  (define (teken-oil-barell! adt)
    (teken-sequent! adt oil-barell-tiles))
  
  (define (teken-kong! adt)
    (teken-sequent! adt kong-tiles))

  (define (teken-help! adt)
    (teken-sequent! adt help-tiles))
  
  ;!  teken-stukken-adt!  !;

  (define spelbord-tiles '())

  (define (teken-stukken-adt! adt tile laag)
    (let ((adt-x (* pixels-horizontaal (adt 'get-x)))
          (adt-y (* pixels-verticaal (adt 'get-y)))
          (nieuwe-tile tile))
      
      (set! spelbord-tiles (cons tile spelbord-tiles))
      ((nieuwe-tile 'set-x!) adt-x)
      ((nieuwe-tile 'set-y!) adt-y)
      ((laag 'add-drawable) nieuwe-tile)))

  (define (teken-platforme-level-1! adt)
    (teken-stukken-adt! adt (make-bitmap-tile "sprites/platforme.png") platforme-layer))
  
  (define (teken-ladder-level-1! adt)
    (teken-stukken-adt! adt (make-bitmap-tile "sprites/ladder.png") ladder-layer))
  
  (define (teken-kleinplatforme-level-1! adt)
    (teken-stukken-adt! adt (make-bitmap-tile "sprites/kleinplatforme.png") platforme-layer))

  (define (teken-platforme-level-2! adt)
    (teken-stukken-adt! adt (make-bitmap-tile "sprites/platforme2.png") platforme-layer))
  
  (define (teken-ladder-level-2! adt)
    (teken-stukken-adt! adt (make-bitmap-tile "sprites/ladder2.png") ladder-layer))
  
  (define (teken-kleinplatforme-level-2! adt)
    (teken-stukken-adt! adt (make-bitmap-tile "sprites/kleinplatforme2.png") platforme-layer))
  
  (define (verwijder-spelbord!)
    (for-each verwijder spelbord-tiles)
    (set! spelbord-tiles  '()))

  (define (verwijder tile)
    ((ladder-layer 'remove-drawable) tile)
    (tile 'clear)
    ((platforme-layer 'remove-drawable) tile)
    (tile 'clear))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Teken ton en spook functie ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
  (define ton-tiles '())

  (define (voeg-ton ton-adt)
    (let ((nieuwe-tile (make-tile-sequence (list
                                            (make-bitmap-tile "sprites/ton.png" "sprites/ton_mask.png")
                                            (make-bitmap-tile "sprites/ton2.png" "sprites/ton2_mask.png")
                                            (make-bitmap-tile "sprites/ton3.png" "sprites/ton3_mask.png")
                                            (make-bitmap-tile "sprites/ton4.png" "sprites/ton4_mask.png")))))
      
      (set! ton-tiles (cons (cons ton-adt nieuwe-tile) ton-tiles))
      ((ton-layer 'add-drawable) nieuwe-tile)))

  (define spook-tiles '())

  (define (voeg-spook spook-adt)
    (let ((nieuwe-tile (make-tile-sequence (list
                                            (make-bitmap-tile "sprites/spook.png" "sprites/spook_mask.png")
                                            (make-bitmap-tile "sprites/spook2.png" "sprites/spook2_mask.png")
                                            (make-bitmap-tile "sprites/spook3.png" "sprites/spook3_mask.png")
                                            (make-bitmap-tile "sprites/spook4.png" "sprites/spook4_mask.png")
                                            (make-bitmap-tile "sprites/spook5.png" "sprites/spook5_mask.png")
                                            (make-bitmap-tile "sprites/spook6.png" "sprites/spook6_mask.png")
                                            (make-bitmap-tile "sprites/spook7.png" "sprites/spook7_mask.png")
                                            (make-bitmap-tile "sprites/spook8.png" "sprites/spook8_mask.png")))))
      
      (set! spook-tiles (cons (cons spook-adt nieuwe-tile) spook-tiles))
      ((spook-layer 'add-drawable) nieuwe-tile)))

  (define (neem-tile adt lijst)
    (cdr (assoc adt lijst)))

  (define (teken-meerdere-adt! adt lijst)
    (let* ((adt-x    (* pixels-horizontaal (adt 'get-x)))
           (adt-y    (* pixels-verticaal   (adt 'get-y)))
           (tile (neem-tile adt lijst)))
      
      ((tile 'set-x!) adt-x)
      ((tile 'set-y!) adt-y)
      (tile 'set-next!)))

  (define (teken-ton ton-adt)
    (teken-meerdere-adt! ton-adt ton-tiles))

  (define (teken-spook spook-adt)
    (teken-meerdere-adt! spook-adt spook-tiles))
  
  (define (verwijder-tonnen!)
    (for-each (lambda (x) ((ton-layer 'remove-drawable)(cdr x))((cdr x) 'clear))ton-tiles)
    (set! ton-tiles  '()))

  (define (verwijder-spoken!)
    (for-each (lambda (x) ((spook-layer 'remove-drawable)(cdr x))((cdr x) 'clear))spook-tiles)
    (set! spook-tiles  '()))

  ;;;;;;;;;;;;;;;;
  ;;    Bord    ;;
  ;;;;;;;;;;;;;;;;

  (define file (opslag-score-adt))
  (define huidige-score 0)
  
  (define (bord-adt)
  
    ;! Tekst !;
  
    (define tekst (make-tile 700 700))
    (define layer-tekst (venster 'make-layer))

    ;! High-Score !;
  
    (define high-score-tile (make-tile 700 700))
    (define layer-high-score (venster 'make-layer))   
    ((tekst 'draw-text)"High Score" 15 150 10 "red")

    (define (print-high-score)
      (high-score-tile 'clear)
      ((high-score-tile 'draw-text) (number->string (car (sort (file 'neem-score) >))) 15 150 30 "white"))      

    ;! Score !;
  
    (define score-tile (make-tile 700 700))
    (define layer-score (venster 'make-layer))   
    ((tekst 'draw-text)"Score" 15 300 10 "red")
    
    (define (print-score score)
      (score-tile 'clear)
      (set! huidige-score (+ huidige-score score))
      ((score-tile 'draw-text)(number->string huidige-score) 15 300 30 "white"))
    
    (define (reset-score)
      (score-tile 'clear)
      (when (> huidige-score 0)
        ((file 'slaag-score)(cons huidige-score (file 'neem-score))))
      (display (file 'neem-score))
      (set! huidige-score 0)
      ((score-tile 'draw-text)(number->string huidige-score) 15 300 30 "white"))
  
    ;! Tijd !;
  
    (define tijd-tile (make-tile 700 700))
    (define layer-tijd (venster 'make-layer))
    ((tekst 'draw-text)"Tijd" 15 450 10 "red")

    (define (print-tijd tijd)
      (tijd-tile 'clear)
      ((tijd-tile 'draw-text)(number->string tijd) 15 450 30 "white"))
  
    ;! Leven !;
  
    (define leven-tile (make-tile 700 700))
    (define layer-leven (venster 'make-layer))
    ((tekst 'draw-text)"Levens" 15 560 10 "red")

    (define (print-leven leven)
      (leven-tile 'clear)
      ((leven-tile 'draw-text)(number->string leven) 15 560 30 "white"))

    ;! Level !;
  
    (define level-tile (make-tile 700 700))
    (define layer-level (venster 'make-layer))
    ((tekst 'draw-text)"L=" 15 560 70 "blue")

    (define (print-level level)
      (level-tile 'clear)
      ((level-tile 'draw-text)(number->string level) 15 590 70 "blue"))

    ;! Aan - Uit !;

    (define (aan)
      ((layer-tekst 'add-drawable) tekst)
      ((layer-high-score 'add-drawable) high-score-tile)
      ((layer-score 'add-drawable) score-tile)      
      ((layer-tijd 'add-drawable) tijd-tile)
      ((layer-leven 'add-drawable) leven-tile)
      ((layer-level 'add-drawable) level-tile))
  
    (define (uit)
      ((layer-tekst 'remove-drawable) tekst)
      ((layer-high-score 'remove-drawable) high-score-tile)
      ((layer-score 'remove-drawable) score-tile)
      ((layer-tijd 'remove-drawable) tijd-tile)
      ((layer-leven 'remove-drawable) leven-tile)
      ((layer-level 'remove-drawable) level-tile))
  
    ;! Dispatch !;

    (define (dispatch-bord-adt msg)
      (cond ((eq? msg 'print-score)print-score)
            ((eq? msg 'print-high-score) (print-high-score))
            ((eq? msg 'reset-score)(reset-score))
            ((eq? msg 'print-tijd) print-tijd)
            ((eq? msg 'print-leven) print-leven)
            ((eq? msg 'print-level) print-level)
            ((eq? msg 'aan) (aan))
            ((eq? msg 'uit)(uit))
            (else (error " adt: dispatch-bord-adt --  file: tekenaar-adt -- error:" msg))))
    dispatch-bord-adt)

  ;;;;;;;;;;;;;;;;
  ;; Menu-score ;;
  ;;;;;;;;;;;;;;;;

  (define (score-menu-adt)
  
    ;! Tekst !;

    (define teken-status 'nee)
  
    (define tekst (make-tile 700 700))
    (define layer-tekst (venster 'make-layer))
    (define score-tile (make-tile 700 700))
    (define layer-score (venster 'make-layer))

    ((tekst 'draw-text) "High Score" 60 165 50 "red")
    ((tekst 'draw-text) "Rank" 25 200 200 "red")
    ((tekst 'draw-text) "Score" 25 400 200 "red")

    (define hoogte 250)
    (define rank 0)
    (define limiet (length (file 'neem-score)))

    (define kleur-lijst (list "red" "darkorange" "yellow" "green" "blue"))

    (define (print-score-lijst)
      (for-each print-score (sort (file 'neem-score) >)))

    (define (print-score score)
      (when (and (< rank limiet)
                 (> score 0))
        (set! rank (+ rank 1))
        ((score-tile 'draw-text)(number->string rank) 25 200 hoogte (car kleur-lijst))
        ((score-tile 'draw-text)(number->string score) 25 400 hoogte (car kleur-lijst))        
        (set! kleur-lijst (cdr kleur-lijst))
        (set! hoogte (+ hoogte 50))))

    (define (reset-score)
      (set! rank 0)
      (set! hoogte 250)
      (set! kleur-lijst (list "red" "darkorange" "yellow" "green" "blue")))

    (define (aan)
      (when (eq? teken-status 'nee)
        ((layer-tekst 'add-drawable) tekst)
        ((layer-score 'add-drawable) score-tile)
        (set! teken-status 'ja)))
  
    (define (uit)
      ((layer-tekst 'remove-drawable) tekst)
      ((layer-score 'remove-drawable) score-tile)
      (set! teken-status 'nee))
  
    ;! Dispatch !;

    (define (dispatch-score-menu-adt msg)
      (cond ((eq? msg 'aan) (aan))
            ((eq? msg 'print-score-lijst)(print-score-lijst))
            ((eq? msg 'uit)(uit))
            (else (error " adt: dispatch-score-menu-adt --  file: tekenaar-adt -- error:" msg))))
    dispatch-score-menu-adt)

  ;;;;;;;;;;;;;;;;
  ;;    Menu    ;;
  ;;;;;;;;;;;;;;;;
  
  (define (menu-adt)

    ;! Tekst !;

    (define teken-status 'nee)
  
    (define tekst (make-tile 700 700))
    (define layer-tekst (venster 'make-layer))
    (define score-tile (make-tile 700 700))
    (define layer-score (venster 'make-layer))
    (define vuur-tile (make-tile 700 700 "Sprites/vuur2.png"))
    (define layer-vuur (venster 'make-layer))
  
    (define lengte 60)
    (define hoogte 350)
    (define dikte 25)
    (define index 0)

    (define (rechts)
      (when (< index 2)
        (score-tile 'clear)
        (set! index (+ index 1))
        (set! lengte (+ lengte 200))
        ((vuur-tile 'set-x!)lengte)
        ((vuur-tile 'set-y!)hoogte)))
  
    (define (links)
      (when (> index 0)
        (score-tile 'clear)
        (set! index (- index 1))
        (set! lengte (- lengte 200))
        ((vuur-tile 'set-x!)lengte)
        ((vuur-tile 'set-y!)hoogte)))
  
    (define (aan)
      (when (eq? teken-status 'nee)
        (set! index 0)
        (set! lengte 60)
        ((vuur-tile 'set-x!)lengte)
        ((vuur-tile 'set-y!)hoogte)
        ((tekst 'draw-text) "Donkey Kong" 60 120 100 "red")
        ((tekst 'draw-text) "Spel" 20 120 350 "red")
        ((tekst 'draw-text) "Scores" 20 300 350 "red")
        ((tekst 'draw-text) "Exit" 20 500 350 "red")
        ((layer-vuur 'add-drawable) vuur-tile)
        ((layer-tekst 'add-drawable) tekst)
        ((layer-score 'add-drawable) score-tile)
        (set! teken-status 'ja)))
  
    (define (uit)      
      ((layer-vuur 'remove-drawable) vuur-tile)
      ((layer-tekst 'remove-drawable) tekst)
      ((layer-score 'remove-drawable) score-tile)
      (set! teken-status 'nee))
  
    ;! Dispatch !;

    (define (dispatch-menu-adt msg)
      (cond ((eq? msg 'index) index)
            ((eq? msg 'aan) (aan))
            ((eq? msg 'rechts) (rechts))
            ((eq? msg 'links) (links))
            ((eq? msg 'uit) (uit))))
    dispatch-menu-adt)
  
  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; Spel-lus functies ;;
  ;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (set-toets-functie! toets-functie)
    ((venster 'set-key-callback!) toets-functie))
  
  (define (set-spel-lus-functie! spel-lus-functie)
    ((venster 'set-update-callback!) spel-lus-functie))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Aan-Uit functies  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;


  (define (verwijder-bonus1)
    (bonus1-tile 'clear)
    ((bonus1-layer 'remove-drawable) bonus1-tile))
  
  (define (verwijder-bonus2)  
    (bonus2-tile 'clear)
    ((bonus2-layer 'remove-drawable) bonus2-tile))
  
  (define (verwijder-bonus3)  
    (bonus3-tile 'clear)    
    ((bonus3-layer 'remove-drawable) bonus3-tile))

  (define (spel-aan)
    ((bonus1-layer 'add-drawable) bonus1-tile)
    ((bonus2-layer 'add-drawable) bonus2-tile)
    ((bonus3-layer 'add-drawable) bonus3-tile)
    ((help-layer 'add-drawable) help-tiles)
    ((barells-layer 'add-drawable) barells-tile)
    ((barells-layer 'add-drawable) oil-barell-tiles)
    ((kong-layer 'add-drawable) kong-tiles)
    ((pauline-layer 'add-drawable) pauline-tile)
    ((mario-layer 'add-drawable) mario-tile)
    ((zwarte-layer 'add-drawable) zwarte-scherm-tile))

  (define (spel-uit)
    (help-tiles 'clear)
    (barells-tile 'clear)
    (oil-barell-tiles 'clear)
    (kong-tiles 'clear)
    (pauline-tile 'clear)
    (mario-tile 'clear)
    (zwarte-scherm-tile 'clear)
    
    ((help-layer 'remove-drawable) help-tiles)
    ((barells-layer 'remove-drawable) barells-tile)
    ((barells-layer 'remove-drawable) oil-barell-tiles)
    ((kong-layer 'remove-drawable) kong-tiles)
    ((pauline-layer 'remove-drawable) pauline-tile)
    ((mario-layer 'remove-drawable) mario-tile)
    ((zwarte-layer 'add-drawable) zwarte-scherm-tile)
    (verwijder-bonus1)
    (verwijder-bonus2)
    (verwijder-bonus3))
  
  ;;;;;;;;;;;;;;;;;;
  ;;   Dispatch   ;;
  ;;;;;;;;;;;;;;;;;;
  
  (define (dispatch-teken-adt msg)
    (cond ((eq? msg 'teken-platforme-level-1!) teken-platforme-level-1!)
          ((eq? msg 'teken-ladder-level-1!) teken-ladder-level-1!)
          ((eq? msg 'teken-kleinplatforme-level-1!) teken-kleinplatforme-level-1!)
          
          ((eq? msg 'teken-platforme-level-2!) teken-platforme-level-2!)
          ((eq? msg 'teken-ladder-level-2!) teken-ladder-level-2!)
          ((eq? msg 'teken-kleinplatforme-level-2!) teken-kleinplatforme-level-2!)

          ((eq? msg 'verwijder-spelbord!) (verwijder-spelbord!))
          
          ((eq? msg 'teken-bonus1!) teken-bonus1!)
          ((eq? msg 'teken-bonus2!) teken-bonus2!)
          ((eq? msg 'teken-bonus3!) teken-bonus3!)
          
          ((eq? msg 'teken-help!) teken-help!)
          ((eq? msg 'teken-barells!) teken-barells!)
          ((eq? msg 'teken-oil-barell!) teken-oil-barell!)
          ((eq? msg 'teken-kong!) teken-kong!)
          ((eq? msg 'teken-pauline!) teken-pauline!)
          ((eq? msg 'teken-mario!) teken-mario!)
          ((eq? msg 'teken-zwarte-schrem!) teken-zwarte-schrem!)
          
          ((eq? msg 'teken-ton!) teken-ton)          
          ((eq? msg 'nieuw-ton!) voeg-ton)
          ((eq? msg 'verwijder-tonnen!) (verwijder-tonnen!))
                  
          ((eq? msg 'teken-spook!) teken-spook)
          ((eq? msg 'nieuw-spook!) voeg-spook)
          ((eq? msg 'verwijder-spoken!) (verwijder-spoken!))
           
          ((eq? msg 'bord-adt) (bord-adt))
          ((eq? msg 'menu-adt) (menu-adt))
          ((eq? msg 'score-menu-adt) (score-menu-adt))
          ((eq? msg 'spel-aan) (spel-aan))
          ((eq? msg 'spel-uit) (spel-uit))

          ((eq? msg 'verwijder-bonus1) (verwijder-bonus1))
          ((eq? msg 'verwijder-bonus2) (verwijder-bonus2))
          ((eq? msg 'verwijder-bonus3) (verwijder-bonus3))
          
          ((eq? msg 'set-toets-functie!) set-toets-functie!)
          ((eq? msg 'set-spel-lus-functie!) set-spel-lus-functie!)
          
          (else (error " adt: dispatch-teken-adt --  file: tekenaar-adt -- error:" msg))))
  dispatch-teken-adt)