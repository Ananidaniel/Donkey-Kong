#lang racket
(#%require "tekenaar-adt.rkt"
           "maak-positie-adt.rkt"
           "variabelen-en-procedures.rkt"
           "afbeelding-adt.rkt"
           "ladder-adt.rkt"
           "platforme-adt.rkt"
           "ton-adt.rkt"
           "spook-adt.rkt"
           "bonus-adt.rkt"
           "mario-adt.rkt")

(define (spel-adt)
  
  ;;;;;;;;;;;;;;;;;;
  ;; Configuratie ;;
  ;;;;;;;;;;;;;;;;;;

  ;!  Teken  !;

  (define venster (maak-teken-adt "donkey kong" px-venster-hoogte px-venster-breedte))

  ;!  Bord  !;
  
  (define bord (venster 'bord-adt))

  ;!  Menu  !;

  (define menu (venster 'menu-adt))

  (define menu-score (venster 'score-menu-adt))

  ;!  Bonus  !;

  (define bonus-1 (bonus-adt (maak-positie-adt 0.600 0.910)))
  (define bonus-2 (bonus-adt (maak-positie-adt 0.500 0.540)))
  (define bonus-3 (bonus-adt (maak-positie-adt 0.450 0.310)))

  ;!  Afbeeldingen  !;

  (define pauline (afbeelding-adt (maak-positie-adt 0.440 0.160)))
  (define kong (afbeelding-adt (maak-positie-adt 0.200 0.240)))
  (define help (afbeelding-adt (maak-positie-adt 0.500 0.165))) 
  (define barells (afbeelding-adt (maak-positie-adt 0.100 0.240)))
  (define oil-barell (afbeelding-adt (maak-positie-adt 0.200 0.880)))
  (define zwarte-scherm (afbeelding-adt (maak-positie-adt 0 0)))

  ;!  Mario  !;
  
  (define mario (mario-adt (maak-positie-adt 0.300 0.900)))
    
  ;!  Ladders  !;

  (define ladders (list
                   (ladder-adt (maak-positie-adt 0.720 0.865))
                   (ladder-adt (maak-positie-adt 0.300 0.745))
                   (ladder-adt (maak-positie-adt 0.720 0.621))
                   (ladder-adt (maak-positie-adt 0.300 0.501))
                   (ladder-adt (maak-positie-adt 0.720 0.380))
                   (ladder-adt (maak-positie-adt 0.560 0.260))))

  (define (teken-stukken-ladders! lijst level-ladders)
    (for-each (lambda (ladder-adt) ((ladder-adt level-ladders) venster)) lijst))

  ;!  Platformes  !;
  
  (define platformes (list
                      (platforme-adt (maak-positie-adt 0.200 0.960))
                      (platforme-adt (maak-positie-adt 0.100 0.839))
                      (platforme-adt (maak-positie-adt 0.200 0.718))
                      (platforme-adt (maak-positie-adt 0.100 0.597))
                      (platforme-adt (maak-positie-adt 0.200 0.476))
                      (platforme-adt (maak-positie-adt 0.100 0.355))
                      (platforme-adt (maak-positie-adt 0.440 0.235))))

  (define (teken-stukken-platforme! lijst level-platforme level-klein-platforme)
    (cond ((null? (cdr lijst))
           (((last lijst) level-klein-platforme) venster))
          (else (((car lijst) level-platforme) venster)
                (teken-stukken-platforme! (cdr lijst) level-platforme level-klein-platforme))))
  
  ;;;;;;;;;;;;;;;;
  ;; Beweeg-adt ;;
  ;;;;;;;;;;;;;;;;
  
  (define (beweeg-adt)

    ;!  Beweeg  !;

    (define (beweeg get set  +/- adt verplaatsing)
      ((adt set)(+/- (adt get) verplaatsing)))             

    ;!  Horizontaal-functie  !;
    
    (define (rechts-beweeg adt)     
      (cond ((ormap (lambda (lijst) (platforme-bots? adt lijst)) platformes)
             ((adt 'verander-status) 'op-platforme)
             (rechts adt))
            (else (when (eq? (adt 'ladder-status) 'nee)
                    ((adt 'verander-status) 'val)))))

    (define (links-beweeg adt)      
      (cond ((ormap (lambda (lijst) (platforme-bots? adt lijst)) platformes)
             ((adt 'verander-status) 'op-platforme)
             (links adt))
            (else (when (eq? (adt 'ladder-status) 'nee)
                    ((adt 'verander-status) 'val)))))
    
    (define (rechts adt)
      (beweeg 'get-x 'set-x! + adt horizontale-verplaating))
    
    (define (links adt)
      (beweeg 'get-x 'set-x! - adt horizontale-verplaating))
    
    ;!  Verticaal-functie !;

    (define (klim-omhoog mario-adt)
      (cond ((ormap (lambda (lijst) (ladder-bots? mario-adt lijst)) ladders)
             (beweeg 'get-y 'set-y! - mario-adt horizontale-verplaating)
             ((mario-adt 'verander-ladder-status) 'ja))
            (else ((mario-adt 'verander-ladder-status) 'nee))))

    (define (klim-omlaag mario-adt)
      (cond ((ormap (lambda (lijst) (ladder-bots? mario-adt lijst)) ladders)            
             (beweeg 'get-y 'set-y! + mario-adt horizontale-verplaating)
             ((mario-adt 'verander-ladder-status) 'ja))
            (else ((mario-adt 'verander-ladder-status) 'nee))))

    (define (zwaartekracht adt)
      (cond ((not (ormap (lambda (lijst) (platforme-bots? adt lijst)) platformes))
                  
             (beweeg 'get-y 'set-y! + adt horizontale-verplaating))
            (else ((adt 'verander-status) 'op-platforme))))

    ;!  Spring-functie  !;

    (define (spring-boven mario-adt)
      (beweeg 'get-y 'set-y! - mario-adt horizontale-verplaating))

    (define teller 5)
    
    (define (boven mario-adt)
      (cond ((> teller 0)
             (spring-boven mario-adt)
             (set! teller (- teller 1)))              
            (else ((mario-adt 'verander-status) 'val)
                  ((mario-adt 'verander-spring-status) 'nee)
                  (set! teller 5))))
                      
    ;;;;;;;;;;;;;;;;;;
    ;;   Dispatch   ;;
    ;;;;;;;;;;;;;;;;;;
  
    (define (dispatch-beweeg-adt msg)
      (cond ((eq? msg 'rechts) rechts-beweeg)
            ((eq? msg 'links) links-beweeg)
            ((eq? msg 'links-1) links)
            ((eq? msg 'rechts-1) rechts)
            ((eq? msg 'omhoog) klim-omhoog)
            ((eq? msg 'omlaag) klim-omlaag)
            ((eq? msg 'zwaartekracht) zwaartekracht)
            ((eq? msg 'boven) boven)
            (else (error "adt: dispatch-beweeg-adt -- file: spel-adt -- error:" msg))))    
    dispatch-beweeg-adt)

  ;;;;;;;;;;;;;;;;;;
  ;; Configuratie ;;
  ;;;;;;;;;;;;;;;;;;

  (define beweeg (beweeg-adt))

  ;;;;;;;;;;;;;;;;;;
  ;;    Tonnen    ;;
  ;;;;;;;;;;;;;;;;;;

  (define ton-stukken '())
  
  (define (voor-alle-tonnen f)
    (for-each f ton-stukken))
  
  (define (ton-maak-langer!)
    (let ((nieuw-stuk (ton-adt (maak-positie-adt 0.100 0.325))))
      (set! ton-stukken (cons nieuw-stuk ton-stukken))
      nieuw-stuk))
  
  (define (verleng-ton!)
    (let ((nieuw-stuk (ton-maak-langer!)))
      ((venster 'nieuw-ton!) nieuw-stuk)))
  
  (define (teken-tonnen!)
    (for-each (lambda (stuk-adt) ((stuk-adt 'teken!) venster)) ton-stukken))

  ;!  Ton-route  !;
  
  (define (ton-route ton-adt)
    (cond ((and (eq? (ton-adt 'status) 'op-platforme)
                (eq? (ton-adt 'richting) 'rechts))
           ((beweeg 'rechts) ton-adt))
          ((and (eq? (ton-adt 'status) 'op-platforme)
                (eq? (ton-adt 'richting) 'links))
           ((beweeg 'links) ton-adt))
          ((eq? (ton-adt 'status) 'val)
           (zwaartekracht ton-adt)
           (cond ((and (eq? (ton-adt 'status) 'op-platforme)
                       (eq? (ton-adt 'richting) 'rechts))
                  ((ton-adt 'verander-richting) 'links))
                 ((and (eq? (ton-adt 'status) 'op-platforme)
                       (eq? (ton-adt 'richting) 'links))
                  ((ton-adt 'verander-richting) 'rechts))))))

  ;!  Reset-tonnen  !;

  (define (reset-tonnen)
    (venster 'verwijder-tonnen!)
    (set! ton-stukken '()))

  (define (geef-score ton-adt)
    (cond ((and (eq? (ton-adt 'ton?-status) 'nee)
                (eq? (mario 'ladder-status) 'nee))
           ((bord 'print-score) ton-punten)
           ((ton-adt 'verander-ton?-status)'ja))))

  ;!  Botsing-ton-mario  !;
  
  (define (botsing-ton-mario ton-adt)
    (cond (((ton-adt 'ton-bots?) mario)
           (mario-startpositie mario)
           (mario 'verander-leven-status)           
           ((bord 'print-leven) (mario 'leven))
           (reset-tonnen))          
          (((ton-adt 'ton?) mario)
           (geef-score ton-adt))))
 
  ;;;;;;;;;;;;;;;;;;
  ;;    Spoken    ;;
  ;;;;;;;;;;;;;;;;;;

  (define spoken (list
                  (spook-adt (maak-positie-adt 0.200 0.550))
                  (spook-adt(maak-positie-adt 0.400 0.795))))
  
  (define spook-stukken '())
  
  (define (voor-alle-spoken f)
    (for-each f spook-stukken))
  
  (define (spook-1! adt)
    (let (( nieuw-stuk adt))
      (set! spook-stukken (cons nieuw-stuk spook-stukken))
      nieuw-stuk))

  (define (verleng-spook-1! adt)
    (let ((nieuw-stuk (spook-1! adt)))
      ((venster 'nieuw-spook!) nieuw-stuk)))

  (define (teken-spoken!)
    (for-each (lambda (stuk-adt) ((stuk-adt 'teken!) venster)) spook-stukken))

  (define (reset-spoken)
    (venster 'verwijder-spoken!)
    (set! spook-stukken '()))

  ;!  Botsing-spook-mario  !;

  (define (botsing-spook-mario spook-adt)
    (cond (((spook-adt 'spook-mario-bots?) mario)           
           (mario-startpositie mario)
           (mario 'verander-leven-status)           
           ((bord 'print-leven) (mario 'leven))
           (reset-tonnen))))

  ;!  Spook-route !;

  (define (spook-limiet? spook-adt)
    (cond ((< (spook-adt 'get-x) begin-spook)
           ((spook-adt 'verander-bots)'ja))
          
          ((> (spook-adt 'get-x) eind-spook)
           ((spook-adt 'verander-bots)'ja))
          (else ((spook-adt 'verander-bots)'nee))))

  (define (spook-route spook-adt)
    (cond ((and (eq? (spook-adt 'richting) 'rechts)
                (eq? (spook-adt 'bots?) 'nee))
           
           ((beweeg 'rechts-1) spook-adt)
           (spook-limiet? spook-adt))
          
          ((and (eq? (spook-adt 'richting) 'links)
                (eq? (spook-adt 'bots?) 'nee))
           
           ((beweeg 'links-1) spook-adt)
           (spook-limiet? spook-adt))          
          ((eq? (spook-adt 'bots?) 'ja)
           (cond ((eq? (spook-adt 'richting) 'rechts)
                  ((spook-adt 'verander-richting) 'links)
                  ((spook-adt 'verander-bots)'nee))
                 
                 ((eq? (spook-adt 'richting) 'links)
                  ((spook-adt 'verander-richting) 'rechts)
                  ((spook-adt 'verander-bots)'nee))))))

  ;;;;;;;;;;;;;;;;;;;;
  ;; Bots? functies ;;
  ;;;;;;;;;;;;;;;;;;;;

  ;!  Ladder-bots?  !;

  (define (ladder-bots? adt ladder)
    ((ladder 'ladder?) adt)) 
            
  ;!  Platforme-bots?  !;
            
  (define (platforme-bots? adt platforme)
    ((platforme 'platforme?) adt (adt 'voet)))

  (define (bonus-mario-bots? bonus-adt)
    (cond ((and ((bonus-1 'bonus-bots?) mario)
                (eq? (bonus-1 'bonus-status) 'nee))
           ((bord 'print-score) bonus-1-punten)
           ((bonus-1 'verander-bonus-status)'ja)
           (venster 'verwijder-bonus1))
          
          ((and ((bonus-2 'bonus-bots?) mario)
                (eq? (bonus-2 'bonus-status) 'nee))
           ((bord 'print-score) bonus-2-punten)
           ((bonus-2 'verander-bonus-status)'ja)
           (venster 'verwijder-bonus2))
                 
          ((and ((bonus-3 'bonus-bots?) mario)
                (eq? (bonus-3 'bonus-status) 'nee))
           ((bord 'print-score) bonus-3-punten)
           ((bonus-3 'verander-bonus-status)'ja)
           (venster 'verwijder-bonus3))))

  (define (reset-bonus)
    ((bonus-1 'verander-bonus-status)'nee)
    ((bonus-2 'verander-bonus-status)'nee)
    ((bonus-3 'verander-bonus-status)'nee))
           
  
  (define (mario-startpositie mario-adt)
    ((mario-adt 'set-x!)(mario-adt 'startpositie-x))
    ((mario-adt 'set-y!)(mario-adt 'startpositie-y)))

  (define tijd timer)
  
  (define (reset-tijd)
    (set! tijd timer))

  (define (verminder-spel-tijd)
    (cond ((= tijd 0)
           (mario-startpositie mario)
           (mario 'verander-leven-status)           
           ((bord 'print-leven) (mario 'leven))           
           (reset-tijd)
           ((bord 'print-tijd) tijd)
           (reset-tonnen))
          (else (set! tijd (- tijd 1))
                ((bord 'print-tijd) tijd))))

  (define (reset-bord)
    (mario'reset-leven-status)
    (reset-tijd)
    (bord 'print-high-score)
    ((bord 'print-tijd) tijd)
    ((bord 'print-leven) (mario 'leven)))

  (define (pauline-bots?)
    (let ((limiet 0.05))
      (and (tussen (- (pauline 'get-x) limiet)
                   (mario 'get-x)
                   (+ (pauline 'get-x)limiet))
           (tussen (-(pauline 'get-y)limiet)
                   (mario 'get-y)
                   (+ (pauline 'get-y)limiet)))))

  ;!  Spring en zwaartkracht  !;

  (define (zwaartekracht adt)
    (cond ((eq? (adt 'status) 'val)
           ((beweeg 'zwaartekracht) adt)
           )))

  (define (spring adt)
    (cond ((eq? (adt 'spring-status) 'ja)
           ((beweeg 'boven) adt))))
           
  ;;;;;;;;;;;;;;;;;;
  ;;    Timers    ;;
  ;;;;;;;;;;;;;;;;;;

  ;!  De tijden  !;

  (define spel-tijd 0)
  (define spel-snelheid 1000)

  (define spring-tijd 0)
  (define spring-snelheid 80)

  (define bonus-tijd 0)
  (define bonus-snelheid 1000)
  
  (define zwaartekracht-tijd 0)
  (define zwaartkracht-snelheid 80)

  (define teken-tijd 0)
  (define teken-snelheid 5000)
  
  (define ton-tijd 0)
  (define ton-snelheid 50)

  (define spook-tijd 0)
  (define spook-snelheid 100)

  (define afbeelding-tijd 0)
  (define afbeelding-snelheid 500)

  (define teller 1)

  (define spel-status 'menu)
  (define level 0)
  
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; Spellus functies ;;
  ;;;;;;;;;;;;;;;;;;;;;;

  (define (spellus-functies-level-1 delta-tijd)
    (when (not (eq? spel-status 'menu))

      (set! bonus-tijd (+ bonus-tijd delta-tijd))
      (set! spel-tijd (+ spel-tijd delta-tijd))
      (set! spring-tijd (+ spring-tijd delta-tijd))
      (set! zwaartekracht-tijd (+ zwaartekracht-tijd delta-tijd))
      (set! teken-tijd (+ teken-tijd delta-tijd))
      (set! ton-tijd (+ ton-tijd delta-tijd))
      (set! spook-tijd (+ spook-tijd delta-tijd))
      (set! afbeelding-tijd (+ afbeelding-tijd delta-tijd))

      ((mario 'teken!) venster)
      ((zwarte-scherm 'teken-zwarte-schrem!) venster)
      (bonus-mario-bots? bonus-adt)

      (voor-alle-tonnen botsing-ton-mario)            
      (voor-alle-spoken botsing-spook-mario)
      
      (cond ((and (eq? (bonus-2 'bonus-status) 'nee)
                  (eq? (bonus-2 'bonus-status) 'gebruikt)))
            ((eq? (bonus-2 'bonus-status) 'ja)
             (when (> bonus-tijd bonus-snelheid)
               ((bonus-2 'verander-bonus-status)'gebruikt)
               (set! bonus-tijd 0))))
      
      (when (> (mario 'get-y) 1)
        (mario-startpositie mario)
        (mario 'verander-leven-status)           
        ((bord 'print-leven) (mario 'leven))  
        (reset-tonnen))

      (when (< (mario 'leven) 1)
        (menu-spel)
        (bord 'reset-score)
        (menu-score 'print-score-lijst))

      (when (> spel-tijd spel-snelheid)
        (verminder-spel-tijd)
        (set! spel-tijd 0))

      (when (> spring-tijd spring-snelheid)
        (spring mario)
        (set! spring-tijd 0))

      (when (> zwaartekracht-tijd zwaartkracht-snelheid)
        (zwaartekracht mario)        
        (set! zwaartekracht-tijd 0))

      (when (> ton-tijd ton-snelheid)
        (teken-tonnen!)
        (voor-alle-tonnen ton-route)        
        (set! ton-tijd 0))

      (when (> teken-tijd teken-snelheid)
        (verleng-ton!)
        (set! teken-tijd 0))
      
      (when (> spook-tijd spook-snelheid)
        (teken-spoken!)
        (voor-alle-spoken spook-route)        
        (set! spook-tijd 0))
      
      (when (> teller 0)
        (verleng-spook-1! (spook-adt (maak-positie-adt 0.200 0.550)))
        (verleng-spook-1! (spook-adt(maak-positie-adt 0.600 0.795)))
        (set! teller (- teller 1)))

      (when (> afbeelding-tijd afbeelding-snelheid)
        ((kong 'teken-kong!) venster)
        ((help 'teken-help!) venster)
        ((oil-barell 'teken-oil-barell!) venster)
        (set! afbeelding-tijd 0))
    
      (when (pauline-bots?)
        (cond ((eq? spel-status 'level-1)
               (level-2))
              ((eq? spel-status 'level-2)
               (menu-spel)
               (bord 'uit)
               (bord 'reset-score)
               (menu-score 'print-score-lijst)
               )))
      ))
          
  ;;;;;;;;;;;;;;;;;;;;
  ;; Toets functies ;;
  ;;;;;;;;;;;;;;;;;;;;

  ;!  Toets-functie-spel  !;

  ;Toetsen die mario laten bewegen
  (define (toets-functie-spel msg)
    (cond ((and (eq? msg 'right)
                (eq? (mario 'spring-status) 'nee))
           ((beweeg 'rechts) mario))         
          ((and (eq? msg 'left)
                (eq? (mario 'spring-status) 'nee))
           ((beweeg 'links) mario))          
          ((eq? msg 'up)
           ((beweeg 'omhoog) mario))
          ((eq? msg 'down)
           ((beweeg 'omlaag) mario))
          ((and (eq? msg '#\space)
                (ormap (lambda (lijst) (platforme-bots? mario lijst)) platformes))
           ((mario 'verander-spring-status) 'ja))))

  ;Toetsen voor in de menu laten bewegen
  (define (toets-functie-menu msg)
    (cond ((eq? msg 'right)
           (menu 'rechts))
          ((eq? msg 'left)
           (menu 'links))
          ((eq? msg #\return)
           (cond ((eq? (menu 'index) 0)
                  (set! spel-status 'level-1)(level-1))
                 ((eq? (menu 'index) 1)
                  (menu-scores))
                 ((eq? (menu 'index) 2)
                  (exit))))))

  (define (toets-functie-menu-score msg)
    (cond ((eq? msg #\return)
           (menu-spel))))
 
  ;;;;;;;;;;;;;;;;;
  ;;   Leven-1   ;;
  ;;;;;;;;;;;;;;;;;
  
  ;Functie om het spel te starten 
  (define (level-1)    

    (venster 'spel-uit)
    (menu 'uit)
    (menu-score 'uit)
    (venster 'verwijder-spelbord!)
    (reset-spoken)
    (reset-tonnen)
    (reset-bonus)
    (mario-startpositie mario)
    
    (bord 'aan)
    (set! level (+ level 1))
    (set! teller 1)
    (set! spel-status 'level-1)
    ((bord 'print-level) level)
        
    (reset-bord)

    ((zwarte-scherm 'teken-zwarte-schrem!) venster)
    ((pauline 'teken-pauline!) venster)
    ((kong 'teken-kong!) venster)
    ((barells 'teken-barells!) venster)
    ((oil-barell 'teken-oil-barell!) venster)
    ((help 'teken-help!) venster)
    ((bonus-1 'teken-bonus1!) venster)
    ((bonus-2 'teken-bonus2!) venster)
    ((bonus-3 'teken-bonus3!) venster)
    
    (teken-stukken-platforme! platformes 'teken-platforme-level-1! 'teken-kleinplatforme-level-1!)
    (teken-stukken-ladders! ladders 'teken-level-1!)
    (venster 'spel-aan)
    
    ((venster 'set-spel-lus-functie!) spellus-functies-level-1)
    ((venster 'set-toets-functie!) toets-functie-spel))

  ;;;;;;;;;;;;;;;;;
  ;;   Leven-2   ;;
  ;;;;;;;;;;;;;;;;;

  (define (level-2)
   
    (venster 'spel-uit)
    (menu 'uit)
    (menu-score 'uit)
    (venster 'verwijder-spelbord!)
    (reset-spoken)
    (reset-tonnen)
    (reset-bonus)
    (mario-startpositie mario)
    
    (bord 'aan)
    
    (set! teller 1)
    (set! level (+ level 1))
    (set! spel-status 'level-2)
    ((bord 'print-level) level)
    
    ((zwarte-scherm 'teken-zwarte-schrem!) venster)
    ((pauline 'teken-pauline!) venster)
    ((kong 'teken-kong!) venster)
    ((barells 'teken-barells!) venster)
    ((oil-barell 'teken-oil-barell!) venster)
    ((help 'teken-help!) venster)
    
    (teken-stukken-platforme! platformes 'teken-platforme-level-2! 'teken-kleinplatforme-level-2!)
    (teken-stukken-ladders! ladders 'teken-level-2!)
    (venster 'spel-aan)
   
    ((venster 'set-spel-lus-functie!) spellus-functies-level-1)
    ((venster 'set-toets-functie!) toets-functie-spel))

  ;;;;;;;;;;;;;;;;;
  ;;     Menu    ;;
  ;;;;;;;;;;;;;;;;;

  (define (menu-spel)

    (venster 'spel-uit)
    (bord 'uit)
    (menu-score 'uit)
    
    (reset-spoken)
    (reset-tonnen)
    (reset-bonus)
    (venster 'verwijder-spelbord!)
                   
    (set! spel-status 'menu)
    (set! level 0)
    
    (menu 'aan)
    
    ((venster 'set-toets-functie!) toets-functie-menu))

  ;;;;;;;;;;;;;;;;;
  ;;  Menu-score ;;
  ;;;;;;;;;;;;;;;;;

  (define (menu-scores)
    
    (menu 'uit)
    (bord 'uit)
    (venster 'spel-uit)
    (menu-score 'print-score-lijst)
    (menu-score 'aan)
        
    ((venster 'set-toets-functie!) toets-functie-menu-score))
  
  ;;;;;;;;;;;;;;;;;;
  ;;   Dispatch   ;;
  ;;;;;;;;;;;;;;;;;;
  
  (define (dispatch-spel-adt msg)
    (cond ((eq? msg 'menu)
           (menu-spel))
          (else (error "dispatch-spel-adt -- spel-adt" msg))))
  
  dispatch-spel-adt)

;;;;;;;;;;;;;;;;;
;; Entry point ;;
;;;;;;;;;;;;;;;;;

(define spel (spel-adt))

(spel 'menu)