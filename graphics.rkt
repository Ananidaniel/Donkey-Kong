#lang racket
;;;;*----------------------------------*
;;;;*        >>>  graphics  <<<        *
;;;;*  >>>  Jaarproject 2015-2016 <<<  *
;;;;*                                  *
;;;;*      by Christophe De Troyer     *
;;;;*        and Thierry Renaux        *
;;;;*                                  *
;;;;*  Based heavily on "Canvas" for   *
;;;;*     Jaarproject 2013-2014 by     *
;;;;*       Brecht De Rooms and        *
;;;;*      Christophe Scholliers       *
;;;;*                                  *
;;;;*    Software Langauges Lab        *
;;;;*----------------------------------*
(require racket/gui/base)
(require file/convertible)
(require racket/string)
(provide
 make-window
 make-tile
 make-bitmap-tile
 make-tile-sequence
 generate-mask)

;;;;---------------------------------------------------------------------
;;;; Note: this code needs to be cleaned up since we mainly did all the dirty
;;;;       graphics work to achieve a decent drawing efficiency and to 
;;;;       make sure students don't waste time on this. 
;;;;       this is NOT a reference on how to code cleanly.
;;;;---------------------------------------------------------------------

;;;;################################ WINDOW #######################################
;;;;---------------------------------------------------------------------
;;;; make-window creates a window that accepts tiles and tile-sequences.
;;;; changing the x-value of a tile will update the canvas.
;;;;---------------------------------------------------------------------
(define (make-window w h title)
  ;; #############################################################
  ;; ###### Initalization of our intelligent game-window #########
  ;; #############################################################  
  (let* ((show-fps #t)
         (fps 0)
         (delta-time 0)
         (previous-time (current-milliseconds))
         ;; Define our dummy keyboard-callback
         (keyboard-callback (lambda (ev) (void)))
         ;; Define our dummy update-callback
         (update-callback (lambda (ev) (void)))
         (buffer-bitmap (make-object bitmap% w h))
         (buffer-bitmap-dc (new bitmap-dc% [bitmap buffer-bitmap]))
         (game-loop (lambda (deltatime events) (void)))
         (game-loop-timer #f)
         (layers '())
         (closed #f))
    
    ;; Define the paint-callback which is called each frame
    (define (paint-callback canvas dc)
      ;; before we do anything, the game-loop is executed.
      (update-callback delta-time)    
      ;; clear the buffer
      (send buffer-bitmap-dc clear)
      ;; Draw all layers on each frame
      (for-each (lambda (layer) ((layer 'draw) buffer-bitmap-dc)) layers)
      ;; clear the provided-dc of window. 
      (send dc clear)
      ;; finally, draw the buffer on the screen. 
      (send dc draw-bitmap buffer-bitmap 0 0)
      ;; calculate frames per second. 
      (set! fps (calculate-fps delta-time))
      ;; If show-fps, construct the fps string and set the fps in the frame label.
      (when show-fps
        (send frame set-label (construct-fps-string title fps))))
    
    ;; Calculate FPS from the time (ms) since last frame
    (define (calculate-fps delta-time)
      (if (not (= delta-time 0))
          (quotient 1000 delta-time) 
          fps))
    
    ;; Construct FPS string
    (define (construct-fps-string title fps)
      (define fps-string (number->string (exact->inexact fps)))
      (set! fps-string (string-append title fps-string))
      (set! fps-string (list-ref (string-split fps-string ".") 0))
      (set! fps-string (string-append "  FPS: " fps-string))
      fps-string)
    
    ;; Make a canvas class that uses our own keyboard callback.
    (define my-canvas%
      (class canvas% ; The base class is canvas%
        ;; Define overriding method to handle keyboard events
        ;; this makes sure our own key-callback is called.
        (define/override (on-char event)
          (keyboard-callback (send event get-key-code)))
        ;; Call the superclass init, passing on all init args
        (super-new)))
    
    ;; Make a frame class that can react to closing events
    (define closing-frame%
      (class frame%
        (super-new)
        (define (on-close)
          (set! closed #t))
        (augment on-close)))
    
    ;; Create frame in which we can place a canvas.
    (define frame (new closing-frame%
                       [label title]
                       [width w]
                       [height (+ h 30)])) ; take border into account
    
    ;; Create the canvas with the custom paint-callback 
    ;; This paint-callback is called each time the canvas is refreshed.
    ;; How fast the canvas is refreshed is handled later.
    (define canvas (new my-canvas% 
                        [parent frame]
                        [paint-callback paint-callback] ))
    
    ;; #############################################################    
    ;; ###### public methods for the window ADT ####################
    ;; #############################################################
    ;;Create and add layers to the window
    (define (add-layer)
      (define layer (make-layer w h canvas))
      (set! layers (append layers (list layer)))
      layer)
    
    ;; Set the backgroudn color of the window
    (define (set-background! colorstring)
      (send buffer-bitmap-dc set-background (make-object color% colorstring)))
    ;; #############################################################    
    ;; ###### Setting up a self-sustaining game-loop ###############
    ;; #############################################################    
    ;; Here we handle how fast the canvas is refreshed and thereby how 
    ;; fast paint-callback will be called. 
    (define (launch-game-loop)
      (let* ((max-fps 500)          ;; The maximum frames per seconds someone can get is 500.
             (min-wait-per-frame 1) ; apparently this has to be at least 1 to avoid locking up.
             (ms-per-frame (quotient 1000 max-fps))) ; calculate the MINIMUM delta-time in ms between two frames.
        
        ;; calculate the min delta-time given the min-wait-per-frame
        (define (calculate-interval)
          (truncate (max  
                     (- ms-per-frame delta-time) 
                     min-wait-per-frame)))
        
        ;; The heart of the self-sustaning loop.
        (define (game-loop)  
          ;; get the new delta-time
          (set! delta-time (- (current-milliseconds) previous-time)) 
          ;; We wait for min-delta-time, which is typically the min-wait-per-frame
          (when (>= delta-time ms-per-frame)
            ;; calculate actual delta-time. 
            (set! previous-time (current-milliseconds))
            ;; call the canvas refresh which will trigger a paint-callback
            (send canvas refresh-now))
          (when (not closed)
            ;; When the game-loop is done we fire the game-loop again
            ;; after waiting min-delta-time ms, unless the window is closed.
            (send game-loop-timer start (calculate-interval) #t)))
        
        ;; a timer drives the game-loop which calls the game-loop after waiting
        ;; 'interval'. A timer normally calls every 'interval' ms but with
        ;; just-once? #t we prevent that since it will be the game-loop itself
        ;; that will keep itself alive.
        (set! game-loop-timer 
              (new timer% [notify-callback game-loop] 
                   [interval (calculate-interval) ]
                   [just-once? #t]))))
    
    ;; dispatch
    (define (dispatch msg)
      (cond ((eq? msg 'make-layer) (add-layer))
            ((eq? msg 'set-background!) set-background!)
            ((eq? msg 'set-key-callback!) (lambda (eh) (set! keyboard-callback eh)))
            ((eq? msg 'set-update-callback!) (lambda (gl) (set! update-callback gl)))
            (else (raise-arguments-error 'window
                                         "wrong message sent"
                                         "message"
                                         msg)))) 
    
    ;; show the frame.
    (send frame show #t)
    
    ;; set backgroudn and foreground. 
    (send buffer-bitmap-dc set-background (make-object color% "black"))
    (send buffer-bitmap-dc set-text-foreground (make-object color% "white"))
    ;; and finally launch the self-sustaining game-loop.
    (launch-game-loop)
	(send canvas focus)
    dispatch))

;;;;################################ BITMAP WITH ROTATED DC MATRIX #######################################
;;;; Creates a bitmap with a rotated rotation matrix. 
;;;; Since this moves the actual bitmap on its dc, we correct it
;;;; by translating it afterwards. (Their might be a simpler solution)
(define (create-rotated-bitmap-and-dc w h rotation transparent)
  (define temp-bitmap (make-object bitmap% w h #f transparent ))
  (define temp-dc (new bitmap-dc% [bitmap temp-bitmap]))
  (cond [(eq? 1 (exact-truncate(/ rotation (/ pi 2))))
         (send temp-dc translate  w 0 )]
        [(eq? 2 (exact-truncate (/ rotation (/ pi 2)))) 
         (send temp-dc translate w h)]
        [(eq? 3 (exact-truncate (/ rotation (/ pi 2))))
         (send temp-dc translate 0 h )])
  
  (send temp-dc rotate (- (* 2 pi) rotation))  
  (send temp-dc set-background (make-object color% 0 0 0 0))      
  (cons temp-bitmap temp-dc))

;;;;################################ TIMER #######################################
;;;; Timer which calls callback every 'interval' milliseconds. 
;;;; Careful, it WILL call this every interval, even when the procedure
;;;; is very heavy and your pc is too slow. In which case DrRacket will lock up.
(define (create-timer callback interval)
  (let ((timer (new timer% [notify-callback callback] [interval interval] [just-once? #f])))
    (define (dispatch msg)
      (cond ((eq? msg 'start) (send timer start interval))
            ((eq? msg 'stop)  (send timer stop))
            ((eq? msg 'timer) timer)
            (else (raise-arguments-error 'timer
                                         "wrong message sent"
                                         "message"
                                         msg))))
    dispatch))

;;;;################################ GET SPRITES FROM DISK #######################################
;;;;---------------------------------------------------------------------
;;;; make-bitmap creates a bitmap given a path to an image file
;;;; String -> get-bitmap
;;;;---------------------------------------------------------------------
(define (get-bitmap file)
  (let ((bitmap (make-object bitmap% 1 1)))
    (send bitmap load-file file)
    bitmap))

;;;;---------------------------------------------------------------------
;;;; make-bitmap creates a bitmap given a path to an image file
;;;; String -> get-bitmap-section
;;;;---------------------------------------------------------------------
(define (get-bitmap-section tilebitmap x y width height)
  (define target-bitmap (make-object bitmap% width height))
  (define target-dc (new bitmap-dc% [bitmap target-bitmap]))
  (send target-dc draw-bitmap-section tilebitmap 0 0 x y width height)
  target-bitmap)

;;;;---------------------------------------------------------------------
;;;; generate-mask generates a mask and saves it to disk.
;;;; String String -> void
;;;;---------------------------------------------------------------------
(define (generate-mask bitmappath background-color)
  (when (string? background-color) (set! background-color (send the-color-database find-color background-color)))
  (define bitmap (get-bitmap bitmappath))
  (define dc (new bitmap-dc% [bitmap bitmap]))
  (for ([w (send bitmap get-width)])
    (for ([h (send bitmap get-height)])
      (define pixel (make-object color%))
      (send dc get-pixel w h pixel)
      (if (and (= (send background-color red) (send pixel red))
               (= (send background-color blue) (send pixel blue))
               (= (send background-color green) (send pixel green)))
          
          (send dc set-pixel w h (make-object color% "white"))
          (send dc set-pixel w h (make-object color% "black")))))
  
  (send (send dc get-bitmap) save-file (string-replace bitmappath ".png" "_mask.png") 'png))


;;;;################################ TILES #######################################
;;;;---------------------------------------------------------------------
;;;; make-bitmap-tile creates a tile from a bitmap with optionally a mask.
;;;; [] mean it is optional.
;;;; String [String] -> Tile
;;;;---------------------------------------------------------------------
(define (make-bitmap-tile bitmappath [mask #f])
  (define bitmap (get-bitmap bitmappath))
  (make-tile (send bitmap get-width) (send bitmap get-height) bitmap mask))

;;;;---------------------------------------------------------------------
;;;; make-tile creates a tile from a width and height with optionally
;;;; a bitmap and a mask. 
;;;; Integer Integer [String] [String] -> Tile
;;;;---------------------------------------------------------------------     
(define (make-tile w h [bitmap #f] [mask #f])
  (when (string? bitmap) (set! bitmap (get-bitmap bitmap)))
  (when (string? mask) (set! mask (get-bitmap mask)))
  (when (not bitmap) (set! bitmap (make-object bitmap% w h #f #t )))
  (define bufferbitmap (make-object bitmap% w h #f #t))
  (let* ((x 0) 
         (y 0) 
         (update-callback  (lambda () #t))
         (rotated-bitmap (make-object bitmap% w h #f #t ))
         (rotated-bitmap-dc (new bitmap-dc% [bitmap rotated-bitmap]))
         (rotated-mask (make-object bitmap% w h #t #f ))
         (rotated-mask-dc (new bitmap-dc% [bitmap rotated-mask]))
         (bitmap-dc (new bitmap-dc%  [bitmap bufferbitmap]))
         (rotation 0))
    (send bitmap-dc draw-bitmap bitmap 0 0 )
    
    ;; ##### Drawing methods to draw on the tile yourself.
    ;; Clear removed your own drawings. 
    ;; void -> void
    (define (clear)
      (set! bufferbitmap (make-object bitmap% w h #f #t))
      (set! rotated-bitmap (make-object bitmap% w h #f #t ))
      (set! rotated-bitmap-dc (new bitmap-dc% [bitmap rotated-bitmap]))
      (set! rotated-mask (make-object bitmap% w h #t #f ))
      (set! rotated-mask-dc (new bitmap-dc% [bitmap rotated-mask]))
      (set! bitmap-dc (new bitmap-dc%  [bitmap bufferbitmap]))
      (send bitmap-dc draw-bitmap bitmap 0 0 )
      (update-callback))
    
    ;; Drawing a rectangle
    ;; Integer Integer Integer Integer (String OR Color%) -> void
    (define (draw-rectangle x y w h color )
      (when (string? color) (set! color (send the-color-database find-color color)))
      (send bitmap-dc set-brush color 'solid)
      (send bitmap-dc set-pen color 1 'transparent)
      (send bitmap-dc draw-rectangle x y w h)
      (update-callback))
    
    ;; Drawing an Ellipse
    ;; Integer Integer Integer Integer (String OR Color%) -> void
    (define (draw-ellipse x y w h color)
      (when (string? color) (set! color (send the-color-database find-color color)))
      (send bitmap-dc set-brush color 'solid)
      (send bitmap-dc set-pen color 1 'transparent)
      (send bitmap-dc draw-ellipse x y w h)
      (update-callback))
    
    ;; Drawing a Line
    ;; Integer Integer Integer Integer Integer (String OR Color%) -> void
    (define (draw-line x y w h width color )
      (when (string? color) (set! color (send the-color-database find-color color)))
      (send bitmap-dc set-pen color width 'solid)
      (send bitmap-dc draw-line x y w h)
      (update-callback))
    
    ;; Drawing Text
    ;; String Integer Integer Integer (String OR Color%) -> void
    (define (draw-text text fontsize x y color)
      (when (string? color) (set! color (send the-color-database find-color color)))
      (send bitmap-dc set-font (make-object font% fontsize 'default))
      (send bitmap-dc set-text-foreground color)
      (send bitmap-dc draw-text text x y)
      (update-callback))
    
    ;; Rotation of 90 degrees clockwise.
    ;; void -> void
    (define (rotate-clockwise)
      (set! rotation (remainder (+ rotation 90) 360))
      (rotate rotation))
    
    ;; Rotation of 90 degrees counterclockwise.
    ;; void -> void
    (define (rotate-counterclockwise)
      (set! rotation (remainder (- rotation 90) 360))
      (when (< rotation 0)
        (set! rotation 270))
      (rotate rotation))
    
    ;; Internal Rotation Function with a hack to solve
    ;; the rather bizar way of rotating in the graphical DrRacket library.
    ;; void -> void
    (define (rotate tempr)
      (set! rotation tempr)
      (unless (or (eq? 90 tempr) (eq? 0 tempr) (eq? 180 tempr)  (eq? 270 tempr))
        (begin (display "ERROR ::: illegal rotation given, only 0,90,180,270 is allowed: ")
               (newline)))
      (define r (/ (* tempr pi) 180))
      (define bitmap-pair (create-rotated-bitmap-and-dc w h r #t))
      (set! rotated-bitmap (car bitmap-pair))
      (set! rotated-bitmap-dc (cdr bitmap-pair))
      (when mask
        (define mask-pair (create-rotated-bitmap-and-dc w h r #f))     
        (set! rotated-mask (car mask-pair))     
        (set! rotated-mask-dc (cdr mask-pair)))
      (update-callback))
    
    ;; Set the X position on the screen
    ;; Integer -> void
    (define (set-x! new_x)
      (set! x new_x)
      (update-callback))
    
    ;; Set the Y position on the screen
    ;; Integer -> void
    (define (set-y! new_y)
      (set! y new_y)
      (update-callback))
    
    (define transparent-color (make-object color% 0 0 0 0))
    
    ;; Drawing procedure called by the layer 
    ;; on which the tile is drawn. Not to be used
    ;; by students manually!
    ;; dc% -> void
    (define (draw dc)    
      (send rotated-bitmap-dc draw-bitmap bufferbitmap 0 0)
      (if mask
          (begin (send rotated-mask-dc draw-bitmap mask 0 0)
                 (send dc draw-bitmap rotated-bitmap x y 'solid transparent-color rotated-mask))
          (send dc draw-bitmap rotated-bitmap x y )))
    
    ;; A procedure to set a callback. This callback
    ;; will notify the parent (layers) that the tile
    ;; has changed and allows us to automatically
    ;; redraw the tiles.
    ;; lambda() -> void
    (define (set-on-update! new_callback)
      (set! update-callback new_callback))
    
    ;; Dispatch
    (define (dispatch msg . args)
      (cond 
        ;; Not to be called manually
        ((eq? msg 'draw)   draw )
        ((eq? msg 'set-on-update!) set-on-update!)
        
        ;; Getters and setters
        ((eq? msg 'set-x!) set-x!)
        ((eq? msg 'set-y!) set-y!)
        ((eq? msg 'get-x)  x)
        ((eq? msg 'get-y)  y)
        ((eq? msg 'get-w)  w)
        ((eq? msg 'get-h)  h)
        ;; Drawing Functions
        ((eq? msg 'rotate-clockwise) (rotate-clockwise))
        ((eq? msg 'rotate-counterclockwise) (rotate-counterclockwise))
        ((eq? msg 'clear) (clear))
        ((eq? msg 'draw-rectangle) draw-rectangle) 
        ((eq? msg 'draw-ellipse) draw-ellipse) 
        ((eq? msg 'draw-line) draw-line) 
        ((eq? msg 'draw-text) draw-text) 
        (else (raise-arguments-error 'tile
                                     "wrong message sent"
                                     "message"
                                     msg))))
    dispatch))

;;;;---------------------------------------------------------------------
;;;; tile-sequence is a sequence of tiles, it is created by passing a list
;;;; of tiles to the tile-sequence. A tile-sequence is meant to animate tiles.
;;;; When it is created, the current tile (index) is set on the first tile that
;;;; was added. Calling next will cycle through the tile-sequence and select the 
;;;; next tile. 
;;;; List<Tile> -> Tile-Sequence
;;;;---------------------------------------------------------------------  
(define (make-tile-sequence tiles)
  ;; Initialize the current index and its callback.
  (let ((index 0) 
        (update-callback (lambda () #t)))
    
    ;; Change its coordiantes on the window
    ;; Integer -> void
    (define (set-x! new_x)
      (for-each (lambda (tile) ((tile 'set-x!) new_x)) tiles)
      (update-callback))
    ;; Integer -> void
    (define (set-y! new_y)
      (for-each (lambda (tile) ((tile 'set-y!) new_y)) tiles)
      (update-callback))
    
    ;; choose which tile in the sequence is currently active
    ;; by providing an index.
    ;; Integer -> void
    (define (set-current! new_index)
      (if (or (>= new_index (length tiles))
              (< new_index 0))
          (begin (display "ERROR ::: illegal index given for tile-sequence: ")
                 (display new_index)
                 (newline))
          (begin (set! index new_index)
                 (update-callback))))
    ;; Set the previous tile as active tile. 
    ;; void -> void
    (define (set-previous!)
      (set! index (remainder  (- index 1) (length tiles)))
      (when (< index 0) (set! index (- (length tiles) 1)))
      (update-callback))
    
    ;; Set the next tile as active tile. 
    ;; void -> void
    (define (set-next!)
      (set! index (remainder  (+ 1 index) (length tiles)))
      (update-callback))
    
    ;; Drawing functions, each of them will forward the 
    ;; drawing instruction to the underlying tiles.
    ;; void -> void
    (define (rotate-clockwise)
      (for-each (lambda (tile) (tile 'rotate-clockwise) ) tiles)
      (update-callback))
    
    ;; void -> void
    (define (rotate-counterclockwise)
      (for-each (lambda (tile) (tile 'rotate-counterclockwise) ) tiles)
      (update-callback))
    
    ;; Integer Integer Integer Integer String -> void
    (define (draw-rectangle x y w h color)
      (for-each (lambda (tile) ((tile 'draw-rectangle) x y w h color )) tiles)
      (update-callback))
    
    ;; Integer Integer Integer Integer String -> void
    (define (draw-ellipse x y w h color)
      (for-each (lambda (tile) ((tile 'draw-ellipse) x y w h color )) tiles)
      (update-callback))
    
    ;; String Integer Integer Integer String -> void
    (define (draw-text text fontsize x y color)
      (for-each (lambda (tile) ((tile 'draw-text) text fontsize x y color )) tiles)
      (update-callback))
    
    ;; Integer Integer Integer Integer Integer String -> void
    (define (draw-line x y w h width color )
      (for-each (lambda (tile) ((tile 'draw-line)x y w h width color  )) tiles)
      (update-callback))
    
    ;; Clears everything that is drawn by the user, 
    ;; if there were bitmaps, the bitmaps are restored.
    ;; void -> void
    (define (clear)
      (for-each (lambda (tile) (tile 'clear)) tiles)
      (update-callback))
    
    
    ;; redraw itself on the provided drawing context
    ;; void -> void
    (define (draw dc)     
      (((current) 'draw) dc))
    
    ;; set update callback which is called every-time a sequence changes
    ;; lambda() -> void
    (define (set-on-update! new_callback)
      (set! update-callback new_callback))
    
    ;; Interal function to retrieve current (private).
    ;; void -> Tile
    (define (current)
      (list-ref tiles index))
    
    ;; Dispatch
    (define (dispatch msg )    
      (cond 
        ;; Not to be called manually
        ((eq? msg 'draw)  draw)
        ((eq? msg 'set-on-update!) set-on-update!)
        
        ;; Moving and dimension and position getters.
        ((eq? msg 'set-x!) set-x!)
        ((eq? msg 'set-y!) set-y!)
        ((eq? msg 'get-x)  ((current) 'get-x))
        ((eq? msg 'get-y)  ((current) 'get-y))
        ((eq? msg 'get-w)  ((current) 'get-w))
        ((eq? msg 'get-h)  ((current) 'get-h))
        
        ;; Animations to switch between tiles
        ((eq? msg 'set-current!) set-current!)
        ((eq? msg 'set-next!) (set-next!))
        ((eq? msg 'set-previous!) (set-previous!))    
        
        ;; Rotation manipulations
        ((eq? msg 'rotate-clockwise) (rotate-clockwise)) 
        ((eq? msg 'rotate-counterclockwise) (rotate-counterclockwise))
        
        ;; Clear all manual drawings
        ((eq? msg 'clear) (clear))
        
        ;; Create manual drawings
        ((eq? msg 'draw-rectangle) draw-rectangle) 
        ((eq? msg 'draw-ellipse) draw-ellipse) 
        ((eq? msg 'draw-line) draw-line) 
        ((eq? msg 'draw-text) draw-text) 
        (else (raise-arguments-error 'tile-sequence
                                     "wrong message sent"
                                     "message"
                                     msg))))
    dispatch))

;;;;################################ LAYER #######################################
;;;;---------------------------------------------------------------------
;;;; layers in a window, each layer has a temporary bitmap. 
;;;; Integer Integer canvas% -> Layer
;;;;---------------------------------------------------------------------  
(define (make-layer w h canvas)
  
  (let* ((drawables '())                             ;; all drawables on this layer.
         (bitmap (make-object bitmap% w h #f #t ))    ;; buffer-bitmap for fast drawing
         (bitmap-dc (new bitmap-dc% [bitmap bitmap])) ; dc of bitmap (drawing context)
         (needs-update #t))                           ;; even faster drawing thanks to dirty bit.
    
    ;; # redraw on temporary bitmap layer.
    ;; void -> void
    (define (redraw)
      (set! bitmap (make-object bitmap% w h #f #t))
      (set! bitmap-dc (new bitmap-dc%  [bitmap bitmap]))
      ;; as you can see, this will redraw all drawables on the layer
      ;; Therefore it is not wise to put one moving object together with a bunch 
      ;; of non-moving objects on ONE layer.
      (for-each (lambda (tile) ((tile 'draw) bitmap-dc)) drawables))
    
    ;; # draw itself on given drawing context.
    ;; dc% -> void
    (define (draw dc)
      (when needs-update 
        (redraw) 
        (set! needs-update #f))
      (send dc draw-bitmap bitmap 0 0))
    
    ;; # methods
    ;; Adds a drawable to the layer which is a tile a tile-sequence or 
    ;; a drawable created by the student which suports 'draw' and 'set-on-update!'
    ;; Drawable (Tile or Tile-Sequence) -> void
    (define (add-drawable drawable)
      ((drawable 'set-on-update!) (lambda () (set! needs-update #t)))
      (set! drawables (cons drawable drawables))
      (redraw))
    
    ;; Remove a drawable to the layer which is a tile a tile-sequence or 
    ;; a drawable created by the student which suports 'draw' and 'set-on-update!'
    ;; Drawable (Tile or Tile-Sequence) -> void
    (define (remove-drawable drawable)
      ((drawable 'set-on-update!) (lambda () #t))
      (set! drawables (remq drawable drawables))
      (redraw))
    
    ;; # dispatch
    (define (dispatch msg)
      (cond ((eq? msg 'add-drawable)  add-drawable)
            ((eq? msg 'remove-drawable) remove-drawable)
            ((eq? msg 'draw) draw)
            (else (raise-arguments-error 'layer
                                         "wrong message sent"
                                         "message"
                                         msg)))) 
    dispatch )) 
