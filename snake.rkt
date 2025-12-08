;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require racket/list)

;; ======================
;; Menu speed variable
;; ======================
(define-struct menu (speed mode color size selector num-fruits))
(define initial-menu (make-menu 0.10 'Plain 'Darkgreen 20 'speed 1))

;; ======================
;; Constants
;; ======================

(define SPEEDS-LIST (list 0.15 0.10 0.07 0.03))
(define SPEED-LABELS '("Slow" "Normal" "Fast" "Crazy"))
(define MODE-OPTIONS '(Plain Obstacles Pacman))
(define SIZE-OPTIONS (list 16 20 24))


;size, cells and grid
(define CELL-NUM-WIDTH 24)
(define CELL-NUM-HEIGHT 24)
(define CELL-SIZE 20)
(define BORDER-SIZE 10)             
(define TOP-BORDER-SIZE (* 3 BORDER-SIZE))

(define SCENE-WIDTH 480)
(define SCENE-HEIGHT 480)
(define TOTAL-WIDTH (+ SCENE-WIDTH (* 2 BORDER-SIZE)))
(define TOTAL-HEIGHT (+ SCENE-HEIGHT TOP-BORDER-SIZE BORDER-SIZE))

;Button
(define BUTTON-WIDTH 100)
(define BUTTON-HEIGHT 30)

;Graphics
(define COLOR-OPTIONS '(Darkgreen Red Blue Magenta Yellow))


(define GRID-COLOR "gray")
(define TRANSPARENT (make-color 0 0 0 0))

(define EYE (overlay/xy (circle 4 "solid" "black") -6 -2 (circle 10 "solid" "white")))


(define (make-snake-head color)
  (scale 0.5 (overlay/xy EYE -20 -25 (overlay/xy EYE 0 -25 (rotate 90 (polygon (list (make-pulled-point 1/2 20 0 0 1/2 -20)
                 (make-posn -10 20)
                 (make-pulled-point 1/2 -20 60 0 1/2 20)
                 (make-posn -10 -20))
           "solid"
           color))))))

(define (make-snake-body color)
  (rectangle 13 20 "solid" color))

(define (make-snake-body-angle color)
  (overlay/xy (rectangle 13 13 "solid" TRANSPARENT) 7 -7
              (overlay/xy (rectangle 13 13 "solid" TRANSPARENT) 0 -14
                          (overlay/xy (rectangle 13 13 "solid" color) -7 -7
                                      (overlay/xy (rectangle 13 13 "solid" color) 0 7 (rectangle 13 13 "solid" color))))))

(define FRUIT-CORE (circle 10 "solid" "red"))
(define FRUIT-LEAF (ellipse 5 10 "solid" "green"))
(define FRUIT 
    (overlay/xy (rotate 45 FRUIT-LEAF) -4 3 FRUIT-CORE))

(define OBSTACLE (rectangle CELL-SIZE CELL-SIZE "solid" "black"))
  
;; ======================
;; Data Structures
;; ======================
(define-struct world (mode menu snake dir foods game-over? score record tick-counter obstacles free-spaces paused?))

;; ======================
;; Initial Game Data
;; ======================
(define initial-snake (vector (make-posn 7 12)))
(define initial-dir 'none)
(define initial-foods (vector (make-posn 15 12)))
(define initial-score 0)
(define initial-record 0)

;; ======================
;; Helper Functions
;; ======================

;movement for the head of the snake
(define (move-head head dir)
  (let ([x (posn-x head)] [y (posn-y head)])
    (cond [(symbol=? dir 'up) (make-posn x (- y 1))]
          [(symbol=? dir 'down) (make-posn x (+ y 1))]
          [(symbol=? dir 'left) (make-posn (- x 1) y)]
          [(symbol=? dir 'right) (make-posn (+ x 1) y)])))

;checks collisions
(define (wall-collision? p)
  (or (< (posn-x p) 1) (>= (posn-x p) (+ CELL-NUM-WIDTH 1))
      (< (posn-y p) 1) (>= (posn-y p) (+ CELL-NUM-HEIGHT 1))))

(define (self-collision? head snake)
  (let loop ([i 1])
    (if (>= i (vector-length snake))
        #f
        (or (equal? head (vector-ref snake i))
            (loop (add1 i))))))

;creates food in random places
(define (random-foods snake obstacles n)
  (let loop ([count n] [acc '()])
    (if (= count 0)
        (list->vector acc)
        (let* ([p (make-posn (+ 1 (random (- CELL-NUM-WIDTH 2)))
                              (+ 1 (random (- CELL-NUM-HEIGHT 2))))]
               [occupied? (or (member p obstacles)
                              (let loop2 ([i 0])
                                (if (>= i (vector-length snake)) #f
                                    (or (equal? (vector-ref snake i) p)
                                        (loop2 (add1 i)))))
                              (member p acc))])
          (if occupied?
              ;; check if there’s still any free space
              (if (>= (count-free-spaces obstacles) (vector-length snake))
                  (loop count acc)   ; try again
                  (list->vector acc)) ; no free space, return what we have
              (loop (sub1 count) (cons p acc)))))))


;creates free spaces
(define (count-free-spaces obstacles)
  (- (* CELL-NUM-WIDTH CELL-NUM-HEIGHT)
     (length obstacles)
     1))


;; ============================================
;; ADDED HELPERS FOR TURN DETECTION
;; ============================================

;returns either 'up 'down 'left 'right based on the x's and y's
(define (direction from to)
  (let* ([dx (- (posn-x to) (posn-x from))]
         [dy (- (posn-y to) (posn-y from))]

         ;; handle wrap-around horizontally
         [dx-wrap (cond [(> dx (/ CELL-NUM-WIDTH 2)) (- dx CELL-NUM-WIDTH)]
                        [(< dx (- (/ CELL-NUM-WIDTH 2))) (+ dx CELL-NUM-WIDTH)]
                        [else dx])]

         ;; handle wrap-around vertically
         [dy-wrap (cond [(> dy (/ CELL-NUM-HEIGHT 2)) (- dy CELL-NUM-HEIGHT)]
                        [(< dy (- (/ CELL-NUM-HEIGHT 2))) (+ dy CELL-NUM-HEIGHT)]
                        [else dy])])
    (cond [(= dx-wrap 0) (if (< dy-wrap 0) 'up 'down)]
          [(= dy-wrap 0) (if (< dx-wrap 0) 'left 'right)]
          [else 'none])))


(define (vertical? dir)
  (or (eq? dir 'up) (eq? dir 'down)))
(define (horizontal? dir)
  (or (eq? dir 'left) (eq? dir 'right)))

;returns the correct tail piece to draw based on the postion of also the previous and next piece,
;only case that doesent apply is if the snake has only one piece (cant get a next piece)
(define (get-body-image dir color)
  (cond [(vertical? dir) (make-snake-body color)]
        [(horizontal? dir) (rotate 90 (make-snake-body color))]))

(define (draw-tail-piece prev curr next color)
  (let* ([dir1 (direction prev curr)]
         [dir2 (direction curr next)])
    (cond
      [(eq? dir1 dir2) (get-body-image dir1 color)]
      [(or (and (eq? dir1 'up) (eq? dir2 'right))
           (and (eq? dir1 'left) (eq? dir2 'down)))
       (rotate 270 (make-snake-body-angle color))]
      [(or (and (eq? dir1 'right) (eq? dir2 'up))
           (and (eq? dir1 'down) (eq? dir2 'left)))
       (rotate 90 (make-snake-body-angle color))]
      [(or (and (eq? dir1 'right) (eq? dir2 'down))
           (and (eq? dir1 'up) (eq? dir2 'left)))
       (rotate 180 (make-snake-body-angle color))]
      [(or (and (eq? dir1 'down) (eq? dir2 'right))
           (and (eq? dir1 'left) (eq? dir2 'up)))
       (rotate 0 (make-snake-body-angle color))]
      [else (make-snake-body color)])))

(define (get-head-image dir color)
  (cond [(symbol=? dir 'right) (rotate 270 (make-snake-head color))]
        [(symbol=? dir 'down) (rotate 180 (make-snake-head color))]
        [(symbol=? dir 'left) (rotate 90 (make-snake-head color))]
        [(symbol=? dir 'up) (make-snake-head color)]
        [else (rotate 270 (make-snake-head color))]))

;; ======================
;; Grid Drawing
;; ======================
(define (draw-vlines i max scene)
  (if (> i max)
      scene
      (draw-vlines
       (add1 i) max
       (add-line scene
                 (* i CELL-SIZE) 0
                 (* i CELL-SIZE) SCENE-HEIGHT
                 GRID-COLOR))))

(define (draw-hlines i max scene)
  (if (> i max)
      scene
      (draw-hlines
       (add1 i) max
       (add-line scene
                 0 (* i CELL-SIZE)
                 SCENE-WIDTH (* i CELL-SIZE)
                 GRID-COLOR))))

(define (draw-grid scene)
  (let* ([vertical (draw-vlines 0 CELL-NUM-WIDTH scene)]
         [horizontal (draw-hlines 0 CELL-NUM-HEIGHT vertical)])
    horizontal))

;; ======================
;; Rendering Snake Game
;; ======================

(define (cell-center n)
  (+ (* (sub1 n) CELL-SIZE)
     (/ CELL-SIZE 2)))

(define (draw-obstacles obstacles menu-size scene)
  (let* ([offset (quotient (- CELL-NUM-WIDTH menu-size) 2)]
         [start (+ offset 1)]
         [end (- CELL-NUM-WIDTH offset)])
    (foldl (lambda (p s)
             (let ([is-outer (or (< (posn-x p) start) (> (posn-x p) end) (< (posn-y p) start) (> (posn-y p) end))])
               (place-image (if is-outer (rectangle CELL-SIZE CELL-SIZE "solid" "red") OBSTACLE)
                            (cell-center (posn-x p))
                            (cell-center (posn-y p))
                            s)))
           scene obstacles)))

(define (draw-tail prev tail scene color)
  (cond
    [(empty? tail) scene]
    [(empty? (rest tail))
     (let* ([cur (first tail)]
            [dir (direction prev cur)])
       (place-image (get-body-image dir color)
                    (cell-center (posn-x cur))
                    (cell-center (posn-y cur))
                    scene))]
    [else
     (let* ([cur (first tail)]
            [nxt (second tail)]
            [img (draw-tail-piece prev cur nxt color)]
            [new-scene
             (place-image img
                          (cell-center (posn-x cur))
                          (cell-center (posn-y cur))
                          scene)])
       (draw-tail cur (rest tail) new-scene color))]))

(define (draw-snake snake dir scene color)
  (let ([snake-list (vector->list snake)])
    (if (empty? snake-list)
        scene
        (let* ([head (first snake-list)]
               [tail (rest snake-list)]
               [scene-with-tail (draw-tail head tail scene color)]
               [head-img (get-head-image dir color)]
               [scene-with-head
                (place-image head-img
                             (cell-center (posn-x head))
                             (cell-center (posn-y head))
                             scene-with-tail)])
          scene-with-head))))

(define (draw-foods foods scene)
  (let loop ([i 0] [s scene])
    (if (>= i (vector-length foods))
        s
        (let ([f (vector-ref foods i)]) ; f è un singolo posn
          (loop (add1 i)
                (place-image FRUIT
                             (cell-center (posn-x f))
                             (cell-center (posn-y f))
                             s))))))





(define (create-score-bar score record paused?)
  (let* ([score-text  (text (string-append "Score: " (number->string score)) 18 "white")]
         [record-text (text (string-append "Record: " (number->string record)) 18 "yellow")]
         [button-spacing 10] ; distanza tra i pulsanti
         
         ;; Restart
         [restart-button (overlay (text "Restart" 16 "black")
                                  (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "gray"))]
         
         ;; Pause
         [pause-button (overlay (text (if paused? "Resume" "Pause") 16 "black")
                                (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "gray"))]
         
         [score-area (rectangle SCENE-WIDTH TOP-BORDER-SIZE "solid" "black")]
         [center-x (/ SCENE-WIDTH 2)]
         
         ;; posizionamento dei pulsanti al centro con spazio
         [restart-x (- center-x (/ BUTTON-WIDTH 2) (/ button-spacing 2))]
         [pause-x (+ center-x (/ BUTTON-WIDTH 2) (/ button-spacing 2))]
         
         [buttons-scene (place-image restart-button restart-x (/ TOP-BORDER-SIZE 2)
                            (place-image pause-button pause-x (/ TOP-BORDER-SIZE 2) score-area))]
         
         ;; posizionamento score e record vicino ai bordi
         [score-x (+ (/ BUTTON-WIDTH 2) 10)]                 ; 10 px di margine sinistro
         [record-x (- SCENE-WIDTH (/ BUTTON-WIDTH 2) 10)])   ; 10 px di margine destro
         
    ;; comporre la scena finale
    (place-image score-text score-x (/ TOP-BORDER-SIZE 2)
      (place-image record-text record-x (/ TOP-BORDER-SIZE 2) buttons-scene))))





(define (draw-pause-button paused? scene)
  (let* ([button-bg (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "gray")]
         [label (if paused? "Resume" "Pause")]
         [button-label (text label 16 "black")]
         [button (overlay button-label button-bg)]
         ;; posizioniamo il pulsante a destra del bottone Restart
         [x (+ (/ SCENE-WIDTH 2) BUTTON-WIDTH 1)] ; 1 pixel di margine
         [y (/ TOP-BORDER-SIZE 2)]) ; stessa altezza del bottone Restart
    (place-image button x y scene)))



(define (render-game w)
  (let* ([color (menu-color (world-menu w))] 
         [inner-scene (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "lightblue")]
         [grid-scene (draw-grid inner-scene)]
         [scene-with-obstacles (draw-obstacles (world-obstacles w) (menu-size (world-menu w)) grid-scene)]
         [scene-with-snake (draw-snake (world-snake w) (world-dir w) scene-with-obstacles color)]
         [scene-with-food (draw-foods (world-foods w) scene-with-snake)]
         [score-bar (create-score-bar (world-score w) (world-record w) (world-paused? w))]
         [outer (rectangle TOTAL-WIDTH TOTAL-HEIGHT "solid" "black")]
         [scene-with-grid
          (place-image scene-with-food
                       (+ BORDER-SIZE (/ SCENE-WIDTH 2))
                       (+ TOP-BORDER-SIZE (/ SCENE-HEIGHT 2))
                       (overlay/align "center" "top" score-bar outer))])
     scene-with-grid))


;; ======================
;; Rendering Menu
;; ======================
(define (render-menu m)
  (let* ([sel (menu-selector m)]
         [cx (/ TOTAL-WIDTH 2)]
         ;; shifted downward
         [title-y    (* TOTAL-HEIGHT 0.15)]
         [subtitle-y (* TOTAL-HEIGHT 0.25)]
         [speed-y    (* TOTAL-HEIGHT 0.40)]
         [mode-y     (* TOTAL-HEIGHT 0.48)]
         [size-y     (* TOTAL-HEIGHT 0.56)]
         [color-y    (* TOTAL-HEIGHT 0.64)]
         [fruits-y   (* TOTAL-HEIGHT 0.72)]
         [start-y    (* TOTAL-HEIGHT 0.80)]
         [bg         (rectangle TOTAL-WIDTH TOTAL-HEIGHT "solid" "black")])
    
    (place-image
     (text "SNAKET" 24 "cyan") cx title-y
     (place-image
      (text "Use W/S to select, A/D to change" 18 "lightgray") cx subtitle-y
      (place-image
       (text (string-append (if (eq? sel 'speed) ">> " "") 
                            "Speed: " (speed-label (menu-speed m)))
             20 (if (eq? sel 'speed) "yellow" "white"))
       cx speed-y
       (place-image
        (text (string-append (if (eq? sel 'mode) ">> " "") 
                             "Mode: " (symbol->string (menu-mode m)))
              20 (if (eq? sel 'mode) "yellow" "white"))
        cx mode-y
        (place-image
         (text (string-append (if (eq? sel 'size) ">> " "") 
                              "Size: " (number->string (menu-size m)))
               20 (if (eq? sel 'size) "yellow" "white"))
         cx size-y
         (place-image
          (text (string-append (if (eq? sel 'color) ">> " "") 
                               "Color: " (symbol->string (menu-color m)))
                20 (if (eq? sel 'color) "yellow" "white"))
          cx color-y
          (place-image
           (text (string-append (if (eq? sel 'num-fruits) ">> " "")
                     "Fruits: " (number->string (menu-num-fruits m)))
                 20 (if (eq? sel 'num-fruits) "yellow" "white"))
           cx fruits-y
           (place-image
            (text "Press SPACEBAR to start" 22 "cyan") cx start-y
            bg))))))))))






;; ======================
;; Key Handlers
;; ======================

(define (speed-label speed)
  (let ([idx (index-of SPEEDS-LIST speed)])
    (if (eq? idx #f)
        "Normal"
        (list-ref SPEED-LABELS idx))))


(define (menu-key m key)
  (let ([selectors '(speed mode size color num-fruits)])
    (cond
      ;; W/S per cambiare selezione
      [(or (key=? key "w") (key=? key "W"))
       (let* ([current (menu-selector m)]
              [idx (index-of selectors current)]
              [new-idx (if (= idx 0)
                           (- (length selectors) 1)
                           (sub1 idx))])
         (make-menu (menu-speed m)
                    (menu-mode m)
                    (menu-color m)
                    (menu-size m)
                    (list-ref selectors new-idx)
                    (menu-num-fruits m)))]
      
      [(or (key=? key "s") (key=? key "S"))
       (let* ([current (menu-selector m)]
              [idx (index-of selectors current)]
              [new-idx (if (= idx (- (length selectors) 1))
                           0
                           (add1 idx))])
         (make-menu (menu-speed m)
                    (menu-mode m)
                    (menu-color m)
                    (menu-size m)
                    (list-ref selectors new-idx)
                    (menu-num-fruits m)))]
      
      ;; A per diminuire il valore dell'opzione selezionata
      [(or (key=? key "a") (key=? key "A"))
       (cond
         [(eq? (menu-selector m) 'speed)
          (let* ([idx (index-of SPEEDS-LIST (menu-speed m))]
                 [new-idx (if (= idx 0)
                              (- (length SPEEDS-LIST) 1)
                              (sub1 idx))])
            (make-menu (list-ref SPEEDS-LIST new-idx)
                       (menu-mode m)
                       (menu-color m)
                       (menu-size m)
                       'speed
                       (menu-num-fruits m)))]
         
         [(eq? (menu-selector m) 'mode)
          (let* ([idx (index-of MODE-OPTIONS (menu-mode m))]
                 [new-idx (if (= idx 0)
                              (- (length MODE-OPTIONS) 1)
                              (sub1 idx))])
            (make-menu (menu-speed m)
                       (list-ref MODE-OPTIONS new-idx)
                       (menu-color m)
                       (menu-size m)
                       'mode
                       (menu-num-fruits m)))]
         
         [(eq? (menu-selector m) 'size)
          (let* ([idx (index-of SIZE-OPTIONS (menu-size m))]
                 [new-idx (if (= idx 0)
                              (- (length SIZE-OPTIONS) 1)
                              (sub1 idx))])
            (make-menu (menu-speed m)
                       (menu-mode m)
                       (menu-color m)
                       (list-ref SIZE-OPTIONS new-idx)
                       'size
                       (menu-num-fruits m)))]
         
         [(eq? (menu-selector m) 'color)
          (let* ([idx (index-of COLOR-OPTIONS (menu-color m))]
                 [new-idx (if (= idx 0)
                              (- (length COLOR-OPTIONS) 1)
                              (sub1 idx))])
            (make-menu (menu-speed m)
                       (menu-mode m)
                       (list-ref COLOR-OPTIONS new-idx)
                       (menu-size m)
                       'color
                       (menu-num-fruits m)))]
         
         [(eq? (menu-selector m) 'num-fruits)
          (let* ([n (menu-num-fruits m)]
                [new-n (if (= n 1) 5 (sub1 n))]) ; cicla 1-5
            (make-menu (menu-speed m)
                       (menu-mode m)
                       (menu-color m)
                       (menu-size m)
                       'num-fruits
                       new-n))])]
      
      ;; D per aumentare il valore dell'opzione selezionata
      [(or (key=? key "d") (key=? key "D"))
       (cond
         [(eq? (menu-selector m) 'speed)
          (let* ([idx (index-of SPEEDS-LIST (menu-speed m))]
                 [new-idx (if (= idx (- (length SPEEDS-LIST) 1))
                              0
                              (add1 idx))])
            (make-menu (list-ref SPEEDS-LIST new-idx)
                       (menu-mode m)
                       (menu-color m)
                       (menu-size m)
                       'speed
                       (menu-num-fruits m)))]
         
         [(eq? (menu-selector m) 'mode)
          (let* ([idx (index-of MODE-OPTIONS (menu-mode m))]
                 [new-idx (if (= idx (- (length MODE-OPTIONS) 1))
                              0
                              (add1 idx))])
            (make-menu (menu-speed m)
                       (list-ref MODE-OPTIONS new-idx)
                       (menu-color m)
                       (menu-size m)
                       'mode
                       (menu-num-fruits m)))]
         
         [(eq? (menu-selector m) 'size)
          (let* ([idx (index-of SIZE-OPTIONS (menu-size m))]
                 [new-idx (if (= idx (- (length SIZE-OPTIONS) 1))
                              0
                              (add1 idx))])
            (make-menu (menu-speed m)
                       (menu-mode m)
                       (menu-color m)
                       (list-ref SIZE-OPTIONS new-idx)
                       'size
                       (menu-num-fruits m)))]
         
         [(eq? (menu-selector m) 'color)
          (let* ([idx (index-of COLOR-OPTIONS (menu-color m))]
                 [new-idx (if (= idx (- (length COLOR-OPTIONS) 1))
                              0
                              (add1 idx))])
            (make-menu (menu-speed m)
                       (menu-mode m)
                       (list-ref COLOR-OPTIONS new-idx)
                       (menu-size m)
                       'color
                       (menu-num-fruits m)))]
         
         [(eq? (menu-selector m) 'num-fruits)
          (let* ([n (menu-num-fruits m)]
                [new-n (if (= n 5) 1 (add1 n))]) ; cicla 1-5
            (make-menu (menu-speed m)
                       (menu-mode m)
                       (menu-color m)
                       (menu-size m)
                       'num-fruits
                       new-n))])]
      
      ;; nessun cambiamento
      [else m])))





(define (handle-key-game w key)
  (let ([dir (world-dir w)]
        [snake (world-snake w)])
    
    (if (symbol=? dir 'none)
        (make-world 'game
            (world-menu w)
            snake
            'right
            (world-foods w)
            (world-game-over? w)
            (world-score w)
            (world-record w)
            (world-tick-counter w)
            (world-obstacles w)
            (world-free-spaces w)
            #f)


        (cond
          [(or (key=? key "up") (key=? key "W") (key=? key "w"))
           (if (symbol=? dir 'down) w
               (make-world 'game
            (world-menu w)
            snake
            'up   ; or 'down, 'left, 'right
            (world-foods w)
            (world-game-over? w)
            (world-score w)
            (world-record w)
            (world-tick-counter w)
            (world-obstacles w)
            (world-free-spaces w)
            #f)
)]

          [(or (key=? key "down") (key=? key "S") (key=? key "s"))
           (if (symbol=? dir 'up) w
               (make-world 'game
            (world-menu w)
            snake
            'down   ; or 'down, 'left, 'right
            (world-foods w)
            (world-game-over? w)
            (world-score w)
            (world-record w)
            (world-tick-counter w)
            (world-obstacles w)
            (world-free-spaces w)
            #f)
)]

          [(or (key=? key "left") (key=? key "A") (key=? key "a"))
           (if (symbol=? dir 'right) w
               (make-world 'game
            (world-menu w)
            snake
            'left   ; or 'down, 'left, 'right
            (world-foods w)
            (world-game-over? w)
            (world-score w)
            (world-record w)
            (world-tick-counter w)
            (world-obstacles w)
            (world-free-spaces w)
            #f)
)]
 
          [(or (key=? key "right") (key=? key "D") (key=? key "d"))
           (if (symbol=? dir 'left) w
               (make-world 'game
            (world-menu w)
            snake
            'right   ; or 'down, 'left, 'right
            (world-foods w)
            (world-game-over? w)
            (world-score w)
            (world-record w)
            (world-tick-counter w)
            (world-obstacles w)
            (world-free-spaces w)
            #f)
)]

          [else w]))))




;; ======================
;; Wrap-around helper
;; ======================
(define (active-grid-bounds size)
  (let* ([offset (quotient (- CELL-NUM-WIDTH size) 2)]
         [min-cell (+ offset 1)]
         [max-cell (- CELL-NUM-WIDTH offset)])
    (list min-cell max-cell min-cell max-cell))) ; min-x, max-x, min-y, max-y
 ; min-x, max-x, min-y, max-y

(define (wrap-head head size)
  (let ([bounds (active-grid-bounds size)])
    (let ([min-x (list-ref bounds 0)]
          [max-x (list-ref bounds 1)]
          [min-y (list-ref bounds 2)]
          [max-y (list-ref bounds 3)]
          [x (posn-x head)]
          [y (posn-y head)])
      (make-posn
       (cond [(< x min-x) max-x]
             [(> x max-x) min-x]
             [else x])
       (cond [(< y min-y) max-y]
             [(> y max-y) min-y]
             [else y])))))

;; ======================
;; Helper: Valid position (inside grid, not on snake, food, or obstacle)
;; ======================
(define (valid-position? p snake food obstacles)
  (and (>= (posn-x p) 1) (<= (posn-x p) CELL-NUM-WIDTH)
       (>= (posn-y p) 1) (<= (posn-y p) CELL-NUM-HEIGHT)
       (not (member p obstacles))
       (not (equal? p food))
       (let loop ([i 0])
         (if (>= i (vector-length snake))
             #t
             (if (equal? p (vector-ref snake i))
                 #f
                 (loop (add1 i)))))))

;; ======================
;; Helper: Count free neighbors of a tile
;; ======================
(define (free-neighbors p snake obstacles)
  (let ([neighbors (list
                    (make-posn (- (posn-x p) 1) (posn-y p))
                    (make-posn (+ (posn-x p) 1) (posn-y p))
                    (make-posn (posn-x p) (- (posn-y p) 1))
                    (make-posn (posn-x p) (+ (posn-y p) 1)))])
    (foldl (lambda (n acc)
             (if (and (>= (posn-x n) 1) (<= (posn-x n) CELL-NUM-WIDTH)
                      (>= (posn-y n) 1) (<= (posn-y n) CELL-NUM-HEIGHT)
                      (not (member n obstacles))
                      (not (equal? n (vector-ref snake 0)))) ; la testa non conta
                 (+ acc 1)
                 acc))
           0 neighbors)))

;; ======================
;; Helper: Safe to place obstacle
;; ======================
(define (safe-to-place? p obstacles snake head)
  ;; due celle davanti alla testa iniziale (always right)
  (let ([front-cells (list (make-posn (+ 1 (posn-x head)) (posn-y head))
                           (make-posn (+ 2 (posn-x head)) (posn-y head)))])
    (and (>= (posn-x p) 1) (<= (posn-x p) CELL-NUM-WIDTH)
         (>= (posn-y p) 1) (<= (posn-y p) CELL-NUM-HEIGHT)
         (not (member p obstacles))
         (not (member p front-cells)) ; <- esclude le due celle davanti alla testa
         ;; non deve essere sul serpente
         (let loop ([i 0])
           (if (>= i (vector-length snake))
               #t
               (if (equal? (vector-ref snake i) p)
                   #f
                   (loop (add1 i)))))
         ;; tutti i vicini devono avere almeno 2 liberi se mettiamo l'ostacolo
         (let ([neighbors (list
                           (make-posn (- (posn-x p) 1) (posn-y p))
                           (make-posn (+ (posn-x p) 1) (posn-y p))
                           (make-posn (posn-x p) (- (posn-y p) 1))
                           (make-posn (posn-x p) (+ (posn-y p) 1)))])
           (foldl (lambda (n acc)
                    (and acc
                         (or (member n obstacles)
                             (>= (- (free-neighbors n snake (cons p obstacles)) 1) 2))))
                  #t
                  neighbors)))))


;; ======================
;; Check all non-obstacle tiles have at least 2 free neighbors
;; ======================
(define (check-obstacle-conditions obstacles snake food)
  (let loop ([x 1] [y 1])
    (if (> y CELL-NUM-HEIGHT)
        #t
        (let ([pos (make-posn x y)])
          (if (member pos obstacles)
              (if (< x CELL-NUM-WIDTH)
                  (loop (+ x 1) y)
                  (loop 1 (+ y 1)))
              (if (< (free-neighbors pos snake obstacles) 2)
                  #f
                  (if (< x CELL-NUM-WIDTH)
                      (loop (+ x 1) y)
                      (loop 1 (+ y 1)))))))))

;; ======================
;; all-reachable? 
;; ======================
(define (all-reachable? snake food obstacles)
  (let ((width CELL-NUM-WIDTH)
        (height CELL-NUM-HEIGHT)
        (start (vector-ref snake 0)))

    ;; neighbors è una variabile che contiene una procedura
    (let ((neighbors
           (lambda (pos)
             (filter (lambda (p)
                       (and (>= (posn-x p) 1) (<= (posn-x p) width)
                            (>= (posn-y p) 1) (<= (posn-y p) height)
                            (not (member p obstacles))))
                     (list
                      (make-posn (+ 1 (posn-x pos)) (posn-y pos))
                      (make-posn (- (posn-x pos) 1) (posn-y pos))
                      (make-posn (posn-x pos) (+ 1 (posn-y pos)))
                      (make-posn (posn-x pos) (- (posn-y pos) 1)))))))

      ;; BFS tramite named let: queue e visited sono liste di posn
      (let loop-bfs ((queue (list start)) (visited (list start)))
        (if (empty? queue)
            ;; BFS terminata: controlla tutte le celle libere siano in `visited`
            (let loop-check ((x 1) (y 1))
              (cond
                [(> y height) #t]
                [else
                 (let ((pos (make-posn x y)))
                   (cond
                     ;; se è un ostacolo, salto
                     [(member pos obstacles)
                      (if (< x width) (loop-check (+ x 1) y) (loop-check 1 (+ y 1)))]
                     ;; se è libera ma non raggiunta -> fallisce
                     [(not (member pos visited)) #f]
                     ;; altrimenti continua
                     [else (if (< x width) (loop-check (+ x 1) y) (loop-check 1 (+ y 1)))]))]))
            ;; BFS continua: prendi current, calcola nuovi vicini non visitati
            (let* ((current (first queue))
                   (rest-queue (rest queue))
                   (new-neighbors (filter (lambda (p) (not (member p visited)))
                                          (neighbors current)))
                   (new-visited (append visited new-neighbors)))
              (loop-bfs (append rest-queue new-neighbors) new-visited)))))))

;; ======================
;; generate-obstacles-safe (usa safe-to-place?)
;; ======================
(define (generate-obstacles-safe snake food)
  (let* ([head (vector-ref snake 0)]
         [num-obstacles (/ (* CELL-NUM-WIDTH CELL-NUM-HEIGHT)
                            (/ (+ CELL-NUM-WIDTH CELL-NUM-HEIGHT) 2.0))])
    (let loop ((count num-obstacles) (acc '()))
      (if (= count 0)
          (if (all-reachable? snake food acc)
              acc
              (generate-obstacles-safe snake food))
          (let ((p (make-posn (+ 1 (random (- CELL-NUM-WIDTH 2)))
                              (+ 1 (random (- CELL-NUM-HEIGHT 2))))))
            (if (safe-to-place? p acc snake head)
                (loop (sub1 count) (cons p acc))
                (loop count acc)))))))



(define (generate-outer-obstacles size)
  (let* ([offset (quotient (- CELL-NUM-WIDTH size) 2)]
         [start (+ offset 1)]
         [end (- CELL-NUM-WIDTH offset)])
    (let loop ([x 1] [y 1] [acc '()])
      (cond [(> y CELL-NUM-HEIGHT) acc]
            [(> x CELL-NUM-WIDTH) (loop 1 (add1 y) acc)]
            [(or (< x start) (> x end) (< y start) (> y end)) (loop (add1 x) y (cons (make-posn x y) acc))]
            [else (loop (add1 x) y acc)]))))



(define (handle-key-unified w key)
  (cond
    ;; ===== Menu =====
    [(symbol=? (world-mode w) 'menu)
     (cond
       ;; Premendo SPACE parte il gioco
       [(key=? key " ")
  (let* ([menu-updated (world-menu w)]
         [num-fruits   (menu-num-fruits menu-updated)]
         [foods        (random-foods initial-snake '() num-fruits)] ; generate all fruits
         [obs          (if (eq? (menu-mode menu-updated) 'Obstacles)
                           (generate-obstacles-safe initial-snake foods)
                           '())]
         [size         (menu-size menu-updated)]
         [outer        (if (< size 25) (generate-outer-obstacles size) '())]
         [total-obstacles (append obs outer)]
         ;; if any food spawned inside obstacles, regenerate individually
         [final-foods  (list->vector
                        (map (lambda (f)
                               (if (member f total-obstacles)
                                   (vector-ref (random-foods initial-snake total-obstacles 1) 0)
                                   f))
                             (vector->list foods)))]
         [free (count-free-spaces total-obstacles)])
    (make-world
     'game
     menu-updated
     initial-snake
     initial-dir
     final-foods
     #f
     0
     (world-record w)
     0
     total-obstacles
     free
     #f))]

       
       ;; Tutti gli altri tasti modificano il menu
       [else
        (make-world
         'menu
         (menu-key (world-menu w) key)
         initial-snake
         initial-dir
         initial-foods
         #f
         0
         (world-record w)
         0
         '()
         0
         #f)])]

    ;; ===== Game =====
    [(symbol=? (world-mode w) 'game)
     (cond
       ;; Se il gioco è finito e premi SPACE → torna al menu
       [(and (or (eq? (world-game-over? w) #t)
                 (eq? (world-game-over? w) 'win))
             (key=? key " "))
        (make-world
         'menu
         (world-menu w)
         initial-snake
         initial-dir
         initial-foods
         #f
         0
         (world-record w)
         0
         '()
         0
         #f)]

       ;; Se premi "L" → score = free-spaces - 1
       [(or (key=? key "l") (key=? key "L"))
        (make-world
         'game
         (world-menu w)
         (world-snake w)
         (world-dir w)
         (world-foods w)
         (world-game-over? w)
         (- (world-free-spaces w) 1)
         (world-record w)
         (world-tick-counter w)
         (world-obstacles w)
         (world-free-spaces w)
         #f)]

       ;; Altrimenti gestisci normalmente i tasti del gioco
       [else
        (handle-key-game w key)])]))





;; ====================== ;; Tick Handler ;; ======================

(define (obstacle-collision? head obstacles)
  (member head obstacles))

;; Helper: replace an element at a given index in a vector
(define (replace-food-at-index foods idx new-food)
  (let* ([n (vector-length foods)]
         [result (make-vector n)])
    (let loop ([i 0])
      (if (< i n)
          (begin
            (vector-set! result i (if (= i idx) new-food (vector-ref foods i)))
            (loop (add1 i)))
          result))))






;; Helper: find index of an element in a vector that satisfies a predicate
(define (vector-index pred vec)
  (let loop ([i 0])
    (cond
      [(>= i (vector-length vec)) #f]
      [(pred (vector-ref vec i)) i]
      [else (loop (add1 i))])))

;; =========================
;; Update game function
;; =========================
(define (update-game w)
  (cond
    ;; gioco finito
    [(or (eq? (world-game-over? w) #t)
         (eq? (world-game-over? w) 'win))
     w]

    ;; serpente fermo
    [(symbol=? (world-dir w) 'none)
     w]

    [else
     (let* ([speed     (menu-speed (world-menu w))]
            [counter   (world-tick-counter w)]
            [threshold (inexact->exact (round (/ speed 0.02)))])
       
       (if (< counter threshold)
           ;; ancora non muovere
           (make-world 'game
                       (world-menu w)
                       (world-snake w)
                       (world-dir w)
                       (world-foods w)
                       #f
                       (world-score w)
                       (world-record w)
                       (add1 counter)
                       (world-obstacles w)
                       (world-free-spaces w)
                       #f)
           
           ;; muovi il serpente
           (let* ([snake      (world-snake w)]
                  [dir        (world-dir w)]
                  [foods      (world-foods w)]
                  [score      (world-score w)]
                  [record     (world-record w)]
                  [head       (vector-ref snake 0)]
                  ;; Pac-Man wrap
                  [new-head (if (eq? (menu-mode (world-menu w)) 'Pacman)
              (wrap-head (move-head head dir)
                         (menu-size (world-menu w)))
              (move-head head dir))]

                  [snake-list (vector->list snake)]
                  [new-snake  (cons new-head snake-list)]
                  ;; indice del frutto mangiato
                  [ate-index (vector-index (lambda (f) (equal? new-head f)) foods)])
             
             ;; serpente ha mangiato un frutto
             (if (not (eq? ate-index #f))
    ;; serpente ha mangiato
    (let* ([new-score  (+ score 1)]
           [new-record (max record new-score)]
           [new-food   (vector-ref (random-foods (list->vector new-snake)
                                                 (world-obstacles w) 1) 0)]
           [new-foods  (replace-food-at-index foods ate-index new-food)]
           [win?       (= new-score (world-free-spaces w))])
      (if win?
          (make-world 'game
                      (world-menu w)
                      (list->vector new-snake)
                      dir
                      new-foods
                      'win
                      new-score
                      new-record
                      0
                      (world-obstacles w)
                      (world-free-spaces w)
                      #f)
          (make-world 'game
                      (world-menu w)
                      (list->vector new-snake)
                      dir
                      new-foods
                      #f
                      new-score
                      new-record
                      0
                      (world-obstacles w)
                      (world-free-spaces w)
                      #f)))
    ;; serpente non mangia
    (let ([shrunk (reverse (rest (reverse new-snake)))])
      (if (or (and (not (eq? (menu-mode (world-menu w)) 'Pacman))
                   (wall-collision? new-head))
              (self-collision? new-head (list->vector shrunk))
              (obstacle-collision? new-head (world-obstacles w))
              (not (eq? (vector-index (lambda (f) (equal? f new-head)) foods) #f)))
          ;; game over
          (make-world 'game
                      (world-menu w)
                      snake
                      dir
                      foods
                      #t
                      score
                      record
                      0
                      (world-obstacles w)
                      (world-free-spaces w)
                      #f)
          ;; avanzamento normale
          (make-world 'game
                      (world-menu w)
                      (list->vector shrunk)
                      dir
                      foods
                      #f
                      score
                      record
                      0
                      (world-obstacles w)
                      (world-free-spaces w)
                      #f))))
)))]))



(define (update-unified w)
  (if (or (symbol=? (world-mode w) 'menu)
          (world-paused? w))
      w
      (update-game w)))


;; ====================== ;; Mouse Handler ;; ======================
(define (handle-mouse-unified w x y event)
  (cond
    ;; Click Restart
[(and (symbol=? (world-mode w) 'game)
      (string=? event "button-down")
      (let* ([center-x (/ SCENE-WIDTH 2)]
             [button-spacing 10]
             [restart-x (- center-x (/ BUTTON-WIDTH 2) (/ button-spacing 2))]
             [restart-left (- restart-x (/ BUTTON-WIDTH 2))]
             [restart-right (+ restart-x (/ BUTTON-WIDTH 2))])
        (and (>= x restart-left) (<= x restart-right)
             (>= y 0) (<= y BUTTON-HEIGHT))))
 (make-world 'menu
             (world-menu w)
             initial-snake
             initial-dir
             initial-foods
             #f
             0
             (world-record w)
             0
             '()
             (count-free-spaces '())
             #f)]

;; Click Pause
[(and (symbol=? (world-mode w) 'game)
      (string=? event "button-down")
      (let* ([center-x (/ SCENE-WIDTH 2)]
             [button-spacing 10]
             [pause-x (+ center-x (/ BUTTON-WIDTH 2) (/ button-spacing 2))]
             [pause-left (- pause-x (/ BUTTON-WIDTH 2))]
             [pause-right (+ pause-x (/ BUTTON-WIDTH 2))])
        (and (>= x pause-left) (<= x pause-right)
             (>= y 0) (<= y BUTTON-HEIGHT))))
 (make-world 'game
             (world-menu w)
             (world-snake w)
             (world-dir w)
             (world-foods w)
             (world-game-over? w)
             (world-score w)
             (world-record w)
             (world-tick-counter w)
             (world-obstacles w)
             (world-free-spaces w)
             (not (world-paused? w)))]


    [else w]))



;; ======================
;; Rendering Game Over Screen
;; ======================
(define (render-game-over w)
  (place-image
   (text "GAME OVER" 36 "red")
   (/ TOTAL-WIDTH 2) 120
   (place-image
    (text (string-append "Final Score: " (number->string (world-score w))) 24 "white")
    (/ TOTAL-WIDTH 2) 180
    (place-image
     (text (string-append "Record: " (number->string (world-record w))) 24 "yellow")
     (/ TOTAL-WIDTH 2) 220
     (place-image
      (text "Press SPACE to return to menu" 22 "cyan")
      (/ TOTAL-WIDTH 2) 280
      (rectangle TOTAL-WIDTH TOTAL-HEIGHT "solid" "black"))))))
;; ======================
;; Rendering Win Screen
;; ======================
(define (render-win w)
  (place-image
   (text "YOU WIN!" 40 "green")
   (/ TOTAL-WIDTH 2) 120
   (place-image
    (text (string-append "Score: " (number->string (world-score w)))
          28 "white")
    (/ TOTAL-WIDTH 2) 200
    (place-image
     (text "Press SPACE to return to menu" 20 "yellow")
     (/ TOTAL-WIDTH 2) 260
     (rectangle TOTAL-WIDTH TOTAL-HEIGHT "solid" "black")))))

;; ====================== ;; Unified Render ;; ======================

(define (render-unified w)
  (cond
    [(symbol=? (world-mode w) 'menu)
     (render-menu (world-menu w))]

    ;; Se il gioco è finito o vinto
    [(or (eq? (world-game-over? w) #t)
         (eq? (world-game-over? w) 'win))
     (cond
       [(eq? (world-game-over? w) 'win)
        (render-win w)]
       [else
        (render-game-over w)])]

    ;; Gioco in corso
    [(symbol=? (world-mode w) 'game)
     (render-game w)]))




;; ====================== ;; Initial World ;; ======================

(define initial-world
  (make-world 'menu
              initial-menu
              initial-snake
              initial-dir
              initial-foods
              #f
              initial-score
              initial-record
              0
              '()
              (count-free-spaces '())
              #f)) ; free-spaces for menu, no obstacles


;; ====================== ;; Run Big-Bang ;; ======================

(big-bang initial-world
  [to-draw render-unified]
  [on-tick update-unified 0.02]
  [on-key handle-key-unified]
  [on-mouse handle-mouse-unified])
