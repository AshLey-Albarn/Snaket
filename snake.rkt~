;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; ======================
;; Menu speed variable
;; ======================
(define-struct menu (speed))
(define initial-menu (make-menu 0.06))

;; ======================
;; Constants
;; ======================

(define MAX-SPEED 0.30)
(define MIN-SPEED 0.01)

;size, cells and grid
(define CELL-NUM-WIDTH 20)
(define CELL-NUM-HEIGHT 20)
(define CELL-SIZE 20)
(define BORDER-SIZE 10)             
(define TOP-BORDER-SIZE (* 3 BORDER-SIZE))

(define SCENE-WIDTH (* CELL-NUM-WIDTH CELL-SIZE))
(define SCENE-HEIGHT (* CELL-NUM-HEIGHT CELL-SIZE))
(define TOTAL-WIDTH (+ SCENE-WIDTH (* 2 BORDER-SIZE)))
(define TOTAL-HEIGHT (+ SCENE-HEIGHT TOP-BORDER-SIZE BORDER-SIZE))

;Button
(define BUTTON-WIDTH 100)
(define BUTTON-HEIGHT 30)

;Graphics
(define GRID-COLOR "gray")
(define SNAKE-COLOR "darkgreen")
(define TRANSPARENT (make-color 0 0 0 0))

(define EYE (overlay/xy (circle 4 "solid" "black") -6 -2 (circle 10 "solid" "white")))
(define SNAKE-HEAD (scale 0.5 (overlay/xy EYE -20 -25 (overlay/xy EYE 0 -25 (rotate 90(polygon (list (make-pulled-point 1/2 20 0 0 1/2 -20)
                 (make-posn -10 20)
                 (make-pulled-point 1/2 -20 60 0 1/2 20)
                 (make-posn -10 -20))
           "solid"
           SNAKE-COLOR))))))
(define SNAKE-BODY (rectangle 13 20 "solid" SNAKE-COLOR))
(define SNAKE-BODY-ANGLE (overlay/xy (rectangle 13 13 "solid" TRANSPARENT) 7 -7
                         (overlay/xy (rectangle 13 13 "solid" TRANSPARENT) 0 -14
                         (overlay/xy (rectangle 13 13 "solid" SNAKE-COLOR) -7 -7
                         (overlay/xy (rectangle 13 13 "solid" SNAKE-COLOR) 0 7 (rectangle 13 13 "solid" SNAKE-COLOR))))))


(define FRUIT-CORE (circle 10 "solid" "red"))
(define FRUIT-LEAF (ellipse 5 10 "solid" "green"))
(define FRUIT 
    (overlay/xy (rotate 45 FRUIT-LEAF) -4 3 FRUIT-CORE))
  
;; ======================
;; Data Structures
;; ======================
(define-struct world (mode menu snake dir food game-over? score record tick-counter))

;; ======================
;; Initial Game Data
;; ======================
(define initial-snake (vector (make-posn 5 10)))
(define initial-dir 'none)
(define initial-food (make-posn 15 10))
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
  (or (< (posn-x p) 1) (>= (posn-x p) CELL-NUM-WIDTH)
      (< (posn-y p) 1) (>= (posn-y p) CELL-NUM-HEIGHT)))

(define (self-collision? head snake)
  (let loop ([i 1])
    (if (>= i (vector-length snake))
        #f
        (or (equal? head (vector-ref snake i))
            (loop (add1 i))))))

;creates food in random places
(define (random-food snake)
  (let ([p (make-posn (+ 1 (random (- CELL-NUM-WIDTH 2)))
                      (+ 1 (random (- CELL-NUM-HEIGHT 2))))])
    (if (let loop ([i 0])
          (if (>= i (vector-length snake))
              #f
              (or (equal? (vector-ref snake i) p)
                  (loop (add1 i)))))
        (random-food snake)
        p)))

;; ============================================
;; ADDED HELPERS FOR TURN DETECTION
;; ============================================

;returns either 'up 'down 'left 'right based on the x's and y's
(define (direction from to)
  (let ([dx (- (posn-x to) (posn-x from))]
        [dy (- (posn-y to) (posn-y from))])
    (cond [(= dx 0) (if (< dy 0) 'up 'down)]
          [(= dy 0) (if (< dx 0) 'left 'right)]
          [else 'none])))

(define (vertical? dir)
  (or (eq? dir 'up) (eq? dir 'down)))
(define (horizontal? dir)
  (or (eq? dir 'left) (eq? dir 'right)))

;returns the correct tail piece to draw based on the postion of also the previous and next piece,
;only case that doesent apply is if the snake has only one piece (cant get a next piece)
(define (draw-tail-piece prev curr next)
  (let* ([dir1 (direction prev curr)]
         [dir2 (direction curr next)])
    (cond
      [(eq? dir1 dir2) (get-body-image dir1)]
      [(or (and (eq? dir1 'up) (eq? dir2 'right))
           (and (eq? dir1 'left) (eq? dir2 'down)))
       (rotate 270 SNAKE-BODY-ANGLE)]

      [(or (and (eq? dir1 'right) (eq? dir2 'up))
           (and (eq? dir1 'down)  (eq? dir2 'left)))
       (rotate 90 SNAKE-BODY-ANGLE)]

      [(or (and (eq? dir1 'right) (eq? dir2 'down))
           (and (eq? dir1 'up)    (eq? dir2 'left)))
       (rotate 180 SNAKE-BODY-ANGLE)]

      [(or (and (eq? dir1 'down) (eq? dir2 'right))
           (and (eq? dir1 'left) (eq? dir2 'up)))
       (rotate 0 SNAKE-BODY-ANGLE)]

      [else SNAKE-BODY])))


(define (get-body-image dir)
  (cond [(vertical? dir) SNAKE-BODY]
        [(horizontal? dir) (rotate 90 SNAKE-BODY)]))

(define (get-head-image dir)
  (cond [(symbol=? dir 'right) (rotate 270 SNAKE-HEAD)]
        [(symbol=? dir 'down) (rotate 180 SNAKE-HEAD)]
        [(symbol=? dir 'left)(rotate 90 SNAKE-HEAD)]
        [(symbol=? dir 'up) SNAKE-HEAD]
        [else (rotate 270 SNAKE-HEAD)]))

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

(define (draw-tail prev tail scene)
  (cond
    [(empty? tail) scene]

    [(empty? (rest tail))
     (let* ([cur (first tail)]
            [dir (direction prev cur)])
       (place-image (get-body-image dir)
                    (* (posn-x cur) CELL-SIZE)
                    (* (posn-y cur) CELL-SIZE)
                    scene))]

    [else
     (let* ([cur (first tail)]
            [nxt (second tail)]
            [img (draw-tail-piece prev cur nxt)]
            [new-scene
             (place-image img
                          (* (posn-x cur) CELL-SIZE)
                          (* (posn-y cur) CELL-SIZE)
                          scene)])
       (draw-tail cur (rest tail) new-scene))]))

(define (draw-snake snake dir scene)
  (let ([snake-list (vector->list snake)])
    (if (empty? snake-list)
        scene
        (let* ([head (first snake-list)]
               [tail (rest snake-list)]
               [scene-with-tail (draw-tail head tail scene)]
               [head-img (get-head-image dir)]
               [scene-with-head
                (place-image head-img
                             (* (posn-x head) CELL-SIZE)
                             (* (posn-y head) CELL-SIZE)
                             scene-with-tail)])
          scene-with-head))))

(define (draw-food food scene)
  (place-image FRUIT
               (* (posn-x food) CELL-SIZE)
               (* (posn-y food) CELL-SIZE)
               scene))

(define (create-score-bar score record)
  (let* ([score-text (text (string-append "Score: " (number->string score)) 18 "white")]
         [record-text (text (string-append "Record: " (number->string record)) 18 "yellow")]
         [button-bg (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "gray")]
         [button-label (text "Restart" 16 "black")]
         [button (overlay button-label button-bg)]
         [score-area (rectangle SCENE-WIDTH TOP-BORDER-SIZE "solid" "black")])
    (place-image score-text 80 (/ TOP-BORDER-SIZE 2)
      (place-image record-text (- SCENE-WIDTH 100) (/ TOP-BORDER-SIZE 2)
        (place-image button (/ SCENE-WIDTH 2) (/ TOP-BORDER-SIZE 2) score-area)))))

(define (render-game w)
  (let* ([inner-scene (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "lightblue")]
         [grid-scene (draw-grid inner-scene)]
         [scene-with-snake (draw-snake (world-snake w) (world-dir w) grid-scene)]
         [scene-with-food (draw-food (world-food w) scene-with-snake)]
         [final-inner
          (if (world-game-over? w)
              (overlay (text "Game Over" 24 "red") scene-with-food)
              scene-with-food)]
         [score-bar (create-score-bar (world-score w) (world-record w))]
         [outer (rectangle TOTAL-WIDTH TOTAL-HEIGHT "solid" "black")]
         [scene-with-grid
          (place-image final-inner
                       (+ BORDER-SIZE (/ SCENE-WIDTH 2))
                       (+ TOP-BORDER-SIZE (/ SCENE-HEIGHT 2))
                       (overlay/align "center" "top" score-bar outer))])
    scene-with-grid))

;; ======================
;; Rendering Menu
;; ======================
(define (render-menu m)
  (place-image
   (text "SET GAME SPEED" 24 "cyan")
   (/ TOTAL-WIDTH 2) 80
   (place-image
    (text "Use W/S or up/down to change" 18 "lightgray")
    (/ TOTAL-WIDTH 2) 140
    (place-image
     (text (string-append "Current speed: " (number->string (exact->inexact (- (+ MAX-SPEED 0.01) (menu-speed m)))))
           20 "yellow")
     (/ TOTAL-WIDTH 2) 200
     (place-image
      (text "Press SPACEBAR to start" 22 "white")
      (/ TOTAL-WIDTH 2) 260
      (rectangle TOTAL-WIDTH TOTAL-HEIGHT "solid" "black"))))))

;; ======================
;; Key Handlers
;; ======================
(define (menu-key m key)
  (cond [(or (key=? key "up") (key=? key "W") (key=? key "w"))
         (make-menu (max MIN-SPEED (- (menu-speed m) 0.01)))]
        [(or (key=? key "down") (key=? key "S") (key=? key "s"))
         (make-menu (min MAX-SPEED (+ (menu-speed m) 0.01)))]
        [else m]))

(define (handle-key-game w key)
  (let ([dir (world-dir w)]
        [snake (world-snake w)])
    
    (if (symbol=? dir 'none)
        (make-world 'game (world-menu w) snake 'right (world-food w)
                    (world-game-over? w) (world-score w) (world-record w)
                    (world-tick-counter w))

        (cond
          [(or (key=? key "up") (key=? key "W") (key=? key "w"))
           (if (symbol=? dir 'down) w
               (make-world 'game (world-menu w) snake 'up (world-food w)
                           (world-game-over? w) (world-score w) (world-record w)
                           (world-tick-counter w)))]

          [(or (key=? key "down") (key=? key "S") (key=? key "s"))
           (if (symbol=? dir 'up) w
               (make-world 'game (world-menu w) snake 'down (world-food w)
                           (world-game-over? w) (world-score w) (world-record w)
                           (world-tick-counter w)))]

          [(or (key=? key "left") (key=? key "A") (key=? key "a"))
           (if (symbol=? dir 'right) w
               (make-world 'game (world-menu w) snake 'left (world-food w)
                           (world-game-over? w) (world-score w) (world-record w)
                           (world-tick-counter w)))]
 
          [(or (key=? key "right") (key=? key "D") (key=? key "d"))
           (if (symbol=? dir 'left) w
               (make-world 'game (world-menu w) snake 'right (world-food w)
                           (world-game-over? w) (world-score w) (world-record w)
                           (world-tick-counter w)))]

          [else w]))))



(define (handle-key-unified w key)
  (cond
    [(symbol=? (world-mode w) 'menu)
     (cond
       [(key=? key " ")
        (make-world
         'game
         (world-menu w)
         initial-snake
         initial-dir
         (random-food initial-snake)
         #f
         0
         (world-record w)
         0)]
       [else
        (make-world
         'menu
         (menu-key (world-menu w) key)
         (world-snake w)
         (world-dir w)
         (world-food w)
         (world-game-over? w)
         (world-score w)
         (world-record w)
         (world-tick-counter w))])]
    [(symbol=? (world-mode w) 'game)
     (handle-key-game w key)]))

;; ======================
;; Tick Handler
;; ======================
(define (update-game w)
  (cond
    [(world-game-over? w) w]
    [(symbol=? (world-dir w) 'none) w]
    [else
     (let* ([speed (menu-speed (world-menu w))]
            [counter (world-tick-counter w)]
            [threshold (inexact->exact (round (/ speed 0.02)))])

       (if (< counter threshold)
           (make-world 'game (world-menu w) (world-snake w)
                       (world-dir w) (world-food w)
                       #f (world-score w) (world-record w)
                       (add1 counter))

           (let* ([snake (world-snake w)]
                  [dir (world-dir w)]
                  [food (world-food w)]
                  [score (world-score w)]
                  [record (world-record w)]
                  [head (vector-ref snake 0)]
                  [new-head (move-head head dir)]
                  [snake-list (vector->list snake)]
                  [new-snake (cons new-head snake-list)]
                  [ate? (equal? new-head food)])

             (if ate?
                 (let* ([new-score (+ score 1)]
                        [new-record (max record new-score)]
                        [new-food (random-food (list->vector new-snake))])
                   (make-world 'game (world-menu w) (list->vector new-snake)
                               dir new-food #f new-score new-record 0))

                 (let ([shrunk (reverse (rest (reverse new-snake)))])
                   (if (or (wall-collision? new-head)
                           (self-collision? new-head (list->vector shrunk)))
                       (make-world 'game (world-menu w) snake dir food
                                   #t score record 0)
                       (make-world 'game (world-menu w)
                                   (list->vector shrunk) dir food
                                   #f score record 0)))))))]))

(define (update-unified w)
  (if (symbol=? (world-mode w) 'game)
      (update-game w)
      w))

;; ======================
;; Mouse Handler
;; ======================
(define (handle-mouse-unified w x y event)
  (if (and (symbol=? (world-mode w) 'game)
           (string=? event "button-down")
           (>= x (- (/ SCENE-WIDTH 2) (/ BUTTON-WIDTH 2)))
           (<= x (+ (/ SCENE-WIDTH 2) (/ BUTTON-WIDTH 2)))
           (>= y 0)
           (<= y BUTTON-HEIGHT))
      (make-world 'game (world-menu w)
                  initial-snake initial-dir (random-food initial-snake)
                  #f 0 (world-record w) 0)
      w))

;; ======================
;; Unified Render
;; ======================
(define (render-unified w)
  (cond
    [(symbol=? (world-mode w) 'menu) (render-menu (world-menu w))]
    [(symbol=? (world-mode w) 'game) (render-game w)]))

;; ======================
;; Initial World
;; ======================
(define initial-world
  (make-world 'menu initial-menu
              initial-snake initial-dir initial-food #f initial-score initial-record 0))

;; ======================
;; Run Big-Bang
;; ======================
(big-bang initial-world
  [to-draw render-unified]
  [on-tick update-unified 0.02]
  [on-key handle-key-unified]
  [on-mouse handle-mouse-unified])
