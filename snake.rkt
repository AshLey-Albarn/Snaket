;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; ======================
;; Menu speed variable
;; ======================
(define game-speed 0.05)
(define-struct menu (speed))
(define initial-menu (make-menu 0.05))

;; ======================
;; Constants
;; ======================
(define WIDTH 20)
(define HEIGHT 20)
(define CELL-SIZE 20)
(define BORDER-SIZE 10)             
(define TOP-BORDER-SIZE (* 3 BORDER-SIZE))

(define SCENE-WIDTH (* WIDTH CELL-SIZE))
(define SCENE-HEIGHT (* HEIGHT CELL-SIZE))
(define TOTAL-WIDTH (+ SCENE-WIDTH (* 2 BORDER-SIZE)))
(define TOTAL-HEIGHT (+ SCENE-HEIGHT TOP-BORDER-SIZE BORDER-SIZE))

;; Button
(define BUTTON-WIDTH 100)
(define BUTTON-HEIGHT 30)

;; ======================
;; Data Structures
;; ======================
(define-struct world (mode menu snake dir food game-over? score record))

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
(define (move-head head dir)
  (let ([x (posn-x head)] [y (posn-y head)])
    (cond [(symbol=? dir 'up) (make-posn x (- y 1))]
          [(symbol=? dir 'down) (make-posn x (+ y 1))]
          [(symbol=? dir 'left) (make-posn (- x 1) y)]
          [(symbol=? dir 'right) (make-posn (+ x 1) y)])))

(define (wall-collision? p)
  (or (< (posn-x p) 1) (>= (posn-x p) WIDTH)
      (< (posn-y p) 1) (>= (posn-y p) HEIGHT)))

(define (self-collision? head snake)
  (let loop ([i 1])
    (if (>= i (vector-length snake))
        #f
        (or (equal? head (vector-ref snake i))
            (loop (add1 i))))))

(define (random-food snake)
  (let ([p (make-posn (+ 1 (random (- WIDTH 2)))
                      (+ 1 (random (- HEIGHT 2))))])
    (if (let loop ([i 0])
          (if (>= i (vector-length snake))
              #f
              (or (equal? (vector-ref snake i) p)
                  (loop (add1 i)))))
        (random-food snake)
        p)))

;; ======================
;; Grid Drawing
;; ======================
(define (draw-grid scene)
  (let* ([scene-with-vlines
          (foldr (lambda (i img)
                   (add-line img (* i CELL-SIZE) 0 (* i CELL-SIZE) SCENE-HEIGHT "gray"))
                 scene
                 (build-list (add1 WIDTH) identity))]
         [scene-with-hlines
          (foldr (lambda (i img)
                   (add-line img 0 (* i CELL-SIZE) SCENE-WIDTH (* i CELL-SIZE) "gray"))
                 scene-with-vlines
                 (build-list (add1 HEIGHT) identity))])
    scene-with-hlines))

;; ======================
;; Rendering Snake Game
;; ======================
(define (render-game w)
  (let* ([inner-scene (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "lightblue")]
         [grid-scene (draw-grid inner-scene)]
         [snake (world-snake w)]
         [snake-list (vector->list snake)]
         [scene-with-snake
          (if (empty? snake-list)
              grid-scene
              (let* ([head (first snake-list)]
                     [tail (rest snake-list)]
                     [scene-with-tail
                      (foldr (lambda (p img)
                               (place-image (rectangle 20 20 "solid" "green")
                                            (* (posn-x p) CELL-SIZE)
                                            (* (posn-y p) CELL-SIZE)
                                            img))
                             grid-scene
                             tail)]
                     [scene-with-head
                      (place-image (rectangle 20 20 "solid" "darkgreen")
                                   (* (posn-x head) CELL-SIZE)
                                   (* (posn-y head) CELL-SIZE)
                                   scene-with-tail)])
                scene-with-head))]
         [food-img
          (place-image (circle 10 "solid" "red")
                       (* (posn-x (world-food w)) CELL-SIZE)
                       (* (posn-y (world-food w)) CELL-SIZE)
                       scene-with-snake)]
         [final-inner
          (if (world-game-over? w)
              (overlay (text "Game Over" 24 "red") food-img)
              food-img)]

         ;; Score bar
         [score-text (text (string-append "Score: " (number->string (world-score w))) 18 "white")]
         [record-text (text (string-append "Record: " (number->string (world-record w))) 18 "yellow")]
         [button-bg (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "gray")]
         [button-label (text "Restart" 16 "black")]
         [button (overlay button-label button-bg)]
         [score-area (rectangle SCENE-WIDTH TOP-BORDER-SIZE "solid" "black")]
         [score-bar
          (place-image score-text 80 (/ TOP-BORDER-SIZE 2)
            (place-image record-text (- SCENE-WIDTH 100) (/ TOP-BORDER-SIZE 2)
              (place-image button (/ SCENE-WIDTH 2) (/ TOP-BORDER-SIZE 2) score-area)))]
         ;; Outer frame
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
   (text "IMPOSTA VELOCITÀ DI GIOCO" 24 "cyan")
   (/ TOTAL-WIDTH 2) 80
   (place-image
    (text "Usa FRECCIA SU/GIÙ per cambiare" 18 "lightgray")
    (/ TOTAL-WIDTH 2) 140
    (place-image
     (text (string-append "Velocità attuale: " (number->string (menu-speed m)))
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
  (cond [(key=? key "up")
         (make-menu (max 0.01 (- (menu-speed m) 0.01)))]
        [(key=? key "down")
         (make-menu (min 0.30 (+ (menu-speed m) 0.01)))]
        [else m]))

(define (handle-key-game w key)
  (let ([dir (world-dir w)] [snake (world-snake w)])
    (if (symbol=? dir 'none)
        (make-world 'game (world-menu w) snake 'right (world-food w)
                    (world-game-over? w) (world-score w) (world-record w))
        (cond [(key=? key "up")
               (if (symbol=? dir 'down) w
                   (make-world 'game (world-menu w) snake 'up (world-food w)
                               (world-game-over? w) (world-score w) (world-record w)))]
              [(key=? key "down")
               (if (symbol=? dir 'up) w
                   (make-world 'game (world-menu w) snake 'down (world-food w)
                               (world-game-over? w) (world-score w) (world-record w)))]
              [(key=? key "left")
               (if (symbol=? dir 'right) w
                   (make-world 'game (world-menu w) snake 'left (world-food w)
                               (world-game-over? w) (world-score w) (world-record w)))]
              [(key=? key "right")
               (if (symbol=? dir 'left) w
                   (make-world 'game (world-menu w) snake 'right (world-food w)
                               (world-game-over? w) (world-score w) (world-record w)))]
              [else w]))))

(define (handle-key-unified w key)
  (cond
    [(symbol=? (world-mode w) 'menu)
     (cond
       [(key=? key " ")
        (begin
          (set! game-speed (menu-speed (world-menu w)))
          (make-world 'game (world-menu w)
                      initial-snake initial-dir (random-food initial-snake)
                      #f 0 (world-record w)))]
       [else
        (make-world 'menu (menu-key (world-menu w) key)
                    (world-snake w) (world-dir w) (world-food w)
                    (world-game-over? w) (world-score w) (world-record w))])]
    [(symbol=? (world-mode w) 'game)
     (handle-key-game w key)]))


;; ======================
;; Tick Handler
;; ======================
(define (update-game w)
  (if (or (world-game-over? w)
          (symbol=? (world-dir w) 'none))
      w
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
              (make-world 'game (world-menu w) (list->vector new-snake) dir new-food
                          #f new-score new-record))
            (let ([shrunk (reverse (rest (reverse new-snake)))])
              (if (or (wall-collision? new-head)
                      (self-collision? new-head (list->vector shrunk)))
                  (make-world 'game (world-menu w) snake dir food #t score record)
                  (make-world 'game (world-menu w) (list->vector shrunk) dir food #f score record)))))))

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
                  #f 0 (world-record w))
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
              initial-snake initial-dir initial-food #f initial-score initial-record))

;; ======================
;; Run Big-Bang
;; ======================
(big-bang initial-world
  [to-draw render-unified]
  [on-tick update-unified game-speed]
  [on-key handle-key-unified]
  [on-mouse handle-mouse-unified])
