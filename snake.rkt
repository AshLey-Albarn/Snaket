;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Menu structure
;; A Menu represents the game settings
;; Fields:
;;   speed        : Number      ; delay between moves in seconds
;;   mode         : Symbol      ; game mode: 'Plain, 'Obstacles, or 'Pacman
;;   color        : Color       ; background or theme color
;;   size         : Number      ; size of each cell in the grid
;;   selector     : Symbol      ; currently selected menu option
;;   num-fruits   : Number      ; number of food items on the board
(define-struct menu (speed mode color size selector num-fruits))
(define initial-menu (make-menu 0.10 'Plain 'Darkgreen 20 'speed 1))

;; ======================
;; Constants
;; ======================

;; Speeds and labels
(define SPEEDS-LIST (list 0.15 0.10 0.07 0.03))
(define SPEED-LABELS '("Slow" "Normal" "Fast" "Crazy"))

;; Game mode options
(define MODE-OPTIONS '(Plain Obstacles Pacman))

;; Cell size options
(define SIZE-OPTIONS (list 16 20 24))

;; Grid and screen dimensions
(define CELL-NUM-WIDTH 24)
(define CELL-NUM-HEIGHT 24)
(define CELL-SIZE 20)
(define BORDER-SIZE 10)
(define TOP-BORDER-SIZE (* 3 BORDER-SIZE))

(define SCENE-WIDTH 480)
(define SCENE-HEIGHT 480)
(define TOTAL-WIDTH (+ SCENE-WIDTH (* 2 BORDER-SIZE)))
(define TOTAL-HEIGHT (+ SCENE-HEIGHT TOP-BORDER-SIZE BORDER-SIZE))

;; Button dimensions
(define BUTTON-WIDTH 100)
(define BUTTON-HEIGHT 30)

;; Colors
(define COLOR-OPTIONS '(Darkgreen Red Blue Magenta Yellow))
(define GRID-COLOR "gray")
(define TRANSPARENT (make-color 0 0 0 0))

;; Eye for snake head
(define EYE (overlay/xy (circle 4 "solid" "black") -6 -2 
                        (circle 10 "solid" "white")))

;; Make snake head image
;; (make-snake-head Color) -> Image
;; returns an image representing the snake's head
(define (make-snake-head color)
  (scale 0.5
         (overlay/xy EYE -20 -25
                     (overlay/xy EYE 0 -25
                                 (rotate 90
                                         (polygon (list
                                                   (make-pulled-point 1/2 20 0 0 1/2 -20)
                                                   (make-posn -10 20)
                                                   (make-pulled-point 1/2 -20 60 0 1/2 20)
                                                   (make-posn -10 -20))
                                                  "solid"
                                                  color))))))

;; Make snake body image
;; (make-snake-body Color) -> Image
;; returns a rectangle representing a snake body segment
(define (make-snake-body color)
  (rectangle 13 20 "solid" color))

;; Make angled snake body
;; (make-snake-body-angle Color) -> Image
;; returns a more complex body segment for turning segments
(define (make-snake-body-angle color)
  (overlay/xy (rectangle 13 13 "solid" TRANSPARENT) 7 -7
              (overlay/xy (rectangle 13 13 "solid" TRANSPARENT) 0 -14
                          (overlay/xy (rectangle 13 13 "solid" color) -7 -7
                                      (overlay/xy (rectangle 13 13 "solid" color) 0 7
                                                  (rectangle 13 13 "solid" color))))))

;; Fruit graphics
(define FRUIT-CORE (circle 10 "solid" "red"))
(define FRUIT-LEAF (ellipse 5 10 "solid" "green"))
(define FRUIT (overlay/xy (rotate 45 FRUIT-LEAF) -4 3 FRUIT-CORE))

;; Obstacle graphics
(define OBSTACLE (rectangle CELL-SIZE CELL-SIZE "solid" "black"))

  
;; World structure
;; A World represents the full game state
;; Fields:
;;   mode          : Symbol        ; either 'menu or 'game
;;   menu          : Menu          ; current menu settings
;;   snake         : Vector<Posn>  ; positions of snake segments
;;   dir           : Symbol        ; direction of snake: 'up, 'down, 'left, 'right, 'none
;;   foods         : Vector<Posn>  ; positions of food items
;;   game-over?    : Boolean|Symbol ; #t for loss, 'win for win, #f otherwise
;;   score         : Number        ; current game score
;;   record        : Number        ; highest score
;;   tick-counter  : Number        ; counter for movement speed
;;   obstacles     : Vector<Posn>  ; positions of obstacles
;;   free-spaces   : Number        ; number of free cells on the board
;;   paused?       : Boolean       ; whether game is paused

(define-struct world (mode menu snake dir foods game-over? score record tick-counter obstacles free-spaces paused?))
;; Example instance:
(define example-world
  (make-world 'menu initial-menu (vector (make-posn 7 12)) 'none (vector (make-posn 15 12)) #f 0 0 0 (vector) 0 #f))

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
;; move-head: Posn Symbol -> Posn
;; Given the head position and direction, returns the new head position
(check-expect (move-head (make-posn 5 5) 'up) (make-posn 5 4))
(check-expect (move-head (make-posn 5 5) 'down) (make-posn 5 6))
(check-expect (move-head (make-posn 5 5) 'left) (make-posn 4 5))
(check-expect (move-head (make-posn 5 5) 'right) (make-posn 6 5))
(define (move-head head dir)
  (let ([x (posn-x head)] [y (posn-y head)])
    (cond [(symbol=? dir 'up) (make-posn x (- y 1))]
          [(symbol=? dir 'down) (make-posn x (+ y 1))]
          [(symbol=? dir 'left) (make-posn (- x 1) y)]
          [(symbol=? dir 'right) (make-posn (+ x 1) y)])))

;; wall-collision?: Posn -> Boolean
;; Checks if a given position collides with the wall boundaries
(check-expect (wall-collision? (make-posn 0 5)) #t)
(check-expect (wall-collision? (make-posn 5 0)) #t)
(check-expect (wall-collision? (make-posn 5 5)) #f)
(define (wall-collision? p)
  (or (< (posn-x p) 1) (>= (posn-x p) (+ CELL-NUM-WIDTH 1))
      (< (posn-y p) 1) (>= (posn-y p) (+ CELL-NUM-HEIGHT 1))))

;; self-collision?: Posn Vector<Posn> -> Boolean
;; Checks if the head collides with any part of the snake body (excluding head)
;; Termination argument: the loop increments i each step and stops at vector-length of snake
(define (self-collision? head snake)
  (let loop ([i 1])
    (if (>= i (vector-length snake))
        #f
        (or (equal? head (vector-ref snake i))
            (loop (add1 i))))))

;; Examples
(check-expect (self-collision? (make-posn 2 2) (vector (make-posn 2 2) (make-posn 3 2))) #f)
(check-expect (self-collision? (make-posn 3 2) (vector (make-posn 2 2) (make-posn 3 2))) #t)


;; random-foods : Vector<Posn> Vector<Posn> Number -> Vector<Posn>
;; Purpose: Creates `n` food positions randomly on the grid, avoiding the snake and obstacles.
;;          Returns a vector of positions.
;; Termination argument: The recursive loop decreases `count` each time a new food is added or stops if no free space remains.
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
              (if (>= (count-free-spaces obstacles) (vector-length snake))
                  (loop count acc)
                  (list->vector acc))
              (loop (sub1 count) (cons p acc)))))))

;; Example usage
(define test-snake (vector (make-posn 5 5) (make-posn 6 5)))
(define test-obstacles (list (make-posn 1 1) (make-posn 2 2)))
(check-expect (vector-length (random-foods test-snake test-obstacles 3)) 3)



;; count-free-spaces : List<Posn> -> Number
;; Purpose: Computes the number of free cells on the grid given obstacle positions.
(define (count-free-spaces obstacles)
  (- (* CELL-NUM-WIDTH CELL-NUM-HEIGHT)
     (length obstacles)
     1)) ; subtract 1 for snake head or reserved space

;; Examples
(check-expect (count-free-spaces '()) (- (* CELL-NUM-WIDTH CELL-NUM-HEIGHT) 1))
(check-expect (count-free-spaces (list (make-posn 1 1) (make-posn 2 2))) 
              (- (* CELL-NUM-WIDTH CELL-NUM-HEIGHT) 2 1))


;; direction : Posn Posn -> Symbol
;; Purpose: Determines the direction from `from` to `to`, taking grid wrap-around into account.
(define (direction from to)
  (let* ([dx (- (posn-x to) (posn-x from))]
         [dy (- (posn-y to) (posn-y from))]
         [dx-wrap (cond [(> dx (/ CELL-NUM-WIDTH 2)) (- dx CELL-NUM-WIDTH)]
                        [(< dx (- (/ CELL-NUM-WIDTH 2))) (+ dx CELL-NUM-WIDTH)]
                        [else dx])]
         [dy-wrap (cond [(> dy (/ CELL-NUM-HEIGHT 2)) (- dy CELL-NUM-HEIGHT)]
                        [(< dy (- (/ CELL-NUM-HEIGHT 2))) (+ dy CELL-NUM-HEIGHT)]
                        [else dy])])
    (cond [(= dx-wrap 0) (if (< dy-wrap 0) 'up 'down)]
          [(= dy-wrap 0) (if (< dx-wrap 0) 'left 'right)]
          [else 'none])))

;; Examples
(check-expect (direction (make-posn 5 5) (make-posn 5 4)) 'up)
(check-expect (direction (make-posn 5 5) (make-posn 5 6)) 'down)
(check-expect (direction (make-posn 5 5) (make-posn 4 5)) 'left)
(check-expect (direction (make-posn 5 5) (make-posn 6 5)) 'right)
(check-expect (direction (make-posn 0 0) (make-posn 23 0)) 'left) ; wrap-around

;; vertical? : Symbol -> Boolean
;; Purpose: Checks if a direction is vertical
(define (vertical? dir)
  (or (eq? dir 'up) (eq? dir 'down)))

(check-expect (vertical? 'up) #t)
(check-expect (vertical? 'down) #t)
(check-expect (vertical? 'left) #f)

;; horizontal? : Symbol -> Boolean
;; Purpose: Checks if a direction is horizontal
(define (horizontal? dir)
  (or (eq? dir 'left) (eq? dir 'right)))

(check-expect (horizontal? 'left) #t)
(check-expect (horizontal? 'right) #t)
(check-expect (horizontal? 'up) #f)

;; get-body-image : Symbol Color -> Image
;; Returns the correct image for a snake body segment based on direction
(define (get-body-image dir color)
  (cond [(vertical? dir) (make-snake-body color)]
        [(horizontal? dir) (rotate 90 (make-snake-body color))]))

;; draw-tail-piece : Posn Posn Posn Color -> Image
;; Purpose: Returns the correct image for a tail piece based on previous, current, and next positions
(define (draw-tail-piece prev curr next color)
  (let* ([dir1 (direction prev curr)]
         [dir2 (direction curr next)])
    (cond
      [(eq? dir1 dir2) (get-body-image dir1 color)]
      [(or (and (eq? dir1 'up) (eq? dir2 'right))
           (and (eq? dir1 'left) (eq? dir2 'down))) (rotate 270 (make-snake-body-angle color))]
      [(or (and (eq? dir1 'right) (eq? dir2 'up))
           (and (eq? dir1 'down) (eq? dir2 'left))) (rotate 90 (make-snake-body-angle color))]
      [(or (and (eq? dir1 'right) (eq? dir2 'down))
           (and (eq? dir1 'up) (eq? dir2 'left))) (rotate 180 (make-snake-body-angle color))]
      [(or (and (eq? dir1 'down) (eq? dir2 'right))
           (and (eq? dir1 'left) (eq? dir2 'up))) (rotate 0 (make-snake-body-angle color))]
      [else (make-snake-body color)])))

;; get-head-image : Symbol Color -> Image
;; Purpose: Returns the correct image for the snake head depending on direction
(define (get-head-image dir color)
  (cond [(symbol=? dir 'right) (rotate 270 (make-snake-head color))]
        [(symbol=? dir 'down) (rotate 180 (make-snake-head color))]
        [(symbol=? dir 'left) (rotate 90 (make-snake-head color))]
        [(symbol=? dir 'up) (make-snake-head color)]
        [else (rotate 270 (make-snake-head color))]))

;; ======================
;; Grid Drawing
;; ======================
;; draw-vlines : Number Number Image -> Image
;; Purpose: Draws vertical grid lines recursively
(define (draw-vlines i max scene)
  (if (> i max)
      scene
      (draw-vlines
       (add1 i) max
       (add-line scene
                 (* i CELL-SIZE) 0
                 (* i CELL-SIZE) SCENE-HEIGHT
                 GRID-COLOR))))

;; draw-hlines : Number Number Image -> Image
;; Purpose: Draws horizontal grid lines recursively
(define (draw-hlines i max scene)
  (if (> i max)
      scene
      (draw-hlines
       (add1 i) max
       (add-line scene
                 0 (* i CELL-SIZE)
                 SCENE-WIDTH (* i CELL-SIZE)
                 GRID-COLOR))))

;; draw-grid : Image -> Image
;; Purpose: Draws the full grid over the given image
(define (draw-grid scene)
  (let* ([vertical (draw-vlines 0 CELL-NUM-WIDTH scene)]
         [horizontal (draw-hlines 0 CELL-NUM-HEIGHT vertical)])
    horizontal))



;; ======================
;; Rendering Snake Game
;; ======================


;; cell-center : Number -> Number
;; Purpose: Given a grid cell index, returns the pixel coordinate of its center
(define (cell-center n)
  (+ (* (sub1 n) CELL-SIZE)
     (/ CELL-SIZE 2)))
(check-expect (cell-center 1) 10)
(check-expect (cell-center 3) 50)
(check-expect (cell-center 10) 190)


;; draw-obstacles : List<Posn> Number Image -> Image
;; Purpose: Draws all obstacles onto the scene. Obstacles that lie outside the central game area (defined by menu-size)
;;          are drawn as red barriers, while inner obstacles use the standard OBSTACLE graphic (black).
;; Termination argument: The function uses foldl, which iterates exactly once over every element in the input list `obstacles`.
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


;; draw-tail : Posn List<Posn> Image Color -> Image
;; Purpose: Recursively draws the snake's body (all segments except the head) onto the scene.
;;          It draws each segment using the correct image (straight or angled) based on the positions of the previous, current, and next segments.
;; Termination argument: The recursion stops in the first conditional branch when the list `tail` is empty. In the recursive call, `(rest tail)` is passed, strictly reducing the list size by one in each step.
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

;; draw-snake : Vector<Posn> Symbol Image Color -> Image
;; Purpose: Draws the entire snake onto the scene. It converts the snake position vector to a list,
;;          then draws the body segments (tail) using `draw-tail`, and finally places the snake's head
;;          (using the current direction `dir`) on top of the resulting scene.
;; Termination argument: The function contains a base case for an empty snake list. The recursive helper function `draw-tail`
;;                       is called with `(rest snake-list)`, ensuring termination.
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

;; draw-foods : Vector<Posn> Image -> Image
;; Purpose: Draws all food items (represented by the FRUIT image) onto the scene at their respective grid positions.
;; Termination argument: The function uses a local recursive loop (`loop`) that increments the counter `i` by 1 in each step,
;; stopping when $i$ reaches the total number of food items (`(vector-length foods)`).
(define (draw-foods foods scene)
  (let loop ([i 0] [s scene])
    (if (>= i (vector-length foods))
        s
        (let ([f (vector-ref foods i)])
          (loop (add1 i)
                (place-image FRUIT
                             (cell-center (posn-x f))
                             (cell-center (posn-y f))
                             s))))))

;; create-score-bar : Number Number Boolean -> Image
;; Purpose: Creates the top score bar GUI element, which includes the current score, the high score (record), and two buttons (Restart and Pause/Resume).
;; Termination argument: This function is non-recursive and terminates after a fixed number of image composition operations.
(define (create-score-bar score record paused?)
  (let* ([score-text  (text (string-append "Score: " (number->string score)) 18 "white")]
         [record-text (text (string-append "Record: " (number->string record)) 18 "yellow")]
         [button-spacing 10]
         
         [restart-button (overlay (text "Restart" 16 "black")
                                  (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "gray"))]
         
         [pause-button (overlay (text (if paused? "Resume" "Pause") 16 "black")
                                (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "gray"))]
         
         [score-area (rectangle SCENE-WIDTH TOP-BORDER-SIZE "solid" "black")]
         [center-x (/ SCENE-WIDTH 2)]
         
         [restart-x (- center-x (/ BUTTON-WIDTH 2) (/ button-spacing 2))]
         [pause-x (+ center-x (/ BUTTON-WIDTH 2) (/ button-spacing 2))]
         
         [buttons-scene (place-image restart-button restart-x (/ TOP-BORDER-SIZE 2)
                            (place-image pause-button pause-x (/ TOP-BORDER-SIZE 2) score-area))]
         
         [score-x (+ (/ BUTTON-WIDTH 2) 10)]
         [record-x (- SCENE-WIDTH (/ BUTTON-WIDTH 2) 10)])
         
    (place-image score-text score-x (/ TOP-BORDER-SIZE 2)
      (place-image record-text record-x (/ TOP-BORDER-SIZE 2) buttons-scene))))



;; draw-pause-button : Boolean Image -> Image
;; Purpose: Creates and places the Pause/Resume button in the score bar area of the game scene. The button's label changes dynamically based on the current `paused?` state.
;; Termination argument: This function is non-recursive and terminates after a fixed number of image composition operations.
(define (draw-pause-button paused? scene)
  (let* ([button-bg (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "gray")]
         [label (if paused? "Resume" "Pause")]
         [button-label (text label 16 "black")]
         [button (overlay button-label button-bg)]
         [x (+ (/ SCENE-WIDTH 2) BUTTON-WIDTH 1)]
         [y (/ TOP-BORDER-SIZE 2)])
    (place-image button x y scene)))


;; render-game : World -> Image
;; Purpose: Renders the complete game visual state. It layers the grid, obstacles, snake, and food onto the game board, creates the top score bar, and assembles the final scene with borders.
;; Termination argument: This function is non-recursive and executes a fixed sequence of image composition calls.
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


;; render-menu : Menu -> Image
;; Purpose: Renders the main menu screen, displaying the game title, control instructions, and configurable settings (Speed, Mode, Size, Color, Fruits).
;; It highlights the currently selected option (indicated by `menu-selector`) with a ">>" prefix and yellow text color.
;; Termination argument: This function is non-recursive and executes a fixed sequence of image composition operations.
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


;; next-in-list : List<X> X -> X
;; Purpose: Finds the element `x` in the list `lst` and returns the element immediately following it. 
;;          If `x` is the last element in the list, it returns `x` (clamping the value).
;; Termination argument: The function recursively calls itself with `(rest lst)`,
;; strictly reducing the length of the list by 1 at each step. Since the list is finite, it eventually becomes empty, triggering the base case.
(define (next-in-list lst x)
  (cond
    [(empty? lst) x]
    [(equal? (first lst) x)
     (if (empty? (rest lst))
         (first lst)
         (first (rest lst)))]
    [else
     (next-in-list (rest lst) x)]))
(check-expect (next-in-list '() 5) 5)
(check-expect (next-in-list '(1 2 3) 2) 3)
(check-expect (next-in-list '(1 2 3) 3) 3)
(check-expect (next-in-list '(4 5 6) 7) 7)
(check-expect (next-in-list '(1 2 2 3) 2) 2)



(define (prev-in-list lst x)
  (letrec ((helper
            (lambda (prev remaining full)
              (cond
                [(empty? remaining) #f]
                [(equal? (first remaining) x)
                 (if (not (eq? prev #f))
                     prev
                     (let loop ((l full))
                       (if (empty? (rest l))
                           (first l)
                           (loop (rest l)))))]
                [else
                 (helper (first remaining) (rest remaining) full)]))))
    (helper #f lst lst)))
(check-expect (prev-in-list '() 5) #f)
(check-expect (prev-in-list '(1 2 3) 2) 1)
(check-expect (prev-in-list '(1 2 3) 1) 3)
(check-expect (prev-in-list '(4 5 6) 7) #f)
(check-expect (prev-in-list '(1 2 2 3) 2) 1)


;; speed-label : Number -> String
;; Purpose: Converts a given numeric speed value (delay in seconds) into its corresponding descriptive label string (e.g., "Slow", "Normal", "Fast").
;; Termination argument: The local recursive helper function calls itself with `(rest slist)` and `(rest llist)`,
;; strictly reducing the length of both lists by one in each step. Since `SPEEDS-LIST` is finite, the recursion will eventually terminate either when a match is found or when `slist` becomes empty, triggering the base case.
(define (speed-label speed)
  (letrec ((helper (lambda (slist llist)
                     (cond
                       [(empty? slist) "Normal"]
                       [(equal? (first slist) speed) (first llist)]
                       [else (helper (rest slist) (rest llist))]))))
    (helper SPEEDS-LIST SPEED-LABELS)))


;; menu-key : Menu String -> Menu
;; Purpose: Handles keyboard input specifically when the game is in the main configuration menu. It allows the user to navigate between and modify the game settings.
;;          - Keys "W" or "w": Moves the selector (highlight) up to the previous option in the menu (e.g., from 'mode to 'speed).
;;          - Keys "S" or "s": Moves the selector down to the next option in the menu (e.g., from 'speed to 'mode).
;;          - Keys "A" or "a": Decrements or cycles backward through the options for the currently selected setting (e.g., decreases speed, changes mode to previous option).
;;          - Keys "D" or "d": Increments or cycles forward through the options for the currently selected setting (e.g., increases speed, changes mode to next option).
;;          - Other Keys: Returns the menu state unchanged.
(define (menu-key m key)
  (let ([selectors '(speed mode size color num-fruits)]
        [sel (menu-selector m)])
    
    (cond
      [(member key '("w" "W"))
       (make-menu (menu-speed m)
                  (menu-mode m)
                  (menu-color m)
                  (menu-size m)
                  (prev-in-list selectors sel)
                  (menu-num-fruits m))]

      [(member key '("s" "S"))
       (make-menu (menu-speed m)
                  (menu-mode m)
                  (menu-color m)
                  (menu-size m)
                  (next-in-list selectors sel)
                  (menu-num-fruits m))]

      
  [(or (string=? key "a") (string=? key "A"))
   (cond
     [(eq? sel 'speed)      (make-menu (prev-in-list SPEEDS-LIST (menu-speed m)) (menu-mode m) (menu-color m) (menu-size m) 'speed (menu-num-fruits m))]
     [(eq? sel 'mode)       (make-menu (menu-speed m) (prev-in-list MODE-OPTIONS (menu-mode m)) (menu-color m) (menu-size m) 'mode (menu-num-fruits m))]
     [(eq? sel 'size)       (make-menu (menu-speed m) (menu-mode m) (menu-color m) (prev-in-list SIZE-OPTIONS (menu-size m)) 'size (menu-num-fruits m))]
     [(eq? sel 'color)      (make-menu (menu-speed m) (menu-mode m) (prev-in-list COLOR-OPTIONS (menu-color m)) (menu-size m) 'color (menu-num-fruits m))]
     [(eq? sel 'num-fruits) (make-menu (menu-speed m) (menu-mode m) (menu-color m) (menu-size m) 'num-fruits (if (= (menu-num-fruits m) 1) 5 (sub1 (menu-num-fruits m))))])]
  
  [(or (string=? key "d") (string=? key "D"))
   (cond
     [(eq? sel 'speed)      (make-menu (next-in-list SPEEDS-LIST (menu-speed m)) (menu-mode m) (menu-color m) (menu-size m) 'speed (menu-num-fruits m))]
     [(eq? sel 'mode)       (make-menu (menu-speed m) (next-in-list MODE-OPTIONS (menu-mode m)) (menu-color m) (menu-size m) 'mode (menu-num-fruits m))]
     [(eq? sel 'size)       (make-menu (menu-speed m) (menu-mode m) (menu-color m) (next-in-list SIZE-OPTIONS (menu-size m)) 'size (menu-num-fruits m))]
     [(eq? sel 'color)      (make-menu (menu-speed m) (menu-mode m) (next-in-list COLOR-OPTIONS (menu-color m)) (menu-size m) 'color (menu-num-fruits m))]
     [(eq? sel 'num-fruits) (make-menu (menu-speed m) (menu-mode m) (menu-color m) (menu-size m) 'num-fruits (if (= (menu-num-fruits m) 5) 1 (add1 (menu-num-fruits m))))])]
  
  [else m])
))





;; menu-key : Menu String -> Menu
;; Purpose: Handles keyboard input when the game is in the 'menu mode. It updates the menu state (`Menu` structure) by changing the currently selected option (using 'w'/'s')
;;          or modifying the value of the selected option (using 'a'/'d').
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
            'up
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
            'down
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
            'left
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
            'right
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


;; active-grid-bounds : Number -> List<Number>
;; Purpose: Calculates the grid boundaries (min X, max X, min Y, max Y) for the active, central play area based on the desired `size` of the inner square grid.
;;          The resulting active area is centered within the total grid defined by CELL-NUM-WIDTH/HEIGHT.
(define (active-grid-bounds size)
  (let* ([offset (quotient (- CELL-NUM-WIDTH size) 2)]
         [min-cell (+ offset 1)]
         [max-cell (- CELL-NUM-WIDTH offset)])
    (list min-cell max-cell min-cell max-cell)))
(check-expect (active-grid-bounds 24) (list 1 24 1 24))
(check-expect (active-grid-bounds 1)  (list 12 13 12 13))
(check-expect (active-grid-bounds 10) (list 8 17 8 17))



;; wrap-head : Posn Number -> Posn
;; Purpose: Takes a potential snake head position (`head`) and a game grid `size`, and applies wrap-around logic.
;;          If the head moves outside the active playing area boundaries (determined by `active-grid-bounds`), it is repositioned to the corresponding cell on the opposite side of the active area.
;; Termination argument: This function is non-recursive and terminates after a fixed set of coordinate checks and arithmetic operations.
(define (wrap-head head size)
  (local 
    ((define (extract-bounds lst)
       (cond
         [(empty? lst) (error "Bounds list too short")]
         [else
          (let ([min-x (first lst)]
                [rest1 (rest lst)])
            (if (empty? rest1) (error "Bounds list too short")
                (let ([max-x (first rest1)]
                      [rest2 (rest rest1)])
                  (if (empty? rest2) (error "Bounds list too short")
                      (let ([min-y (first rest2)]
                            [rest3 (rest rest2)])
                        (if (empty? rest3) (error "Bounds list too short")
                            (let ([max-y (first rest3)])
                              (list min-x max-x min-y max-y))))))))])))
    (let* ([bounds (extract-bounds (active-grid-bounds size))]
           [min-x (first bounds)]
           [max-x (second bounds)]
           [min-y (third bounds)]
           [max-y (fourth bounds)]
           [x (posn-x head)]
           [y (posn-y head)])
      (make-posn
       (cond [(< x min-x) max-x]
             [(> x max-x) min-x]
             [else x])
       (cond [(< y min-y) max-y]
             [(> y max-y) min-y]
             [else y])))))
(check-expect (wrap-head (make-posn 1 1) 24) (make-posn 1 1))
(check-expect (wrap-head (make-posn 0 0) 24) (make-posn 24 24))
(check-expect (wrap-head (make-posn 25 25) 24) (make-posn 1 1))
(check-expect (wrap-head (make-posn 12 5) 10) (make-posn 12 17))
(check-expect (wrap-head (make-posn 7 17) 10) (make-posn 17 17))



;; valid-position? : Posn Vector<Posn> Posn List<Posn> -> Boolean
;; Purpose: Checks if a given position `p` is a valid and unoccupied cell within the game grid boundaries. 
;;          A position is valid if it is within the grid (1 to CELL-NUM-WIDTH/HEIGHT, inclusive) and does not coincide with any existing obstacles, the single food item, or any segment of the snake's body.
;; Termination argument: The function contains a local recursive loop (`loop`) that iterates over the `snake` vector.
;; The counter `i` starts at 0 and increments by 1 in each recursive call, ensuring termination when $i$ reaches the length of the vector.
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
(check-expect (valid-position? (make-posn 5 5) (vector) (make-posn 10 10) '()) #t)
(check-expect (valid-position? (make-posn 0 5) (vector) (make-posn 10 10) '()) #f)
(check-expect (valid-position? (make-posn 25 5) (vector) (make-posn 10 10) '()) #f)
(check-expect (valid-position? (make-posn 5 5) (vector (make-posn 5 5)) (make-posn 10 10) '()) #f)
(check-expect (valid-position? (make-posn 5 5) (vector) (make-posn 5 5) '()) #f)
(check-expect (valid-position? (make-posn 5 5) (vector) (make-posn 10 10) (list (make-posn 5 5))) #f)


;; free-neighbors : Posn Vector<Posn> List<Posn> -> Number
;; Purpose: Calculates the number of adjacent (north, south, east, west) cells around a given position `p` that are currently free. 
;;          A cell is considered free if it is within the grid bounds, is not an obstacle, and is not occupied by the snake's body (excluding the snake's head itself).
;; Termination argument: This function is non-recursive. It iterates exactly four times using `foldl` over the fixed list `neighbors` (containing 4 elements), ensuring termination.
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
                      (not (equal? n (vector-ref snake 0))))
                 (+ acc 1)
                 acc))
           0 neighbors)))
(check-expect (free-neighbors (make-posn 5 5) (vector (make-posn 0 0)) '()) 4)
(check-expect (free-neighbors (make-posn 1 1) (vector (make-posn 0 0)) '()) 2)
(check-expect (free-neighbors (make-posn 24 24) (vector (make-posn 0 0)) '()) 2)
(check-expect (free-neighbors (make-posn 5 5) (vector (make-posn 6 5)) '()) 3)
(check-expect (free-neighbors (make-posn 5 5) (vector (make-posn 0 0)) (list (make-posn 5 6) (make-posn 4 5))) 2)


;; safe-to-place? : Posn List<Posn> Vector<Posn> Posn -> Boolean
;; Purpose: Determines if a food item can be placed safely at position `p`. This check ensures:
;;          1. `p` is within grid bounds.
;;          2. `p` is not an obstacle.
;;          3. `p` is not occupied by the snake (body or head).
;;          4. `p` is not directly in front of the snake's head (preventing immediate food collection).
;;          5. Placing food at `p` does not immediately isolate any neighboring cells, ensuring the snake can still navigate and avoiding trivial win states (Dead-End Detection using `free-neighbors`).
;; Termination argument: The function contains a local recursive loop (`loop`) that iterates over the `snake` vector, strictly reducing the index `i` towards the vector's length.
;; The second check uses `foldl`, which iterates over a fixed 4-element list of neighbors, guaranteeing termination.
(define (safe-to-place? p obstacles snake head)
  (let ([front-cells (list (make-posn (+ 1 (posn-x head)) (posn-y head))
                           (make-posn (+ 2 (posn-x head)) (posn-y head)))])
    (and (>= (posn-x p) 1) (<= (posn-x p) CELL-NUM-WIDTH)
         (>= (posn-y p) 1) (<= (posn-y p) CELL-NUM-HEIGHT)
         (not (member p obstacles))
         (not (member p front-cells))
         (let loop ([i 0])
           (if (>= i (vector-length snake))
               #t
               (if (equal? (vector-ref snake i) p)
                   #f
                   (loop (add1 i)))))
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
(check-expect (safe-to-place? (make-posn 5 5) '() (vector (make-posn 1 1)) (make-posn 1 1)) #t)
(check-expect (safe-to-place? (make-posn 0 0) '() (vector (make-posn 1 1)) (make-posn 1 1)) #f)
(check-expect (safe-to-place? (make-posn 25 25) '() (vector (make-posn 1 1)) (make-posn 1 1)) #f)
(check-expect (safe-to-place? (make-posn 2 1) '() (vector (make-posn 1 1)) (make-posn 1 1)) #f)
(check-expect (safe-to-place? (make-posn 5 5) (list (make-posn 5 6)) (vector (make-posn 1 1)) (make-posn 1 1)) #t)


;; check-obstacle-conditions : List<Posn> Vector<Posn> Posn -> Boolean
;; Purpose: Iterates through every cell in the grid (from (1, 1) to (CELL-NUM-WIDTH, CELL-NUM-HEIGHT)) to ensure that the current placement of obstacles does not create an immediate dead-end for the snake.
;;          A cell that is not an obstacle must have at least two free neighbors (excluding the snake's head). If any non-obstacle cell fails this check, the function returns #f.
;; Termination argument: The function uses a nested recursive loop structure (`loop` with variables `x` and `y`) that iterates systematically through every cell in the W x H grid.
;; Since W and H are fixed constants, the loop will eventually complete when y exceeds CELL-NUM-HEIGHT, guaranteeing termination.
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
(check-expect (check-obstacle-conditions '() (vector (make-posn 0 0)) (make-posn 5 5)) #t)
(check-expect (check-obstacle-conditions (list (make-posn 1 1)) (vector (make-posn 0 0)) (make-posn 5 5)) #t)
(check-expect (check-obstacle-conditions (list (make-posn 1 1) (make-posn 1 2) (make-posn 2 1) (make-posn 2 2)) (vector (make-posn 0 0)) (make-posn 5 5)) #t)
(check-expect (check-obstacle-conditions '() (vector (make-posn 1 1) (make-posn 2 2)) (make-posn 5 5)) #t)
(check-expect (check-obstacle-conditions (list (make-posn 12 12)) (vector (make-posn 0 0)) (make-posn 5 5)) #t)



;; all-reachable? : Vector<Posn> Posn List<Posn> -> Boolean
;; Purpose: Checks for **board segmentation** by determining if every non-obstacle cell in the grid is reachable from the snake's current head position. This is achieved by running a Breadth-First Search (BFS) starting at the snake's head.
;;          If, after the search completes, any non-obstacle cell remains unvisited, it means the board is split into isolated sections, and the function returns #f.
;; Termination argument: The function uses two recursive loops:
;;          1. `loop-bfs`: This is a classic BFS algorithm. Since the number of nodes (grid cells) is finite, the search must eventually visit all reachable nodes, and the queue will become empty, guaranteeing termination.
;;          2. `loop-check`: A nested recursive loop that iterates systematically through every cell in the W x H grid. Since W and H are fixed constants, this loop is guaranteed to terminate when y exceeds height.
(define (all-reachable? snake food obstacles)
  (let ((width CELL-NUM-WIDTH)
        (height CELL-NUM-HEIGHT)
        (start (vector-ref snake 0)))

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

      (let loop-bfs ((queue (list start)) (visited (list start)))
        (if (empty? queue)
            (let loop-check ((x 1) (y 1))
              (cond
                [(> y height) #t]
                [else
                 (let ((pos (make-posn x y)))
                   (cond
                     [(member pos obstacles)
                      (if (< x width) (loop-check (+ x 1) y) (loop-check 1 (+ y 1)))]
                     [(not (member pos visited)) #f]
                     [else (if (< x width) (loop-check (+ x 1) y) (loop-check 1 (+ y 1)))]))]))
            (let* ((current (first queue))
                   (rest-queue (rest queue))
                   (new-neighbors (filter (lambda (p) (not (member p visited)))
                                          (neighbors current)))
                   (new-visited (append visited new-neighbors)))
              (loop-bfs (append rest-queue new-neighbors) new-visited)))))))
(check-expect (all-reachable? (vector (make-posn 1 1)) (make-posn 5 5) '()) #t)
(check-expect (all-reachable? (vector (make-posn 12 12)) (make-posn 24 24) '()) #t)
(check-expect (all-reachable? (vector (make-posn 1 1)) (make-posn 5 5) (list (make-posn 2 1) (make-posn 1 2))) #f)
(check-expect (all-reachable? (vector (make-posn 5 5)) (make-posn 10 10) (list (make-posn 1 1) (make-posn 1 2))) #t)


;; generate-obstacles-safe : Vector<Posn> Posn -> List<Posn>
;; Purpose: Generates a list of obstacle positions ensuring that the placement is safe and does not immediately partition the game board or block the snake's path.
;;          It calculates a target number of obstacles, then repeatedly attempts to place them at random positions using the safe-to-place? check.
;; If the final set of obstacles makes the board unsolvable (checked by all-reachable?), the function restarts the generation process entirely.
;; Termination argument: This function is doubly recursive:
;;          1. Outer Recursion (Base Case Failure): If the final set of generated obstacles fails the all-reachable? test (meaning the board is partitioned),
;;             the function calls itself recursively (generate-obstacles-safe snake food), restarting the process. This is a probabilistic recursion that assumes a valid configuration will eventually be found.
;;          2. Inner Recursion (Loop): The inner named loop (loop) attempts to place the required number of obstacles (count). Since count is strictly decreased only upon successful placement, the inner loop continues until count reaches 0.
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



;; generate-outer-obstacles : Number -> List<Posn>
;; Purpose: Generates a list of positions representing "outer" obstacles or walls that border the central active game area. The size of the active area is determined by the input parameter `size`. 
;;          The function iterates through every cell in the entire grid (CELL-NUM-WIDTH by CELL-NUM-HEIGHT) and collects only those cells that fall outside the defined inner playing boundaries.
;; Termination argument: The function uses a nested recursive loop structure (`loop` with variables `x` and `y`) that iterates systematically through every cell in the W x H grid.
;; Since W and H are fixed constants, the loop is guaranteed to terminate when y exceeds CELL-NUM-HEIGHT.
(define (generate-outer-obstacles size)
  (let* ([offset (quotient (- CELL-NUM-WIDTH size) 2)]
         [start (+ offset 1)]
         [end (- CELL-NUM-WIDTH offset)])
    (let loop ([x 1] [y 1] [acc '()])
      (cond [(> y CELL-NUM-HEIGHT) acc]
            [(> x CELL-NUM-WIDTH) (loop 1 (add1 y) acc)]
            [(or (< x start) (> x end) (< y start) (> y end)) (loop (add1 x) y (cons (make-posn x y) acc))]
            [else (loop (add1 x) y acc)]))))


;; handle-key-unified : World String -> World
;; Purpose: Acts as the main key event dispatcher, deciding how to handle a keyboard press based on the current mode of the game (menu or game).
;;          - In 'menu mode: If the key is "spacebar", it initializes a new game based on current menu settings (calculating obstacles, foods, and boundaries) and switches the mode to 'game.
;;            Otherwise, it updates the menu settings using `menu-key`.
;;          - In 'game mode: If the game is over (win or loss) and the key is "spacebar", it resets the world state to 'menu mode. If the key is 'l' (for 'lose' or 'leak' in some contexts, or simply a debugging tool),
;;            it forces a loss condition. Otherwise, it delegates control to the game-specific key handler, `handle-key-game`.
(define (handle-key-unified w key)
  (cond
    [(symbol=? (world-mode w) 'menu)
     (cond
       [(key=? key " ")
  (let* ([menu-updated (world-menu w)]
         [num-fruits   (menu-num-fruits menu-updated)]
         [foods        (random-foods initial-snake '() num-fruits)]
         [obs          (if (eq? (menu-mode menu-updated) 'Obstacles)
                           (generate-obstacles-safe initial-snake foods)
                           '())]
         [size         (menu-size menu-updated)]
         [outer        (if (< size 25) (generate-outer-obstacles size) '())]
         [total-obstacles (append obs outer)]
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

    [(symbol=? (world-mode w) 'game)
     (cond
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

       [else
        (handle-key-game w key)])]))




;; ====================== ;; Tick Handler ;; ======================


;; obstacle-collision? : Posn List<Posn> -> Boolean
;; Purpose: Checks if the snake's head position (`head`) is currently overlapping with any of the defined grid obstacles (`obstacles`). Returns #t if a collision occurs, and #f otherwise.
(define (obstacle-collision? head obstacles)
  (member head obstacles))



;; replace-food-at-index : Vector<Posn> Number Posn -> Vector<Posn>
;; Purpose: Creates and returns a new food vector where the element at the specified index (`idx`) has been replaced by `new-food`, while all other elements remain unchanged.
;;          The function explicitly uses `vector-set!` to build the new vector in place, and then returns the complete vector.
;; Termination argument: The function uses a local recursive loop (`loop`) where the counter `i` starts at 0 and increments by 1 in each step.
;; The loop terminates when `i` reaches the total length of the vector (`n`), ensuring all elements are processed exactly once.
(define (replace-food-at-index foods idx new-food)
  (let* ([n (vector-length foods)]
         [result (make-vector n)])
    (let loop ([i 0])
      (if (< i n)
          (begin
            (vector-set! result i (if (= i idx) new-food (vector-ref foods i)))
            (loop (add1 i)))
          result))))
(check-expect (replace-food-at-index (vector (make-posn 1 1) (make-posn 2 2)) 0 (make-posn 3 3)) (vector (make-posn 3 3) (make-posn 2 2)))
(check-expect (replace-food-at-index (vector (make-posn 1 1) (make-posn 2 2)) 1 (make-posn 3 3)) (vector (make-posn 1 1) (make-posn 3 3)))
(check-expect (replace-food-at-index (vector (make-posn 1 1) (make-posn 2 2) (make-posn 4 4)) 2 (make-posn 5 5)) (vector (make-posn 1 1) (make-posn 2 2) (make-posn 5 5)))
(check-expect (replace-food-at-index (vector (make-posn 1 1)) 0 (make-posn 9 9)) (vector (make-posn 9 9)))
(check-expect (replace-food-at-index (vector (make-posn 1 1) (make-posn 2 2)) 5 (make-posn 3 3)) (vector (make-posn 1 1) (make-posn 2 2)))






;; vector-index : (Any -> Boolean) Vector<Any> -> (or/c Number #f)
;; Purpose: Searches for the first element in the vector `vec` that satisfies the given predicate function `pred`.
;;          It returns the index (position) of the first matching element, or #f if no element satisfies the predicate.
;; Termination argument: The function uses a local recursive loop (`loop`) where the counter `i` starts at 0 and increments by 1 in each step.
;; The loop terminates when `i` reaches the total length of the vector (`(vector-length vec)`), ensuring all elements are checked exactly once.
(define (vector-index pred vec)
  (let loop ([i 0])
    (cond
      [(>= i (vector-length vec)) #f]
      [(pred (vector-ref vec i)) i]
      [else (loop (add1 i))])))
(check-expect (vector-index (lambda (x) (= x 3)) (vector 1 2 3 4)) 2)
(check-expect (vector-index (lambda (x) (= x 5)) (vector 1 2 3 4)) #f)
(check-expect (vector-index (lambda (x) (even? x)) (vector 1 3 4 5)) 2)
(check-expect (vector-index (lambda (x) (odd? x)) (vector 2 4 6 7)) 3)



;; =========================
;; Update game function
;; =========================


;; update-game : World -> World
;; Purpose: The main tick function for the game. It advances the game state based on the configured speed.
;;          1. Game Over Check: If the game is already over ('win' or #t), it returns the current world unchanged.
;;          2. Pause Check: If the snake's direction is 'none' (paused), it returns the current world unchanged.
;;          3. Speed Control (Tick Counting): It uses a counter (`world-tick-counter`) and a speed threshold derived from `menu-speed` to determine if a full game step should occur. If the counter is below the threshold, it just increments the counter and returns the world.
;;          4. Movement: If the threshold is met, it calculates the `new-head` position, applying wrap-around logic (`wrap-head`) if the mode is 'Pacman'.
;;          5. Food Consumption: It checks for food collision (`ate-index`). If food is eaten, it generates a new food item, increases the score, updates the record, grows the snake, and checks for a win condition (snake fills all free spaces).
;;          6. Collision Check: If no food is eaten, it checks for collisions: wall (if not Pacman mode), self-collision, or obstacle collision. If any collision occurs, it sets the `world-game-over?` flag to #t.
;;          7. Normal Move: If no collision and no food is eaten, it moves the snake by dropping the tail, keeping the length the same.
(define (update-game w)
  (cond
    [(or (eq? (world-game-over? w) #t)
         (eq? (world-game-over? w) 'win))
     w]

    [(symbol=? (world-dir w) 'none)
     w]

    [else
     (let* ([speed     (menu-speed (world-menu w))]
            [counter   (world-tick-counter w)]
            [threshold (inexact->exact (round (/ speed 0.02)))])
       
       (if (< counter threshold)
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
           
           (let* ([snake      (world-snake w)]
                  [dir        (world-dir w)]
                  [foods      (world-foods w)]
                  [score      (world-score w)]
                  [record     (world-record w)]
                  [head       (vector-ref snake 0)]
                  [new-head (if (eq? (menu-mode (world-menu w)) 'Pacman)
              (wrap-head (move-head head dir)
                         (menu-size (world-menu w)))
              (move-head head dir))]

                  [snake-list (vector->list snake)]
                  [new-snake  (cons new-head snake-list)]
                  [ate-index (vector-index (lambda (f) (equal? new-head f)) foods)])
             
             (if (not (eq? ate-index #f))
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
    (let ([shrunk (reverse (rest (reverse new-snake)))])
      (if (or (and (not (eq? (menu-mode (world-menu w)) 'Pacman))
                   (wall-collision? new-head))
              (self-collision? new-head (list->vector shrunk))
              (obstacle-collision? new-head (world-obstacles w))
              (not (eq? (vector-index (lambda (f) (equal? f new-head)) foods) #f)))
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



;; update-unified : World -> World
;; Purpose: Acts as the top-level timer function for the entire game application. It decides whether to advance the game state or return the world unchanged based on the current mode and pause state.
;;          - If the world is in 'menu mode or if the game is explicitly paused (world-paused? is true), it returns the world structure w without modifying it.
;;          - Otherwise (if in 'game mode and not paused), it calls the core logic function, update-game, to advance the snake's position, handle collisions, and update scores.
(define (update-unified w)
  (if (or (symbol=? (world-mode w) 'menu)
          (world-paused? w))
      w
      (update-game w)))


;; ====================== ;; Mouse Handler ;; ======================

;; handle-mouse-unified : World Number Number String -> World
;; Purpose: Handles mouse click events within the game world, specifically checking for clicks on control buttons located in the game's display area.
;;          1. Restart Button Check: If the world is in 'game mode and the mouse event is a "button-down" click within the calculated bounds of the **Restart button** (left of center), it resets the game state to 'menu mode with initial settings.
;;          2. Pause Button Check: If the world is in 'game mode and the mouse event is a "button-down" click within the calculated bounds of the **Pause button** (right of center), it toggles the `world-paused?` flag, either pausing or unpausing the game.
;;          3. No Action: For all other world modes or clicks outside these button areas, it returns the world state unchanged.
;; Termination argument: This function is non-recursive and terminates after a fixed series of conditional checks and structure creation operations. The coordinate and boundary calculations are direct arithmetic operations.
(define (handle-mouse-unified w x y event)
  (cond
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



;; render-game-over : World -> Image
;; Purpose: Creates and displays the "Game Over" screen image when the snake game ends (either by loss or a win, though 'win' state typically uses a similar but distinct rendering).
;;          It draws text elements on top of a black background, centered horizontally. The displayed information includes:
;;          1. The "GAME OVER" message.
;;          2. The final score achieved in the completed game (world-score w).
;;          3. The highest score ever achieved (world-record w).
;;          4. Instructions to press SPACE to navigate back to the menu.
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

;; render-win : World -> Image
;; Purpose: Creates and displays the "You Win!" screen image when the player successfully completes the game (the snake has consumed all available food and filled all free spaces on the board).
;;          It draws text elements on top of a black background, centered horizontally. The displayed information includes:
;;          1. The large "YOU WIN!" message in green.
;;          2. The final score achieved (world-score w).
;;          3. Instructions to press SPACE to navigate back to the menu.
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



;; render-unified : World -> Image
;; Purpose: The main image rendering dispatcher for the entire game application.
;; It determines which screen to display based on the current state of the game (the `world-mode` and `world-game-over?` fields of the World structure `w`).
;;          1. Menu Mode: If the mode is 'menu, it calls `render-menu` to draw the configuration screen.
;;          2. Game Over States: If the game is over, it checks the specific outcome:
;;             - If `world-game-over?` is 'win, it calls `render-win`.
;;             - Otherwise (if `world-game-over?` is true, indicating a loss), it calls `render-game-over`.
;;          3. Active Game Mode: If the mode is 'game and the game is not over, it calls `render-game` to draw the active gameplay screen (snake, food, obstacles, score).
(define (render-unified w)
  (cond
    [(symbol=? (world-mode w) 'menu)
     (render-menu (world-menu w))]

    [(or (eq? (world-game-over? w) #t)
         (eq? (world-game-over? w) 'win))
     (cond
       [(eq? (world-game-over? w) 'win)
        (render-win w)]
       [else
        (render-game-over w)])]
    
    [(symbol=? (world-mode w) 'game)
     (render-game w)]))




;; initial-world : World
;; Purpose: Defines the initial state of the entire game world structure when the program starts.
;;          It sets the game mode to **'menu** (indicating the user starts at the configuration screen). All gameplay-related fields (snake position, direction, foods, score, etc.) are set to their starting or default values, and the game is initially not paused.
;; Fields initialized:
;; - world-mode: 'menu (Starting state)
;; - world-menu: initial-menu (Default menu settings)
;; - world-snake: initial-snake (Starting snake position)
;; - world-dir: initial-dir (Starting direction, likely 'right or 'none)
;; - world-foods: initial-foods (Starting food vector)
;; - world-game-over?: #f (Not game over)
;; - world-score: initial-score (Starting score, likely 0)
;; - world-record: initial-record (Highest score achieved so far)
;; - world-tick-counter: 0 (Game clock reset)
;; - world-obstacles: '() (No obstacles initially)
;; - world-free-spaces: (count-free-spaces '()) (Total available cells on the board)
;; - world-paused?: #f (Not paused)
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
              #f))


;; ====================== ;; Run Big-Bang ;; ======================


;; big-bang call
;; Purpose: Initiates the entire Snake game application using the big-bang function from the #lang htdp/universe library (or similar Racket world-building environment).
;;          This function starts the game loop with the defined `initial-world` state and connects all core event handlers:
;;          - [to-draw render-unified]: Specifies the function responsible for drawing the current state of the world (menu, game, game over, or win screen).
;;          - [on-tick update-unified 0.02]: Specifies the function called repeatedly at a fixed rate (every 0.02 seconds, or 50 times per second) to advance the game state.
;;          - [on-key handle-key-unified]: Specifies the function called whenever a keyboard key is pressed.
;;          - [on-mouse handle-mouse-unified]: Specifies the function called whenever a mouse button is clicked or moved.
(big-bang initial-world
  [to-draw render-unified]
  [on-tick update-unified 0.02]
  [on-key handle-key-unified]
  [on-mouse handle-mouse-unified])
