# README.md

## MILESTONE

### Snaket

**Group Members:**

-   Leonardo Aaron Ricetti
    
-   Daniele Porro
    

----------

## Implemented Features

-   Implemented full game loop using  `big-bang`  with  `2htdp/universe`.
    
-   Added menu screen for selecting game speed (UP/DOWN keys) and starting the game (SPACE).
    
-   Implemented snake movement with directional input and prevention of instant reversal.
    
-   Added rendering of:
    
    -   Grid, borders, and playfield background
        
    -   Snake head oriented by direction
        
    -   Straight and curved body segments using corner detection system (`turn?`,  `direction`,  `corner-image`)
        
    -   Fruit with graphical leaf
        
    -   Score bar with live score and session record
        
    -   Restart button displayed during Game Over
        
    
-   Implemented food generation in random valid positions avoiding the snake.
    
-   Added collision detection system:
    
    -   Wall collision
        
    -   Self-collision
        
-   Implemented score increase, snake growth, and record updating.
    
-   Added ability to restart the game via the button or starting new from the menu.
    
-   Introduced speed-based tick control using  `menu-speed`  and a tick counter.
    
-   Added mouse handler for clicking the Restart button.
    

----------

## Changes Since Previous Submission / Proposal

-   Completed rendering system, including head rotation and detailed body/turn graphics.
    
-   Implemented new tail rendering logic with correct corner placement.
    
-   Refactored  `draw-tail`,  `draw-snake`, and collision logic.
    
-   Added menu structure with customizable speed control.
    
-   Improved random food placement to never spawn on the snake.
    
-   Added full  `world`  structure with state fields (mode, menu, snake, dir, food, score, record, tick-counter).
    
-   Added complete unified handlers (`render-unified`,  `handle-key-unified`,  `update-unified`,  `handle-mouse-unified`).
    
-   Finalized Game Over behavior with overlay text and restart button.

-   Designed custom graphics for snake head, body, turns, and fruit using  `2htdp/image`.
    
-   Organized constants and clarified coordinate system for grid layout.

-   Changed list for vectors for better control as advised.
    

----------

## Notes / Known Issues

-   Only arrow-key controls implemented so far (W/A/S/D planned).
    
-   Scaling and accessibility not implemented (fixed grid and cell size).
    
-   **Future Addition:**  
    We plan to extend the game into  **multiple minigames**, each with  **its own world state**  rather than packing everything into a single data type. This allows obstacles or unique mechanics per minigame.
    
-   **Future Addition:**  
    Code should support  **easily changing the canvas layout**, such as number of rows or grid size. 
    