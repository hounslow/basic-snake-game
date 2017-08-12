;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)



;; =================
;; =================
;; Constants:

(define BOARD-SIZE 10)   ; The board is 10 cells by 10 cells

(define CELL-PIXELS 14)                         ; cells are square
(define BOARD-WIDTH (* BOARD-SIZE CELL-PIXELS)) ;
(define BOARD-HEIGHT BOARD-WIDTH)               ;

;;
;; Images for the head and body elements of the snake
;; as well as for the food.
;; (NOTE: in the first version you only use HEAD.)

(define HEAD (circle (/ CELL-PIXELS 2) "solid" "green"))
(define BODY (circle (/ CELL-PIXELS 2) "solid" "red"))
(define FOOD (circle (/ CELL-PIXELS 2) "solid" "blue"))


(define MTS (empty-scene BOARD-WIDTH BOARD-HEIGHT))


;; =================
;; =================
;; Data Definitions:

;; Direction is one of: 
;;  - "U"
;;  - "D"
;;  - "L"
;;  - "R"
;; interp. the four directions a snake could travel

(define D1 "U")
(define D2 "D")
(define D3 "L")
(define D4 "R")

(define (fn-for-direction d)
  (cond [(string=? "U" d)(...)]
        [(string=? "D" d)(...)]
        [(string=? "L" d)(...)]
        [(string=? "R" d)(...)]))

;; Template rules used:
;; one of: 4 cases 
;; atomic distinct: "U"
;; atomic distinct: "D"
;; atomic distinct: "L"
;; atomic distinct: "R"

(define-struct cell (c r))   ; c and r stand for column and row
;; Cell is  (make-cell Integer[-1, BOARD-SIZE] Integer[-1, BOARD-SIZE])
;; interp. a cell position on the board from top-left corner
;;         -1 and BOARD-SIZE are on the edges of the board and indicate
;;         "going out of bounds"/game-over condition

(define C0 (make-cell 0 0))
(define C1 (make-cell 1 3))

(define (fn-for-cell x)
  (... (cell-c x)
       (cell-r x)))

;; Template rules used:
;; compound: 2 fields


;; Body is one of:
;;  - (cons Cell empty)
;;  - (cons Cell Body)

(define B1 (cons (make-cell 0 0) empty))
(define B2 (cons (make-cell 10 10 )(cons (make-cell 15 15) empty)))

#;
(define (fn-for-body b)
  (cons [(empty? (rest b)) (...(fn-for-cell (first b)))]
        [else
         (... (fn-for-cell (first b)
                           (fn-for-body (rest b))))]))

(define-struct snake (dir head body)) 
;; Snake is (make-snake Direction Cell Body)
;; interp. a snake with a head and body moving in some direction

(define S1 (make-snake D1 C0 B1))
(define S2 (make-snake D2 C1 B2))

(define (fn-for-snake s)
  (...(fn-for-direction (snake-dir s))
      (fn-for-cell (snake-head s))
      (fn-for-body (snake-body s))))

;; Template rules used:
;; compound: 2 fields
;; atomic distinct: direction
;; atomic distcint: cell
;; atomic distinct: body

(define-struct game (snake))
;; Game is (make-game Snake) ; later on we will add fields to game
;; interp. the game state with the snake

(define G0 S1)
(define G1 S2)

(define (fn-for-game g)
  (...(fn-for-snake (game-snake g))))

;; Template rules used:
;; atomic distinct: snake

;; =================
;; =================
;; Functions:

;; WS -> WS
;; start the world with (main empty)
;; 
(define (main ws)
  (big-bang ws                      ; WS
            (on-tick   tock 0.5)    ; WS -> WS
            (to-draw   render-head) ; WS -> Image
            (on-key    handle-key)
            (stop-when hit-wall last-img))) ;WS KeyEvent -> WS

;; Game -> Game
;; Makes the Snake Move

(check-expect (tock (make-game (movingsnake (make-snake "D" (make-cell 0 0)(cons (make-cell 0 0) empty))))) 
              (make-game (movingsnake (make-snake "D" (make-cell 0 1)(cons (make-cell 0 0) empty)))))
(check-expect (tock (make-game (movingsnake (make-snake "L" (make-cell 5 5)(cons (make-cell 5 5) empty)))))
              (make-game (movingsnake (make-snake "L" (make-cell 4 5)(cons (make-cell 5 5) empty)))))
(check-expect (tock (make-game (movingsnake (make-snake "U" (make-cell 5 5)(cons (make-cell 5 4) empty)))))
              (make-game (movingsnake (make-snake "U" (make-cell 5 4)(cons (make-cell 5 5) empty)))))
(check-expect (tock (make-game (movingsnake (make-snake "R" (make-cell 0 0)(cons (make-cell 0 0) empty)))))
              (make-game (movingsnake (make-snake "R" (make-cell 1 0) (cons (make-cell 0 0) empty)))))



;(define (tock g) 0) ;stub

;<template taken from Game>

(define (tock g)
  (make-game (movingsnake (game-snake g))))

;;====================================================MOVINGSNAKE HELPER FUNCTION===================================================================

;; Snake -> Snake
;; Consumes a snake and produces the next snake based on the snake direction, snake-dir

(check-expect (movingsnake (make-snake "D" (make-cell 0 0)(cons (make-cell 1 0) empty))) 
              (make-snake "D" (make-cell 0 1)(cons (make-cell 0 0) empty)))
(check-expect (movingsnake (make-snake "U" (make-cell 5 5)(cons (make-cell 5 6) empty))) 
              (make-snake "U" (make-cell 5 4)(cons (make-cell 5 5) empty)))
(check-expect (movingsnake (make-snake "L" (make-cell 6 6)(cons (make-cell 7 6) empty))) 
              (make-snake "L" (make-cell 5 6)(cons (make-cell 6 6) empty)))
(check-expect (movingsnake (make-snake "R" (make-cell 1 6)(cons (make-cell 2 6) empty))) 
              (make-snake "R" (make-cell 2 6)(cons (make-cell 1 6) empty)))


;(define (movingsnake g) 0) ;stub

;<template from Snake>

(define (movingsnake g)
  (make-snake (snake-dir g)
              (advance-cell g)
              (movebody (snake-head g) (snake-body g))))

;;================================================================MOVEBODY HELPER FUNCTION===========================================================

;; Cell Body -> Body
;; Create new body with head (first body) and body (last body) removed.

(check-expect (movebody (make-cell 1 1) (cons (make-cell 1 2) empty)) 
              (cons (make-cell 1 1) empty))
(check-expect (movebody (make-cell 0 0) (cons (make-cell 0 1) (cons (make-cell 0 2) empty)))
              (cons (make-cell 0 0) (cons (make-cell 0 1) empty)))

(define (movebody hl b)
  (cons hl (nouveaubody b)))      ;body's position becomes the head's former position

;;==========================================================================================================

;; Body -> Body or empty
;; Consumes a body and cons all cells except for the last one

(check-expect (nouveaubody (list (make-cell 1 1)(make-cell 1 2)))
              (cons (make-cell 1 1) empty))
(check-expect (nouveaubody (list (make-cell 0 0) (make-cell 0 1) (make-cell 0 2)))
              (cons (make-cell 0 0) (cons (make-cell 0 1) empty)))

(define (nouveaubody b)
  (cond [(empty? (rest b)) empty]
        [else
         (cons (first b)
               (nouveaubody (rest b)))]))

;;==========================================================================================================

;; Cell -> Cell
;; Moves snake cell 

;;!!!!!!!!!!!!!!

(define (advance-cell g)
  (cond [(string=? "U" (snake-dir g))(move-up (snake-head g))]
        [(string=? "D" (snake-dir g))(move-down (snake-head g))]
        [(string=? "L" (snake-dir g))(move-left (snake-head g))]
        [(string=? "R" (snake-dir g))(move-right (snake-head g))]))


;;================================================================MOVINGSNAKE HELPER FUNCTION===========================================================

;; Cell -> Cell
;; Moves snake cell CELL-PIXEL to the right

(check-expect (move-right (make-cell 0 0))(make-cell 1 0))

;(define (move-right x) 0) ;stub

;<template taken from Cell>

(define (move-right x)
  (make-cell (+ 1 (cell-c x)) (cell-r x)))

;;==========================================================

;; Cell -> Cell
;; Moves snake cell CELL-PIXELS to the left

(check-expect (move-left (make-cell 1 0))(make-cell 0 0))

;(define (move-left x) 0) ;stub

;<template taken from Cell>

(define (move-left x)
  (make-cell (- (cell-c x) 1)(cell-r x)))

;;==========================================================

;; Cell -> Cell
;; Moves snake cell CELL-PIXELS to the up

(check-expect (move-up(make-cell 0 1)) (make-cell 0 0))

;(define (move-up x) 0) ;stub

;<template taken from Cell>

(define (move-up x)
  (make-cell (cell-c x)(- (cell-r x) 1)))

;;==========================================================

;; Cell -> Cell
;; Moves snake cell CELL-PIXELS to the down

(check-expect (move-down(make-cell 0 0))(make-cell 0 1))

;(define (move-down x) 0) ;stub

;<template taken from Cell>

(define (move-down x)
  (make-cell (cell-c x)(+ 1 (cell-r x))))

;;=================================================================================================================================================

;; Game -> Image
;; render an image of Snake from Game

(check-expect (render-head (make-game (make-snake "U" (make-cell 4 5) (cons (make-cell 4 6) empty))))
              (place-in-cell HEAD (make-cell 4 5) (place-in-cell BODY (make-cell 4 6) MTS)))

;(define (render-head 0) 0) ;stub

(define (render-head g)
  (render-snakeA (game-snake g)))


;; Snake -> Image
;; Place snake onto game board field

(check-expect (render-snakeA (make-snake "U" (make-cell 4 5) (cons (make-cell 4 6) empty)))
              (place-in-cell HEAD (make-cell 4 5) (place-in-cell BODY (make-cell 4 6) MTS))) 

;(define (render-snakeA 0) 0) ;stub

(define (render-snakeA g)
  (place-in-cell HEAD (snake-head g)
                 (add-body (snake-body g) MTS)))


;; Body Image -> Image
;; Creates image based on cells in body
(define (add-body b img)
  (cond [(empty? (rest b)) (place-in-cell BODY (first b) img)]
        [else
         (place-in-cell BODY (first b)
                        (add-body (rest b) img))]))

;; Game KeyEvent -> Game 
;; handle-key
(check-expect (handle-key (make-game (make-snake "U" (make-cell 0 0)B1)) "left")
              (make-game (make-snake "L" (make-cell 0 0)B1)))
(check-expect (handle-key (make-game (make-snake "U" (make-cell 0 0)B1)) "right")
              (make-game (make-snake "R" (make-cell 0 0)B1)))
(check-expect (handle-key (make-game (make-snake "U" (make-cell 0 0)B1)) "down")
              (make-game (make-snake "D" (make-cell 0 0)B1)))
(check-expect (handle-key (make-game (make-snake "U" (make-cell 0 0)B1)) "up")
              (make-game (make-snake "U" (make-cell 0 0)B1)))
(check-expect (handle-key (make-game (make-snake "U" (make-cell 0 0)B1)) "g")
              (make-game (make-snake "U" (make-cell 0 0)B1)))


;(define (handle-key ws) ...)

(define (handle-key g ke)
  (cond [(key=? ke "up" ) (make-game (snake-up (game-snake g)))]
        [(key=? ke "down" ) (make-game (snake-down (game-snake g)))]
        [(key=? ke "left" ) (make-game (snake-left (game-snake g)))]
        [(key=? ke "right" ) (make-game (snake-right (game-snake g)))]
        [else g]))


;; Snake -> Snake
;; move snake up

(define (snake-up s)
  (make-snake "U" (snake-head s) (snake-body s)))

;; Snake -> Snake 
;; move snake down

(define (snake-down s)
  (make-snake "D" (snake-head s)(snake-body s)))

;; Snake -> Snake
;; move snake right

(define (snake-right s)
  (make-snake "R" (snake-head s)(snake-body s)))

;; Snake -> Snake
;; move snake left

(define (snake-left s)
  (make-snake "L" (snake-head s)(snake-body s)))


;; Game -> Boolean
(define (hit-wall g)
  (snakeboo (game-snake g)))

;; Snake -> Cell
(define (snakeboo g)
  (cellboo (snake-head g)))

;; Cell -> Boolean

(define (cellboo c)
  (or (= (+ 1 BOARD-WIDTH) (cell-c c))
        (= -1 (cell-c c))
        (= (+ 1 BOARD-HEIGHT) (cell-r c))
        (= -1 (cell-r c))))

;; Game -> Img
(define (last-img g)
  (text "SNAKE TOUCHED THE END" 15 "red"))

;; Image Cell Image -> Image
;; place img in cell c given the board scene scn
(check-expect (place-in-cell HEAD (make-cell 5 6) MTS)
              (place-image HEAD
                           (* 5.5 CELL-PIXELS)
                           (* 6.5 CELL-PIXELS)
                           MTS))

(define (place-in-cell img c scn)
  (place-image img 
               (+ (* (cell-c c) CELL-PIXELS) (/ CELL-PIXELS 2))
               (+ (* (cell-r c) CELL-PIXELS) (/ CELL-PIXELS 2))
               scn))



(main (make-game (make-snake "U" (make-cell 0 10) (cons (make-cell 0 11) (cons (make-cell 0 12) (cons (make-cell 0 13) empty))))))
