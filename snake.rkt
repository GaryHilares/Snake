;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)


;; My snake game.
(@htdw GameState)

;; ================
;; Constants:

;; Window constants
(define WIDTH 600)
(define HEIGHT 400)
(define MTS (empty-scene WIDTH HEIGHT))

;; Grid constants
(define SQR-S 20)
(define SQR-X-AMO (/ WIDTH SQR-S))
(define SQR-Y-AMO (/ HEIGHT SQR-S))

;; Snake constants
(define SNAKE-COLOR "black")

;; Direction constants
(define UP 0)
(define RIGHT 1)
(define DOWN 2)
(define LEFT 3)

;; Other game constants
(define TICKS-PER-UPDATE 3)
(define DF-FOOD-NUM 3)

;; ===============
;; Data definitions:

(@htdd Position)
(define-struct pos (x y))
;; Position is (make-pos Integer Integer).
;; interp. The xy-coordinates of a game element in the grid (not in the screen)
;;         of the game.
(define POS-TL (make-pos 0 0))
(define POS-TR (make-pos (sub1 SQR-X-AMO) 0))
(define POS-BL (make-pos 0 (sub1 SQR-Y-AMO)))
(define POS-BR (make-pos (sub1 SQR-X-AMO) (sub1 SQR-Y-AMO)))
(define POS-OFFSCREEN (make-pos -1 -1))

#;
(define (fn-for-pos pos)
  (... (pos-x pos)
       (pos-y pos)))

(@htdd Direction)
;; Direction is Natural.
;; interp. UP is up, RIGHT is right, DOWN is down, and LEFT is left. 
;; <Examples are redundant for enumeration>.

#;
(define (fn-for-dir dir)
  (cond [(= UP dir) (...)]
        [(= RIGHT dir) (...)]
        [(= DOWN dir) (...)]
        [else (...)]))

(@htdd Snake)
(define-struct snake (head body dir))
;; Snake is (make-snake Position (listof Position) Direction).
;; interp. head is the position of the snake's head,
;;         body is the position of its body (including the head), and
;;         dir is where the snake is currently heading.
;; CONSTRAINT: All positions in body are adjacent.
;; CONSTRAINT: head is always the last member of body (therefore, snake is never
;;             empty).
(define SNAKE1 (make-snake POS-TL (list POS-TL) RIGHT))
(define SNAKE2 (make-snake (make-pos 1 1) (list (make-pos 1 0) (make-pos 1 1))
                           DOWN))
(define SNAKE3 (make-snake POS-TL (list POS-TL) LEFT))
(define SNAKE4 (make-snake (make-pos 0 1) (list (make-pos 0 0) (make-pos 1 0)
                                                (make-pos 1 1) (make-pos 0 1))
                           UP))

#;
(define (fn-for-snake snake)
  (... (fn-for-pos (snake-head snake))
       (fn-for-lop (snake-body snake))
       (fn-for-dir (snake-dir snake))))

(@htdd Food)
(define-struct food (pos color))
;; Food is (make-food Position Color).
;; interp. pos is the position of the food, color is its color.
(define F1 (make-food POS-BR "blue"))
(define F2 (make-food POS-TR "red"))

#;
(define (fn-for-food food)
  (... (fn-for-pos (food-pos food))
       (food-color food)))

(@htdd GameState)
(define-struct gs (snake lof ticks gameover?))
;; Game state is (make-state Snake (listof Food) Natural)
;; interp. snake is the snake of the game,
;;         lof is a list of the food in the game,
;;         ticks is the amount of ticks since the last update, and
;;         gameover? represents the end of the game (if the player has lost).
(define G1 (make-gs SNAKE1 (list F1 F2) TICKS-PER-UPDATE false))
(define G2 (make-gs SNAKE1 (list F1 F2) 1 false))
(define G3 (make-gs SNAKE1 (list F1) 1 false))
(define G4 (make-gs SNAKE2 (list (make-food (make-pos 1 2) "red")) 1 false))
(define G5 (make-gs SNAKE2 (list (make-food (make-pos 1 2) "red")) 1 true))
(define G6 (make-gs SNAKE3 empty 1 false))
(define G7 (make-gs SNAKE4 (list F1) 1 false))

#;
(define (fn-for-gs gs)
  (... (fn-for-snake (gs-snake gs))
       (fn-for-lof (gs-lof gs))
       (gs-ticks gs)
       (gs-gameover? gs)))

;; ===============
;; Functions

(@htdf main)
(@signature GameState -> GameState)
;; Start the game with (main (create-game DF-FOOD-NUM)).
;; <No tests for main function>.

(@template-origin htdw-main)
(define (main gs)
  (big-bang gs
    (on-tick update-game) ;; GameState -> GameState
    (to-draw render-game) ;; GameState -> Image
    (on-key handle-key))) ;; GameState KeyEvent -> GameState


(@htdf update-game)
(@signature GameState -> GameState)
;; Updates the state of the game.
#; (define (update-game gs) gs) ; stub
(check-expect (update-game G1)
              (if (> TICKS-PER-UPDATE 1)
                  (make-gs SNAKE1 (list F1 F2) (sub1 TICKS-PER-UPDATE) false)
                  (make-gs (update-snake SNAKE1 false)
                           (list F1 F2)
                           TICKS-PER-UPDATE
                           false)))
(check-expect (update-game G2)
              (make-gs (update-snake SNAKE1 false)
                       (list F1 F2)
                       TICKS-PER-UPDATE
                       false))
(check-expect (update-game G3)
              (make-gs (update-snake SNAKE1 false)
                       (list F1)
                       TICKS-PER-UPDATE
                       false))
(check-random (update-game G4)
              (make-gs (update-snake SNAKE2 true)
                       (add-random-food empty)
                       TICKS-PER-UPDATE
                       false))
(check-expect (update-game G5) G5)
(check-expect (update-game G6) (make-gs (gs-snake G6)
                                        (gs-lof G6)
                                        (gs-ticks G6)
                                        true))
(check-expect (update-game G7) (make-gs (gs-snake G7)
                                        (gs-lof G7)
                                        (gs-ticks G7)
                                        true))

(@template-origin GameState)
(define (update-game gs)
  (if (not (gs-gameover? gs))
      (cond [(zero? (sub1 (gs-ticks gs)))
             (if (not (will-gameover? (gs-snake gs)))
                 (if (will-snake-eat-food? (gs-snake gs) (gs-lof gs))
                     (make-gs (update-snake (gs-snake gs) true)
                              (add-random-food (remove-food (gs-snake gs)
                                                            (gs-lof gs)))
                              TICKS-PER-UPDATE
                              false)
                     (make-gs (update-snake (gs-snake gs) false)
                              (gs-lof gs)
                              TICKS-PER-UPDATE
                              false))
                 (make-gs (gs-snake gs) (gs-lof gs) (gs-ticks gs) true))]
            [else
             (make-gs (gs-snake gs) (gs-lof gs) (sub1 (gs-ticks gs)) false)])
      gs))

(@htdf will-gameover?)
(@signature Snake -> Boolean)
;; Produces true if snake will lose the game on this update, false otherwise.
#; (define (will-gameover? snake) false) ; stub
(check-expect (will-gameover? SNAKE1) false)
(check-expect (will-gameover? SNAKE2) false)
(check-expect (will-gameover? SNAKE3) true)
(check-expect (will-gameover? SNAKE4) true)

(@template-origin Snake)
(define (will-gameover? snake)
  (local [(define next-pos (next-head-position snake))]
    (or (member? next-pos (snake-body snake))
        (offscreen? next-pos))))

(@htdf offscreen?)
(@signature Position -> Boolean)
;; Produces true if given position is offscreen, false otherwise.
#; (define (offscreen? pos) false) ; stub
(check-expect (offscreen? POS-TL) false)
(check-expect (offscreen? POS-TR) false)
(check-expect (offscreen? POS-BL) false)
(check-expect (offscreen? POS-BR) false)
(check-expect (offscreen? (make-pos 2 2)) false)
(check-expect (offscreen? (make-pos -1 0)) true)
(check-expect (offscreen? (make-pos 0 -1)) true)
(check-expect (offscreen? POS-OFFSCREEN) true)
(check-expect (offscreen? (make-pos SQR-X-AMO 1)) true)
(check-expect (offscreen? (make-pos 1 SQR-Y-AMO)) true)
(check-expect (offscreen? (make-pos SQR-X-AMO SQR-Y-AMO)) true)

(@template-origin Position)
(define (offscreen? pos)
  (or (< (pos-x pos) 0)
      (>= (pos-x pos) SQR-X-AMO)
      (< (pos-y pos) 0)
      (>= (pos-y pos) SQR-Y-AMO)))

(@htdf will-snake-eat-food?)
(@signature Snake (listof Food) -> Boolean)
;; Produces whether the snake will eat a piece of food on its next move or not.
#; (define (will-snake-eat-food? snake food) false) ; stub
(check-expect (will-snake-eat-food? SNAKE1 (list F1 F2)) false)
(check-expect (will-snake-eat-food? SNAKE1 (list F1)) false)
(check-expect (will-snake-eat-food? SNAKE1
                                    (list (make-food (make-pos 1 0) "blue")))
              true)
(check-expect (will-snake-eat-food? SNAKE2
                                    (list (make-food (make-pos 1 2) "red")))
              true)

(@template-origin use-abstract-fn)
(define (will-snake-eat-food? snake food)
  (ormap (lambda (piece) (same-pos? (food-pos piece)
                                    (next-head-position snake)))
         food))

(@htdf same-pos?)
(@signature Position Position -> Boolean)
;; Produces true if two positions are equal, false otherwise.
#; (define (same-pos? pos1 pos2) false) ; stub
(check-expect (same-pos? POS-TL POS-TL) true)
(check-expect (same-pos? POS-BR POS-BR) true)
(check-expect (same-pos? POS-TL POS-BR) false)
(check-expect (same-pos? POS-BR POS-TL) false)

(@template-origin Position)
(define (same-pos? pos1 pos2)
  (and (= (pos-x pos1) (pos-x pos2))
       (= (pos-y pos1) (pos-y pos2))))

(@htdf next-head-position)
(@signature Snake -> Position)
;; Produces the next-position where the snake's head will be.
#; (define (next-head-position snake) (snake-head snake)) ; stub
(check-expect (next-head-position SNAKE1) (make-pos 1 0))
(check-expect (next-head-position SNAKE2) (make-pos 1 2))

(@template-origin Snake encapsulated Position Direction)
(define (next-head-position snake)
  (local [(define (move pos dx dy)
            (make-pos (+ (pos-x pos) dx) (+ (pos-y pos) dy)))
          (define (move-in-direction dir pos)
            (cond [(= UP dir) (move pos 0 -1)]
                  [(= RIGHT dir) (move pos 1 0)]
                  [(= DOWN dir) (move pos 0 1)]
                  [else (move pos -1 0)]))]
    (move-in-direction (snake-dir snake) (snake-head snake))))

(@htdf update-snake)
(@signature Snake Boolean -> Snake)
;; Produces next snake given current snake and whether it is eating food or not.
#; (define (update-snake snake eating-food?) snake) ; stub
(check-expect (update-snake SNAKE1 true)
              (make-snake (make-pos 1 0)
                          (list POS-TL (make-pos 1 0))
                          RIGHT))
(check-expect (update-snake SNAKE2 true)
              (make-snake (make-pos 1 2)
                          (list (make-pos 1 0) (make-pos 1 1) (make-pos 1 2))
                          DOWN))
(check-expect (update-snake SNAKE1 false)
              (make-snake (make-pos 1 0)
                          (list (make-pos 1 0))
                          RIGHT))
(check-expect (update-snake SNAKE2 false)
              (make-snake (make-pos 1 2)
                          (list (make-pos 1 1) (make-pos 1 2))
                          DOWN))

(@template-origin Snake)
(define (update-snake snake eating-food?)
  (if eating-food?
      (make-snake (next-head-position snake)
                  (append (snake-body snake) (list (next-head-position snake)))
                  (snake-dir snake))
      (make-snake (next-head-position snake)
                  (append (rest (snake-body snake))
                          (list (next-head-position snake)))
                  (snake-dir snake))))

(@htdf remove-food)
(@signature Snake (listof Food) -> (listof Food))
;; Remove food to be eaten by snake.
#; (define (remove-food snake food) food) ; stub
(check-expect (remove-food SNAKE1 (list F1 F2)) (list F1 F2))
(check-expect (remove-food SNAKE1 (list F1)) (list F1))
(check-expect (remove-food SNAKE2 (list F2)) (list F2))
(check-expect (remove-food SNAKE2 (list (make-food (make-pos 1 2) "red")))
              empty)
(check-expect (remove-food SNAKE2 (list F1 (make-food (make-pos 1 2) "red")))
              (list F1))

(@template-origin use-abstract-fn)
(define (remove-food snake food)
  (filter (lambda (piece) (not (same-pos? (next-head-position snake)
                                          (food-pos piece))))
          food))

(@htdf add-random-food)
(@signature (listof Food) -> (listof Food))
;; Add food at random position.
(check-random (add-random-food empty)
              (list (make-food (make-pos (random 0 SQR-X-AMO)
                                         (random 0 SQR-Y-AMO))
                               (random-vivid-color 128))))
(check-random (add-random-food (list F1))
              (list (make-food (make-pos (random 0 SQR-X-AMO)
                                         (random 0 SQR-Y-AMO))
                               (random-vivid-color 128))
                    F1))

(@template-origin (listof Food))
(define (add-random-food food)
  (cons (make-food (make-pos (random 0 SQR-X-AMO) (random 0 SQR-Y-AMO))
                   (random-vivid-color 128))
        food))

(@htdf random-vivid-color)
(@signature Natural -> Color)
;; Produces random color with at least n of R, G or B and at most n of the rest.
;; Constraint: 0 <= n <= 255
#; (define (random-vivid-color n) "red") ; stub
(check-random (random-vivid-color 255)
              (local [(define rn (random 3))]
                (cond [(= rn 0) (make-color 255 0 0)]
                      [(= rn 1) (make-color 0 255 0)]
                      [(= rn 2) (make-color 0 0 255)])))
(check-random (random-vivid-color 0)
              (local [(define rn (random 3))]
                (cond [(= rn 0) (make-color (random 256)
                                            (random 256)
                                            (random 256))]
                      [(= rn 1) (make-color (random 256)
                                            (random 256)
                                            (random 256))]
                      [(= rn 2) (make-color (random 256)
                                            (random 256)
                                            (random 256))])))
(check-random (random-vivid-color 128)
              (local [(define rn (random 3))]
                (cond [(= rn 0) (make-color (+ 128 (random 128))
                                            (random 128)
                                            (random 128))]
                      [(= rn 1) (make-color (random 128)
                                            (+ 128 (random 128))
                                            (random 128))]
                      [(= rn 2) (make-color (random 128)
                                            (random 128)
                                            (+ 128 (random 128)))])))

(@template-origin Natural)
(define (random-vivid-color n)
  (local [(define rn (random 3))]
    (cond [(= rn 0) (make-color (+ n (random (- 256 n)))
                                (random (- 256 n))
                                (random (- 256 n)))]
          [(= rn 1) (make-color (random (- 256 n))
                                (+ n (random (- 256 n)))
                                (random (- 256 n)))]
          [(= rn 2) (make-color (random (- 256 n))
                                (random (- 256 n))
                                (+ n (random (- 256 n))))])))

(@htdf render-game)
(@signature GameState -> Image)
;; Render state of the game to image.
#; (define (render-game gs) empty-image) ; stub
(check-expect (render-game G1)
              (render-snake SNAKE1 (render-lof (list F1 F2) MTS)))
(check-expect (render-game G4)
              (render-snake
               SNAKE2
               (render-lof (list (make-food (make-pos 1 2) "red")) MTS)))
(check-expect (render-game G5)
              (place-image (text "Game over!" 36 "black")
                           (/ WIDTH 2)
                           (/ HEIGHT 2)
                           MTS))

(@template-origin fn-composition)
(define (render-game gs)
  (if (not (gs-gameover? gs))
      (render-snake (gs-snake gs) (render-lof (gs-lof gs) MTS))
      (place-image (text "Game over!" 36 "black")
                   (/ WIDTH 2)
                   (/ HEIGHT 2)
                   MTS)))

(@htdf render-snake)
(@signature Snake Image -> Image)
;; Adds the snake to the given image.
#; (define (render-snake snake img) img) ; stub
(check-expect (render-snake SNAKE1 MTS) (render-square POS-TL SNAKE-COLOR MTS))
(check-expect (render-snake SNAKE2 MTS)
              (render-square (make-pos 1 1)
                             SNAKE-COLOR
                             (render-square (make-pos 1 0) SNAKE-COLOR MTS)))

(@template-origin use-abstract-fn)
(define (render-snake snake img)
  (foldl (lambda (pos cur-img) (render-square pos SNAKE-COLOR cur-img))
         img
         (snake-body snake)))

(@htdf render-square)
(@signature Position Color Image -> Image)
;; Renders a square of the given color in the given position of the given image.
#; (define (render-square pos color img) img) ; stub
(check-expect (render-square (make-pos 0 0) "blue" MTS)
              (place-image/align (square SQR-S "solid" "blue")
                                 0
                                 0
                                 "left"
                                 "top"
                                 MTS))
(check-expect (render-square (make-pos 1 2) "red" MTS)
              (place-image/align (square SQR-S "solid" "red")
                                 (* 1 SQR-S)
                                 (* 2 SQR-S)
                                 "left"
                                 "top"
                                 MTS))
(check-expect (render-square (make-pos 1 2) "red"
                             (render-square (make-pos 0 0) "blue" MTS))
              (place-image/align (square SQR-S "solid" "red")
                                 (* 1 SQR-S)
                                 (* 2 SQR-S)
                                 "left"
                                 "top"
                                 (place-image/align (square SQR-S
                                                            "solid"
                                                            "blue")
                                                    0
                                                    0
                                                    "left"
                                                    "top"
                                                    MTS)))

(@template-origin Position)
(define (render-square pos color img)
  (place-image/align (square SQR-S "solid" color)
                     (* (pos-x pos) SQR-S)
                     (* (pos-y pos) SQR-S)
                     "left"
                     "top"
                     img))


(@htdf render-lof)
(@signature (listof Food) Image -> Image)
;; Adds the food to the given image.
#; (define (render-lof food img) img) ; stub
(check-expect (render-lof empty MTS) MTS)
(check-expect (render-lof (list F1) MTS) (render-square POS-BR "blue" MTS))
(check-expect (render-lof (list F2) MTS) (render-square POS-TR "red" MTS))
(check-expect (render-lof (list F1 F2) MTS)
              (render-square POS-BR "blue" (render-square POS-TR "red" MTS)))

(@template-origin use-abstract-fn)
(define (render-lof food img)
  (foldr (lambda (piece cur-img)
           (render-square (food-pos piece) (food-color piece) cur-img))
         img
         food))

(@htdf handle-key)
(@signature GameState KeyEvent -> GameState)
;; Changes direction of snake with WASD if game running. Restarts game with P.
#; (define (handle-key gs ke) gs) ; stub
(check-expect (handle-key G1 "d") (make-gs (set-snake-direction SNAKE1 RIGHT)
                                           (gs-lof G1)
                                           (gs-ticks G1)
                                           false))
(check-expect (handle-key G1 "s") (make-gs (set-snake-direction SNAKE1 DOWN)
                                           (gs-lof G1)
                                           (gs-ticks G1)
                                           false))
(check-expect (handle-key G4 "a") (make-gs (set-snake-direction SNAKE2 LEFT)
                                           (gs-lof G4)
                                           (gs-ticks G4)
                                           false))
(check-expect (handle-key G4 "w") (make-gs (set-snake-direction SNAKE2 UP)
                                           (gs-lof G4)
                                           (gs-ticks G4)
                                           false))
(check-expect (handle-key G5 "w") G5)
(check-random (handle-key G2 "p") (create-game 2))
(check-random (handle-key G3 "p") (create-game 1))
(check-expect (handle-key G1 "f") G1)
(check-expect (handle-key G4 "e") G4)

(@template-origin KeyEvent)
(define (handle-key gs ke)
  (cond [(key=? ke "w")
         (if (not (gs-gameover? gs))
             (make-gs (set-snake-direction (gs-snake gs) UP)
                      (gs-lof gs)
                      (gs-ticks gs)
                      false)
             gs)]
        [(key=? ke "d")
         (if (not (gs-gameover? gs))
             (make-gs (set-snake-direction (gs-snake gs) RIGHT)
                      (gs-lof gs)
                      (gs-ticks gs)
                      false)
             gs)]
        [(key=? ke "s")
         (if (not (gs-gameover? gs))
             (make-gs (set-snake-direction (gs-snake gs) DOWN)
                      (gs-lof gs)
                      (gs-ticks gs)
                      false)
             gs)]
        [(key=? ke "a")
         (if (not (gs-gameover? gs))
             (make-gs (set-snake-direction (gs-snake gs) LEFT)
                      (gs-lof gs)
                      (gs-ticks gs)
                      false)
             gs)]
        [(key=? ke "p") (create-game (length (gs-lof gs)))]
        [else gs]))

(@htdf set-snake-direction)
(@signature Snake Direction -> Snake)
;; Returns a snake in the same position but pointing to a different direction.
#; (define (set-snake-direction snake dir) snake) ; stub
(check-expect (set-snake-direction SNAKE1 DOWN)
              (make-snake POS-TL (list POS-TL) DOWN))
(check-expect (set-snake-direction SNAKE1 RIGHT)
              (make-snake POS-TL (list POS-TL) RIGHT))
(check-expect (set-snake-direction SNAKE2 UP)
              (make-snake (make-pos 1 1)
                          (list (make-pos 1 0) (make-pos 1 1)) UP))
(check-expect (set-snake-direction SNAKE2 LEFT)
              (make-snake (make-pos 1 1)
                          (list (make-pos 1 0) (make-pos 1 1)) LEFT))

(@template-origin Snake)
(define (set-snake-direction snake dir)
  (make-snake (snake-head snake)
              (snake-body snake)
              dir))

(@htdf create-game)
(@signature Natural -> Game)
;; Produces game with n food pieces in random positions.
(check-random (create-game 0)
              (make-gs (make-snake (make-pos (round (/ SQR-X-AMO 2))
                                             (round (/ SQR-Y-AMO 2)))
                                   (list (make-pos (round (/ SQR-X-AMO 2))
                                                   (round (/ SQR-Y-AMO 2))))
                                   RIGHT)
                       empty
                       TICKS-PER-UPDATE
                       false))
(check-random (create-game 1)
              (make-gs (make-snake (make-pos (round (/ SQR-X-AMO 2))
                                             (round (/ SQR-Y-AMO 2)))
                                   (list (make-pos (round (/ SQR-X-AMO 2))
                                                   (round (/ SQR-Y-AMO 2))))
                                   RIGHT)
                       (add-random-food empty)
                       TICKS-PER-UPDATE
                       false))
(check-random (create-game 2)
              (make-gs (make-snake (make-pos (round (/ SQR-X-AMO 2))
                                             (round (/ SQR-Y-AMO 2)))
                                   (list (make-pos (round (/ SQR-X-AMO 2))
                                                   (round (/ SQR-Y-AMO 2))))
                                   RIGHT)
                       (add-random-food (add-random-food empty))
                       TICKS-PER-UPDATE
                       false))

(@template-origin fn-composition)
(define (create-game n)
  (make-gs (make-snake (make-pos (round (/ SQR-X-AMO 2))
                                 (round (/ SQR-Y-AMO 2)))
                       (list (make-pos (round (/ SQR-X-AMO 2))
                                       (round (/ SQR-Y-AMO 2))))
                       RIGHT)
           (foldr (lambda (n lst) (add-random-food lst)) empty
                  (build-list n identity))
           TICKS-PER-UPDATE
           false))
