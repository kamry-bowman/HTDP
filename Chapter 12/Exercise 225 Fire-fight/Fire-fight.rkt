;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Fire-fight) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Game Resources

(define WIDTH 250)
(define HEIGHT 250)
(define GROUND-COLOR "brown")
(define GROUND-HEIGHT (* HEIGHT 1/10))
(define GROUND (rectangle WIDTH GROUND-HEIGHT "solid" GROUND-COLOR))

(define FOREST-HEIGHT (* HEIGHT 1/5))
(define FOREST-COLOR "green")
(define FOREST (rectangle WIDTH FOREST-HEIGHT "solid" FOREST-COLOR))

(define BG (place-image/align
            FOREST
            0 (- HEIGHT GROUND-HEIGHT FOREST-HEIGHT)
            "left" "top"
            (place-image/align
             GROUND
             0 (- HEIGHT GROUND-HEIGHT)
             "left" "top"
             (empty-scene WIDTH HEIGHT))))

; PLANE IMAGE DEFINITION
(define PLANE-COLOR "black")
(define PLANE-WIDTH (/ WIDTH 10))
(define PLANE-FUSELAGE-HEIGHT (/ PLANE-WIDTH 5))
(define PLANE-WINGS-WIDTH (* PLANE-WIDTH 2/3))
(define PLANE-WINGS-THICKNESS (/ PLANE-WINGS-WIDTH 4))

(define PLANE-FUSELAGE (rectangle PLANE-WIDTH PLANE-FUSELAGE-HEIGHT "solid" PLANE-COLOR))

(define PLANE-WINGS (rotate 110 (rectangle PLANE-WINGS-WIDTH PLANE-WINGS-THICKNESS "solid" PLANE-COLOR)))

(define PLANE-ALTITUDE (* HEIGHT 1/5))

(define PLANE
  (overlay/offset PLANE-WINGS (* PLANE-WIDTH -.1) 0 PLANE-FUSELAGE))

(define PLANE-LEFT
  (flip-horizontal PLANE))


; FIRE IMAGE DEFINITION
(define FIRE-OUTER-COLOR "red")
(define FIRE-OUTER-DIAMETER (* WIDTH .05))
(define FIRE-OUTER-RADIUS (* FIRE-OUTER-DIAMETER .5))

(define FIRE-OUTER-TRI-HEIGHT (* FIRE-OUTER-RADIUS 4))
(define FIRE-OUTER-SIDE-LENGTH (expt (+ (expt FIRE-OUTER-RADIUS 2) (expt FIRE-OUTER-TRI-HEIGHT 2)) 1/2))

(define FIRE-OUTER-BASE (circle FIRE-OUTER-RADIUS "solid" FIRE-OUTER-COLOR))
(define FIRE-OUTER-PEAK (rotate 180 (triangle/sss FIRE-OUTER-SIDE-LENGTH FIRE-OUTER-SIDE-LENGTH FIRE-OUTER-DIAMETER "solid" FIRE-OUTER-COLOR)))

(define FIRE-OUTER (overlay/offset FIRE-OUTER-PEAK 0 FIRE-OUTER-DIAMETER FIRE-OUTER-BASE))

(define FIRE-INNER-COLOR "yellow")
(define FIRE-INNER-RADIUS (* FIRE-OUTER-RADIUS .75))
(define FIRE-INNER-DIAMETER (* 2 FIRE-INNER-RADIUS))
(define FIRE-INNER-TRI-HEIGHT (* FIRE-INNER-RADIUS 4))
(define FIRE-INNER-SIDE-LENGTH (expt (+ (expt FIRE-INNER-RADIUS 2) (expt FIRE-INNER-TRI-HEIGHT 2)) 1/2))

(define FIRE-INNER-BASE (circle FIRE-INNER-RADIUS "solid" FIRE-INNER-COLOR))
(define FIRE-INNER-PEAK (rotate 180 (triangle/sss FIRE-INNER-SIDE-LENGTH FIRE-INNER-SIDE-LENGTH FIRE-INNER-DIAMETER "solid" FIRE-INNER-COLOR)))

(define FIRE-INNER (overlay/offset FIRE-INNER-PEAK 0 FIRE-INNER-DIAMETER FIRE-INNER-BASE))

(define FIRE-ALTITUDE (- HEIGHT GROUND-HEIGHT FOREST-HEIGHT))

(define FIRE (overlay FIRE-INNER FIRE-OUTER))
(define FIRE-WIDTH (image-width FIRE))
(define FIRE-HEIGHT (image-height FIRE))


; Water Image Definition
(define WATER-DROP-COLOR "blue")
(define WATER-DROP-DIAMETER (* WIDTH .02))
(define WATER-DROP-RADIUS (* WATER-DROP-DIAMETER .5))
(define WATER-DROP-SPACE (* 1.5 WATER-DROP-DIAMETER))

(define WATER-DROP-TRI-HEIGHT (* WATER-DROP-RADIUS 4))
(define WATER-DROP-SIDE-LENGTH (expt (+ (expt WATER-DROP-RADIUS 2) (expt WATER-DROP-TRI-HEIGHT 2)) 1/2))

(define WATER-DROP-BASE (circle WATER-DROP-RADIUS "solid" WATER-DROP-COLOR))
(define WATER-DROP-PEAK (rotate 180 (triangle/sss WATER-DROP-SIDE-LENGTH WATER-DROP-SIDE-LENGTH WATER-DROP-DIAMETER "solid" WATER-DROP-COLOR)))

(define WATER-DROP (overlay/offset WATER-DROP-PEAK 0 WATER-DROP-DIAMETER WATER-DROP-BASE))

(define WATER (overlay/offset WATER-DROP 0 WATER-DROP-SPACE (overlay/offset WATER-DROP WATER-DROP-SPACE 0 WATER-DROP)))
(define WATER-WIDTH (image-width WATER))
(define WATER-HEIGHT (image-height WATER))

; Game Variables
(define FPS 60)
(define PLANE-SPEED (/ 100 FPS))
(define WATER-SPEED (/ 120 FPS))
(define WATER-LIMIT 5)
(define FIRE-RESPAWN-RATE (floor (* (/ (- FIRE-ALTITUDE PLANE-ALTITUDE) WATER-SPEED) .66)))
;Determines how many pixels difference between fire and water counts as a hit
(define X-OVERLAP-LIMIT (* FIRE-WIDTH 3/4))
(define Y-OVERLAP-LIMIT (- FIRE-HEIGHT FIRE-OUTER-TRI-HEIGHT))

;(place-image/align WATER (* WIDTH 1/3) (* HEIGHT 1/2) "left" "bottom" (place-image/align FIRE (* WIDTH 1/2) (- HEIGHT GROUND-HEIGHT FOREST-HEIGHT) "left" "bottom" (place-image PLANE (/ WIDTH 2) PLANE-ALTITUDE BG)))

; A plane is a structure, with (make-plan N N) having the first Number represent the plane's position,
; and the second representing the plane's horizontal velocity
(define-struct plane [pos vel])


; A firefight is a structure representing the current overall game state.
; Its elements are:
;    -plane, which is a plane structure
;    -waters, which is a list of posns representing current Posn for any falling water
;    -fire, which is a list of fires representing the active fires in the game. A fire is represented by a Number representing the x-position of the fire
;    -time, is a number representing remaining frames left in the game, which is equal to total game second times FPS
(define-struct firefight [plane waters fires time])

; Fires is a List of Numbers, representing x positions of fires in the game
; Waters is a List of Posns, representing Posns of waters in the game

;; Representative examples
(define WaterHit1 (make-posn 30 (+ (- FIRE-ALTITUDE FIRE-HEIGHT) FIRE-OUTER-TRI-HEIGHT)))
(define FireHit1 30)
(define WaterHit2 (make-posn 50 (+ (- FIRE-ALTITUDE FIRE-HEIGHT) FIRE-OUTER-TRI-HEIGHT)))
(define FireHit2 50)
(define WaterMiss1 (make-posn 100 (+ (- FIRE-ALTITUDE FIRE-HEIGHT 1) FIRE-OUTER-TRI-HEIGHT)))
(define FireMiss1 100)

(define Extinguish1 (make-firefight (make-plane 25 1) (list (make-posn 20 100) WaterHit1) (list FireHit1 FireHit2) 5))
(define Extinguished1 (make-firefight (make-plane 25 1) (list (make-posn 20 100)) (list FireHit2) 5))

(define Extinguish2 (make-firefight (make-plane 25 3) (list (make-posn 20 100) WaterHit1 WaterHit2 WaterMiss1) (list FireHit1 FireHit2 FireMiss1) 20))
(define Extinguished2 (make-firefight (make-plane 25 3) (list (make-posn 20 100) WaterMiss1) (list FireMiss1) 20))


; Firefight->Img
; Render consumes a firefight, and renders it as an image
(check-expect (render (make-firefight (make-plane (* WIDTH 1/2) 1)
                                        (list (make-posn (* WIDTH 1/3) (* HEIGHT 1/2)))
                                        (list (* WIDTH 1/2))
                                        0))
              (place-image/align (text "0" 12 "black") 1 1 "left" "top"  (place-image/align WATER (* WIDTH 1/3) (* HEIGHT 1/2) "left" "bottom"
                                   (place-image/align FIRE (* WIDTH 1/2) (- HEIGHT GROUND-HEIGHT FOREST-HEIGHT) "left" "bottom"
                                                      (place-image/align PLANE (/ WIDTH 2) PLANE-ALTITUDE "left" "bottom"
                                                                   BG)))))

;; Rendering Functions

(define (render f)
  (render-time (quotient (firefight-time f) FPS) (render-waters (firefight-waters f) (render-fires (firefight-fires f) (render-plane (firefight-plane f) BG)))))

(define (render-final f)
  (render-game-ending f (render f)))

(define (render-game-ending f img)
  (place-image (text (if (fires-out? f)
                               "WINNER!"
                               "LOSER!")
                           40
                           "red")
               (* WIDTH 1/2)
               (* HEIGHT 1/2)
               img))

; Number, Img -> Img
; Takes the Time Number, and illustrates it on the given image
(check-expect (render-time 5 BG) (place-image/align (text "5" 12 "black") 1 1 "left" "top" BG))
(define (render-time t img)
  (place-image/align (text (number->string t) 12 "black") 1 1 "left" "top" img))

; List-of-Numbers, Img -> Img
; Render-fires consumes a list of fires and draw an image of them on img
(check-expect (render-fires '() BG) BG)
(check-expect (render-fires (list 4) BG) (place-image/align FIRE 4 FIRE-ALTITUDE "left" "bottom" BG))
(check-expect (render-fires (list 4 8) BG) (place-image/align FIRE 4 FIRE-ALTITUDE "left" "bottom" (place-image/align FIRE 8 FIRE-ALTITUDE "left" "bottom" BG)))
(define (render-fires lof img)
  (cond [(empty? lof) img]
        [(cons? lof) (render-fire (first lof) (render-fires (rest lof) img))]))

; Number -> Img
; Render-fire consumes a Number representing fire, and places it on the given image
(check-expect (render-fire 4 BG) (place-image/align FIRE 4 FIRE-ALTITUDE "left" "bottom" BG))
(define (render-fire x img)
  (place-image/align FIRE x FIRE-ALTITUDE "left" "bottom" img))

; List-of-posns, Img -> Img
; Render-waters consumes a list of Posns, representing any water positions in the game, and draws them on the image
(check-expect (render-waters '() BG) BG)
(check-expect (render-waters (list (make-posn (* WIDTH 1/3) (* HEIGHT 1/2))) BG) (place-image/align WATER (* WIDTH 1/3) (* HEIGHT 1/2) "left" "bottom" BG))
(define (render-waters lop img)
  (cond [(empty? lop) img]
        [(cons? lop) (render-water (first lop) (render-waters (rest lop) img))]))

; Posn, Img -> Img
; Render-water consumes a Posn, and draws a water image at that position on the Img
(check-expect (render-water (make-posn (* WIDTH 1/3) (* HEIGHT 1/2)) BG) (place-image/align WATER (* WIDTH 1/3) (* HEIGHT 1/2) "left" "bottom" BG))
(define (render-water p img)
  (place-image/align WATER (posn-x p) (posn-y p) "left" "bottom" img))

; Number, Img->Img
; Render-plane consumes a Number, representing plane x position, and draws it on the image
(check-expect (render-plane (make-plane (/ WIDTH 2) 1) BG) (place-image/align PLANE (/ WIDTH 2) PLANE-ALTITUDE "left" "bottom" BG))
(define (render-plane p img)
  (place-image/align (plane-select p) (plane-pos p) PLANE-ALTITUDE "left" "bottom" img))

; Plane -> Image
; Determines direction of plane, and returns an image corresponding to its direction
(check-expect (plane-select (make-plane 5 4)) PLANE)
(check-expect (plane-select (make-plane 5 -1)) PLANE-LEFT)
(define (plane-select p)
  (if (positive? (plane-vel p))
      PLANE
      PLANE-LEFT))

;; Dynamic World Logic

; Firefight -> Firefight
; Consumes a firefight, and advances the game-state, returning an updated firefight structure
; Updates the plane velocity based on current velocity, updates water position, and extinguishes/adds fires
(define (tock f)
  (move (extinguish f)))

; Firefight -> Firefight
; Consumes a firefight, determines if any fires or waters need to be extinguished, and does so, returning a firefight with
; all extinguishable waters and fires extinguished
(check-expect (extinguish Extinguish1) Extinguished1)
(check-expect (extinguish Extinguish2) Extinguished2)
(define (extinguish f)
  (make-firefight (firefight-plane f) (extinguish-waters (firefight-waters f) (firefight-fires f)) (extinguish-fires (firefight-fires f) (firefight-waters f)) (firefight-time f)))

; Waters, Fires -> Waters
; Consumes a list of waters and fires, and removes all waters that need to be extinguished
(check-expect (extinguish-waters (list WaterHit1 (make-posn 50 50)) (list FireHit1 150)) (list (make-posn 50 50)))
(define (extinguish-waters w f)
  (cond [(empty? w) w]
        [(cons? w) (if (or (water-hit? (first w) f X-OVERLAP-LIMIT Y-OVERLAP-LIMIT)
                           (water-fallen? (first w)))
                               (extinguish-waters (rest w) f)
                               (cons (first w) (extinguish-waters (rest w) f)))]))

;Determines if water has gone past the tree-tops, which is determined as the distance between the water's y coordinate,
;forest tops dropping to 0
(define (water-fallen? w)
  (<= (- ( - HEIGHT FOREST-HEIGHT GROUND-HEIGHT) (posn-y w)) 0))
  

; Water, List of Fires, Number, Number -> Boolean
; Consumes a Water, a List of Fires, and two Numbers. Determines if Water's left and top extremity is within Number or Number pixels of any of the Fire's left or top extremity, returns true if so, else false
(define (water-hit? w lof x-limit y-limit)
  (cond [(empty? lof) #false]
        [(cons? lof) (if (overlap? w (make-posn (first lof) FIRE-ALTITUDE) x-limit y-limit)
                         #true
                         (water-hit? w (rest lof) x-limit y-limit))]))

; Consumes two posns, and checks if their x or y coordinates overlap by x-limit or y-limit
(define (overlap? p1 p2 x-limit y-limit)
  (and (<= (abs (- (posn-x p1) (posn-x p2))) x-limit)
      (<= (abs (- (posn-y p1) (posn-y p2))) y-limit)))

; Consumes a list of fires and waters, and removes all fires that need to be extinguished
(check-expect (extinguish-fires (list FireHit1 150) (list WaterHit1 (make-posn 50 50))) (list 150))
(define (extinguish-fires f w)
  (cond [(empty? f) f]
        [(cons? f) (if (fire-hit? (first f) w X-OVERLAP-LIMIT Y-OVERLAP-LIMIT)
                       (extinguish-fires (rest f) w)
                       (cons (first f) (extinguish-fires (rest f) w)))]))

; Consumes a fire, a list-of-waters, and an x-limit and y-limit, and determines whether the fire's x or y coordinates overlaps
; any of the waters in list-of-waters, returns true if so, else false
(define (fire-hit? f low x-limit y-limit)
  (cond [(empty? low) #false]
        [(cons? low) (if (overlap? (make-posn f FIRE-ALTITUDE) (first low) x-limit y-limit)
                         #true
                         (fire-hit? f (rest low) x-limit y-limit))]))

(define (move f)
  (make-firefight (move-plane (firefight-plane f))
                  (move-waters (firefight-waters f))
                  (add-fires (firefight-fires f) (firefight-time f))
                  (update-time (firefight-time f))))

; Plane -> Plane
; Receives a plane, updates it based on unit of movement
(check-expect (move-plane (make-plane 5 1)) (make-plane 6 1))
(check-expect (move-plane (make-plane 5 -2)) (make-plane 3 -2))
(define (move-plane p)
  (make-plane (wrap-plane (+ (plane-pos p) (plane-vel p))) (plane-vel p)))

; Plane -> Plane
; Receives a plane, and updates it to wrap around if it has gone off screen
(define (wrap-plane n)
  (cond [(< n (* -1 PLANE-WIDTH)) WIDTH]
        [(> n (+ WIDTH PLANE-WIDTH)) (* -1 PLANE-WIDTH)]
        [else n]))


; Waters -> Waters
; Receives Waters, and updates it based on one unit of movement
(check-expect (move-waters (list (make-posn 100 100) (make-posn 120 100))) (list (make-posn 100 (+ 100 WATER-SPEED)) (make-posn 120 (+ 100 WATER-SPEED))))
(define (move-waters w)
  (cond [(empty? w) '()]
        [(cons? w) (cons (move-water (first w)) (move-waters (rest w)))]))

; Water->Water
; Receives a Water, and updates it based on one unit of movement
(define (move-water w)
  (make-posn (posn-x w) (+ (posn-y w) WATER-SPEED)))

; Fires, Time -> Fires
; Checks whether a new fire needs to be added, and if so, adds it
(define (add-fires f t)
  (if (= (modulo t FIRE-RESPAWN-RATE) 0)
      (cons (new-fire f) f)
      f))

; Fires -> Fire
; Randomly generates a new fire, and if it doesn't overlap with existing fire, returns it
(define (new-fire f)
  (check-new-fire (random (- WIDTH FIRE-WIDTH)) f))

; Fire -> Fires
; Checks whether candidate fire overlaps with an existing fire, if so, calls new-fire for a new candidate fire, otherwise returns candidate
(define (check-new-fire candidate f)
  (if (member? candidate f)
      (new-fire f)
      candidate))

; Time->Time
; Updates time
(define (update-time t)
  (- t 1))

; Firefight, Key-event -> Firefight
; Consumes a key-event, and uses it to change direction of plane or launch water, if applicable
(define (control f ke)
  (cond [(string=? ke "right") (change-vel 1 (firefight-plane f) f)]
        [(string=? ke "left") (change-vel -1 (firefight-plane f) f)]
        [(string=? ke " ") (drop-water f)]
        [else f]))

; Number, Plane, Firefight
; Consumes a Number (1 or -1) representing positive or negative directions respectively. Updates Firefight to reflect a plane with new velocity with given direction
(define (change-vel dir p f)
  (make-firefight (make-plane (plane-pos p) (* dir (abs (plane-vel p)))) (firefight-waters f) (firefight-fires f) (firefight-time f)))

; Firefight -> Firefight
; Consumes a Firefight, and adds a water if not over allowable water limit
(define (drop-water f)
  (if (<= (length (firefight-waters f)) WATER-LIMIT)
      (new-water f)
      f))

; Firefight -> Firefight
; Adds a new water to firefight
(define (new-water f)
  (make-firefight (firefight-plane f) (cons (generate-water (firefight-plane f)) (firefight-waters f)) (firefight-fires f) (firefight-time f)))

; Plane -> Water
; Generates a new water based on plane position
(define (generate-water p)
  (make-posn (+ (plane-pos p) (* PLANE-WIDTH .5)) (+ PLANE-ALTITUDE WATER-HEIGHT)))

; Firefight -> Boolean
; Returns true if there are no fires left or time has gone down to 0
(define (stopper f)
  (or (fires-out? f)
      (time-out? f)))

; Firefight -> Boolean
; Determines if there are no fires left, returning 0
(define (fires-out? f)
  (= (length (firefight-fires f)) 0))

; Firefight -> Boolean
;; Determines if time has dropped to 0, returning boolean
(define (time-out? f)
  (<= (firefight-time f) 0))

; Number -> firefight
; T represents time limit
(define (main t)
  (big-bang (make-firefight (make-plane 0 PLANE-SPEED) '() (add-fires '() 0) (* t FPS))
    [to-draw render]
    [on-tick tock (/ 1 FPS)]
    [on-key control]
    [stop-when stopper render-final]))
              