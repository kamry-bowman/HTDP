;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |6.1 Space Invader|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WORLD-WIDTH 400)
(define WORLD-HEIGHT 400)
(define LAND-HEIGHT (/ WORLD-HEIGHT 8))
(define SKY-HEIGHT (- WORLD-HEIGHT LAND-HEIGHT))
(define SKY-COLOR "blue")
(define LAND-COLOR "brown")
(define LAND (rectangle WORLD-WIDTH LAND-HEIGHT "solid" LAND-COLOR))
(define SKY (rectangle WORLD-WIDTH SKY-HEIGHT "solid" SKY-COLOR))

(define WORLD (place-image
               (above SKY LAND)
               (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2)
               (empty-scene WORLD-WIDTH WORLD-HEIGHT)))

(define UFO-WIDTH (* WORLD-WIDTH .15))
(define UFO-HEIGHT (/ UFO-WIDTH 2))
(define UFO-BRIDGE-RADIUS (/ UFO-HEIGHT 2))
(define UFO-CHASSIS-HEIGHT (/ UFO-HEIGHT 2))
(define UFO-COLOR "green")
(define UFO
  (overlay/align
   "middle"
   "bottom"
   (ellipse UFO-WIDTH UFO-CHASSIS-HEIGHT "solid" UFO-COLOR)
   (circle UFO-BRIDGE-RADIUS "solid" UFO-COLOR)))
(define UFO-JUMP (/ WORLD-WIDTH 100))
(define UFO-DESCENT 2)

(define TANK-WIDTH UFO-WIDTH)
(define TANK-HEIGHT TANK-WIDTH)
(define TANK-COLOR "orange")
(define TANK-TRACKS-COLOR "black")
(define TANK-TRACKS-HEIGHT (/ TANK-HEIGHT 4))
(define TANK-CANNON-WIDTH (/ TANK-WIDTH 6))
(define TANK-CANNON-HEIGHT (/ TANK-HEIGHT 3))
(define TANK-CABIN-WIDTH TANK-WIDTH)
(define TANK-CABIN-HEIGHT (- TANK-HEIGHT TANK-TRACKS-HEIGHT TANK-CANNON-HEIGHT))

(define TANK-TRACKS (rectangle TANK-WIDTH TANK-TRACKS-HEIGHT "solid" TANK-TRACKS-COLOR))

(define TANK-CANNON (rectangle TANK-CANNON-WIDTH TANK-CANNON-HEIGHT "solid" TANK-COLOR))

(define TANK-CABIN (rectangle TANK-CABIN-WIDTH TANK-CABIN-HEIGHT "solid" TANK-COLOR))

(define TANK (above
              TANK-CANNON
              TANK-CABIN
              TANK-TRACKS))

(define TANK-SPEED 2)

(define MISSILE-HEIGHT (/ TANK-HEIGHT 2))
(define MISSILE-WIDTH TANK-CANNON-WIDTH)
(define MISSILE-HYPOTENUSE (sqrt (- (sqr MISSILE-HEIGHT) (sqr (/ MISSILE-WIDTH 2)))))
(define MISSILE-COLOR "red")
(define MISSILE (flip-vertical (triangle/sss MISSILE-HYPOTENUSE MISSILE-HYPOTENUSE MISSILE-WIDTH "solid" MISSILE-COLOR)))
(define MISSILE-SPEED 6)
(define MISSILE-LAUNCH-Y (- SKY-HEIGHT TANK-HEIGHT (/ MISSILE-HEIGHT 2)))

;structures

;Aim is a structure
;interpretation make-aim (ufo tank) represents the world-state when there is a tank
;and a UFO but no missile. The ufo is a Posn structure, and the tank is a TANK structure.
(define-struct aim [ufo tank])
;;e.g., (make-aim (make-posn 20 10) (make-tank 28 -3)))

;Fired is a structure
;interpretation make-fired (ufo tank) represents the world-state when there is a tank
;and a UFO but no missile. The ufo is a UFO structure, and the tank is a TANK structure.
(define-struct fired [ufo tank missile])
;;e.g., (make-fired (make-posn 20 100) (make-tank 100 3) (make-posn 22 103))

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick
;;e.g., make-tank (20 5) means a tank at the x-position, moving 5 to the right per tick
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game


;Game Logic

;;Move helper functions



;Tank -> Tank
;(move-tank t) receives tank t, and uses its velocity
;to update its position
(check-expect (move-tank (make-tank 8 3)) (make-tank 11 3))
(define (move-tank t)
  (make-tank (+ (tank-loc t) (tank-vel t)) (tank-vel t)))

;Posn->Posn
;Receives a missile, updates its position based on game-set missile movement, then returns
(define (move-missile m)
  (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED)))

;helper function for move-ufo
;Number->Number
;Calculates random x position for ufo to be set to by move-ufo
;;(define (random-ufo-jump jump-lim)
;;  (* (random jump-lim)
;;     (if (= (random 1) 0) 1 -1)))

(check-expect (ufo-random-handler (make-posn 40 50) 1) (make-posn (+ 40 UFO-JUMP) (+ 50 UFO-DESCENT)))
(define (ufo-random-handler u dir)
  (make-posn (if (<= 0 (+ (posn-x u) (* dir UFO-JUMP)) WORLD-WIDTH)
                 (+ (posn-x u) (* dir UFO-JUMP))
                 (+ (posn-x u) (* -1 dir UFO-JUMP)))
             (+ (posn-y u) UFO-DESCENT)))
  
;Posn->Posn
;Receive a ufo, applies game-movement logic for ufo and returns
(define (move-ufo u)
  (ufo-random-handler u (if (= (random 2) 1) 1 -1)))
  

;SIGS->SIGS
;(move s) receives a SIGS structure s, and advances each element of the stucture according
;to its respective movement logic
(define (move s)
  (cond [(aim? s)
         (make-aim  (move-ufo (aim-ufo s))
                    (move-tank (aim-tank s)))]
        [(fired? s)
         (if (<= (+ (posn-y (move-missile (fired-missile s))) (/ MISSILE-HEIGHT 2)) 0)
             (make-aim (move-ufo (fired-ufo s))
                       (move-tank (fired-tank s)))
             (make-fired (move-ufo (fired-ufo s))
                         (move-tank (fired-tank s))
                         (move-missile (fired-missile s))))]))
         
             
         

;Helper function for stop-when
;UFO MISSILE -> Boolean
;Accepts a ufo and missile posn and sees if they are within given x-lim and y-lim ranges
(define (range-check u m x-lim y-lim)
  (if (and (<= (abs (- (posn-x u) (posn-x m))) x-lim)
           (<= (abs (- (posn-y u) (posn-y m))) y-lim))
      #true
      #false))
      
;Check for game-ending conditions, then fires final render function and ends the game
(define (game-over? s)
  (cond [(aim? s) (if (> (+ (posn-y (aim-ufo s)) (/ UFO-HEIGHT 2)) SKY-HEIGHT) #true #false)]
        [(fired? s) (if (range-check (fired-ufo s) (fired-missile s) 10 10)
                         #true
                         #false)])) 

;helper function for update-tank-direction
;tank, Number -> tank
;(update-tank-vel t n)receives a tank structure, and returns a new tank with velocity n
(define (update-tank-vel t n)
  (make-tank (tank-loc t) n))

;helper function for control
;SIGS, Number -> SIGS
;receives a SIGS and a number (-1 or 1 indicating left or right), and returns a SIGS with the tank's velocity updated directinally based on the number
(define (update-tank-direction s n)
  (cond [(aim? s) (make-aim (aim-ufo s)
                            (update-tank-vel (aim-tank s) (* n TANK-SPEED)))]
        [(fired? s) (make-fired (fired-ufo s)
                                (update-tank-vel (fired-tank s) (* n TANK-SPEED))
                                (fired-missile s))]))

;helper function for handle-space
;Number->posn
;(launch-missile s creates a missile based on SIGS, specifically launching it at tank position)
(define (launch-missile n)
  (make-posn n MISSILE-LAUNCH-Y))

;helper function for control
;SIGS->SIGS
;receive a SIGS and return a fired SIGS if SIGS is aim structure, launching missile. If SIGS is fired structure returns SIGS unchanged.
(define (handle-space s)
  (cond [(aim? s) (make-fired (aim-ufo s)
                              (aim-tank s)
                              (launch-missile (tank-loc (aim-tank s))))]
        [(fired? s) s]))

;receives SIGS and a key event, which it interprets and then returns SIGS
(define (control s ke)
  (cond [(equal? ke "left") (update-tank-direction s -1)]
        [(equal? ke "right") (update-tank-direction s 1)]
        [(equal? ke " ") (handle-space s)]
        [else s]))

;Sets the y-position of the tank on the background
(define TANK-Y (- SKY-HEIGHT (/ TANK-HEIGHT 2)))

;Tank Image -> Image
;adds t to the given image im
(define (tank-render t im)
  (place-image TANK
               (tank-loc t)
               TANK-Y
               im))

;UFO Image -> Image
;adds u to the given image im
(define (ufo-render u im)
  (place-image UFO
               (posn-x u)
               (posn-y u)
               im))

;Missile Image -> Image
;adds m to the given image im
(define (missile-render m im)
  (place-image MISSILE
               (posn-x m)
               (posn-y m)
               im))

;SIGS -> image
;Receive a SIGS, either an aim structure, or a fired structure, and draws
;the UFO, TANK, and possibly MISSILE constants on the world state
(define (render SIGS)
        (cond [(aim? SIGS)
               (tank-render (aim-tank SIGS)
                            (ufo-render (aim-ufo SIGS) WORLD))]
              [(fired? SIGS)
               (missile-render (fired-missile SIGS) (tank-render (fired-tank SIGS)
                                                   (ufo-render (fired-ufo SIGS) WORLD)))]))

(define (render-final outcome)
  (text "Done" 25 "black"))
               
(define start-world
  (make-fired (make-posn (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2))
              (make-tank (/ WORLD-WIDTH 3) 2)
              (make-posn (/ WORLD-WIDTH 3) (* WORLD-HEIGHT 2/3))))

(define taking-aim (make-aim (make-posn 100 50) (make-tank 28 1)))

(define gonna-hit (make-fired (make-posn 20 100)
                              (make-tank 100 3)
                              (make-posn 22 103)))

(define (main SIGS)
      (big-bang SIGS
        [on-tick move]
        [on-key control]
        [to-draw render]
        [stop-when game-over? render-final]))
  
(main taking-aim)