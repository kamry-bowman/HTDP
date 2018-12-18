;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 476|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct transition [current key next])
(define-struct fsm [initial transitions final])


; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.

(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does an-fsm recognize the given string
(define (fsm-match? an-fsm a-string)
  (local ((define path (map (lambda (chr) (make-string 1 chr)) (string->list a-string)))
          (define transitions (fsm-transitions an-fsm))
          (define (find-trans key current options)
            (cond
              [(empty? options) #false]
              [else (local ((define a-trans (first options)))
                      (if (and
                           (string=? current (transition-current a-trans))
                           (string=? key (transition-key a-trans)))
                          (transition-next a-trans)
                          (find-trans key current (rest options))))]))
          (define (good-path? lot current)
            (cond
              [(empty? lot) (eq? current (fsm-final an-fsm))]
              [else (local ((define next (find-trans (first lot) current transitions)))
                      (if (boolean? next)
                          #false
                          (good-path? (rest lot) next)))])))
    (good-path? path (fsm-initial an-fsm))))

(check-expect (fsm-match? fsm-a-bc*-d "abbbbd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "babbbbd") #false)
(check-expect (fsm-match? fsm-a-bc*-d "aa") #false)
(check-expect (fsm-match? fsm-a-bc*-d "acd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match? fsm-a-bc*-d "d") #false)
(check-expect (fsm-match? fsm-a-bc*-d "aed") #false)