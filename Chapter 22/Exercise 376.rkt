;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 376|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))
(define e5 `(machine (action ,a0) (action)))


; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))


; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))
(check-expect (xexpr-attr e5) '())
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; 2nd-in-Xexpr.v2
; - [List-of Attributes]
; - Xexpr.v2
; 2nd-in-Xexpr.v2 -> Boolean
; determines whether x is an element of [List-of Attribute]
; #false otherwise
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr.v2 -> Symbol
; Obtains the name off of an xexpr.v2
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(define (xexpr-name xe)
  (first xe))

; Xexpr.v2 -> [List-of Xexpr.v2]
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))

(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
               (first optional-loa+content))
             (define remainder
               (rest optional-loa+content)))
            (if (list-of-attributes? loa-or-x)
                remainder
                optional-loa+content))])))

(define a1 '((color "blue")))
(define a2 '((color "green") (toggle "on")))

; [List-of Attributes] Symbol -> Maybe String
; Consumes a List-of Attributes and an Symbol, returns
; the String if the Symbol matches, else false
(check-expect (find-attr a1 'color) "blue")
(check-expect (find-attr a1 'togggle) #false)
(check-expect (find-attr a2 'toggle) "on")

(define (find-attr loa a)
  (local ((define maybe-attr (assq a loa)))
    (if (eq? maybe-attr #false)
        #false
        (second maybe-attr))))

; An XWord is '(word ((text String)))
(define w1 '(word ((text "cat"))))
(define w2 '(word ((text "dog"))))
(define w3 '(word ((text "god"))))
; determines if an expression is an XWord
(check-expect (word? w1) #true)
(check-expect (word? w2) #true)
(check-expect (word? w3) #true)
(check-expect (word? '(word ((idea "stuff")))) #false)
(check-expect (word? '(word 5)) #false)
(define (word? p)
  (match p
    [(list word (list (list text str)))
     (and (eq? word 'word)
              (eq? text 'text))]
    [else #false]))

; XWord => String
(check-expect (word-text w1) "cat")
(check-expect (word-text w2) "dog")
(check-expect (word-text w3) "god")
(define (word-text p)
  (match p
    [(list word (list (list text str))) str]))
      
      

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))

; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
(define li1 '(li (word ((text "one")))))
(define li2 '(li (word ((text "two")))))

(define SIZE 12) ; font size
(define COLOR "black") ; font color
(define BT ; a graphical constant
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))

; Image -> Image
; marks item with bullet
(check-expect (render-item1 li1) (beside/align 'center BT (text "one" 12 'black)))
(check-expect (render-item1 li2) (beside/align 'center BT (text "two" 12 'black)))

(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

(define ul0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(render-item1 '(li (word ((text "one")))))

(define ul0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))


; XEnum.v1 -> Image
; renders a simple enumeration as an image
(check-expect (render-enum1 ul0) ul0-rendered)
(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
    ; XItem.v1 Image -> Image
          (define(deal-with-one item so-far)
            (above/align 'left
                         (render-item1 item)
                         so-far)))
    (foldr deal-with-one empty-image content)))

 
; Image -> Image
; marks item with bullet
(define hello-ex (text "hello" 12 'black))
(check-expect (bulletize hello-ex) (beside/align 'center BT hello-ex))
(define (bulletize item)
  (beside/align 'center BT item))


(define u1
  `(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))
    (li ,ul0)))

(define u1-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))
   (beside/align 'center BT  ul0-rendered)
  ))
; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(check-expect (render-enum ul0) ul0-rendered)
(check-expect (render-enum u1) u1-rendered)
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          (define (draw-one item img)
            (above/align 'left img (render-item item))))
    (foldl draw-one empty-image content)))


(define one-bullet
   (beside/align 'center BT hello-ex))

; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect (render-item '(li (word ((text "hello"))))) one-bullet)
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (cond [(word? content) (bulletize (text (word-text content) SIZE COLOR))]
          [else (bulletize (render-enum content))])))

(define enumOneWord
  '(ul
    (li (word ((text "hello"))))
    (li (word ((text "two"))))))

(define enumTwoWord
  '(ul
    (li (word ((text "hello"))))
    (li (word ((text "two"))))
    (li (word ((text "hello"))))))

(define enumFourWord
  `(ul
    (li (word ((text "hello"))))
    (li (word ((text "two"))))
    (li ,enumTwoWord)))
; XEnum.v2 => Number
; Counts the occurences of "hello"




; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))
