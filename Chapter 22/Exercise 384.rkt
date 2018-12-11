;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 384|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)


; An Xexpr.v3 is one of:
;  – Symbol
;  – String
;  – Number
;  – (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
;  – (cons Symbol [List-of Xexpr.v3])
; 
; An Attribute*.v3 is a [List-of Attribute.v3].
;   
; An Attribute.v3 is a list of two items:
;   (list Symbol String)

(read-plain-xexpr/web
    (string-append
       "http://www.ccs.neu.edu/"
       "home/matthias/"
       "HtDP2e/Files/machine-configuration.xml"))

(define PREFIX "https://www.google.com/finance?q=")
(define SIZE 22) ; font size 
 
(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
; price represents the current price of a stock, and delta
; represents the % change in price in the past day
 
; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; [StockWorld -> StockWorld]
          ; Returns a StockWorld value based on the
          ; "price" and "priceChange" attributes
          ; on meta tags at the targeted url
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image
          ; Accepts a StockWorld and uses it to draw a image
          ; to represent the current price
          (define (render-stock-data w)
            (local (; [StockWorld -> String] -> Image
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
  (get '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")
  
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

(check-expect
  (get '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")

(check-expect
  (get-xexpr '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")

(define (get-xexpr x s)
  (cond 
  [(cons? x) (if (cons? (second x))
                 (local ((define prop (find-attr (second x) 'itemprop)))
                   (if (and (string? prop)
                            (string=? prop s))
                       (find-attr (second x) 'content)
                       #false))
                 #false)]
  [else #false]))

(define (find-attr loa s)
  (cond [(empty? loa) #false]
        [else (local ((define attr (first loa)))
           (if (eq? (first attr) s)
               (second attr)
               (find-attr (rest loa) s)))]))