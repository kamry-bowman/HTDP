;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 268|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; sort structure: [x][list-of-x][x x ->boolean]->[list-of x]
; instantiate [x = List-of Inventory][[List-of Inventory] [Inventory Inventory -> Boolean]] -> [List-of Inventory]

(define-struct inventory [name description ap sp])
(define medium-mku (make-inventory "computer" "HP-5" 3 9))
(define high-mku (make-inventory "wine" "Bordeaux" 2 20))
(define low-mku (make-inventory "grain" "corn" 3 3.1))

; [List-of Inventory] -> [List-of Inventory]
; Sorts a list-of inventory from the greatest profit (difference between inventory-sp and inventory-ap of inventory structure) to lowest profit
(check-expect (most-profitable `(,low-mku ,medium-mku ,high-mku ,low-mku)) `(,high-mku ,medium-mku ,low-mku ,low-mku))
(define (most-profitable loi)
  (local ( ; Inventory Inventory -> Boolean
           ; Determines whether first inventory is more
           ; profitable than second. True if so, else false
           (define (is-more-profitable i1 i2)
             (local ((define (profit i)
                       (- (inventory-sp i)
                          (inventory-ap i))))
               (> (profit i1) (profit i2)))))
    (sort loi is-more-profitable)))