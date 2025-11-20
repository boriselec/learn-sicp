#lang sicp

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let
    ((p1 (* (lower-bound x) (lower-bound y)))
     (p2 (* (upper-bound x) (lower-bound y)))
     (p3 (* (lower-bound x) (upper-bound y)))
     (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y)))))

;; 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound a) (cdr a))
(define (lower-bound a) (car a))
(newline)

(display (lower-bound (make-interval 1 2)))
(newline)
(display (upper-bound (make-interval 1 2)))
(newline)
(display (add-interval (make-interval 1 2) (make-interval 100 200)))
(newline)
(display "-----")
(newline)

;; 2.8
(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

(display (sub-interval (make-interval 1 2) (make-interval 100 200)))
(newline)
(display "-----")
(newline)

;; 2.9
(define (radius a)
  (/ (+ (lower-bound a) (upper-bound a)) 2))

(display (radius (make-interval 100 102)))
(newline)
(display "-----")
(newline)

;; 2.10
(define (div-interval2 x y)
  (if
    (and
      (<= (lower-bound y) 0)
      (>= (upper-bound y) 0))
    (error "Error: division by zero.")

    (mul-interval
      x
      (make-interval
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))))))

(define (error . args)
  (display "Error: ")
  (for-each display args)
  (newline)
  nil)

(display (div-interval2 (make-interval 100 200) (make-interval 2 4)))
(newline)
(display (div-interval2 (make-interval 100 200) (make-interval -2 4)))
(newline)
(display (div-interval2 (make-interval 100 200) (make-interval -20 -4)))
(newline)
(display (div-interval2 (make-interval 100 200) (make-interval 0 5)))
(newline)
(display "-----")
(newline)
