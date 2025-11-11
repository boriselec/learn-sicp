#lang sicp

(define (cons x y)
  (define (dispatch m)
    (cond
      ((= m 0) x)
      ((= m 1) y)
      (else (error "Unexpected argument: must be 0 or 1."))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

(car (cons "1" "2"))
(cdr (cons "1" "2"))

(display "-----")
(newline)

;; 2.4
(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (x _) x)))

(define (cdr2 z)
  (z (lambda (_ x) x)))

(car2 (cons2 "1" "2"))
(cdr2 (cons2 "1" "2"))

(display "-----")
(newline)

;; 2.5
(define (cons3 x y)
  (* (expt 2 x) (expt 3 y)))

(define (cast-to-exact n) (inexact->exact (round n)))

(define (car3 z)
  (cond
    ((= (modulo z 3) 0) (car3 (/ z 3)))
    (else (cast-to-exact (log z 2)))))

(define (cdr3 z)
  (cond
    ((= (modulo z 2) 0) (cdr3 (/ z 2)))
    (else (cast-to-exact (log z 3)))))

(cons3 4 5)
(car3 (cons3 4 5))
(cdr3 (cons3 4 5))

(display "-----")
(newline)

;; 2.6

(define zero (lambda (_) (lambda (x) x)))
zero

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(add-1 zero)

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (plus a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define (to-integer n)
  ((n (lambda (i) (+ i 1))) 0))
(define (to-dots n)
  ((n (lambda (_) (display "."))) ""))

(to-integer (plus zero zero))
(to-integer (plus one one))
(to-integer (plus one two))
(to-integer (plus two two))

(display "dots")
(newline)
(to-dots (plus zero zero))
(newline)
(to-dots (plus one one))
(newline)
(to-dots (plus one two))
(newline)
(to-dots (plus two two))
(newline)
