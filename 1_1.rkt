(module learn-sicp sicp)

;; 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(= a b)
;;...

;; 1.3
(define (sum-squares a b)
  (+ (* a a) (* b b)))

(define (sum-max-two-squares a b c)
  (cond
    ((and (>= a b) (>= b c)) (sum-squares a b))
    ((and (>= a b) (>= c b)) (sum-squares a c))
    (else (sum-squares b c))))

(sum-max-two-squares 1 2 3)
(sum-max-two-squares 1 1 1)
(sum-max-two-squares 5 1 1)
(sum-max-two-squares -5 1 -2)

;; 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;; newton


(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (sqrt-iter 1.0))


(sqrt 2)
(sqrt 25)
