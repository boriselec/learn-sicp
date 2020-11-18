(define (inc x) (+ x 1))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(sum
  (lambda (x) x) 1 inc 10)

; 1.29
(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (coeff k)
    (cond
      ((= k 0) 1)
      ((= k n) 1)
      ((even? k) 2)
      (else 4)))
  (define (yk k)
    (f (+ a (* k h))))
  (define (term k)
    (* (coeff k) (yk k)))
  (* (/ h 3) (sum term 0 inc n)))
(integral (lambda (x) (* x x x)) 0 1 100)
(integral sin 0 1 1000)
