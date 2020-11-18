(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(module learn-sicp sicp
        (sum identity 1 inc 10))

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
  )

(integral 1 2 3 4)
