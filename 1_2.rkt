(module learn-sicp sicp)

(define (fib n)
  (cond 
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib2 n)
  (define (fib-iter a b counter)
    (if (= counter 0)
        a
        (fib-iter b (+ a b) (- counter 1))))
  (fib-iter 0 1 n))

(fib 40)
(fib2 40)

;; 1.11

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))

(f 3)
(f 4)
(f 5)
(f 10)

(define (f2 n)
  (define (f-iter a b c counter)
    (if (= counter 0)
        a
        (f-iter b c (+ a b c) (- counter 1))))
  (f-iter 0 1 2 n))

(f2 3)
(f2 4)
(f2 5)
(f2 10)

;; 1.12

(define (pascal-triangle-elem row column)
  (cond
    ((= column 1) 1)
    ((= column row) 1)
    (else (+ 
            (pascal-triangle-elem (- row 1) (- column 1)) 
            (pascal-triangle-elem (- row 1) column)))))

(pascal-triangle-elem 5 1)
(pascal-triangle-elem 5 2)
(pascal-triangle-elem 5 3)
(pascal-triangle-elem 5 4)
(pascal-triangle-elem 5 5)

;; 1.16

(define (fast-exp b n)
  (define (fast-exp-iter b n a)
    (cond
      ((= n 0) a)
      ((even? n) (fast-exp-iter (* b b) (/ n 2) a))
      (else (fast-exp-iter b (- n 1) (* a b)))))
  (fast-exp-iter b n 1))

(fast-exp 2 10)
(fast-exp 3 5)

