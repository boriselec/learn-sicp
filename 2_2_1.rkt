#lang sicp

(cdr (list 1 2 3 4))
(cadr (list 1 2 3 4))
(null? nil)
(null? 1)

;; 2.17
(define (last-pair l)
  (if (= (length l) 1)
      (car l)
      (last-pair (cdr l))))

(last-pair (list 1 2))

;; 2.18
(display "---")
(newline)

(define (reverse2 l)
  (if (= (length l) 1)
      l
      (append
        (reverse2 (cdr l))
        (list (car l)))))
(reverse2 (list 1))
(reverse2 (list 1 2 3 4))

;; 2.19
(display "---")
(newline)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define (first-denomination l) (car l))
  (define (except-first-denomination l) (cdr l))
  (define (no-more? l) (null? l))

  (cond ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
      (+ (cc amount
             (except-first-denomination coin-values))
         (cc (- amount
                (first-denomination coin-values))
             coin-values)))))

(cc 100 us-coins)
(cc 100 uk-coins)

;; 2.20
(display "---")
(newline)

(define (same-parity first . arg)
  (define (filter-parity is-even? l)
    (if (null? l)
        nil
        (if (eq? (even? (car l)) is-even?)
            (cons (car l) (filter-parity is-even? (cdr l)))
            (filter-parity is-even? (cdr l)))))
  (cons
    first
    (filter-parity (even? first) arg)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;; 2.21
(display "---")
(newline)

(define (square-list items)
  (if (null? items)
      nil
      (cons
        (* (car items) (car items))
        (square-list (cdr items)))))
(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4))
(square-list2 (list 1 2 3 4))

;; 2.22
(display "---")
(newline)

(define (square x) (* x x))
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter
          (cdr things)
          (append
            answer
            (list (square (car things)))))))
  (iter items nil))
(square-list3 (list 1 2 3 4))

;; 2.23
(display "---")
(newline)

(define (for-each2 f l)
  (cond ((null? l) #t)
    (else
      (f (car l))
      (for-each2 f (cdr l)))))

(for-each2 (lambda (x) (newline) (display x))
           (list 1 2 3))
