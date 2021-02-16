#lang racket

;; Calculate a square root of a number using Newton's method
;; See SICP, p. 23
(define (square-root x)
  (letrec ([error-margin (/ 1 100000)]
           [square
            (λ (z) (* z z))]
           [average
            (λ (a b) (/ (+ a b) 2))]
           [good-enough?
            (λ (g1 x1)
              (< (abs (- (square g1) x1)) error-margin))]
           [guestimate
            (λ (guess x)
              (if (good-enough? guess x)
                  guess
                  (let* ([q (/ x guess)]
                         [new-guess (average q guess)])
                    (guestimate new-guess x))))])
    (guestimate 1 x)))