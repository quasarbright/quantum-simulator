#lang racket

;; quantum spin state, restricted to horizontal and vertical bases

(provide spin-state
         SPIN_UP
         SPIN_DOWN
         SPIN_LEFT
         SPIN_RIGHT)
(require "vec.rkt")

(define rad2 (sqrt 2))
(define -rad2 (- rad2))
(define 1/rad2 (/ rad2))
(define -1/rad2 (- 1/rad2))

;; A SpinState is a Vec
;; where
;; x is a Complex representing the spin-up amplitude
;; y is a Complex representing the spin-down amplitude
;; Represents the state of an electron's spin, decomposed into the vertical basis

;; Complex Complex -> SpinState
(define (spin-state up down)
  (unless (<= (magnitude up) 1)
    (error 'spin-state "up must have magnitude of <= 1"))
  (unless (<= (magnitude down) 1)
    (error 'spin-state "down must have magnitude of <= 1"))
  (unless (<= (abs (- 1 (+ (sqr (magnitude up)) (sqr (magnitude down)))))
              ; tolerance
              0.0001)
    (error 'spin-state "probability must be 1"))
  (vec up down))

(define SPIN_UP (spin-state 1 0))
(define SPIN_DOWN (spin-state 0 1))
(define SPIN_LEFT (spin-state 1/rad2 -1/rad2))
(define SPIN_RIGHT (spin-state 1/rad2 1/rad2))
