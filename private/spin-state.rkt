#lang racket

;; quantum spin state, restricted to horizontal and vertical bases

(provide (all-defined-out))
(require "vec.rkt")

(define rad2 (sqrt 2))
(define -rad2 (- rad2))
(define 1/rad2 (/ rad2))
(define -1/rad2 (- 1/rad2))

;; A SpinState is a Vec
;; where
;; x is a Complex representing the spin-up (z-axis) amplitude
;; y is a Complex representing the spin-down (z-axis) amplitude
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

(define SPIN_UP (spin-state 1 0)); positive in z
(define SPIN_DOWN (spin-state 0 1)); negative in z
(define SPIN_LEFT (spin-state 1/rad2 -1/rad2)); negative in x
(define SPIN_RIGHT (spin-state 1/rad2 1/rad2)); positive in x
(define SPIN_OUT (spin-state 1/rad2 (* 1/rad2 0+1i))); positive in y
(define SPIN_IN (spin-state 1/rad2 (* 1/rad2 0-1i))); negative in y
