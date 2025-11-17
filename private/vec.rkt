#lang racket

;; 2D Vector utilities

(module+ test (require rackunit))
(provide
 (struct-out vec)
 )
(require)

;; A Vec is a
(struct vec [x y] #:transparent)
;; where
;; x is a Complex
;; y is an Complex
;; Represents a vector in 2D space
