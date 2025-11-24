#lang racket

;; 2D Vector utilities

(module+ test (require rackunit))
(provide (all-defined-out))
(require)

;; A Vec is a
(struct vec [x y] #:transparent)
;; where
;; x is a Complex
;; y is an Complex
;; Represents a vector in 2D space

;; Vec Vec -> Vec
(define (vec-add v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))))

;; Vec Vec -> Complex
(define (vec-dot v1 v2)
  (+ (* (vec-x v1) (vec-x v2))
     (* (vec-y v1) (vec-y v2))))

;; Vec Vec -> Vec
(define (vec-sub v1 v2)
  (vec-add v1 (vec-scale -1 v2)))

;; Vec Complex -> Vec
(define (vec-scale k v)
  (vec (* k (vec-x v))
       (* k (vec-y v))))

;; Vec Vec -> Vec
;; calculate reflection of incoming ray vs normal
(define (vec-reflect v norm)
  (define norm-unit (vec-normalize norm))
  ;; v^ = v - 2(v dot n)n
  (vec-sub v (vec-scale (* 2 (vec-dot v norm-unit)) norm-unit)))

(module+ test
  ;; -> \
  (check-within (vec-reflect (vec 1 0) (vec 1 1))
                (vec 0 -1)
                0.1)
  ;; \ <-
  (check-within (vec-reflect (vec -1 0) (vec 1 1))
                (vec 0 1)
                0.1)
  ;; \
  ;; ^
  ;; |
  (check-within (vec-reflect (vec 0 1) (vec 1 1))
                (vec -1 0)
                0.1)
  ;; |
  ;; v
  ;; \
  (check-within (vec-reflect (vec 0 -1) (vec 1 1))
                (vec 1 0)
                0.1))

;; Vec -> Vec
(define (vec-normalize v)
  (vec-scale (/ (vec-magnitude v)) v))

;; Vec -> Real
(define (vec-magnitude v)
  (match v
    [(vec x y)
     (sqrt (+ (* (conjugate x) x)
              (* (conjugate y) y)))]))

;; Vec (Complex -> Complex) -> Vec
(define (vec-map v f)
  (vec (f (vec-x v))
       (f (vec-y v))))

;; Vec -> Boolean
;; Is this vector at the origin?
(define (vec-zero? v)
  (and (zero? (vec-x v))
       (zero? (vec-y v))))
