#lang racket

; primitives for calculations
(struct vector (x y z))
(struct line-seg (start end))
(struct sphere (position radius))

; satelite keeps list of other satelites it is connected to
(struct satelite (conns))

(define (vector-op op v1 v2)
  (vector
   (op (vector-x v1) (vector-x v2))
   (op (vector-y v1) (vector-y v2))
   (op (vector-z v1) (vector-z v2))))

(define (vector-unop op v)
  (vector
   (op (vector-x v))
   (op (vector-y v))
   (op (vector-z v))))

(define (vector-len v)
  (sqrt
   (+
    (sqr (vector-x v))
    (sqr (vector-y v))
    (sqr (vector-y v)))))

(define (distance-3d p1 p2)
  (sqrt (+
    (sqr (- (vector-x p2) (vector-x p1)))
    (sqr (- (vector-y p2) (vector-y p1)))
    (sqr (- (vector-z p2) (vector-z p1))))))

(define (dot-product v1 v2)
  (+
   (* (vector-x v1) (vector-x v2))
   (* (vector-y v1) (vector-y v2))
   (* (vector-z v1) (vector-z v2))))

(define (line-seg-intersection l1 l2)
  '(0 0 0))

(define (line-seg-length l)
  (distance-3d (line-seg-start l) (line-seg-end l)))

(define (closest-point-on-line-seg l v)
  (define vec-diff (vector-op - (line-seg-end l) (line-seg-start l)))
  (define vec-len (vector-len vec-diff))
  (define line-to-point (vector-op - v (line-seg-start l)))
  (define frac-of-line (/ (dot-product vec-diff line-to-point) vec-len))
  (cond
    [(> 1 frac-of-line 0) #f]
    [else
     (vector-op
      + (line-seg-start l)
      (vector-unop
       (lambda (v) (* frac-of-line))
       (vector-op - (line-seg-end l) (line-seg-start l))))]))
  

(define (line-seg-intersects-sphere? l sph)
  (define closest-point (closest-point-on-line-seg l (sphere-position sph)))
  (match closest-point
    [(struct vector (x y z)) (point-in-sphere? closest-point sph)]
    [#f #f]))

(define (point-in-sphere? p sph)
  (<= (distance-3d (sphere-position sph) p) (sphere-radius sph)))

(define (connect-satelites sats)
  sats)

(define (find-route sat-map)
  '())

(define (route-call sat-map)
  '())

(define test-sphere (sphere (vector 0 0 0) 12))
(define test-segment (line-seg (vector -4 -4 -4) (vector 12 12 12)))
(displayln (line-seg-intersects-sphere? test-segment test-sphere))
(displayln "Hello, World!")