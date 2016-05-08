#lang racket

(require plot)
(require graph)
(require csv-reading)

; primitives for calculations
(struct vector (x y z))
(struct line-seg (start end))
(struct sphere (position radius))

; satelite keeps list of other satelites it is connected to
(struct satelite (name longitude latitude height conns))

; FUNCTIONS

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
  (sqrt (vector-sqlen v)))

(define (vector-sqlen v)
  (+
   (sqr (vector-x v))
   (sqr (vector-y v))
   (sqr (vector-z v))))

(define (vector-unpack v)
  (list
   (vector-x v)
   (vector-y v)
   (vector-z v)))

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

(define (line-seg-length l)
  (distance-3d (line-seg-start l) (line-seg-end l)))

(define (closest-point-on-line-seg l v)
  (define vec-diff (vector-op - (line-seg-end l) (line-seg-start l)))
  (define vec-len (vector-sqlen vec-diff))
  (define line-to-point (vector-op - v (line-seg-start l)))
  (define frac-of-line (/ (dot-product vec-diff line-to-point) vec-len))
  (cond
    [(or (< frac-of-line 0) (> frac-of-line 1)) #f]
    [else
     (vector-op
      + (line-seg-start l)
      (vector-unop
       (lambda (v) (* frac-of-line v))
       (vector-op - (line-seg-end l) (line-seg-start l))))]))

(define (line-seg-intersects-sphere? l sph)
  (define closest-point (closest-point-on-line-seg l (sphere-position sph)))
  (match closest-point
    [(struct vector (x y z)) (point-in-sphere? closest-point sph)]
    [#f #f]))

(define (point-in-sphere? p sph)
  (<= (distance-3d p (sphere-position sph)) (sphere-radius sph)))

(define (lat-lon-to-coords r lat lon [h 0])
  (define adj-lat (* lat (/ pi 180)))
  (define adj-lon (* lon (/ pi 180)))
  (define adj-r (+ r h))
  (vector
   (* (- adj-r) (cos adj-lat) (cos adj-lon))
   (* adj-r (sin adj-lat))
   (* adj-r (cos adj-lat) (sin adj-lon))))

(define (connect-satelites sats)
  sats)

(define (find-route sat-map)
  '())

(define (route-call sat-map)
  '())

; CALCULATIONS AND DEFINITIONS

(define satelite-points
  (call-with-input-file "data.csv"
    (lambda (in)
      (csv-map
       (lambda (values)
         (match values
           [(list seed) seed]
           [(list name latitude longitude height)
            (satelite name (string->number latitude) (string->number longitude) (string->number height) '())]
           [(list "ROUTE" lat1 lon1 lat2 lon2)
            (list (cons (string->number lat1) (string->number lon1)) (cons (string->number lat2) (string->number lon2)))]))
       in))))

(define *earth-radius* 6371)
(define *earth-sphere* (sphere (vector 0 0 0) *earth-radius*))

(define satelite-plot-points
   (for/list ([e satelite-points] #:when (satelite? e))
     (cons (satelite-name e) (lat-lon-to-coords *earth-radius* (satelite-latitude e) (satelite-longitude e) (* (satelite-height e) 2)))))

(define satelite-plot-connections
  (for/list ([e satelite-plot-points])
    (for/list ([os satelite-plot-points]
               #:when (and
                       (not (eqv? e os))
                       (not (line-seg-intersects-sphere? (line-seg (cdr e) (cdr os)) *earth-sphere*))))
      (cons (cdr e) (cdr os)))))

(plot3d
 (list
  (isosurface3d
   (lambda (x y z)
    (sqrt (+ (sqr x) (sqr y) (sqr z)))) *earth-radius*
   (- *earth-radius*) *earth-radius* (- *earth-radius*) *earth-radius* (- *earth-radius*) *earth-radius*
   #:alpha 0.25)
  (map
   (lambda (v) (point-label3d (vector-unpack (cdr v)) (car v)))
   satelite-plot-points)
  (flatten (map
   (lambda (v)
     (flatten (for/list ([l v])
       (match l
         [(cons (struct vector (x1 y1 z1)) (struct vector (x2 y2 z2)))
          (lines3d (list (list x1 y1 z1) (list x2 y2 z2)))]))))
   satelite-plot-connections)))
 #:title (car satelite-points)
 #:altitude 25)