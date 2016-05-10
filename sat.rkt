#lang racket

(require plot)
(require graph)
(require csv-reading)

; STRUCTURES

(struct vector (x y z))
(struct line-seg (start end))
(struct sphere (position radius))

; target and destination are satelites with 0 height
(struct satelite (name latitude longitude height ecef))

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
  ; convert to radians to operate on with sin/cos since they take radians
  (define adj-lat (* lat (/ pi 180)))
  (define adj-lon (* lon (/ pi 180)))
  ; adjust by height, since satelite position is given in the form of lat/lon/altitude
  ;  given this, adjusting the radius of the perfect by the altitude is then equivalent
  ;  to "raising" them above the surface of the position on the real sphere.
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

(define (dijkstra-solve graph source target)
  #f)

; CALCULATIONS AND DEFINITIONS
  
; geocentric coordinates, the center of the earth is origo (0 0 0)
(define *earth-radius* 6371)
(define *earth-sphere* (sphere (vector 0 0 0) *earth-radius*))

(define satelite-points
  (call-with-input-file "data.csv"
    (lambda (in)
      (flatten
       (csv-map
        (lambda (values)
          (match values
            [(list seed) seed]
            [(list name latitude longitude height)
             (let ([lat (string->number latitude)] [lon (string->number longitude)] [h (string->number height)])
               (satelite name lat lon h (lat-lon-to-coords *earth-radius* lat lon h)))]
            [(list "ROUTE" lat1 lon1 lat2 lon2)
             (let ([lat1 (string->number lat1)] [lon1 (string->number lon1)][lat2 (string->number lat2)] [lon2 (string->number lon2)])
               (cons
                (satelite "START" lat1 lon1 1 (lat-lon-to-coords *earth-radius* lat1 lon1 1))
                (satelite "DEST" lat2 lon2 1 (lat-lon-to-coords *earth-radius* lat2 lon2 1))))]))
        in)))))

(define-values (satelite-graph satelite-lines)
  (let ([graph (weighted-graph/undirected '())])
    ; populate graph with nodes
    (for ([s satelite-points] #:when (satelite? s))
      (add-vertex! graph s))
    ; connect all nodes which can reach eachother
    (define lines
      (for/list ([s1 satelite-points])
        (for/list ([s2 satelite-points]
                   #:when (and (satelite? s1) (satelite? s2)
                               (not (eqv? s1 s2)) ; don't connect to self
                               (not (line-seg-intersects-sphere? (line-seg (satelite-ecef s1) (satelite-ecef s2)) *earth-sphere*))))
          (add-edge! graph s1 s2 (distance-3d (satelite-ecef s1) (satelite-ecef s2)))
          (cons (satelite-ecef s1) (satelite-ecef s2)))))
    (values graph lines)))

(plot3d
 (list
  ; draw earth sphere
  (isosurface3d
   (lambda (x y z)
    (sqrt (+ (sqr x) (sqr y) (sqr z)))) *earth-radius*
   (- *earth-radius*) *earth-radius* (- *earth-radius*) *earth-radius* (- *earth-radius*) *earth-radius*
   #:alpha 0.25)
  ; draw points for satelites, with labels for the names
  (map
   (lambda (s) (point-label3d (vector-unpack (satelite-ecef s)) (satelite-name s)))
   (get-vertices satelite-graph))
  ; draw the lines visualizing which satelites have line of sight and can communicate
  (for/list ([line-list satelite-lines])
    (for/list ([line-pair line-list])
      (match line-pair
        [(cons (struct vector (x1 y1 z1)) (struct vector (x2 y2 z2)))
         (lines3d (list (list x1 y1 z1) (list x2 y2 z2)))]))))
 #:title (car satelite-points)
 #:altitude 25)