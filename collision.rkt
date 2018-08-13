#lang typed/racket/base

(require racket/list
         racket/match)

(struct vec ([x : Real] [y : Real]) #:transparent)

(: dot (vec vec -> Real))
(define (dot a b)
  (match-define (vec ax ay) a)
  (match-define (vec bx by) b)
  (+ (* ax bx) (* ay by)))

(: scale (Real vec -> vec))
(define (scale s a)
  (match-define (vec x y) a)
  (vec (* s x) (* s y)))  

(: centroid ((Listof vec) -> vec))
(define (centroid a*)
  (define-values (xsum ysum n)
    (for/fold ([xsum : Real 0] [ysum : Real 0] [n : Integer 0])
              ([a (in-list a*)])
      (match-define (vec x y) a)
      (values (+ xsum x) (+ ysum y) (add1 n))))
  (vec (/ xsum n) (/ ysum n)))

(: orthogonal (vec -> vec))
(define (orthogonal a)
  (match-define (vec x y) a)
  (vec y (- x)))

(: vertices->edges ((Listof vec) -> (Listof vec)))
(define (vertices->edges v*)
  (define w* (append (rest v*) (list (first v*))))
  (for/list ([a (in-list v*)]
             [b (in-list w*)])
    (vec (- (vec-x b) (vec-x a))
       (- (vec-y b) (vec-y a)))))

(: project (vec (Listof vec) -> (Values Real Real)))
(define (project a v*)
  (for/fold ([u : Real +inf.0] [v : Real -inf.0]) ([b (in-list v*)])
    (let ([p (dot a b)])
      (values (min u p) (max v p)))))

#|
Test that a separates s* and t*.
  - If it does return false.
  - If it doesn't return the displacement vector.
|#
(: separating-axis-vector (vec (Listof vec) (Listof vec) -> (Option vec)))
(define (separating-axis-vector a s* t*)
  (let-values ([(smin smax) (project a s*)]
               [(tmin tmax) (project a t*)])
    (and (>= smax tmin)
         (>= tmax smin)
         (let* ([d (min (- tmax smin) (- smax tmin))]
                [d (/ d (dot a a))])
           (scale d a)))))        

(: is-collision? ((Listof vec) (Listof vec) -> (Option vec)))
(define (is-collision? a* b*)
  (define axis*
    (map orthogonal (append (vertices->edges a*)
                            (vertices->edges b*))))
  (let/ec break : (Option vec)
    (let ([exit (lambda () (break #f))])
      (define push-v* : (Listof vec)
        (for/list ([axis (in-list axis*)])
          (or (separating-axis-vector axis a* b*) (exit))))

      (define mpv
        (argmin (lambda ([u : vec]) (dot u u)) push-v*))

      (define d
        (match-let ([(vec ax ay) (centroid a*)]
                    [(vec bx by) (centroid b*)])
          (vec (- bx ax) (- by ay))))

      (if (> (dot mpv d) 0)
          (match-let ([(vec x y) mpv]) (vec (- x) (- y)))
          mpv))))

#|
(define A (list (vec 1 1) (vec 5 1) (vec 1 5)))
(define B (list (vec 6 1) (vec 1 6) (vec 5 10)))
(define C (list (vec 0 4) (vec 2 2) (vec 5 10)))

(require typed/racket/class typed/racket/draw)
(is-collision? C A)
(let* ([bm (make-object bitmap% 500 500)]
       [dc (send bm make-dc)])
  (send dc set-brush "white" 'transparent)
  (send dc draw-polygon
        (map (lambda ([i : vec])
               (cons (* 50 (vec-x i))
                     (* 50 (vec-y i))))
             A))
  (send dc draw-polygon
        (map (lambda ([i : vec])
               (cons (* 50 (vec-x i))
                     (* 50 (vec-y i))))
             C))
  bm)
|#