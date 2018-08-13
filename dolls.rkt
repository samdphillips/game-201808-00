#lang racket/base

(require racket/class
         racket/draw
         racket/match

         threading
         (only-in math pi)

         "bbox.rkt")

(struct draw (render bbox)
  #:transparent
  ; #;#;#;
  #:methods gen:custom-write
  [(define (write-proc v outp mode)
     (if (port-writes-special? outp)
         (write (draw->bitmap v) outp)
         (write "#<draw>" outp)))])

(define (draw->bitmap d)
  (match-define (draw render (bbox x0 y0 x1 y1)) d)
  (let* ([width (inexact->exact (ceiling (- x1 x0)))]
         [height (inexact->exact (ceiling (- y1 y0)))]
         [bm (make-object bitmap% width height)]
         [dc (send bm make-dc)])
    (send dc translate (- x0) (- y0))
    (render dc)
    bm))

(define (draw-rotate r d)
  (match-define (draw p box) d)
  (draw (lambda (dc)
          (let ([m (send dc get-transformation)])
            (send dc rotate r)
            (p dc)
            (send dc set-transformation m)))
        (bbox-rotate r box)))

(define (draw-translate tx ty d)
  (match-define (draw p (bbox x0 y0 x1 y1)) d)
  (draw (lambda (dc)
          (let ([m (send dc get-transformation)])
            (send dc translate tx ty)
            (p dc)
            (send dc set-transformation m)))
        (bbox (+ x0 tx) (+ y0 ty) (+ x1 tx) (+ y1 ty))))

(define draw-over
  (case-lambda
    [(d0 d1) (match-define (draw p0 a) d0)
             (match-define (draw p1 b) d1)
             (draw (lambda (dc) (p0 dc) (p1 dc))
                   (bbox-merge a b))]
    [(d0 d1 . rest)
     (apply draw-over (draw-over d0 d1) rest)]))

(define (draw-pivot dc x y)
  (send dc draw-ellipse
        (- x 2.5) (- y 2.5) 5 5))

(define (draw-body dc)
  (let ([r (* 0.25 50)])
    (send dc draw-rounded-rectangle 0 0 50 100)
    (draw-pivot dc r (- 100 r))
    (draw-pivot dc (- 50 r) (- 100 r))))

(define (draw-leg dc)
  (send dc draw-rounded-rectangle 0 0 20 100)
  (draw-pivot dc 10 (* 0.25 20)))

(define (d->r d)
  (/ (* d 2 pi) 360))

(define (draw-section bm x0 y0 x1 y1)
  (lambda (dc)
    (send dc draw-bitmap-section bm 0 0 x0 y0 (- x1 x0) (- y1 y0))))

(define bender-atlas
  (make-object bitmap% "bender.png" 'png/alpha))

(define bender-torso
  (draw (draw-section bender-atlas 0 0 278 713)
        (bbox 0 0 278 713)))

(define bender-arm-lr
  (draw (draw-section bender-atlas 445 199 643 418)
        (bbox 0 0 (- 643 445) (- 418 199))))

(define bender-arm-ur
  (draw (draw-section bender-atlas 445 0 643 199)
        (bbox 0 0 198 199)))

(define bender-arm-r (draw-over
                      bender-arm-ur
                      (~>> bender-arm-lr
                           (draw-translate (- 447 569) (- 199 234))
                           (draw-rotate (d->r -50))
                           (draw-translate 143 162))))

(define bender-arm-ul
  (draw (draw-section bender-atlas 279 0 447 197)
        (bbox 0 0 (- 447 279) 197)))

(define bender-arm-ll
  (draw (draw-section bender-atlas 279 197 447 417)
        (bbox 0 0 (- 447 279) (- 417 197))))

(define bender-arm-l (draw-over
                      bender-arm-ul
                      (~>> bender-arm-ll
                           (draw-translate (- 279 323) (- 197 240))
                           (draw-rotate (d->r -20))
                           (draw-translate (- 323 279) 169))))

(draw-over
 (~>> bender-arm-r
      (draw-translate -68 -52)
      (draw-rotate (d->r 25))
      (draw-translate 222 450))
 bender-torso
 (~>> bender-arm-l
      (draw-translate (- 279 397) -47)
      (draw-rotate (d->r 0))
      (draw-translate 44 468)))
