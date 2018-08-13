#lang racket/base

(require racket/match)

(module+ test
  (require rackunit))

(struct bbox (x0 y0 x1 y1) #:transparent)

(define (bbox-merge a b)
  (match-define (bbox ax0 ay0 ax1 ay1) a)
  (match-define (bbox bx0 by0 bx1 by1) b)
  (bbox (min ax0 bx0)
        (min ay0 by0)
        (max ax1 bx1)
        (max ay1 by1)))

(module+ test
  (let ([a (bbox -10 -10 10 10)]
        [b (bbox -5 -5 5 5)]
        [c (bbox -10 -15 5 15)])
    (check-equal? (bbox-merge a a) a)
    (check-equal? (bbox-merge a b) a)
    (check-equal? (bbox-merge a c) (bbox -10 -15 10 15))))

(define (bbox-rotate r box)
  (match-define (bbox x0 y0 x1 y1) box)
  (define-values (nx0 ny0 nx1 ny1)
    (let ([s (sin r)]
          [c (cos r)])
      (for*/fold ([xmin #f] [ymin #f] [xmax #f] [ymax #f])
                 ([x (list x0 x1)]
                  [y (list y0 y1)])
        (let ([xp (+ (* c x) (* s y))]
              [yp (+ (* (- s) x) (* c y))])
          (define-syntax-rule (minv A B) (if A (min A B) B))
          (define-syntax-rule (maxv A B) (if A (max A B) B))
          (values (minv xmin xp)
                  (minv ymin yp)
                  (maxv xmax xp)
                  (maxv ymax yp))))))
  (bbox nx0 ny0 nx1 ny1))

(provide (struct-out bbox)
         bbox-merge
         bbox-rotate)