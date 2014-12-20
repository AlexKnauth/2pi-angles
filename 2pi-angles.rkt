#lang racket/base

(require 2htdp/image
         racket/list
         racket/match
         racket/math
         racket/format
         (for-syntax racket/base
                     ))

(define-syntax (.... stx)
  (syntax/loc stx (error "....")))

(define (flatten* . args)
  (flatten args))

(define 2pi (* 2 pi))

(define transparent "transparent")

(define (draw-unit-circle-with-radians-in-terms-of-2pi size)
  (define width size)
  (define height size)
  (define ctr-x (* 1/2 width))
  (define ctr-y (* 1/2 height))
  (define circle-radius (* 1/4 size))
  (define line-length (* 11/8 circle-radius))
  (define circle-mode "outline")
  (define circle-color "black")
  (define line-color "black")
  (define text-color "black")
  (define (mx->cx my-x) (+ my-x ctr-x))
  (define (my->cy my-y) (+ (- my-y) ctr-y))
  (define mts
    (empty-scene width height))
  (define circle-img
    (circle circle-radius circle-mode circle-color))
  (define mts+circle
    (underlay mts circle-img))
  (define angle-fractions
    (remove-duplicates
     (flatten*
      (for*/list ([n (in-list '(1 2 3 4 6 8 12))]
                  [i (in-range n)])
        (/ i n)))))
  (for/fold ([img mts+circle])
            ([frac (in-list angle-fractions)])
    (define angle (* frac 2pi)) ; radians
    (define sz (/ (+ line-length (* 10 (log (/ (denominator frac)))))
                  circle-radius))
    (define cos-a (cos angle))
    (define sin-a (sin angle))
    (define line.r (* sz circle-radius))
    (define text.r
      (+ line.r (* 1/5 circle-radius)))
    (define img+line
      (scene+line img
                  ctr-x ctr-y
                  (mx->cx (* line.r cos-a)) (my->cy (* line.r sin-a))
                  line-color))
    (define text-str
      (cond [(= 0 frac) "0, 2π"]
            [else (format "~a*2π" frac)]))
    (define 2pi-size (exact-round (* 1/10 line.r)))
    (define frac-size (exact-round (* 1/10 line.r)))
    (define text-img
      (cond [(= 0 frac) (text "0, 2π" 2pi-size text-color)]
            [else (define n-img (text (~v (numerator frac)) frac-size text-color))
                  (define d-img (text (~v (denominator frac)) frac-size text-color))
                  (beside (above n-img
                                 (line (max (image-width n-img) (image-width d-img)) 0 text-color)
                                 (line 0 1 transparent)
                                 d-img)
                          (line 1 0 transparent)
                          (text "2π" 2pi-size text-color))]))
    (place-image text-img
                 (mx->cx (* text.r cos-a)) (my->cy (* text.r sin-a))
                 img+line)))

(module+ test
  (draw-unit-circle-with-radians-in-terms-of-2pi 500))
