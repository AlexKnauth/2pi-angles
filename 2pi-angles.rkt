#lang racket/base

(require 2htdp/image
         racket/list
         racket/match
         racket/math
         racket/format
         syntax/parse/define
         (for-syntax racket/base
                     ))

(define 2pi (* 2 pi))

(define transparent "transparent")

(define default-angle-fractions ;'(0 1/2 1/3 2/3 1/4 3/4 1/6 5/6 1/8 3/8 5/8 7/8 1/12 5/12 7/12 11/12)
  (remove-duplicates
   (for*/list ([n (in-list '(1 2 3 4 6 8 12))]
               [i (in-range n)])
     (/ i n))))

(define-simple-macro (defmulti [id:id expr:expr] ...)
  (begin (define id expr) ...))

(define (draw-unit-circle-with-radians-in-terms-of-2pi
         #:size size
         #:angle-fractions [angle-fractions default-angle-fractions]
         #:2pi-text [2pi-text "2π"] ; other possible values could be "τ" or "4η", etc.
         )
  (defmulti
    [width size]
    [height size]
    [ctr-x (* 1/2 width)]
    [ctr-y (* 1/2 height)]
    [circle-radius (* 1/4 size)]
    [line-length (* 11/8 circle-radius)]
    [circle-mode "outline"]
    [circle-color "black"]
    [line-color "black"]
    [text-color "black"]
    [mts (empty-scene width height)]
    [circle-img (circle circle-radius circle-mode circle-color)]
    [mts+circle (underlay mts circle-img)])
  (define (mx->cx my-x) (+ my-x ctr-x))
  (define (my->cy my-y) (+ (- my-y) ctr-y))
  (for/fold ([img mts+circle])
            ([frac (in-list angle-fractions)])
    (defmulti
      [angle (* frac 2pi)] ; radians
      [sz (/ (+ line-length (* 10 (log (/ (denominator frac)))))
             circle-radius)]
      [cos-a (cos angle)]
      [sin-a (sin angle)]
      [line.r (* sz circle-radius)]
      [text.r (+ line.r (* 1/5 circle-radius))]
      [2pi-size (exact-round (* 1/10 line.r))]
      [frac-size (exact-round (* 1/10 line.r))])
    (define img+line
      (scene+line img
                  ctr-x ctr-y
                  (mx->cx (* line.r cos-a)) (my->cy (* line.r sin-a))
                  line-color))
    (define text-img
      (cond [(= 0 frac) (text (format "0, ~a" 2pi-text) 2pi-size text-color)]
            [else (define n-img (text (~v (numerator frac)) frac-size text-color))
                  (define d-img (text (~v (denominator frac)) frac-size text-color))
                  (define frac-img
                    (above n-img
                           (line (max (image-width n-img) (image-width d-img)) 0 text-color)
                           (line 0 1 transparent)
                           d-img))
                  (define 2pi-img (text 2pi-text 2pi-size text-color))
                  (beside frac-img (line 1 0 transparent) 2pi-img)]))
    (place-image text-img
                 (mx->cx (* text.r cos-a)) (my->cy (* text.r sin-a))
                 img+line)))

(module+ test
  (draw-unit-circle-with-radians-in-terms-of-2pi #:size 500))
