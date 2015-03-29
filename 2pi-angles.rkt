#lang sweet-exp racket/base

require 2htdp/image
        racket/list
        racket/match
        racket/math
        racket/format
        syntax/parse/define
        my-cond/iffy
        sweet-exp-utils/def
        for-syntax racket/base

def 2pi = {2 * pi}

def transparent = "transparent"

def default-angle-fractions =  ;'(0 1/2 1/3 2/3 1/4 3/4 1/6 5/6 1/8 3/8 5/8 7/8 1/12 5/12 7/12 11/12)
  remove-duplicates
    for*/list ([d (in-list '(1 2 3 4 6 8 12))] [n (in-range d)])
      {n / d}

define
  draw-unit-circle-with-radians-in-terms-of-2pi
    #:size size
    #:angle-fractions [angle-fractions default-angle-fractions]
    #:2pi-text [2pi-text "2π"] ; other possible values could be "τ" or "4η", etc.
  def width = size
  def height = size
  def ctr-x = {1/2 * width}
  def ctr-y = {1/2 * height}
  def circle-radius = {1/4 * size}
  def line-length = {11/8 * circle-radius}
  def circle-mode = "outline"
  def circle-color = "black"
  def line-color = "black"
  def text-color = "black"
  def mts = empty-scene[width height]
  def circle-img = circle[circle-radius circle-mode circle-color]
  def mts+circle = underlay[mts circle-img]
  def mx->cx(my-x) = {my-x + ctr-x}
  def my->cy(my-y) = {(- my-y) + ctr-y}
  for/fold ([img mts+circle]) ([frac (in-list angle-fractions)])
    def angle = {frac * 2pi} ; radians
    def sz = {{line-length + {10 * log{1 / (denominator frac)}}} / circle-radius}
    def cos-a = cos(angle)
    def sin-a = sin(angle)
    def line.r = {sz * circle-radius}
    def text.r = {line.r + {1/5 * circle-radius}}
    def 2pi-size = exact-round{1/10 * line.r}
    def frac-size = exact-round{1/10 * line.r}
    def img+line =
      scene+line[img
                 ctr-x ctr-y
                 mx->cx{line.r * cos-a} my->cy{line.r * sin-a}
                 line-color]
    def text-img =
      my-cond
        if {frac = 0}
          text[format("0, ~a" 2pi-text) 2pi-size text-color]
        else
          def n-img = text[~v(numerator(frac)) frac-size text-color]
          def d-img = text[~v(denominator(frac)) frac-size text-color]
          def frac-img =
            above(n-img
                  line[max[image-width(n-img) image-width(d-img)] 0 text-color]
                  line[0 1 transparent]
                  d-img)
          def 2pi-img = text[2pi-text 2pi-size text-color]
          beside(frac-img line[1 0 transparent] 2pi-img)
    place-image[text-img
                mx->cx{text.r * cos-a} my->cy{text.r * sin-a}
                img+line]

module+ test
  draw-unit-circle-with-radians-in-terms-of-2pi[#:size 500]
