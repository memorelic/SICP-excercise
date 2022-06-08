#lang racket
(require sicp-pict)

;; Excercise 2.44:
;; Define the procedure up-split used by cornersplit.
;; It is similar to right-split, except that it switches the roles
;; of below and beside.

(define (up-split painter n)
  (if (= n 0)
      painter
       (let ((smaller (up-split painter (- n 1))))
         (below painter (beside smaller smaller)))))

;; Excercise 2.45:
;; Right-split and up-split can be expressed as instances
;; of a general splitting operation. Define a procedure split
;; with the property that evaluating

;(define right-split (split beside below))
;(define up-split2 (split below beside))

;; produces procedures right-split and up-split with the same
;; behaviors as the ones already defined.

(define (split origin-placer split-placer)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split origin-placer split-placer) painter (- n 1))))
          (origin-placer painter (split-placer smaller smaller))))))

(define right-split (split beside below))
(define up-split2 (split below beside))

;; Excercise 2.46:
;; A two-dimensional vector v running from the
;; origin to a point can be represented as a pair consisting of an
;; x-coordinate and a y-coordinate. Implement a data abstraction
;; for vectors by giving a constructor make-vect and corresponding
;; selectors xcor-vect and ycor-vect. In terms of your selectors
;; and constructor, implement procedures add-vect, sub-vect, and
;; scale-vect that perform the operations vector addition, vector
;; subtraction, and multiplying a vector by a scalar:

;; (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2),
;; (x1, y1) - (x2, y2) = (x1 - x2, y1 - y2),
;; s · (x, y) = (sx, sy).