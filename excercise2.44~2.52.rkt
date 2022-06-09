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

;(define (make-vect xcor ycor)
;  (list xcor ycor))

;(define (xcor-vect v)
;  (car v))

;(define (ycor-vect v)
;  (cadr v))

;(define (add-vect vect1 vect2)
;  (make-vect (+ (xcor-vect vect1)
;                (xcor-vect vect2))
;             (+ (ycor-vect vect1)
;                (ycor-vect vect2))))

;(define (sub-vect vect1 vect2)
;  (make-vect (- (xcor-vect vect1)
;                (xcor-vect vect2))
;             (- (ycor-vect vect1)
;                (ycor-vect vect2))))

;(define (scale-vect factor vect)
;  (make-vect (* factor (xcor-vect vect))
;             (* factor (ycor-vect vect))))

;; Excercise 2.47:
;; Here are two possible constructors for frames:

;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))

;(define (make-frame-al origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))

;; For each constructor supply the appropriate selectors to produce
;; an implementation for frames.

;(define (origin-frame f)
;    (car f))

;(define (edge1-frame f)
;    (cadr f))

;(define (edge2-frame f)
;    (caddr f))

;(define (origin-frame-al f)
;    (car f))

;(define (edge1-frame-al f)
;    (cadr f))

;(define (edge2-frame-al f)
;    (cddr f))

;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect
;     (origin-frame frame)
;     (add-vect (scale-vect (xcor-vect v)
;                           (edge1-frame frame))
;               (scale-vect (ycor-vect v)
;                           (edge2-frame frame))))))

;; Excercise 2.48:
;; A directed line segment in the plane can be
;; represented as a pair of vectors—the vector running from the
;; origin to the start-point of the segment, and the vector running
;; from the origin to the end-point of the segment. Use your vector
;; representation from Exercise 2.46 to define a representation
;; for segments with a constructor make-segment and selectors
;; start-segment and end-segment.

;(define (make-segment start end)
;    (list start end))

;(define (start-segment s)
;    (car s))

;(define (end-segment s)
;    (cadr s))

;; Excercise 2.49:
;; Use segments->painter to define the following
;; primitive painters:

;; a. The painter that draws the outline of the designated frame.
;; b. The painter that draws an “X” by connecting opposite corners
;;    of the frame.
;; c. The painter that draws a diamond shape by connecting the midpoints
;;    of the sides of the frame.
;; d. The wave painter.

;; This should be done by original sicp package picture language;

(define top-left (make-vect 0.0 1.0))
(define top-right (make-vect 1.0 1.0))
(define bottom-left (make-vect 0.0 0.0))
(define bottom-right (make-vect 1.0 0.0))

;; a)
(define top (make-segment top-left top-right))
(define left (make-segment top-left bottom-left))
(define right (make-segment top-right bottom-right))
(define bottom (make-segment bottom-left bottom-right))


(define rect (segments->painter (list top bottom left right)))

;; b)
(define left-top-to-right-bottom (make-segment top-left
                                               bottom-right))

(define right-top-to-left-bottom (make-segment top-right
                                               bottom-left))

(define cross (segments->painter (list left-top-to-right-bottom right-top-to-left-bottom)))

;; c)
(define top-mid-point (make-vect 0.5 1.0))

(define bottom-mid-point (make-vect 0.5 0.0))

(define left-mid-point (make-vect 0.0 0.5))

(define right-mid-point (make-vect 1.0 0.5))

(define top-to-left (make-segment top-mid-point
                                  left-mid-point))

(define top-to-right (make-segment top-mid-point
                                   right-mid-point))

(define bottom-to-left (make-segment bottom-mid-point
                                     left-mid-point))

(define bottom-to-right (make-segment bottom-mid-point
                                      right-mid-point))

(define square (segments->painter (list top-to-left top-to-right bottom-to-left bottom-to-right)))

;; d)
;; omitted

;; Excercise 2.50:
;; Define the transformation flip-horiz, which flips
;; painters horizontally, and transformations that rotate painters
;; counterclockwise by 180 degrees and 270 degrees.

(define (flip-horiz painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))
                       
(define (rotate180 painter)
    (transform-painter painter
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0)))

(define (rotate270 painter)
    (transform-painter painter
                       (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

;; Excercise 2.51:
;; Define the below operation for painters. Below takes
;; two painters as arguments. The resulting painter, given a frame,
;; draws with the first painter in the bottom of the frame and with the
;; second painter in the top. Define below in two different ways—first
;; by writing a procedure that is analogous to the beside procedure
;; given above, and again in terms of beside and suitable rotation
;; operations (from Exercise 2.50).

(define (below1 painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
        (let ((paint-top
                (transform-painter painter2
                                   split-point
                                   (make-vect 1.0 0.5)
                                   (make-vect 0.0 1.0)))
              (paint-down
                (transform-painter painter1
                                   (make-vect 0.0 0.0)
                                   (make-vect 1.0 0.0)
                                   split-point)))
            (lambda (frame)
                (paint-top frame)
                (paint-down frame)))))

(define (below2 painter1 painter2)
    (lambda (frame)
        ((flip-horiz
            (rotate90
                (beside
                    (rotate270
                        (flip-horiz painter1))
                    (rotate270
                        (flip-horiz painter2)))))
         frame)))

;; Excercise 2.52:
;; Make changes to the square limit of wave shown in
;; Figure 2.9 by working at each of the levels described above.
;; In particular:

;; a. Add some segments to the primitive wave painter of Exercise
;;    2.49 (to add a smile, for example).
;; b. Change the pattern constructed by corner-split (for example,
;;    by using only one copy of the up-split and right-split images
;;    instead of two).
;; c. Modify the version of square-limit that uses square-of-four
;;    so as to assemble the corners in a different pattern. (For example,
;;    you might make the big Mr. Rogers look outward from each
;;    corner of the square.)

;; a)
;; ommitted;

;; b)
(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1)))
              (corner (corner-split painter (- n 1))))
            (beside (below painter up)
                    (below right corner)))))

;; c)
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
    (let ((combine4 (square-of-four identity flip-horiz flip-vert rotate180)))
        (combine4 (corner-split painter n))))