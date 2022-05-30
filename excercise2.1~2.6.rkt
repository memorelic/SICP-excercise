#lang racket

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
          (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; Excercise 2.1:
;; Define a better version of make-rat that handles both
;; positive and negative arguments. Make-rat should normalize the
;; sign so that if the rational number is positive, both the numerator
;; and denominator are positive, and if the rational number is negative,
;; only the numerator is negative.

;; show in line 3: make-rat has been changed.

;; Excercise 2.2:
;; Consider the problem of representing line segments
;; in a plane. Each segment is represented as a pair of points:
;; a starting point and an ending point.
;; Define a constructor
;; make-segment and selectors start-segment and end-segment
;; that define the representation of segments in terms of points.
;; Furthermore, a point can be represented as a pair of numbers:
;; the x coordinate and the y coordinate. Accordingly, specify a
;; constructor make-point and selectors x-point and y-point
;; that define this representation. Finally, using your selectors and
;; constructors, define a procedure midpoint-segment that takes
;; a line segment as argument and returns its midpoint (the point
;; whose coordinates are the average of the coordinates of the
;; endpoints). To try your procedures, you’ll need a way to print
;; points:

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (define (average v1 v2) (/ (+ v1 v2) 2))
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (make-point (average (x-point p1) (x-point p2))
                (average (y-point p1) (y-point p2)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(define start (make-point 1 3))
;(define end (make-point 3 1))
;(define seg (make-segment start end))

;(print-point (midpoint-segment seg))

;; Excercise 2.3:
;; Implement a representation for rectangles in a plane.
;; (Hint: You may want to make use of Exercise 2.2.) In terms of
;; your constructors and selectors, create procedures that compute
;; the perimeter and the area of a given rectangle. Now implement
;; a different representation for rectangles. Can you design your system
;; with suitable abstraction barriers, so that the same perimeter
;; and area procedures will work using either representation?

;; define rectangle base on two points:
(define (make-rect top-left right-down)
  (cons top-left right-down))

(define (top-left rect)
  (car rect))

(define (right-down rect)
  (cdr rect))

(define (rect-height rect)
  (abs (- (y-point (top-left rect)) (y-point (right-down rect)))))

(define (rect-width rect)
  (abs (- (x-point (right-down rect)) (x-point (top-left rect)))))

(define (rect-area rect)
  (* (rect-height rect) (rect-width rect)))

(define (rect-circumference rect)
  (* 2 (+ (rect-height rect) (rect-width rect))))


;; Excercise 2.4:
;; Here is an alternative procedural representation of pairs.
;; For this representation, verify that (car (cons x y)) yields
;; x for any objects x and y.

(define (lambdacons x y)
  (lambda (m) (m x y)))

(define (lambdacar z)
  (z (lambda (p q) p)))

;; What is the corresponding definition of cdr? (Hint: To verify that
;; this works, make use of the substitution model of Section 1.1.5.)

(define (lambdacdr z)
  (z (lambda (p q) q)))


;; Excercise 2.5:
;; Show that we can represent pairs of nonnegative integers
;; using only numbers and arithmetic operations if we represent
;; the pair a and b as the integer that is the product 2^a*3^b. Give the
;; corresponding definitions of the procedures cons, car, and cdr.

(define (arithmeticons a b)
  (* (expt 2 a) (expt 3 b)))

(define (arithmeticar z)
  (if (= (remainder z 2) 0)
      (+ 1 (arithmeticar (/ z 2)))
      0))

(define (arithmeticdr z)
  (if (= (remainder z 3) 0)
      (+ 1 (arithmeticdr (/ z 3)))
      0))

;; In case representing pairs as procedures wasn’t mindboggling
;; enough, consider that, in a language that can manipulate
;; procedures, we can get by without numbers (at least insofar as
;; nonnegative integers are concerned) by implementing 0 and the
;; operation of adding 1 as

(define (inc x) (+ 1 x))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; This representation is known as Church numerals, after its inventor,
;; Alonzo Church, the logician who invented the λ-calculus.

;; Define one and two directly (not in terms of zero and add-1). (Hint:
;; Use substitution to evaluate (add-1 zero)). Give a direct definition
;; of the addition procedure + (not in terms of repeated application
;; of add-1).

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus church1 church2)
  (lambda (f) (lambda (x) ((church2 f) ((church1 f) x)))))

;(((plus one two) inc) 0)