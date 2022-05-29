#lang racket

(define dx 0.00001)
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; Excercise 1.40:
;; Define a procedure cubic that can be used together
;; with the newtons-method procedure in expressions of the form

;; (newtons-method (cubic a b c) 1)

;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.

(define (cubic a b c)
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

;; Excercise 1.41:
;; Define a procedure double that takes a procedure
;; of one argument as argument and returns a procedure that applies
;; the original procedure twice. For example, if inc is a procedure
;; that adds 1 to its argument, then (double inc) should be a procedure
;; that adds 2. What value is returned by

;; (((double (double double)) inc) 5)

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

;; (double double) is a procedure that return a procedure that applies its parameter four times:

;; ((double f) x)→ (f (f x))
;; (((double double) f) x) → ((double (double f)) x)

;; this is a procedure that apply f 2×2×2×2=16 times
;; so the result will be 21;

;; Excercise 1.42:
;; Let f and g be two one-argument functions. The
;; composition f after g is defined to be the function x |-> f (g (x)).
;; Define a procedure compose that implements composition. For example,
;; if inc is a procedure that adds 1 to its argument,

;; ((compose square inc) 6) => 49

(define (compose f g)
  (lambda (x) (f (g x))))

;; Excercise 1.43:
;; If f is a numerical function and n is a positive integer,
;; then we can form the nth  repeated application of f, which
;; is defined to be the function whose value at x is f(f(...(f(x))...)).
;; For example, if f is the function x |-> x + 1, then the nth repeated
;; application of f is the function x |-> x + n. If f is the operation
;; of squaring a number, then the nth repeated application of f is the
;; function that raises its argument to the 2^n-th power.
;; Write a procedure that takes as inputs a procedure that computes f and
;; a positive integer n and returns the procedure that computes the nth
;; repeated application of f. Your procedure should be able to be
;; used as follows:

;; ((repeated square 2) 5) => 625

(define (repeated f n)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeated f (- n 1)))))

;; Excercise 1.44:
;; The idea of smoothing a function is an important
;; concept in signal processing. If f is a function and dx is some small
;; number, then the smoothed version of f is the function whose
;; value at a point x is the average of f (x - dx), f (x), and f (x + dx).
;; Write a procedure smooth that takes as input a procedure that computes
;; f and returns a procedure that computes the smoothed f .
;; It is sometimes valuable to repeatedly smooth a function (that is,
;; smooth the smoothed function, and so on) to obtain the n-fold
;; smoothed function. Show how to generate the n-fold smoothed
;; function of any given function using smooth and repeated from
;; Exercise 1.43.

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (nth-smooth f n)
  ((repeated smooth n) f))

;; 