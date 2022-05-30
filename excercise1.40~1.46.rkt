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

(define (average v1 v2)
  (/ (+ v1 v2) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

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

;; Excercise 1.45:
;; We saw in Section 1.3.3 that attempting to compute
;; square roots by naively finding a fixed point of y |-> x/y does not
;; converge, and that this can be fixed by average damping. The same
;; method works for finding cube roots as fixed points of the average damped y |-> x/y^2.
;; Unfortunately, the process does not work for
;; fourth roots—a single average damp is not enough to make a fixedpoint search for y
;; y |-> x/y^3 converge.
;; On the other hand, if we average damp twice(i.e.,use the average damp of the average damp of
;; y |→ x/y^3) the fixed-point search does converge. Do some experiments to determine 
;; how many average damps are required to compute nth roots
;; as a fixed-point search based upon repeated average damping of y |-> x/y^(n-1)
;; Use this to implement a simple procedure for computing n-th roots using fixed-point, average-damp,
;; and the repeated procedure of Excercise 1.43. Assume that any arithmetic operations you need are
;; available as primitives.

(define (expt base n)
  (if (= n 0)
      1
      ((repeated (lambda (x) (* base x)) n) 1)))

(define (average-damp-n-times f n)
  ((repeated average-damp n) f))

;; (define (nth-root x nth)
;;  (fixed-point 
;;    ((repeated average-damp (floor (log nth 2))) 
;;    (lambda (y) 
;;      (/ x (power y (- nth 1)))))
;;   1.0))

(define (damped-nth-root n damp-times)
  (lambda (x)
    (fixed-point
     (average-damp-n-times
      (lambda (y)
        (/ x (expt y (- n 1))))
      damp-times)
     1.0)))

;; n 次方根	        1	2	3	4	5	6	7	8	...	15	16	...	31	32	...
;; 收敛所需的平均阻尼次数	1	1	1	2	2	2	2	3	...	3	4	...	4	5	...
;; 计算n次方根的不动点收敛，最少需要 log2(n) 向下取整 次平均阻尼

(define (damped-times n)
  (floor (log n 2)))

(define (nth-root n)
  (damped-nth-root n (damped-times n)))

;(define sqrt (nth-root 2))
;(sqrt (* 3 3))

;; Excercise 1.46:
;; Several of the numerical methods described in
;; this chapter are instances of an extremely general computational
;; strategy known as iterative improvement. Iterative improvement
;; says that, to compute something, we start with an initial guess
;; for the answer, test if the guess is good enough, and otherwise
;; improve the guess and continue the process using the improved
;; guess as the new guess. Write a procedure iterative-improve
;; that takes two procedures as arguments: a method for telling
;; whether a guess is good enough and a method for improving a
;; guess. Iterative-improve should return as its value a procedure
;; that takes a guess as argument and keeps improving the guess
;; until it is good enough. Rewrite the sqrt procedure of Section
;; 1.1.7 and the fixed-point procedure of Section 1.3.3 in terms of
;; iterative-improve.

(define (iterative-improve close-enough? improve)
    (lambda (first-guess)
        (define (try guess)
            (let ((next (improve guess)))
                (if (close-enough? guess next)
                    next
                    (try next))))
        (try first-guess)))

;(define (fixed-point f first-guess)
;    (define tolerance 0.00001)
;    (define (close-enough? v1 v2)
;        (< (abs (- v1 v2)) tolerance))
;    (define (improve guess)
;        (f guess))
;    ((iterative-improve close-enough? improve) first-guess))

;(define (sqrt x)
;    (define dx 0.00001)
;    (define (close-enough? v1 v2)
;        (< (abs (- v1 v2)) dx))
;  (define (average x y)
;        (/ (+ x y) 2))
;    (define (improve guess)
;        (average guess (/ x guess))) 
;    ((iterative-improve close-enough? improve) 1.0))