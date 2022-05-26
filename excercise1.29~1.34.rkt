#lang racket

;; Excercise 1.29:
;; Simpson’s Rule is a more accurate method of numerical
;; integration than the method illustrated above. Using Simpson’s
;; Rule, the integral of a function f between a and b is approximated
;; as:

;; (h/3) * (y0 + 4y1 + 2y2 + 4y3 + 2y4 + . . . + 2y_(n-2) + 4y_(n-1) + y_n),

;; where h = (b - a)/n, for some even integer n, and yk = f (a + kh).
;; (Increasing n increases the accuracy of the approximation.) Define
;; a procedure that takes as arguments f , a, b, and n and returns the
;; value of the integral, computed using Simpson’s Rule. Use your
;; procedure to integrate cube between 0 and 1 (with n = 100 and n =
;; 1000), and compare the results to those of the integral procedure
;; shown above.

;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

(define (round-to-next-even x)
  (+ x (remainder x 2)))

(define (simpson-integral f a b n)
  (define (inc x) (+ x 1))
  (define fixed-n (round-to-next-even n))
  (define h (/ (- b a) fixed-n))
  (define (simpson-term nth)
    (let ((yk (f (+ a (* nth h)))))
      (cond [(or (= 0 nth) (= nth fixed-n)) (* 1 yk)]
            [(even? nth) (* 2 yk)]
            [else (* 4 yk)])))
  (* (/ h 3) (sum simpson-term 0 inc fixed-n)))

(define (cube x) (* x x x))

;(simpson-integral cube 0 1 100) 
;(simpson-integral cube 0 1 1000)

;; Excercise 1.30:
;; The sum procedure above generates a linear recursion.
;; The procedure can be rewritten so that the sum is performed
;; iteratively.
;; Show how to do this by filling in the missing expressions
;; in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Excercise 1.31:
;; a):
;; The sum procedure is only the simplest of a vast number of
;; similar abstractions that can be captured as higher-order
;; procedures. Write an analogous procedure called product
;; that returns the product of the values of a function at points
;; over a given range. Show how to define factorial in terms of
;; product. Also use product to compute approximations to 1⁄4
;; using the formula

;; PI/4 = 2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7 ...

;; b):
;; If your product procedure generates a recursive process, write
;; one that generates an iterative process. If it generates an iterative
;; process, write one that generates a recursive process.
      
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (product-iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (fact n)
  (product-iterative (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (PI-approximate n)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (* 4.0 (product-iterative term 1 (lambda (x) (+ x 1)) n)))

;; Excercise 1.32:
;; a):
;; Showthat sum and product (Exercise 1.31) are both special cases
;; of a still more general notion called accumulate that combines a
;; collection of terms, using some general accumulation function:

;; (accumulate combiner null-value term a next b)

;; Accumulate takes as arguments the same term and range specifications
;; as sum and product, together with a combiner procedure
;; (of two arguments) that specifies how the current term is
;; to be combined with the accumulation of the preceding terms
;; and a null-value that specifies what base value to use when
;; the terms run out. Write accumulate and show how sum and
;; product can both be defined as simple calls to accumulate.

;; b):
;; If your accumulate procedure generates a recursive process,
;; write one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.

(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recursive combiner
                                      null-value
                                      term
                                      (next a)
                                      next
                                      b))))

(define (accumulate-iterative combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-sum term a next b)
  (accumulate-iterative + 0 term a next b))

(define (accumulate-product term a next b)
  (accumulate-iterative * 1 term a next b))

(define (accumualte-fact n)
  (accumulate-product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

;; You can obtain an even more general version of
;; accumulate (Exercise 1.32) by introducing the notion of a filter
;; on the terms to be combined. That is, combine only those terms
;; derived from values in the range that satisfy a specified condition.
;; The resulting filtered-accumulate abstraction takes the same
;; arguments as accumulate, together with an additional predicate
;; of one argument that specifies the filter. Write filteredaccumulate
;; as a procedure. Show how to express the following
;; using filtered-accumulate:
;; a):
;; the sum of the squares of the prime numbers in the interval a to
;; b (assuming that you have a prime? predicate already written)
;; b):
;; the product of all the positive integers less than n that are
;; relatively prime to n (i.e., all positive integers i < n such that
;; GCD(i , n) = 1).

 (define (filtered-accumulate combiner null-value term a next b filter) 
  (if (> a b) null-value 
      (if (filter a) 
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter)) 
          (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filter)))))

;; Excercise 1.34:
;; Suppose we define the procedure:

(define (f g) (g 2))

;; The we have:
;; (f square)
;; => 4
;; (f (lambda (z) (* z (+ z 1)))
;; => 6

;; What happens if we (preversely) ask the interpreter to evaluate the
;; combination (f f)? Explain.

;; (f f) => (f 2) => (2 2)
;; Error:
;;        application: not a procedure;
;;        expected a procedure that can be applied to arguments
;;        given: 2