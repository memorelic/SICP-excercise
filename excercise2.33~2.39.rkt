#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Excercise 2.33:
;; Fill in the missing expressions to complete the following
;; definitions of some basic list-manipulation operations as
;; accumulations:
;; 1.map
;; 2.append
;; 3.length

(define (accumap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (accumappend seq1 seq2)
  (accumulate cons seq2 seq1))

(define (accumlength sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; Excercise 2.34:
;; Evaluating a polynomial in x at a given value of x can
;; be formulated as an accumulation. We evaluate the polynomial

;; a_n*x^n + a_(n-1)*x^(n-1) + ... + a_1*x + a_0;

;; using a well-known algorithm called Horner’s rule, which structures
;; the computation as

;; (...(a_n*x + a_(n-1))*x + ... + a_1)*x + a_0;

;; In other words, we start with a_n, multiply by x,
;; add a_(n-1), multiply by x, and so on, until we reach a_0.
;; Fill in the following template to produce a procedure that evaluates
;; a polynomial using Horner’s rule. Assume that the coefficients of
;; the polynomial are arranged in a sequence, from a0 through an.

;(define (horner-eval x coefficient-sequence)
;  (accumulate (lambda (this-coeff higher-terms) <??>)
;              0
;              coefficient-sequence))

;; For example, to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would
;; evaluate

;; (horner-eval 2 (list 1 3 0 5 0 1))
;; => 79

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; Excercise 2.35:
;; Redefine count-leaves from Section 2.2.2 as an accumulation:

(define (count-leaves t)
  (accumulate + 0 (map (lambda (subtree)
                         (if (pair? subtree)
                             (count-leaves subtree)
                             1))
                       t)))

;; another idea: fringe tree to list, use accumulate to

; (count-leaves (list (list 1 (list 2 3)) (list (list 4 5) (list 6 7))))

;; Excercise 2.36:
;; The procedure accumulate-n is similar to accumulate
;; except that it takes as its third argument a sequence of
;; sequences, which are all assumed to have the same number of
;; elements. It applies the designated accumulation procedure to
;; combine all the first elements of the sequences, all the second
;; elements of the sequences, and so on, and returns a sequence
;; of the results. For instance, if s is a sequence containing four
;; sequences, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the
;; value of (accumulate-n + 0 s) should be the sequence (22 26
;; 30). Fill in the missing expressions in the following definition of

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             '()
             (cons (accumulate op init (map car seqs))
                   (accumulate-n op init (map cdr seqs)))))
             