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


;; Excercise 2.37:
;; Suppose we represent vectors v = (v_i) as sequences
;; of numbers, and matrices m = (m_ij ) as sequences of vectors (the
;; rows of the matrix). For example, the matrix

;; 1 2 3 4
;; 4 5 6 6
;; 6 7 8 9

;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8
;; 9)). With this representation, we can use sequence operations to
;; concisely express the basic matrix and vector operations. These
;; operations (which are described in any book on matrix algebra)
;; are the following:

;; (dot-product v w) returns the sum sum(vi*wi) ,
;; (matrix-*-vector m v) returns the vector t, where t_i = sum(m_ij*v_j) ,
;; (matrix-*-matrix m n) returns the matrix p, where p_ij = sum(m_ik*n_kj) ,
;; (transpose m) returns the matrix n, where n_ij = m_ji .

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for computing
;; the other matrix operations. (The procedure accumulate-n
;; is defined in Exercise 2.36.)

(define (matrix-*-vector m v)
  (map (lambda (col)
         (dot-product col v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (col-of-m)
           (map (lambda (col-of-cols)
                  (dot-product col-of-m
                               col-of-cols))
                cols))
         m)))

(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

;; Excercise 2.38:
;; The accumulate procedure is also known as
;; fold-right, because it combines the first element of the sequence
;; with the result of combining all the elements to the right.
;; There is also a fold-left, which is similar to fold-right, except
;; that it combines elements working in the opposite direction:

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; What are the values of:

;; (fold-right / 1 (list 1 2 3)) => 3/2
;; (fold-left / 1 (list 1 2 3))  => 1/6
;; (fold-right list nil (list 1 2 3))
;; (fold-left list nil (list 1 2 3))

;; Give a property that op should satisfy to guarantee that fold-right
;; and fold-left will produce the same values for any sequence

;; If two result produced by fold-right and fold-left are the same, then the procedure which
;; transfered as a argument must be satisfy Commutative property(交换律)

;; Excercise 2.39:
;; Complete the following definitions of reverse
;; (Exercise 2.18) in terms of fold-right and fold-left from
;; Exercise 2.38.

(define (right-reverse sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              '()
              sequence))

(define (left-reverse sequence)
  (fold-left (lambda (x y)
               (cons y x))
             '()
             sequence))





