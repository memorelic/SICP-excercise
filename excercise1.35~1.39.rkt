#lang racket

;; Excercise 1.35:
;; Show that the golden ratio Φ (Section 1.2.2) is a fixed
;; point of the transformation x |-> 1 + 1/x, and use this fact to compute
;; Φ by means of the fixed-point procedure.

;; x = 1 + 1/x => x^2 - x - 1 = 0
;; x = (1 + √5) / 2 == Φ

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; Excercise 1.36:
;; Modify fixed-point so that it prints the sequence
;; of approximations it generates, using the newline and display
;; primitives shown in Exercise 1.22. Then find a solution to
;; X^X = 1000 by finding a fixed point of x |-> log(1000)/log(x).
;; (Use Scheme’s primitive log procedure, which computes natural
;; logarithms.) Compare the number of steps this takes with and
;; without average damping. (Note that you cannot start fixedpoint
;; with a guess of 1, as this would cause division by log(1) = 0.)

(define (average a b)
  (/ (+ a b) 2))

;;(display (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))
;;(newline)
;;(display (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))

;; Excercise 1.37:
;; a):
;; An infinite continued fraction is an expression of the form

;; f = (N1 / (D1 + (N2 / (D2 + (N3 / D3 + ...))))...

;; As an example, one can show that the infinite continued
;; fraction expansion with the N_i and the D_i all equal to 1
;; produces 1/φ, where φ is the golden ratio (described in Section
;; 1.2.2).
;; One way to approximate an infinite continued fraction is
;; to truncate the expansion after a given number of terms. Such
;; a truncation—a so-called k-term finite continued fraction—has
;; the form

;; f = (N1 / (D1 + (N2 / (D2 + (N3 / D3 + ...))))...

;; Suppose that n and d are procedures of one argument (the term
;; index i ) that return the N_i and D_i of the term of the continued
;; fraction. Define a procedure cont-frac such that evaluating
;; (cont-frac n d k) computes the value of the k-term finite
;; continued fraction. Check your procedure by approximating
;; 1/φ using

; (cont-frac (lambda (i) 1.0)
;            (lambda (i) 1.0)
;            k)

;; for successive values of k. How large must you make k in order
;; to get an approximation that is accurate to 4 decimal places?

;; b):
;; If your cont-frac procedure generates a recursive process,
;; write one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.

(define (cont-frac-recursive n d k)
  (define (frac-rec i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (frac-rec (+ i 1))))))
  (frac-rec 1))

(define (cont-frac-iterative n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

;(cont-frac-recursive (lambda (i) 1.0)
;                     (lambda (i) 1.0)
;                     100)

;(cont-frac-iterative (lambda (i) 1.0)
;                     (lambda (i) 1.0)
;                     100)

;; Excercise 1.38:
;; In 1737, the Swiss mathematician Leonhard Euler
;; published a memoir De Fractionibus Continuis, which included a
;; continued fraction expansion for e - 2, where e is the base of the
;; natural logarithms. In this fraction, the N_i are all 1, and the D_i
;; are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...
;; Write a program that uses your cont-frac procedure from Excercise 1.37
;; to approximate e, based on Euler's expansion

(define (Euler-number n)
  (+ 2.0 (cont-frac-iterative (lambda (i) 1.0)
                              (lambda (i)
                                (if (= (remainder i 3) 2)
                                    (* 2 (/ (+ i 1) 3))
                                    1))
                              n)))

;; Excercise 1.39:
;; A continued fraction representation of the tangent
;; function was published in 1770 by the German mathematician J.H.
;; Lambert:

;; tan(x) = x / (1 - (x^2 / (3 - (x^2 / (5 - ...)

;; where x is in radians. Define a procedure (tan-cf x k) that computes
;; an approximation to the tangent function based on Lambert’s
;; formula. k specifies the number of terms to compute, as in
;; Exercise 1.37.

(define PI 3.14159265357)

(define (tan-cf x k)
  (cont-frac-iterative (lambda (i)
                         (if (= i 1)
                             x
                             (* x x -1)))
                       (lambda (i)
                         (- (* 2.0 i) 1))
                       k))
