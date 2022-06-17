#lang racket

(define (square x) (* x x))

(define (real-part1 z) (car z))

(define (imag-part1 z) (cdr z))

(define (magnitude1 z)
  (sqrt (+ (square (real-part1 z)) (square (imag-part1 z)))))

(define (angle1 z)
  (atan (imag-part1 z) (real-part1 z)))

(define (make-from-real-imag1 x y)
  (cons x y))

(define (make-from-mag-ang1 r a)
  (cons (* r (cos a)) (* r (sin a))))


;; --------------------------------------------------------------

(define (real-part2 z)
  (* (magnitude2 z) (cos (angle2 z))))

(define (imag-part2 z)
  (* (magnitude2 z) (sin (angle2 z))))

(define (magnitude2 z) (car z))
(define (angle2 z) (cdr z))

(define (make-from-real-imag2 x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang2 r a)
  (cons r a))

;; -------------------------------------------------------------

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))