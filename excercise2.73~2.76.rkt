#lang racket
(require "getput.rkt")

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

;; -----------------------------------------------------

(define (real-part-rect z) (car z))
(define (imag-part-rect z) (cdr z))

(define (angle-rect z)
  (atan (imag-part-rect z)
        (real-part-rect z)))

(define (magnitude-rect z)
  (sqrt (+ (square (real-part-rect z))
           (square (imag-part-rect z)))))

(define (make-from-real-imag-rect x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rect r a)
  (attach-tag 'rectangular (cons
                            (* r (cos a))
                            (* r (sin a)))))

;; -------------------------------------------------------

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; ----------------------------------------------------

(define (real-part z)
  (cond [(rectangular? z)
         (real-part-rect (contents z))]
        [(polar? z)
         (real-part-polar (contents z))]
        [else
         (error "Unknow type -- REAL-PART" z)]))

(define (imag-part z)
  (cond [(rectangular? z)
         (imag-part-rect (contents z))]
        [(polar? z)
         (imag-part-polar (contents z))]
        [else
         (error "Unknow type -- IMAG-PART" z)]))

(define (magnitude z)
  (cond [(rectangular? z)
         (magnitude-rect (contents z))]
        [(polar? z)
         (magnitude-polar (contents z))]
        [else
         (error "Unknow type -- MAGNITUDE" z)]))

(define (angle z)
  (cond [(rectangular? z)
         (angle-rect (contents z))]
        [(polar? z)
         (angle-polar (contents z))]
        [else
         (error "Unknow type -- ANGLE" z)]))

(define (make-from-real-imag x y)
  (make-from-real-imag-rect x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; -------------------------------------------------
;; Data-Directed Programming and Additivity

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; We use the list (rectangular) rather than the symbol rectangular to allow
;; for the possibility of operations with multiple arguments, not all of the
;; same type.

;; 就是说有些函数允许多个类型的参数
;; 例如我定义一个same函数，它判断一个polar和一个rect坐标是否是同一位置
;; 此时就需要填写 '(rectangular polar) 这样的。
;; 不过这个例子不太恰当，应当是同一个包内的不同类型

;; The type the constructors are installed under needn't be a list because a
;; constructor is always used to make an object of one particular type.

;; 构造函数不用列表基于一个事实：我们不可能调用同一个函数构造不同的类型的值。

(define (intall-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (real-partg z) (apply-generic 'real-part z))
(define (imag-partg z) (apply-generic 'imag-part z))
(define (magnitudeg z) (apply-generic 'magnitude z))
(define (angleg z) (apply-generic 'angle z))

(define (make-from-real-imagg x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-angg r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Excercise 2.73:
;; Section 2.3.2 described a program that performs symbolic differentiation:

;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp) (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv (addend exp) var)
;                   (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;          (make-product (multiplier exp)
;                        (deriv (multiplicand exp) var))
;          (make-product (deriv (multiplier exp) var)
;                        (multiplicand exp))))
;        ; more rules can be added here
;        (else (error
;               "unknown expression type - DERIV" exp))))

;; We can regard this program as performing a dispatch on the type of
;; the expression to be differentiated. In this situation the “type tag”
;; of the datum is the algebraic operator symbol (such as +) and the
;; operation being performed is deriv. We can transform this program
;; into data-directed style by rewriting the basic derivative procedure
;; as

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [else
         ((get 'deriv (operator exp))
          (operands exp)
          var)]))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; a. Explain what was done above. Why can’twe assimilate the predicates
;;    number? and variable? into the data-directed dispatch?

;; number 和 variable两种类型并没有被我们打上tag，所以能以数据导向方式进行处理，
;; 我们用了Scheme的数字和符号类型表达这两种类型，使用内置的number?和variable?就能判断，
;; 不需要画蛇添足再加上一层了，实在要加也可以。
;; 但是这样一来，求导程序的每个包都要加上 number? 和 same-variable? 谓词,
;; 而这样的分派实际上是没有必要的。

;; b. Write the procedures for derivatives of sums and products, and
;;    the auxiliary code required to install them in the table used by
;;    the program above.

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (install-sum-deriv-package)
  ;; internal procedures
  (define (addend s)
    (car s))
  (define (augend s)
    (cadr s))
  (define (make-sum x y)
    (cond [(=number? x 0) y]
          [(=number? y 0) x]
          [(and (number? x) (number? y)) (+ x y)]
          [else
           (attach-tag '+ x y)]))
  ;; interface to the rest of the system
  (put 'addend '+ addend)
  (put 'augend '+ augend)
  (put 'make-sum '+ make-sum)
  (put 'deriv '+
       (lambda (exp var)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))))
  'done)

(define (make-sum x y)
    ((get 'make-sum '+) x y))

(define (addend sum)
    ((get 'addend '+) (contents sum)))

(define (augend sum)
    ((get 'augend '+) (contents sum)))

;; c. Choose any additional differentiation rule that you like, such as
;;    the one for exponents (Exercise 2.56), and install it in this datadirected
;;    system.

;; d. In this simple algebraic manipulator the type of an expression
;;    is the algebraic operator that binds it together. Suppose, however,
;;    we indexed the procedures in the opposite way, so that the
;;    dispatch line in deriv looked like

;    ((get (operator exp) ’deriv) (operands exp) var)

;; What corresponding changes to the derivative system are required?