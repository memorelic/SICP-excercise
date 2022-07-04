#lang racket

(require "getput.rkt")


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)    ;此处新增
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond [t1->t2 (apply-generic op (t1->t2 a1) a2)]
                            [t2->t1 (apply-generic op a1 (t2->t1 a2))]
                            [else
                             (error "No method for these types" (list op type-tags))]))))
                (error "No method for these types" (list op type-tags)))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))

  ;; Excercise 2.81:
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  "scheme-number-package-installed")

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-number-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  "rational-number-package-installed")

(define (install-rectangular-complex-package)
  ;; internal procedures
  (define (square x) (* x x))
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
  "rectangular-complex-number-package-installed")

(define (install-polar-complex-package)
  ;; internal procedures
  (define (square x) (* x x))
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
  "polar-complex-number-package-installed")


(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  ;; 此种实现在对比polar和rectangular型的复数时存在精度问题
  ;; 可单独考虑定义一个相等函数，在某个精度内，算作"相等"
  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))
  "complex-number-package-installed")

;; Excercise 2.77:
;; Louis Reasoner tries to evaluate the expression
;; (magnitude z) where z is the object shown in Figure 2.24. To
;; his surprise, instead of the answer 5 he gets an error message
;; from apply-generic, saying there is no method for the operation
;; magnitude on the types (complex). He shows this interaction
;; to Alyssa P. Hacker, who says “The problem is that the complexnumber
;; selectors were never defined for complex numbers, just
;; for polar and rectangular numbers. All you have to do to make
;; this work is add the following to the complex package:”

;; (put ’real-part ’(complex) real-part)
;; (put ’imag-part ’(complex) imag-part)
;; (put ’magnitude ’(complex) magnitude)
;; (put ’angle ’(complex) angle)

;; Describe in detail why this works. As an example, trace through all
;; the procedures called in evaluating the expression (magnitude z)
;; where z is the object shown in Figure 2.24. In particular, how many
;; times is apply-generic invoked? What procedure is dispatched to
;; in each case?

;; Answer:
;; 未将四个选择函数Put进表中时，调用函数magnitude，会在表中寻找'magnitude '(complex)的项，
;; 可是此时表中没有此项，所以报错。
;; 追踪的函数调用过程如下：

;; (magnitude z)                                                   ; 这个 magnitude 是最外层的通用操作
;; (apply-generic 'magnitude z)
;; (map type-tag (list z))                                         ; => '(complex)
;; (get 'magnitude '(complex))                                     ; => magnitude  ; 这个 magnitude 是定义于 complex 包中的 magnitude
;; (apply magnitude (map contents (list z)))                       ; =>  (apply magnitude '((rectangular 3 . 4)))
;; (magnitude '(rectangular 3 . 4))
;; (apply-generic 'magnitude '(rectangular 3 . 4))
;; (map type-tag (list '(rectangular 3 . 4)))                      ; => '(rectangular)
;; (get 'magnitude '(rectangular))                                 ; => magnitude  ; 这个 magnitude 是定义于 rectangular 包中的 magnitude
;; (apply magnitude (map contents (list '(rectangular 3 . 4))))    ; => (apply magnitude '((3 . 4)))
;; (magnitude '(3 . 4))
;; (sqrt (+ (square (real-part '(3 . 4)))
;;         (square (imag-part '(3 . 4)))))

;; 此调用过程分别调用了3“层” magnitude函数：
;; 第一层时最外层的通用magnitude；
;; 第二层调用了complex包中的magnitude；
;; 第三层调用了rectangular包中的magnitude，此为实际工作的函数；
;; 在这个过程中，一共调用了两次apply-generic函数：
;; 第一此调用剥去了'(complex)标志；
;; 第二次调用博取了'(rectangular)标志。

;; Excercise 2.78:
;; The internal procedures in the scheme-number
;; package are essentially nothing more than calls to the primitive
;; procedures +, -, etc. It was not possible to use the primitives of the
;; language directly because our type-tag system requires that each
;; data object have a type attached to it. In fact, however, all Lisp
;; implementations do have a type system, which they use internally.
;; Primitive predicates such as symbol? and number? determine
;; whether data objects have particular types. Modify the definitions
;; of type-tag, contents, and attach-tag from Section 2.4.2 so
;; that our generic system takes advantage of Scheme’s internal
;; type system. That is to say, the system should work as before
;; except that ordinary numbers should be represented simply as
;; Scheme numbers rather than as pairs whose car is the symbol
;; scheme-number.

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond [(number? datum) 'scheme-number]
        [(pair? datum) (car datum)]
        [else
         (error "Bad tagged datum -- TYPE-TAG" datum)]))

(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else
         (error "Bad tagged datum -- CONTENT" datum)]))

;; Excercise 2.79:
;; Define a generic equality predicate equ? that tests
;; the equality of two numbers, and install it in the generic arithmetic
;; package. This operation should work for ordinary numbers, rational
;; numbers, and complex numbers.

(define (equ? x y) (apply-generic 'equ x y))

;; Excercise 2.80:
;; Define a generic predicate =zero? that tests if its
;; argument is zero, and install it in the generic arithmetic package.
;; This operation should work for ordinary numbers, rational numbers,
;; and complex numbers.

;; 略，实现方式同Excercise 2.79。

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(install-scheme-number-package)
(install-rational-number-package)
(install-rectangular-complex-package)
(install-polar-complex-package)
(install-complex-package)

;; Excercise 2.81:
;; Louis Reasoner has noticed that apply-generic
;; may try to coerce the arguments to each other’s type even if they
;; already have the same type. Therefore, he reasons, we need
;; to put procedures in the coercion table to coerce arguments of
;; each type to their own type. For example, in addition to the
;; scheme-number->complex coercion shown above, he would do:

; (define (scheme-number->scheme-number n) n)
; (define (complex->complex z) z)
; (put-coercion ’scheme-number ’scheme-number
;     scheme-number->scheme-number)
; (put-coercion ’complex ’complex complex->complex)

;; a. With Louis’s coercion procedures installed, what happens
;;    if apply-generic is called with two arguments of type
;;    scheme-number or two arguments of type complex for an
;;    operation that is not found in the table for those types? For
;;    example, assume that we’ve defined a generic exponentiation
;;    operation:

(define (exp x y) (apply-generic 'exp x y))

; (exp 2 3) This can get results.
; (define two (make-scheme-number 2))
; (define three (make-scheme-number 3))
; (exp two three)

;;    and have put a procedure for exponentiation in the Schemenumber
;;    package but not in any other package:

;;    (put ’exp ’(scheme-number scheme-number)
;;        (lambda (x y) (tag (expt x y))))

;;    What happens if we call exp with two complex numbers as arguments?

;;    如果加入了强制到自己类型的函数，解释器会假死。

;; b. Is Louis correct that something had to be done about coercion
;;    with arguments of the same type, or does apply-generic work
;;    correctly as is?

;;    这种写法并没有修复同类型强制的问题，一旦试图调用不存在的通用操作，解释器就会陷入假死。

;; c. Modify apply-generic so that it doesn’t try coercion if the two
;;    arguments have the same type.

;;    修改见上文apply-generic

;; Excercise 2.82:
;; Show how to generalize apply-generic to handle
;; coercion in the general case of multiple arguments. One strategy is
;; to attempt to coerce all the arguments to the type of the first argument,
;; then to the type of the second argument, and so on. Give an
;; example of a situation where this strategy (and likewise the twoargument
;; version given above) is not sufficiently general. (Hint:
;; Consider the case where there are some suitable mixed-type operations
;; present in the table that will not be tried.)

;; 假设有这样一个继承结构：
;;   A
;;  / \
;; B   C
;; 当我们遇到传入B和C类变量为参数，这种策略只会试图将B转换为C或将C转换为B，并不会试着调整为A类型，从而调用失败。