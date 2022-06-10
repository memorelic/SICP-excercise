#lang racket

;; Excercise 2.53:
;; What would the interpreter print in response to
;; evaluating each of the following expressions?

; (list 'a 'b 'c) => ('a 'b 'c)
; (list (list 'george)) => '((george))
; (cdr '((x1 x2) (y1 y2))) => '((y1 y2))
; (cadr '((x1 x2) (y1 y2)))  => '(y1 y2)
; (pair? (car '(a short list))) => false
; (memq 'red '((red shoes) (blue socks))) => false
; (memq 'red '(red shoes blue socks)) => '(red shoes blue socks)

;; Excercise 2.54:
;; Two lists are said to be equal? if they contain equal
;; elements arranged in the same order. For example

; (equal? '(this is a list) '(this is a list))

;; is true, but

; (equal? ’(this is a list) ’(this (is a) list))

;; is false. To be more precise, we can define equal? recursively in
;; terms of the basic eq? equality of symbols by saying that a and b
;; are equal? if they are both symbols and the symbols are eq?, or
;; if they are both lists such that (car a) is equal? to (car b) and
;; (cdr a) is equal? to (cdr b). Using this idea, implement equal?
;; as a procedure.

(define (equal? list1 list2) 
   (cond ((and (not (pair? list1)) (not (pair? list2))) 
          (eq? list1 list2)) 
         ((and (pair? list1) (pair? list2)) 
          (and (equal? (car list1) (car list2)) (equal? (cdr list1) (cdr list2)))) 
         (else false)))

;; Excercise 2.55:
;; Eva Lu Ator types to the interpreter the expression
; (car ”abracadabra)
;; To her surprise, the interpreter prints back quote. Explain.

;; (car ''something) is treated by the interpreter as: 
;; (car (quote (quote something))) 
;; The first occurrence of 'quote' quotes the next entity 
;; (quote something),which is actually a list with two elements, so 
;; caring this list yields 'quote. 


(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))



(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))



;; Excercise 2.56:
;; Show how to extend the basic differentiator to handle
;; more kinds of expressions. For instance, implement the differentiation
;; rule:

;; d(u^n)/dx = n*u^(n-1)*d(u)/dx

;; by adding a new clause to the deriv program and defining
;; appropriate procedures exponentiation?, base, exponent, and
;; make-exponentiation. (You may use the symbol ^ to denote
;; exponentiation.) Build in the rules that anything raised to the
;; power 0 is 1 and anything raised to the power 1 is the thing itself.

(define (make-exponentiation b e)
  (cond [(= e 0) 1]
        [(= e 1) b]
        [else
         (list '^ b e)]))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '^)))


;; Excercise 2.57:
;; Extend the differentiation program to handle sums
;; and products of arbitrary numbers of (two or more) terms. Then
;; the last example above could be expressed as

; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and
;; products, without changing the deriv procedure at all. For exam205
;; ple, the addend of a sum would be the first term, and the augend
;; would be the sum of the rest of the terms.

;(define (make-sum a1 a2)
;  (cond [(=number? a1 0) a2]
;        [(=number? a2 0) a1]
;        [(and (number? a1) (number? a2)) (+ a1 a2)]
;        [else (list '+ a1 a2)]))

;(define (make-product m1 m2)
;  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
;        [(=number? m1 1) m2]
;        [(=number? m2 1) m1]
;        [(and (number? m1) (number? m2)) (* m1 m2)]
;        [else (list '* m1 m2)]))

(define (single-operand? l)
  (and (list? l) (= (length l) 1)))

(define (make-sum a1 . a2)
  (if (single-operand? a2)
      (let ((op2 (car a2)))
        (cond [(=number? a1 0) op2]
              [(=number? op2 0) a1]
              [(and (number? a1) (number? op2)) (+ a1 op2)]
              [else (list '+ a1 op2)]))
      (cons '+ (cons a1 a2))))

(define (addend s)
  (cadr s))

(define (augend s)
  (let ((tail (cddr s)))
    (if (single-operand? tail)
        (car tail)
        (apply make-sum tail))))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (let ((tail (cddr p)))
    (if (single-operand? tail)
        (car tail)
        (apply make-product tail))))

(define (make-product m1 . m2)
  (if (single-operand? m2)
      (let ((op2 (car m2)))
        (cond [(or (=number? m1 0) (=number? op2 0)) 0]
              [(=number? m1 1) op2]
              [(=number? op2 1) m1]
              [(and (number? m1) (number? op2)) (* m1 op2)]
              [else (list '* m1 op2)]))
      (cons '* (cons m1 m2))))

;; Excercise 2.58:
;; Suppose we want to modify the differentiation
;; program so that it works with ordinary mathematical notation,
;; in which + and * are infix rather than prefix operators. Since the
;; differentiation program is defined in terms of abstract data, we
;; can modify it to work with different representations of expressions
;; solely by changing the predicates, selectors, and constructors that
;; define the representation of the algebraic expressions on which
;; the differentiator is to operate.

;; a).Show how to do this in order to differentiate algebraic expressions
;;    presented in infix form, such as (x + (3 * (x + (y +2)))).
;;    To simplify the task, assume that + and * always take
;;    two arguments and that expressions are fully parenthesized.

;; b).The problem becomes substantially harder if we allow standard
;;    algebraic notation, such as (x + 3 * (x + y + 2)), which
;;    drops unnecessary parentheses and assumes that multiplication
;;    is done before addition. Can you design appropriate
;;    predicates, selectors, and constructors for this notation such
;;    that our derivative program still works?


;; a)
;; 如果只是换成中缀表示，只允许操作数有两个参数并且用括号隔开，类似 (x + (3 * (x + (y + 2))))这样的写法,
;; 那么所有要改的部分只有构造函数、选择函数、谓词函数，例如，将(list '* m1 m2)改为(list m1 '* m2)，
;; 选择函数和谓词函数的判断相应修改。
;; deriv的这个函数整体上不用修改。

;; b)
;; 如果允许标准代数写法，即 (x + 3 * (x + y + 2))，
;; 则没办法只是通过修改谓词、选择函数和构造函数来达到正确计算求导的目的；
;; 因为此时我们需要判断各个符号的优先级关系。
;; 一个简单的处理办法是将优先级处理单独写一个函数，将标准代数写法转换成波兰式写法，
;; 例如，该函数的工作方式是：interp('(x + 3 * (x + y + 2))) => '(+ x (* 3 (+ x y 2)))
;; 然后我们就能够使用现目前的deriv进行求解。


(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [(exponentiation? exp)
         (let ((n (exponent exp))
               (u (base exp)))
           (make-product
            n
            (make-product
             (make-exponentiation
              u
              (- n 1))
             (deriv u var))))]
        [else
         (error "unknown expression type -- DERIV" exp)]))
