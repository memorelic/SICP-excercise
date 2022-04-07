#lang racket

;; Excercise 1.3:
;; Define a procedure that takes three numbers as arguments
;; and returns sum of the squares of two large numbers

(define square (lambda (x) (* x x)))

(define sum-of-two-large-numbers-square
  (lambda (x y z)
   (let ([maxnum (max x y z)]
         [minnum (min x y z)]
         [sum    (+ x y z)])
     (+ (square maxnum) (square (- sum maxnum minnum))))))

;; Excercise 1.4:
;; Observe that our model of evaluation allows for combinations
;; whose operators are compound expressions.
;; Use this observation to describe the behavior of the following procedure.

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 此函数的语义为 a + |b|，在 b > 0的情况下，执行 a + b，否则执行 a - b

;; excersice 1.5:
;; Ben Bitdiddle has invented a test to determine
;; whether the interpreter is faced with is using applicative-order evalutation
;; or normal-order evaluation. He defines the following two procedures:
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; Then he evaluates the expression

;(test 0 (p))

;; What behavior will Ben observe with an interpreter that uses
;; applicative-order evaluation? What behavior will he observe
;; with an interpreter that uses normal-order evaluation? explain
;; your answer
;; (Assume that the evaluation rule for the special form 'if' is
;; the same whether the interpreter is using normal or applicative
;; order: The predicate expression is evaluated first, and the result
;; determines whether to evaluate the consequent or the alternative expression

;; applicative-order : 程序直接卡死，因为表达式求值需要首先求出operator和operands的值
;;                     而对(p)求值会得到对p自身的调用，程序会一直卡死在这里;
;; normal-order: 程序会得到0这个结果，因为正则序求值首先对表达式进行替换，
;;               (test 0 (p)) => (if (= x 0) 0 (p))
;;               此处if语句为特殊语句，按照规则，因为x = 0，所以此语句会被替换为0，最后结果为0;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (mysqrt x)
  (sqrt-iter 1.0 x))

;; excercise 1.6:
;; Alyssa P.Hacker doesn't see why 'if' needs to be provided as a special form.
;; "Why can't I just define it as an ordinary procedure in terms of cond?" she asks.
;; Alssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new
;; version of 'if':

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Eva demonstrates the program for Alyssa:
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

;; Delighted, Alyssa uses new-if to rewrite the square-root program:
; 
; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x) x)))


;; What happens when Alyssa attempts to use this to compute square roots? Explain.

;; 程序卡死，无法跳出，因为new-if是一个普通结构
;; if结构会在求得predicate的真值后，根据真值选择替换if表达式语句为consequent或alternative，而后再求值
;; 而new-if作为普通结构不会做替换，它被当作普通过程调用，需要对参数求值
;; 而此处因为是递归求值，函数包含了对自身的调用，如果每次调用都必须计算递归部分的值则永远无法跳出。

;; excersice 1.7:
;; The good-enough? test used in computing square
;; roots will not be very effective for finding the square roots of very
;; small numbers. Also, in real computers, arithmetic operations are
;; almost always performed with limited precision. This makes our
;; test inadequate for very large numbers. Explain these statements,
;; with examples showing how the test fails for small and large numbers.
;; An alternative strategy for implementing good-enough? is to
;; watch how guess changes from one iteration to the next and to
;; stop when the change is a very small fraction of the guess. Design
;; a square-root procedure that uses this kind of end test. Does this
;; work better for small and large numbers?

;  (mysqrt 0.0001) => 0.03230844833048122 结果与预期 0.01 差距较大
;  (mysqrt 12345678901234) => has not finished.


;; 对于较小的数来说，good-enough?中的检测本身精度甚至不如被开方数，所以导致结果出错
;; 对于较大的数来说，当结果趋近于正确答案时，同样因为精度问题，趋近答案和它的improve始终不能在精度范围内
;; 致使计算来回摆荡，无法得出结果。可参照下表。

; 
; iteration	guess	        (improve guess x)	(- (square guess) x)
; 0	        1.0	        6172839450617.5	        -12345678901233.0
; 1	6172839450617.5	        3086419725309.75 	3.8103946883087414e+25
; 2	3086419725309.75	1543209862656.875	9.525986720768766e+24
; 3	1543209862656.875	771604931332.4375	2.3814966801891054e+24
; 4	771604931332.4375	385802465674.21875	5.953741700441899e+23
; 5	385802465674.21875	192901232853.10938	1.4884354250796105e+23
; 6	192901232853.10938	96450616458.55469	3.7210885623903846e+22
; 7	96450616458.55469	48225308293.27734	9.302721402889541e+21
; 8	48225308293.27734	24112654274.63867	2.3256803476359655e+21
; 9	24112654274.63867	12056327393.319334	5.8142008382257175e+20
; 10	12056327393.319334	6028164208.659653	1.4535501786922326e+20
; 11	6028164208.659653	3014083128.3297105	3.6338751380886356e+19
; 12	3014083128.3297105	1507043612.1639276	9.084684758802912e+18
; 13	1507043612.1639276	753525902.074542	2.2711681032851973e+18
; 14	753525902.074542	376771142.9778979	5.677889394183511e+17
; 15	376771142.9778979	188401955.01397642	1.4194414850197034e+17
; 16	188401955.01397642	94233741.7076047	3.548295097418716e+16
; 17	94233741.7076047	47182376.47141617	8867652397314325.0
; 18	47182376.47141617	23722017.581583798	2213830970589212.0
; 19	23722017.581583798	12121224.408177208	550388439239736.8
; 20	12121224.408177208	6569870.942541849	134578402252156.9
; 21	6569870.942541849	4224503.311279173	30817525300421.72
; 22	4224503.311279173	3573450.5222935397	5500749325774.699
; 23	3573450.5222935397	3514142.3366335463	423869734045.97266
; 24	3514142.3366335463	3513641.86446291	3517460886.28125
; 25	3513641.86446291	3513641.8288200637	250472.396484375
; 26	3513641.8288200637	3513641.8288200637	0.001953125
; 27	3513641.8288200637	3513641.8288200637	0.001953125
; 28	3513641.8288200637	3513641.8288200637	0.001953125


(define (good-enough2? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.00000000001))

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess (improve2 guess x))
      guess
      (sqrt-iter2 (improve guess x) x)))

(define (improve2 guess x)
  (average2 guess (/ x guess)))

(define (average2 x y)
  (/ (+ x y) 2))

(define (mysqrt2 x)
  (sqrt-iter2 1.0 x))

;; Excercise 1.8:
;; Newton’s method for cube roots is based on the fact
;; that if y is an approximation to the cube root of x, then a better
;; approximation is given by the value
;; (x/y^2 + 2y) / 3
;; Use this formula to implement a cube-root procedure analogous
;; to the square-root procedure. (In Section 1.3.4 we will see how to
;; implement Newton’s method in general as an abstraction of these
;; square-root and cube-root procedures.)