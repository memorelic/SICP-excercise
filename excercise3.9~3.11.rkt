#lang racket

;; Excercise 3.9:
;; In Section 1.2.1 we used the substitution model to analyze
;; two procedures for computing factorials, a recursive version

(define (factorial-recur n)
  (if (= n 1)
      1
      (* n (factorial-recur (- n 1)))))

;; and an iterative version

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial-iter n)
  (fact-iter 1 1 n))

;; Show the environment structures created by evaluating (factorial 6)
;; using each version of the factorial procedure.

;; recursive version:
;;                +------------------------------------------------------------------------------------------+
;;global env -->  |                                                                                          |
;;                |                                                                                          |
;;                +------------------------------------------------------------------------------------------+
;;                   ^               ^              ^                ^               ^               ^
;;            (f 6)  |        (f 5)  |       (f 4)  |         (f 3)  |        (f 2)  |        (f 1)  |
;;                   |               |              |                |               |               |
;;                +------+        +------+        +------+         +------+        +------+        +------+
;;                |      |        |      |        |      |         |      |        |      |        |      |
;;          E1 -> | n: 6 |  E2->  | n: 5 |  E3 -> | n: 4 |  E4 ->  | n: 3 |  E5 -> | n: 2 |  E6 -> | n: 1 |
;;                |      |        |      |        |      |         |      |        |      |        |      |
;;                +------+        +------+        +------+         +------+        +------+        +------+
;;
;;             (* 6 (f 5))      (* 5 (f 4))     (* 4 (f 3))      (* 3 (f 2))     (* 2 (f 1))        1

;; iterative version:
;;         +-----------------------------------------------------------------------------------------------------------------------------+
;;global   |                                                                                                                             |
;;env -->  |                                                                                                                             |
;;         |                                                                                                                             |
;;         +-----------------------------------------------------------------------------------------------------------------------------+
;;            ^               ^                 ^                ^               ^                 ^                  ^               ^
;;      (f 6) |     (i 1 1 6) |       (i 1 2 6) |      (i 2 3 6) |     (i 6 4 6) |      (i 24 5 6) |      (i 120 6 6) |   (i 720 7 6) |
;;            |               |                 |                |               |                 |                  |               |
;;        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+
;;        |       |        | p: 1  |        | p: 1  |        | p: 2  |        | p: 6  |        | p: 24 |        | p:120 |        | p:720 |
;;  E1 -> | n: 6  |  E2 -> | c: 1  |  E3 -> | c: 2  |  E4 -> | c: 3  |  E5 -> | c: 4  |  E6 -> | c: 5  |  E7 -> | c: 6  |  E8 -> | c: 7  |
;;        |       |        | m: 6  |        | m: 6  |        | m: 6  |        | m: 6  |        | m: 6  |        | m: 6  |        | m: 6  |
;;        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+        +-------+
;;        (i 1 1 6)        (i 1 2 6)        (i 2 3 6)        (i 6 4 6)       (i 24 5 6)       (i 120 6 6)      (i 720 7 6)       720

;; Excercise 3.10:
;; In the make-withdraw procedure, the local variable
;; balance is created as a parameter of make-withdraw. We could
;; also create the local state variable explicitly, using let, as follows:

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;; Recall from Section 1.3.2 that let is simply syntactic sugar for a
;; procedure call:

; (let ((<var> <exp>)) <body>)

;; is interpreted as an alternate syntax for

; ((lambda (<var>) <body>) <exp>)

;; Use the environment model to analyze this alternate version of
;; make-withdraw, drawing figures like the ones above to illustrate
;; the interactions

; (define W1 (make-withdraw 100))
; (W1 50)
; (define W2 (make-withdraw 100))

;; Show that the two versions of make-withdraw create objects with
;; the same behavior. How do the environment structures differ for
;; the two versions?

;;              +------------------------------------+
;;global env -> |                                    |
;;              |   make-withdraw --+                |
;;              +-------------------|----------------+
;;                                  |       ^
;;                                  |       |
;;                                  v       |
;;                                [*][*]----+
;;                                 |
;;                                 |
;;                                 v
;;                        parameters: initial-amount
;;                        body: ((lambda (balance)
;;                                   (lambda (amount)
;;                                       (if (>= balance amount)
;;                                           (begin (set! balance (- balance amount))
;;                                                  balance)
;;                                           "Insufficient funds")))
;;                               initial-amount)


;;              +------------------------------------+
;;global env -> |                                    |
;;              |                                    |
;;              +------------------------------------+
;;                     ^
;;(make-withdraw 100)  |
;;                     |
;;                 +--------------+
;;                 |              |
;;          E1 ->  | initial: 100 |
;;                 |              |
;;                 +--------------+
;;
;;                ((lambda (balance)
;;                     (lambda (amount)
;;                         (if (>= balance amount)
;;                             (begin (set! balance (- balance amount))
;;                                    balance)
;;                             "Insufficient funds")))
;;                 initial)


;;              +------------------------------------+
;;global env -> |                                    |
;;              |                                    |
;;              +------------------------------------+
;;                     ^
;;(make-withdraw 100)  |
;;                     |
;;                 +--------------+
;;                 |              |
;;          E1 ->  | initial: 100 |
;;                 |              |
;;                 +--------------+
;;                              ^
;;((lambda (balance) ...) 100)  |
;;                              |
;;                        +--------------+
;;                        |              |
;;                 E2 ->  | balance: 100 |
;;                        |              |
;;                        +--------------+
;;
;;                       (lambda (amount)
;;                           (if (>= balance amount)
;;                               (begin (set! balance (- balance amount))
;;                                      balance)
;;                               "Insufficient funds"))

;;              +------------------------------------+
;;global env -> |                                    |
;;              |                                    |
;;              +------------------------------------+
;;                     ^
;;(make-withdraw 100)  |
;;                     |
;;                 +--------------+
;;                 |              |
;;          E1 ->  | initial: 100 |
;;                 |              |
;;                 +--------------+
;;                              ^
;;((lambda (balance) ...) 100)  |
;;                              |
;;                        +--------------+
;;                        |              |
;;                 E2 ->  | balance: 100 |
;;                        |              |
;;                        +--------------+
;;                           |        ^
;;                           |        |
;;                           v        |
;;                         [*][*]-----+
;;                          |
;;                          |
;;                          v
;;                   parameters: amount
;;                   body: (if (>= balance amount)
;;                             (begin (set! balance (- balance amount))
;;                                    balance)
;;                             "Insufficient funds")

;;              +-------------------------------------------+
;;global env -> |                                           |
;;              |   w1                                      |
;;              +---|---------------------------------------+
;;                  |                               ^
;;                  |          (make-withdraw 100)  |
;;                  |                               |
;;                  |                    +--------------+
;;                  |                    |              |
;;                  |             E1 ->  | initial: 100 |
;;                  |                    |              |
;;                  |                    +--------------+
;;                  |                               ^
;;                  | ((lambda (balance) ...) 100)  |
;;                  |                               |
;;                  |                    +--------------+
;;                  |                    |              |
;;                  |             E2 ->  | balance: 100 |
;;                  |                    |              |
;;                  |                    +--------------+
;;                  |                      |        ^
;;                  |                      |        |
;;                  |                      v        |
;;                  +------------------> [*][*]-----+
;;                                        |
;;                                        |
;;                                        v
;;                                 parameters: amount
;;                                 body: (if (>= balance amount)
;;                                           (begin (set! balance (- balance amount))
;;                                           balance)
;;                                       "Insufficient funds")

;;              +-------------------------------------------+
;;global env -> |                                           |
;;              |   w1                                      |
;;              +---|---------------------------------------+
;;                  |                               ^
;;                  |          (make-withdraw 100)  |
;;                  |                               |
;;                  |                    +--------------+
;;                  |                    |              |
;;                  |             E1 ->  | initial: 100 |
;;                  |                    |              |
;;                  |                    +--------------+
;;                  |                               ^
;;                  | ((lambda (balance) ...) 100)  |
;;                  |                               |
;;                  |                    +--------------+
;;                  |                    |              |
;;                  |             E2 ->  | balance: 100 |
;;                  |                    |              |
;;                  |                    +--------------+
;;                  |                      |        ^  ^
;;                  |                      |        |  |                                    +------------+
;;                  |                      v        |  |                                    |            |
;;                  +------------------> [*][*]-----+  +------------------------------------| amount: 50 | <- E3
;;                                        |                                                 |            |
;;                                        |                                                 +------------+
;;                                        v
;;                                 parameters: amount                                    (if (>= balance amount)
;;                                 body: (if (>= balance amount)                             (begin (set! balance (- balance amount))
;;                                           (begin (set! balance (- balance amount))               balance)
;;                                           balance)                                        "Insufficient funds")
;;                                       "Insufficient funds")

;;              +-------------------------------------------+
;;global env -> |                                           |
;;              |   w1                                      |
;;              +---|---------------------------------------+
;;                  |                               ^
;;                  |          (make-withdraw 100)  |
;;                  |                               |
;;                  |                    +--------------+
;;                  |                    |              |
;;                  |             E1 ->  | initial: 100 |
;;                  |                    |              |
;;                  |                    +--------------+
;;                  |                               ^
;;                  | ((lambda (balance) ...) 100)  |
;;                  |                               |
;;                  |                    +--------------+
;;                  |                    |              |
;;                  |             E2 ->  | balance: 50  |
;;                  |                    |              |
;;                  |                    +--------------+
;;                  |                      |        ^
;;                  |                      |        |
;;                  |                      v        |
;;                  +------------------> [*][*]-----+
;;                                        |
;;                                        |
;;                                        v
;;                                 parameters: amount
;;                                 body: (if (>= balance amount)
;;                                           (begin (set! balance (- balance amount))
;;                                           balance)
;;                                       "Insufficient funds")

;;              +-----------------------------------------------------------------------------------------+
;;global env -> |                                                                                         |
;;              |   w1                                        w2                                          |
;;              +---|-----------------------------------------|-------------------------------------------+
;;                  |                               ^         |                               ^
;;                  |          (make-withdraw 100)  |         |                               |
;;                  |                               |         |                               |
;;                  |                    +--------------+     |                      +--------------+
;;                  |                    |              |     |                      |              |
;;                  |             E1 ->  | initial: 100 |     |               E1 ->  | initial: 100 |
;;                  |                    |              |     |                      |              |
;;                  |                    +--------------+     |                      +--------------+
;;                  |                               ^         |                               ^
;;                  | ((lambda (balance) ...) 100)  |         | ((lambda (balance) ...) 100)  |
;;                  |                               |         |                               |
;;                  |                    +--------------+     |                      +--------------+
;;                  |                    |              |     |                      |              |
;;                  |             E2 ->  | balance: 50  |     |               E2 ->  | balance: 100 |
;;                  |                    |              |     |                      |              |
;;                  |                    +--------------+     |                      +--------------+
;;                  |                      |        ^         |                          |       ^
;;                  |                      |        |         |                          |       |
;;                  |                      v        |         |                          v       |
;;                  +------------------> [*][*]-----+         +----------------------->[*][*]----+
;;                                        |                                             |
;;                                        |                                             |
;;                                        v                                             v
;;                         parameters: amount                             parameters: amount
;;                         body: (if (>= balance amount)                  body: (if (>= balance amount)
;;                                   (begin (set! balance                           (begin (set! balance
;;                                                (- balance amount))                      (- balance amount))
;;                                          balance)                                balance)
;;                                   "Insufficient funds")                          "Insufficient funds")

;; Excercise 3.11:
;; In Section 3.2.3 we saw how the environment model
;; described the behavior of procedures with local state. Nowwe have
;; seen howinternal definitions work. A typical message-passing procedure
;; contains both of these aspects. Consider the bank account
;; procedure of Section 3.1.1:

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request - MAKE-ACCOUNT"
                       m))))
  dispatch)

;; Show the environment structure generated by the sequence of interactions

(define acc (make-account 50))
; ((acc 'deposit) 40)
; => 90
; ((acc 'withdraw) 60)
; => 30

;; Where is the local state for acc kept? Suppose we define another
;; account

(define acc2 (make-account 100))

;; How are the local states for the two accounts kept distinct? Which
;; parts of the environment structure are shared between acc and
;; acc2?

;; 定义make-account后的环境如下：
;;          +-------------------------------------+
;;global -> |                                     |
;;env       | make-account                        |
;;          +----|--------------------------------+
;;               |       ^
;;               |       |
;;               v       |
;;             [*][*]----+
;;              |
;;              |
;;              v
;;  parameters: balance
;;  body: (define withdraw ...)
;;        (define deposit ...)
;;        (define dispatch ...)
;;        (dispatch)

;; 执行 (define acc (make-account 50))后的环境：
;;          +----------------------------------------------------+
;;global -> |                                                    |
;;env       | make-account                                       |
;;          +----|-----------------------------------------------+
;;               |       ^                     ^
;;               |       |                     |
;;               v       |           E1 -> +------------------+
;;             [*][*]----+                 | balance: 50      |<----------+
;;              |                          |                  |           |
;;              |                          | withdraw --------------->[*][*]----> parameters: amount
;;              v                          |                  |                   body: ...
;;  parameters: balance                    |                  |<----------+
;;  body: (define withdraw ...)            |                  |           |
;;        (define deposit ...)             | deposit ---------------->[*][*]----> parameters: amount
;;        (define dispatch ...)            |                  |                   body: ...
;;        (lambda (m) ...)                 |                  |<----------+
;;                                         |                  |           |
;;                                         | dispatch --------------->[*][*]----> parameters: m
;;                                         |                  |                   body: ...
;;                                         +------------------+
;;                                         dispatch

;; 最后,将以上求值得到的值和符号acc关联(前面的求值会返回dispatch, 于是acc就直接指向E1环境中dispatch过程对象):
;;          +----------------------------------------------------+
;;global -> |                                                    |
;;env       | make-account      acc                              |
;;          +----|---------------|-------------------------------+
;;               |       ^       |             ^
;;               |       |       |             |
;;               v       |       |   E1 -> +------------------+
;;             [*][*]----+       |         | balance: 50      |<----------+
;;              |                |         |                  |           |
;;              |                |         | withdraw --------------->[*][*]----> parameters: amount
;;              v                |         |                  |                   body: ...
;;  parameters: balance          |         |                  |<----------+
;;  body: (define withdraw ...)  |         |                  |           |
;;        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
;;        (define dispatch ...)  |         |                  |                   body: ...
;;        (lambda (m) ...)       |         |                  |<----------+
;;                               |         |                  |           |
;;                               +---------->dispatch --------------->[*][*]----> parameters: m
;;                                         |                  |                   body: ...
;;                                         +------------------+

;; 求值表达式 ((acc 'deposit) 40) ,产生以下环境:
;;          +----------------------------------------------------+
;;global -> |                                                    |
;;env       | make-account      acc                              |
;;          +----|---------------|-------------------------------+
;;               |       ^       |             ^
;;               |       |       |             |
;;               v       |       |   E1 -> +------------------+
;;             [*][*]----+       |         | balance: 50      |<----------+
;;              |                |         |                  |           |
;;              |                |         | withdraw --------------->[*][*]----> parameters: amount
;;              v                |         |                  |                   body: ...
;;  parameters: balance          |         |                  |<----------+
;;  body: (define withdraw ...)  |         |                  |           |
;;        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
;;        (define dispatch ...)  |         |                  |                   body: ...
;;        (lambda (m) ...)       |         |                  |<----------+
;;                               |         |                  |           |
;;                               +---------->dispatch --------------->[*][*]----> parameters: m
;;                                         |                  |                   body: ...
;;                                         +------------------+
;;                                                    ^
;;                                                    |
;;                                                    |
;;                                   (acc 'deposit)   |
;;                                                    |
;;                                            +-------------+
;;                                            |             |
;;                                      E2 -> | m: 'deposit |
;;                                            |             |
;;                                            +-------------+
;;                                            (cond ((eq? m 'withdraw)
;;                                                    withdraw)
;;                                                  ((eq? m 'deposit)
;;                                                    deposit)
;;                                                  (else
;;                                                    (error "..." m)))


;; (acc 'deposit) 将返回过程 deposit ,这个 deposit 又作为新的过程操作符,被参数 40 应用,并且 E2 在求值之后消失:
;;          +----------------------------------------------------+
;;global -> |                                                    |
;;env       | make-account      acc                              |
;;          +----|---------------|-------------------------------+
;;               |       ^       |             ^
;;               |       |       |             |
;;               v       |       |   E1 -> +------------------+
;;             [*][*]----+       |         | balance: 50      |<----------+
;;              |                |         |                  |           |
;;              |                |         | withdraw --------------->[*][*]----> parameters: amount
;;              v                |         |                  |                   body: ...
;;  parameters: balance          |         |                  |<----------+
;;  body: (define withdraw ...)  |         |                  |           |
;;        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
;;        (define dispatch ...)  |         |                  |                   body: ...
;;        (lambda (m) ...)       |         |                  |<----------+
;;                               |         |                  |           |
;;                               +---------->dispatch --------------->[*][*]----> parameters: m
;;                                         |                  |                   body: ...
;;                                         +------------------+
;;                                                    ^
;;                                                    |
;;                                                    |
;;                                       (deposit 40) |
;;                                                    |
;;                                            +------------+
;;                                            |            |
;;                                      E3 -> | amount: 40 |
;;                                            |            |
;;                                            +------------+
;;                                            (set! balance (+ balance amount))

;; 表达式在E3环境中求值,沿着外围环境指针查找并修改 balance 的值,求值完毕之后, E3 消失:
;;          +----------------------------------------------------+
;;global -> |                                                    |
;;env       | make-account      acc                              |
;;          +----|---------------|-------------------------------+
;;               |       ^       |             ^
;;               |       |       |             |
;;               v       |       |   E1 -> +------------------+
;;             [*][*]----+       |         | balance: 90      |<----------+
;;              |                |         |                  |           |
;;              |                |         | withdraw --------------->[*][*]----> parameters: amount
;;              v                |         |                  |                   body: ...
;;  parameters: balance          |         |                  |<----------+
;;  body: (define withdraw ...)  |         |                  |           |
;;        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
;;        (define dispatch ...)  |         |                  |                   body: ...
;;        (lambda (m) ...)       |         |                  |<----------+
;;                               |         |                  |           |
;;                               +---------->dispatch --------------->[*][*]----> parameters: m
;;                                         |                  |                   body: ...
;;                                         +------------------+

;; 然后,进行第二次求值 ((acc 'withdraw) 60) ,得出以下环境:
;;          +----------------------------------------------------+
;;global -> |                                                    |
;;env       | make-account      acc                              |
;;          +----|---------------|-------------------------------+
;;               |       ^       |             ^
;;               |       |       |             |
;;               v       |       |   E1 -> +------------------+
;;             [*][*]----+       |         | balance: 90      |<----------+
;;              |                |         |                  |           |
;;              |                |         | withdraw --------------->[*][*]----> parameters: amount
;;              v                |         |                  |                   body: ...
;;  parameters: balance          |         |                  |<----------+
;;  body: (define withdraw ...)  |         |                  |           |
;;        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
;;        (define dispatch ...)  |         |                  |                   body: ...
;;        (lambda (m) ...)       |         |                  |<----------+
;;                               |         |                  |           |
;;                               +---------->dispatch --------------->[*][*]----> parameters: m
;;                                         |                  |                   body: ...
;;                                         +------------------+

;; 接着求值表达式 (withdraw 60) :
;;          +----------------------------------------------------+
;;global -> |                                                    |
;;env       | make-account      acc                              |
;;          +----|---------------|-------------------------------+
;;               |       ^       |             ^
;;               |       |       |             |
;;               v       |       |   E1 -> +------------------+
;;             [*][*]----+       |         | balance: 90      |<----------+
;;              |                |         |                  |           |
;;              |                |         | withdraw --------------->[*][*]----> parameters: amount
;;              v                |         |                  |                   body: ...
;;  parameters: balance          |         |                  |<----------+
;;  body: (define withdraw ...)  |         |                  |           |
;;        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
;;        (define dispatch ...)  |         |                  |                   body: ...
;;        (lambda (m) ...)       |         |                  |<----------+
;;                               |         |                  |           |
;;                               +---------->dispatch --------------->[*][*]----> parameters: m
;;                                         |                  |                   body: ...
;;                                         +------------------+
;;                                                   ^
;;                                                   |
;;                                                   |
;;                                             +------------+
;;                                             |            |
;;                                       E5 -> | amount: 60 |
;;                                             |            |
;;                                             +------------+
;;                                            (if (>= balance amount)
;;                                                (begin (set! balance (- balance amount))
;;                                                       balance)
;;                                                "...")

;; 以下是求值完毕之后的环境:
;;         +----------------------------------------------------+
;;global -> |                                                    |
;;env       | make-account      acc                              |
;;          +----|---------------|-------------------------------+
;;               |       ^       |             ^
;;               |       |       |             |
;;               v       |       |   E1 -> +------------------+
;;             [*][*]----+       |         | balance: 30      |<----------+
;;              |                |         |                  |           |
;;              |                |         | withdraw --------------->[*][*]----> parameters: amount
;;              v                |         |                  |                   body: ...
;;  parameters: balance          |         |                  |<----------+
;;  body: (define withdraw ...)  |         |                  |           |
;;        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
;;        (define dispatch ...)  |         |                  |                   body: ...
;;        (lambda (m) ...)       |         |                  |<----------+
;;                               |         |                  |           |
;;                               +---------->dispatch --------------->[*][*]----> parameters: m
;;                                         |                  |                   body: ...
;;                                         +------------------+