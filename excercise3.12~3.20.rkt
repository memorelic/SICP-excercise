#lang sicp

;; Excercise 3.12:
;; The following procedure for appending lists was introduced
;; in Section 2.2.1:

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;; Append forms a new list by successively consing the elements of
;; x onto y. The procedure append! is similar to append, but it is a
;; mutator rather than a constructor. It appends the lists by splicing
;; them together, modifying the final pair of x so that its cdr is now y.
;; (It is an error to call append! with an empty x.)

(define (append! x y)
  (set-cdr! (last-pair x) y) 
  x)

;; Here last-pair is a procedure that returns the last pair 
;; in its argument:

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; Consider the interaction

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
; z
; => (a b c d)
; (cdr x)
; <response>
(define w (append! x y))
; w
; => (a b c d)
; (cdr x)
; <response>

;; What are the missing <response>s? Draw box-and-poiter diagrams to explain your answer.
;; the first (cdr x) => (b)
;; the second (cdr x) => (b c d)

;; Because z is represent as:
;;x --> [*]----> [*]----> '()
;;       |        |
;;       v        v
;;       'a       'b
;;
;;y --> [*]----> [*]----> '()
;;       |        |
;;       v        v
;;       'c       'd
;;
;;z --> [*]---->[*]---->[*]---->[*]----> '()
;;       |       |       |       |
;;       v       v       v       v
;;      'a      'b      'c      'd

;; and for the second one:
;;w------+
;;       |
;;       |
;;       v
;;x --> [*]----> [*]----+
;;       |        |     |
;;       v        v     |
;;       'a       'b    |
;;                      |
;;       +--------------+
;;       |
;;       v
;;y --> [*]----> [*]----> '()
;;       |        |
;;       v        v
;;       'c       'd
;;
;;z --> [*]---->[*]---->[*]---->[*]----> '()
;;       |       |       |       |
;;       v       v       v       v
;;      'a      'b      'c      'd

;; Excercise 3.13:
;; Consider the following make-cycle procedure,
;; which uses the last-pair procedure defined in Exercise 3.12:

(define (make-cycle x)
  (set-cdr! (last-pair x) x) x)

;; Draw a box-and-pointer diagram that shows the structure zs created
;; by:

(define zs (make-cycle (list 'a 'b 'c)))

;; What happens if we try to compute (last-pair zs)?

;; it will compute an infinite loop, because:

;;          +-----------------------+
;;          |                       |
;;          v                       |
;;zs ----> [*]----> [*]----> [*]----+
;;          |        |        |
;;          v        v        v
;;         'a       'b       'c

;; Excercise 3.14:
;; The following procedure is quite useful, although
;; obscure:

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; Loop uses the “temporary” variable temp to hold the old value of
;; the cdr of x, since the set-cdr! on the next line destroys the cdr.
;; Explain what mystery does in general. Suppose v is defined by
;; (define v (list ’a ’b ’c ’d)). Draw the box-and-pointer diagram
;; that represents the list to which v is bound. Suppose that we
;; now evaluate (define w (mystery v)). Draw box-and-pointer
;; diagrams that show the structures v and w after evaluating this expression.
;; What would be printed as the values of v and w?

;; 实际上mystery是一个修改版的reverse函数:

;;v --> [*]----> [*]----> [*]----> '()
;;       |        |        |
;;       v        v        v
;;       'a       'b       'c

;; (mystery v)
;; (mystery (list 'a 'b 'c))
;;
;; (loop (list 'a 'b 'c) '())
;;
;; (let ((temp (list 'b 'c)))
;;     (set-cdr! (list 'a 'b 'c) '())
;;     (loop (list 'b 'c) (list 'a)))
;;
;; (loop (list 'b 'c) (list 'a))
;;
;; (let ((temp (list 'c)))
;;     (set-cdr! (list 'b 'c) (list 'a))
;;     (loop (list 'c) (list 'b 'a)))
;;
;; (loop (list 'c) (list 'b 'a))
;;
;; (let ((temp '()))
;;     (set-cdr! (list 'c) (list 'b 'a))
;;     (loop '() (list 'c 'b 'a)))
;;
;; (loop '() (list 'c 'b 'a))
;;
;; (list 'c 'b 'a)

;;v------------------------+
;;                         |
;;                         v
;;w --> [*]----> [*]----> [*]----> '()
;;       |        |        |
;;       v        v        v
;;       'c       'b       'a

(define m (list 'a 'b))
(define z1 (cons m m))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow) x)

;; Excercise 3.15:
;; Draw box-and-pointer diagrams to explain the effect
;; of set-to-wow! on the structures z1 and z2 above.

;;z1 --> [*][*]
;;        |  |
;;        v  v
;; x --> [*][*]--> [*][/]
;;        |         |
;;        v         v
;;      'wow!      'b

;;z2 --> [*][*]--> [*][*]--> [*][/]
;;        |         |         |
;;        |         v         v
;;        |        'a        'b
;;        |                   ^
;;        |                   |
;;        +------> [*][*]--> [*][/]
;;                  |
;;                  v
;;                'wow!

;; Excercise 3.16:
;; Ben Bitdiddle decides to write a procedure to count
;; the number of pairs in any list structure. “It’s easy,” he reasons.
;; “The number of pairs in any structure is the number in the car plus
;; the number in the cdr plus one more to count the current pair.” So
;; Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Show that this procedure is not correct. In particular, draw boxand-
;; pointer diagrams representing list structures made up of exactly
;; three pairs for which Ben’s procedure would return 3; return 4
;; return 7; never return at all.

(define str1 '(foo bar baz)) 
(count-pairs str1) ; 3 
; str1 -> ( . ) -> ( . ) -> ( . ) ->null 
;          |        |        | 
;          v        v        v 
;         'foo     'bar     'baz 
  
(define xx '(foo)) 
(define yy (cons xx xx)) 
(define str2 (list yy)) 
(count-pairs str2) ; 4 
; str2 -> ( . ) -> null 
;          | 
;          v 
;         ( . ) 
;          | | 
;          v v 
;         ( . ) -> 'null 
;          | 
;          v 
;         'foo 
  
(define str3 (cons yy yy)) 
(count-pairs str3) ; 7 
; str3 -> ( . ) 
;          | | 
;          v v 
;         ( . ) 
;          | | 
;          v v 
;         ( . ) -> null 
;          | 
;          v 
;         'foo 
  
(define str4 '(foo bar baz)) 
(set-cdr! (cddr str4) str4) 
;(count-pairs str4) ; maximum recursion depth exceeded 
;          ,-------------------, 
;          |                   | 
;          v                   | 
; str4 -> ( . ) -> ( . ) -> ( . ) 
;          |        |        | 
;          v        v        v 
;         'foo     'bar     'baz

;; Excercise 3.17:
;; Devise a correct version of the count-pairs procedure
;; of Exercise 3.16 that returns the number of distinct pairs in
;; any structure. (Hint: Traverse the structure, maintaining an auxiliary
;; data structure that is used to keep track of which pairs have
;; already been counted.)

(define (count-real-pairs x)
  (length (inner x '())))

(define (false? v)
  (if (pair? v)
      #f
      #t))

(define (inner x memo-list)
  (if (and (pair? x) (false? (memq x memo-list)))
      (inner (car x)
             (inner (cdr x)
                    (cons x memo-list)))
      memo-list))

;; Excercise 3.18:
;; Write a procedure that examines a list and determines
;; whether it contains a cycle, that is, whether a program that
;; tried to find the end of the list by taking successive cdrs would go
;; into an infinite loop. Exercise 3.13 constructed such lists.

;; 网上抄的解法，因为第一次想就已经找到常量空间解法了。

(define (isloop? lst)
    (let ((identity (cons '() '())))
        (define (iter remain-list)
            (cond ((null? remain-list)
                    #f)
                  ((eq? identity (car remain-list))
                    #t)
                  (else
                    (set-car! remain-list identity)
                    (iter (cdr remain-list)))))
        (iter lst)))

;; Excercise 3.19:
;; Redo Exercise 3.18 using an algorithm that takes
;; only a constant amount of space. (This requires a very clever idea.)

;; 使用快慢步解决这个问题，快的每次走两步，慢的每次走一步，如果有环那么肯定会相遇


(define (loop? lst)
  (define (safe-cdr l)
    (if (pair? l)
        (cdr l)
        '()))
  (define (iter a b)
    (cond [(or (null? a) (null? b)) #f]
          [(eq? a b) #t]
          [(eq? a (safe-cdr b)) #t]
          [else
           (iter (safe-cdr a) (safe-cdr (safe-cdr b)))]))
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))

;; Excercise 3.20:
;; Draw environment diagrams to illustrate the evaluation
;; of the sequence of expressions

;(define x (cons 1 2))
;(define z (cons x x))
;(set-car! (cdr z) 17)
;(car x)
;17

;; using the procedural implementation of pairs given above
;; (Compare Excercise 3.11.)

;;          +------------------------------+
;;global -> |                              |
;;env       |  x                           |
;;          +--|---------------------------+
;;             |           ^
;;             |           |
;;             |        +----------+
;;             |  E1 -> | x: 1     |
;;             |        | y: 2     |
;;             |        |          |
;;             |        | set-x! -----> ...
;;             |        | set-y! -----> ...
;;             +--------->dispatch ---> parameters: m
;;                      |          |    body: (cond ((eq? m 'car) 'car)
;;                      +----------+                ((eq? m 'cdr) 'cdr)
;;                                                  ((eq? m 'set-car!) 'set-car!)
;;                                                  ((eq? m 'set-cdr!) 'set-cdr!)
;;                                                  (else
;;                                                    (error "..." m)))

;;          +-------------------------------------------------------+
;;global -> |                                                       |
;;env       |  z                           x                        |
;;          +--|---------------------------|------------------------+
;;             |           ^               |          ^
;;             |           |               |          |
;;             |           |               |      +----------+
;;             |           |               |      | x: 1     |
;;             |           |               |      | y: 2     |
;;             |           |               |      |          |
;;             |           |               |      | set-x! -----> ...
;;             |           |               |      | set-y! -----> ...
;;             |           |               +------->dispatch ---> parameters: m
;;             |           |                      |  ^ ^     |    body: ...
;;             |           |                      +--|-|-----+
;;             |        +----------+                 | |
;;             |  E2 -> | x: ------------------------+ |
;;             |        | y: --------------------------+
;;             |        |          |
;;             |        | set-x! -----> ...
;;             |        | set-y! -----> ...
;;             +--------->dispatch ---> parameters: m
;;                      |          |    body: (cond ((eq? m 'car) 'car)
;;                      +----------+                ((eq? m 'cdr) 'cdr)
;;                                                  ((eq? m 'set-car!) 'set-car!)
;;                                                  ((eq? m 'set-cdr!) 'set-cdr!)
;;                                                  (else
;;                                                    (error "..." m)))

;; (set-car! (cdr z) 17)有以下两个步骤：
;; 1. 执行(cdr z)返回x
;; 2. 执行(set-car! x 17)，引发表达式((x 'set-car!) 17)执行，然后引发(set-x! 17)

;; 最终x的car被设置为17

;;          +-------------------------------------------------------+
;;global -> |                                                       |
;;env       |  z                           x                        |
;;          +--|---------------------------|------------------------+
;;             |           ^               |          ^
;;             |           |               |          |
;;             |           |               |      +----------+
;;             |           |               |      | x: 17    |
;;             |           |               |      | y: 2     |
;;             |           |               |      |          |
;;             |           |               |      | set-x! -----> ...
;;             |           |               |      | set-y! -----> ...
;;             |           |               +------->dispatch ---> parameters: m
;;             |           |                      |  ^ ^     |    body: ...
;;             |           |                      +--|-|-----+
;;             |        +----------+                 | |
;;             |  E2 -> | x: ------------------------+ |
;;             |        | y: --------------------------+
;;             |        |          |
;;             |        | set-x! -----> ...
;;             |        | set-y! -----> ...
;;             +--------->dispatch ---> parameters: m
;;                      |          |    body: (cond ((eq? m 'car) 'car)
;;                      +----------+                ((eq? m 'cdr) 'cdr)
;;                                                  ((eq? m 'set-car!) 'set-car!)
;;                                                  ((eq? m 'set-cdr!) 'set-cdr!)
;;                                                  (else
;;                                                    (error "..." m)))