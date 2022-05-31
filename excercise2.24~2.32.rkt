#lang racket

;; Excercise 2.24:
;; Suppose we evaluate the expression
;; (list 1 (list 2 (list 3 4))).
;; Give the result printed by the interpreter, the
;; corresponding box-and-pointer structure, and the interpretation
;; of this as a tree (as in Figure 2.6).

;;   +---+---+  +---+---+
;;   | * | *-+->| * | / |
;;   +-+-+---+  +-+-+---+
;;     |          |   
;;     V          V      
;;   +---+      +---+---+  +---+---+
;;   | 1 |      | * | *-+->| * | / |
;;   +---+      +-+-+---+  +---+---+
;;                |          |
;;                V          V
;;              +---+      +---+---+  +---+---+
;;              | 2 |      | * | * +->| * | / |
;;              +---+      +-+-+---+  +-+-+---+
;;                           |          |
;;                           V          V
;;                         +---+      +---+
;;                         | 3 |      | 4 |
;;                         +---+      +---+

;; Result should be: (1 (2 (3 4)))

;; Excercise 2.25:
;; Give combinations of cars and cdrs that will pick 7
;; from each of the following lists:

;(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
;(car (car '((7))))
;(define l (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))

;; Excercise 2.26:
;; Suppose we define l1 and l2 to be two lists:

(define l1 (list 1 2 3))
(define l2 (list 4 5 6))

;; What result is printed by the interpreter in response to evaluating
;; each of the following expressions:

;(append l1 l2)
;; => (1 2 3 4 5 6)
;(cons l1 l2)
;; => ((1 2 3) 4 5 6)
;(list l1 l2)
;; => ((1 2 3) (4 5 6))

;; Excercise 2.27:
;; Modify your reverse procedure of Exercise 2.18 to
;; produce a deep-reverse procedure that takes a list as argument
;; and returns as its value the list with its elements reversed and with
;; all sublists deep-reversed as well. For example,

(define lx (list (list 1 2) (list 3 4)))

;; lx
;; => ((1 2) (3 4))
;; (reverse lx)
;; => ((3 4) (1 2))
;; (deep-reverse lx)
;; => ((4 3) (2 1))

(define (deep-reverse items)
  (cond [(null? items) '()]
        [(not (pair? items)) items]
        [else
         (list (deep-reverse (car (cdr items)))
               (deep-reverse (car items)))]))

;; Excercise 2.28:
;; Write a procedure fringe that takes as argument a
;; tree (represented as a list) and returns a list whose elements are all
;; the leaves of the tree arranged in left-to-right order. For example,

;; lx has been defined in Excercise 2.27

;; (fringe lx)
;; => (1 2 3 4)
;; (fringe (list lx lx))
;; => (1 2 3 4 1 2 3 4)

(define (fringe items)
  (cond [(null? items) '()]
        [(not (pair? items)) (list items)]
        [else
         (append (fringe (car items))
                 (fringe (car (cdr items))))]))

;; Excercise 2.29:
;; A binary mobile consists of two branches, a left
;; branch and a right branch. Each branch is a rod of a certain
;; length, from which hangs either a weight or another binary
;; mobile. We can represent a binary mobile using compound data
;; by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number)
;; together with a structure, which may be either a number (representing
;; a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; a):
;; Write the corresponding selectors left-branch and rightbranch,
;; which return the branches of a mobile, and branchlength
;; and branch-structure, which return the components
;; of a branch.

;; b):
;; Using your selectors, define a procedure total-weight that returns
;; the total weight of a mobile.

;; c):
;; A mobile is said to be balanced if the torque applied by its topleft
;; branch is equal to that applied by its top-right branch (that
;; is, if the length of the left rod multiplied by the weight hanging
;; from that rod is equal to the corresponding product for the right
;; side) and if each of the submobiles hanging off its branches is
;; balanced. Design a predicate that tests whether a binary mobile
;; is balanced.

;; d):
;; Suppose we change the representation of mobiles so that the
;; constructors are

;; How much do you need to change your programs to convert to
;; the new representation?