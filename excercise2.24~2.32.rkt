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

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;; b):
;; Using your selectors, define a procedure total-weight that returns
;; the total weight of a mobile.

(define (hangs-another-mobile? branch)
  (pair? (branch-structure branch)))

(define (branch-weight branch)
  (if (hangs-another-mobile? branch)
      (total-weight (branch-structure branch))
      (branch-structure branch)))
  
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; c):
;; A mobile is said to be balanced if the torque applied by its topleft
;; branch is equal to that applied by its top-right branch (that
;; is, if the length of the left rod multiplied by the weight hanging
;; from that rod is equal to the corresponding product for the right
;; side) and if each of the submobiles hanging off its branches is
;; balanced. Design a predicate that tests whether a binary mobile
;; is balanced.

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (same-torque? left right)
  (= (branch-torque left)
     (branch-torque right)))

(define (mobile-balance? mobile)
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
      (and                                        
       (same-torque? left right)
       (branch-balance? left)
       (branch-balance? right))))

(define (branch-balance? branch)
  (if (hangs-another-mobile? branch)
      (mobile-balance? (branch-structure branch))
      #t))

;; d):
;; Suppose we change the representation of mobiles so that the
;; constructors are

(define (make-mobile2 left right)
  (cons left right))

(define (make-branch2 length structure)
  (cons length structure))

;; How much do you need to change your programs to convert to
;; the new representation?

;; Just need to change 4 selectors to:

(define (left-branch2 mobile)
    (car mobile))

(define (right-branch2 mobile)
    (cdr mobile))

(define (branch-length2 branch)
    (car branch))

(define (branch-structure2 branch)
    (cdr branch))

;; Excercise 2.30:
;; Define a procedure square-tree analogous to the
;; square-list procedure of Exercise 2.21. That is, square-tree
;; should behave as follows:

;(square-tree
; (list 1
;       (list 2 (list 3 4) 5)
;       (list 6 7)))

;; => (1 (4 (9 16) 25) (36 (49))

;; Define square-tree both directly (i.e., without using any higherorder
;; procedures) and also by using map and recursion.

(define (square-tree tree)
  (define (square x) (* x x))
  (cond [(null? tree) '()]
        [(not (pair? tree)) (square tree)]
        [else
         (cons (square-tree (car tree))
               (square-tree (cdr tree)))]))

(define (square-tree2 tree)
  (define (square x) (* x x))
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree2 subtree)
             (square subtree)))
       tree))

;(square-tree2
; (list 1
;       (list 2 (list 3 4) 5)
;       (list 6 7)))

;; Excercise 2.31:
;; Abstract your answer to Exercise 2.30 to produce a
;; procedure tree-map with the property that square-tree could be
;; defined as

;; (define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  (map (lambda (subtree)
         (cond [(null? subtree) '()]
               [(not (pair? subtree)) (proc subtree)]
               [else
                (tree-map proc subtree)]))
       tree))

;; Excercise 2.32:
;; We can represent a set as a list of distinct elements,
;; and we can represent the set of all subsets of the set as a list of
;; lists. For example, if the set is (1 2 3), then the set of all subsets is
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the
;; following definition of a procedure that generates the set of subsets
;; of a set and give a clear explanation of why it works:

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; my mind has just been light on a wonderful solution to excercise 2.29,
;; I will finish it on days later.

;; In general, It will not need to define so much helper funcion,
;; just use recersion to solve the problem.