#lang racket

(define (element-of-set? x set)
  (cond [(null? set) false]
        [(equal? x (car set)) true]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))

;; Excercise 2.59:
;; Implement the union-set operation for the
;; unordered-list representation of sets.

(define (union-set set1 set2) 
   (cond [(null? set1) set2]
         [(null? set2) set1] 
         [(element-of-set? (car set1) set2) 
          (union-set (cdr set1) set2)]
         [else (union-set (cdr set1) (cons (car set1) set2))]))

;; 可以用之前的accumulate和filter函数重写这几个函数
;; example:
(define (union s1 s2) 
   (append s1 (filter (lambda (x) 
                        (not (element-of-set? x s1))) 
                      s2)))


;; Excercise 2.60:
;; We specified that a set would be represented as a
;; list with no duplicates. Now suppose we allow duplicates. For
;; instance, the set {1, 2, 3} could be represented as the list (2 3 2
;; 1 3 2 2). Design procedures element-of-set?, adjoin-set,
;; union-set, and intersection-set that operate on this representation.
;; How does the efficiency of each compare with the
;; corresponding procedure for the non-duplicate representation?
;; Are there applications for which you would use this representation
;; in preference to the non-duplicate one?

;; function element-of-set? can be used in both normal version and allow duplicate version.
;; and I don't if we need to get rid of dupes of two set in union-set function

(define (adjoin-set-dp x set)
  (cons x set))

(define (union-set-dp set1 set2)
  (append set1 set2))


(define (element-of-set-ordered? x set)
  (cond [(null? set) false]
        [(= x (car set)) true]
        [(< x (car set)) false]
        [else (element-of-set-ordered? x (cdr set))]))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond [(= x1 x2)
               (cons x1 (intersection-set-ordered (cdr set1)
                                                  (cdr set2)))]
              [(< x1 x2)
               (intersection-set-ordered (cdr set1) set2)]
              [(< x2 x1)
               (intersection-set-ordered set1 (cdr set2))]))))

;; Excercise 2.61:
;; Give an implementation of adjoin-set using the
;; ordered representation. By analogy with element-of-set? show
;; how to take advantage of the ordering to produce a procedure that
;; requires on the average about half as many steps as with the unordered
;; representation.

(define (adjoin-set-ordered x set)
  (if (null? set)
      (list x)
      (let ((current-element (car set))
            (remain-element (cdr set)))
        (cond [(= x current-element) set]
              [(> x current-element)
               (cons current-element (adjoin-set-ordered x remain-element))]
              [(< x current-element)
               (cons x set)]))))

;; Excercise 2.62:
;; Give a O(n) implementation of union-set for sets
;; represented as ordered lists.

(define (union-set-ordered set1 set2)
  (cond [(and (null? set1) (null? set2)) '()]
        [(null? set1) set2]
        [(null? set2) set1]
        [else
         (let ((x (car set1))
               (y (car set2)))
           (cond [(= x y)
                  (cons x (union-set-ordered (cdr set1) (cdr set2)))]
                 [(< x y)
                  (cons x (union-set-ordered (cdr set1) set2))]
                 [(> x y)
                  (cons y (union-set-ordered set1 (cdr set2)))]))]))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (cond [(null? set) false]
        [(= x (entry set)) true]
        [(< x (entry set))
         (element-of-set-tree? x (left-branch set))]
        [(> x (entry set))
         (element-of-set-tree? x (right-branch set))]))

(define (adjoin-set-tree x set)
  (cond [(null? set) (make-tree x '() '())]
        [(= x (entry set)) set]
        [(< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set))]
        [(> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set)))]))

;; Excercise 2.63:
;; Each of the following two procedures converts a binary
;; tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;; a.Do the two procedures produce the same result for every tree? If
;;   not, how do the results differ? What lists do the two procedures
;;   produce for the trees in Figure 2.16?

;; b. Do the two procedures have the same order of growth in the
;;    number of steps required to convert a balanced tree with n elements
;;    to a list? If not, which one grows more slowly?

;; a)
;; Yes, obviously,
;; For the same tree, tree->list-1 and tree->list-2 result to the same list;
;; For the tree wich contains same element but different shape, they also produce same result.

;; b)
;; No, because solution1 call function append, and this will cost O(n) in time,
;; For a tree with n elements, it will call n times append, so this solution cost O(n^2) in time.
;; while solution2 just need to call function cons, which is O(1) in time,
;; So for the same tree with n elements, it just need O(n) in time.

;; Excercise 2.64:
;; The following procedure list->tree converts
;; an ordered list to a balanced binary tree. The helper procedure
;; partial-tree takes as arguments an integer n and list of at least
;; n elements and constructs a balanced tree containing the first n
;; elements of the list. The result returned by partial-tree is a pair
;; (formed with cons) whose car is the constructed tree and whose
;; cdr is the list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree
                                 (cdr non-left-elts)
                                 right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree
                       this-entry left-tree right-tree)
                      remaining-elts))))))))

;; a. Write a short paragraph explaining as clearly as you can how
;;    partial-tree works. Draw the tree produced by list->tree
;;    for the list (1 3 5 7 9 11).

;; (1 3)(5 7 9 11)             ; 分割左右子树

; (5 7 9 11)                  ; 创建 1 节点
;     /
;    /
; 1(3)

;   (5 7 9 11)               ; 创建 1 的左子树(空)
;      /
;     /
;   1(3)
;   /
;  /
;'()

;    (5 7 9 11)              ; 创建 1 的右子树(包含 3)
;      /
;     /
;    1
;   / \
;  /   \
;'()    3

;       5 (7 9 11)           ; 创建树根 5
;      /
;     /
;    1
;   / \
;  /   \
;'()    3

;       5                    ; 创建 9 节点
;      / \
;     /   \
;    1     9 (7 11)
;   / \
;  /   \
;'()    3

;         5                  ; 创建 9 的左子树(包含 7)
;        / \
;       /   \
;      /     \
;     /       \
;    1         9 (11)
;   / \       /
;  /   \     /
;'()    3   7

;         5                  ; 创建 9 的右子树(包含 11)
;        / \
;       /   \
;      /     \
;     /       \
;    1         9
;   / \       / \
;  /   \     /   \
;'()    3   7    11

;; b. What is the order of growth in the number of steps required by
;;    list->tree to convert a list of n elements?

;; For every element in a list, list->tree must run cons and make-tree with both O(1) in time to
;; combine this node, left-tree and right-tree.
;; So for a list which contains n elements, It will cost O(n) in time.

;; Excercise 2.65:
;; Use the results of Exercise 2.63 and Exercise 2.64 to
;; give O(n) implementations of union-set and intersection-set
;; for sets implemented as (balanced) binary trees

(define (union-tree set1 set2)
  (list->tree (intersection-set-ordered (tree->list-2 set1) (tree->list-2 set2))))

(define (intersection-tree set1 set2)
  (list->tree (union-set-ordered (tree->list-2 set1) (tree->list-2 set2))))

;; Excercise 2.66:
;; Implement the lookup procedure for the case where
;; the set of records is structured as a binary tree, ordered by the numerical
;; values of the keys.

;; return the key of a record
(define (key record)
  (void))

(define (lookup given-key records)
  (if (null? records)
      false
      (let ((entry-key (key (entry records))))
        (cond [(= given-key entry-key) given-key]
              [(< given-key entry-key)
               (lookup given-key (left-branch records))]
              [(> given-key entry-key)
               (lookup given-key (right-branch records))]))))
  