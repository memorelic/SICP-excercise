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