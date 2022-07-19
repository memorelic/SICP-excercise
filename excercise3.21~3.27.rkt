#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; Excercise 3.21:
;; Ben Bitdiddle decides to test the queue implementation
;; described above. He types in the procedures to the Lisp interpreter
;; and proceeds to try them out:

(define q1 (make-queue))
(insert-queue! q1 'a)
;; => ((a) a)
(insert-queue! q1 'b)
;; => ((a b) b)
(delete-queue! q1)
;; => ((b) b)
(delete-queue! q1)
;; => (() b)

;; “It’s all wrong!” he complains. “The interpreter’s response shows
;; that the last item is inserted into the queue twice. And when I
;; delete both items, the second b is still there, so the queue isn’t
;; empty, even though it’s supposed to be.” Eva Lu Ator suggests
;; that Ben has misunderstood what is happening. “It’s not that
;; the items are going into the queue twice,” she explains. “It’s
;; just that the standard Lisp printer doesn’t know how to make
;; sense of the queue representation. If you want to see the queue
;; printed correctly, you’ll have to define your own print procedure
;; for queues.” Explain what Eva Lu is talking about. In particular,
;; show why Ben’s examples produce the printed results that they do.
;; Define a procedure print-queue that takes a queue as input and
;; prints the sequence of items in the queue.

;; 当解释器打印出((a b) b)的时候，实际上是将q1变量的car和cdr部分都打印了出来，
;; 其中car的部分指向(a b)，而cdr部分指向b，这不仅暴露了队列的底层实现，而且还会让人造成误会。
;; 实际上cdr只是标识了queue的末尾，不用打印

(define (print-queue queue)
  (car queue))

;; Excercise 3.22:
;; Instead of representing a queue as a pair of pointers,
;; we can build a queue as a procedure with local state. The local state
;; will consist of pointers to the beginning and the end of an ordinary
;; list. Thus, the make-queue procedure will have the form

;(define (make-queue)
;  (let ((front-ptr ... )
;        (rear-ptr ... ))
;    hdefinitions of internal proceduresi
;    (define (dispatch m) ...)
;    dispatch))
 
;; Complete the definition of make-queue and provide implementations
;; of the queue operations using this representation.

(define (make-queue-oop)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (insert-queue! item)
      (cond [(empty-queue?)
             (let ((init-list (list item)))
               (set! front-ptr init-list)
               (set! rear-ptr init-list)
               front-ptr)]
            [else
             (let ((new-item (list item)))
               (set-cdr! rear-ptr new-item)
               (set! rear-ptr new-item)
               front-ptr)]))
    (define (delete-queue!)
      (cond [(empty-queue?)
             (error "DELETE! called with an empty queue")]
            [else
             (set! front-ptr (cdr front-ptr))
             front-ptr]))
    (define (empty-queue?)
      (null? front-ptr))
    (define (dispatch m)
      (cond [(eq? m 'insert-queue!)
             insert-queue!]
            [(eq? m 'delete-queue!)
             (delete-queue!)]
            [(eq? m 'empty-queue?)
             (empty-queue?)]
            [else
             (error "Unknow operation -- DISPATCH" m)]))
    dispatch))
            

;; Excercise 3.23:
;; A deque (“double-ended queue”) is a sequence in
;; which items can be inserted and deleted at either the front or the
;; rear. Operations on deques are the constructor make-deque, the
;; predicate empty-deque?, selectors front-deque and rear-deque,
;; and mutators front-insert-deque!, rear-insert-deque!,
;; front-delete-deque! and rear-delete-deque!. Show how to
;; represent deques using pairs, and give implementations of the
;; operations.23 All operations should be accomplished in O(1) steps.

;; ptr setter



(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT-DEQUE called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
    (if (empty-deque? deque)
        (error "REAR-DEQUE called with an empty deque" deque)
        (car (rear-ptr deque))))

(define (insert-rear-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond [(empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque]
          [else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           deque])))

(define (delete-front-deque! deque)
  (cond [(empty-deque? deque)
         (error "DELETE-FRONT-DEQUE! called with an empty deque" deque)]
        [else
         (set-front-ptr! deque (cdr (front-ptr deque)))
         deque]))

(define (insert-front-deque! deque item)
  (cond [(empty-deque? deque)
         (insert-rear-deque! deque item)]
        [else
         (set-front-ptr! deque (cons item (front-ptr deque)))
         deque]))

(define (delete-rear-deque! deque)
  (define (iter deque lst)
    (cond [(null? (cdr (cdr lst)))
           (set-cdr! lst '())
           (set-rear-ptr! deque lst)
           deque]
          [else
           (iter deque (cdr lst))]))
  (cond [(empty-deque? deque)
         (error "DELETE-REAR-DEQUE! called with an empty deque" deque)]
        [(null? (cdr (front-ptr deque))) ; 长度为1时
         (set-front-ptr! deque '())
         deque]
        [else
         (iter deque (front-ptr deque))])) ; 长度大于1时

(define (print-deque deque)
    (car deque))