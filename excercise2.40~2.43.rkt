#lang racket

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (square x) (* x x))
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (next test-divisor))]))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

;; Excercise 2.40:
;; Define a procedure unique-pairs that, given an integer
;; n, generates the sequence of pairs (i , j) with 1 <= j < i <= n.
;; Use unique-pairs to simplify the definition of prime-sum-pairs
;; given above.

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; Excercise 2.41:
;; Write a procedure to find all ordered triples of distinct
;; positive integers i , j , and k less than or equal to a given integer
;; n that sum to a given integer s.



;; Excercise 2.42:
;; The “eight-queens puzzle” asks how to place eight
;; queens on a chessboard so that no queen is in check from any
;; other (i.e., no two queens are in the same row, column, or diagonal).
;; One possible solution is shown in Figure 2.8. One way to solve
;; the puzzle is to work across the board, placing a queen in each column.
;; Once we have placed k ¡ 1 queens, we must place the kth
;; queen in a position where it does not check any of the queens already
;; on the board. We can formulate this approach recursively:
;; Assume that we have already generated the sequence of all possible
;; ways to place k ¡ 1 queens in the first k ¡ 1 columns of the
;; board. For each of these ways, generate an extended set of positions
;; by placing a queen in each row of the kth column. Now filter
;; these, keeping only the positions for which the queen in the kth
;; column is safe with respect to the other queens. This produces the
;; sequence of all ways to place k queens in the first k columns. By
;; continuing this process, we will produce not only one solution, but
;; all solutions to the puzzle.

;; We implement this solution as a procedure queens, which returns
;; a sequence of all solutions to the problem of placing n queens on
;; an n*n chessboard. Queens has an internal procedure queen-cols
;; that returns the sequence of all ways to place queens in the first k
;; columns of the board.

;(define (queens board-size)
;  (define (queen-cols k)
;    (if (= k 0)
;        (list empty-board)
;        (filter
;         (lambda (positions) (safe? k positions))
;         (flatmap
;          (lambda (rest-of-queens)
;            (map (lambda (new-row)
;                   (adjoin-position new-row
;                                    k
;                                    rest-of-queens))
;                 (enumerate-interval 1 board-size)))
;          (queen-cols (- k 1))))))
;  (queen-cols board-size))

;; In this procedure rest-of-queens is a way to place k ¡ 1 queens
;; in the first k ¡ 1 columns, and new-row is a proposed row in which
;; to place the queen for the kth column. Complete the program
;; by implementing the representation for sets of board positions,
;; including the procedure adjoin-position, which adjoins a new
;; row-column position to a set of positions, and empty-board,
;; which represents an empty set of positions. You must also write
;; the procedure safe?, which determines for a set of positions,
;; whether the queen in the kth column is safe with respect to the
;; others. (Note that we need only check whether the new queen is
;; safe—the other queens are already guaranteed safe with respect to
;; each other.)

;; Excercise 2.43:
;; Louis Reasoner is having a terrible time doing
;; Exercise 2.42. His queens procedure seems to work, but it runs
;; extremely slowly. (Louis never does manage to wait long enough
;; for it to solve even the 6 £ 6 case.) When Louis asks Eva Lu Ator
;; for help, she points out that he has interchanged the order of the
;; nested mappings in the flatmap, writing it as

;(flatmap
; (lambda (new-row)
;   (map (lambda (rest-of-queens)
;          (adjoin-position new-row k rest-of-queens))
;        (queen-cols (- k 1))))
; (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run slowly.
;; Estimate how long it will take Louis’s program to solve the
;; eight-queens puzzle, assuming that the program in Exercise 2.42
;; solves the puzzle in time T.