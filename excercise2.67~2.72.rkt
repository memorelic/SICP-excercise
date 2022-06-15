#lang racket

;; Huffman tree

;; leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


;; tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else
         (error "bad bit -- CHOOSE-BRANCH" bit)]))

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) (cons x set)]
        [else
         (cons (car set)
               (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; Excercise 2.67:
;; Define an encoding tree and a sample message:
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; Use the decode procedure to decode the message, and give the result.

;  / \
; A  / \
;   B  / \
;      D  C
; (decode sample-message sample-tree) => '(A D A B B C A)

;; Excercise 2.68:
;; The encode procedure takes as arguments a message
;; and a tree and produces the list of bits that gives the encoded
;; message.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; Encode-symbol is a procedure, which you must write, that returns
;; the list of bits that encodes a given symbol according to a given
;; tree. You should design encode-symbol so that it signals an error if
;; the symbol is not in the tree at all. Test your procedure by encoding
;; the result you obtained in Excercise 2.67 with the sample tree and
;; seeing whether it is the same as the original sample message.

(define (encode-symbol sym tree)
  (cond [(leaf? tree) '()]
        [(symbol-in-tree? sym (left-branch tree))
         (cons 0 (encode-symbol sym (left-branch tree)))]
        [(symbol-in-tree? sym (right-branch tree))
         (cons 1 (encode-symbol sym (right-branch tree)))]
        [else
         (error "This Symbol not in tree: " sym)]))

(define (symbol-in-tree? given-sym tree)
   (foldr (lambda (x y) (or x y)) false (map (lambda (x) (eq? given-sym x)) (symbols tree))))

;; (encode '(A D A B B C A) sample-tree)

;; Excercise 2.69:
;; The following procedure takes as its argument a list
;; of symbol-frequency pairs (where no symbol appears in more than
;; one pair) and generates a Huffman encoding tree according to the
;; Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; Make-leaf-set is the procedure given above that transforms the
;; list of pairs into an ordered set of leaves. Successive-merge is the
;; procedure you must write, using make-code-tree to successively
;; merge the smallest-weight elements of the set until there is only
;; one element left, which is the desired Huffman tree. (This procedure
;; is slightly tricky, but not really complicated. If you find yourself
;; designing a complex procedure, then you are almost certainly
;; doing something wrong. You can take significant advantage of the
;; fact that we are using an ordered set representation.)

(define (successive-merge ordered-set)
  (cond [(= 0 (length ordered-set)) '()]
        [(= 1 (length ordered-set)) (car ordered-set)]
        [else
         (let ((new-sub-tree (make-code-tree (car ordered-set)
                                             (cadr ordered-set)))
               (remained-ordered-set (cddr ordered-set)))
           (successive-merge (adjoin-set new-sub-tree remained-ordered-set)))]))

;(encode '(A D A B B C A) (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

;; Excercise 2.70:
;; The following eight-symbol alphabet with associated
;; relative frequencies was designed to efficiently encode the lyrics
;; of 1950s rock songs. (Note that the “symbols” of an “alphabet”
;; need not be individual letters.)

;; A 2    | NA 16
;; BOOM 1 | SHA 3
;; GET 2  | YIP 9
;; JOB 2  | WAH 1

;; Use generate-huffman-tree (Exercise 2.69) to generate a corresponding
;; Huffman tree, and use encode (Exercise 2.68) to encode
;; the following message:

;; Get a job
;; Sha na na na na na na na na
;; Get a job
;; Sha na na na na na na na na
;; Wah yip yip yip yip yip yip yip yip yip
;; Sha boom

;; How many bits are required for the encoding? What is the smallest
;; number of bits that would be needed to encode this song ifwe used
;; a fixed-length code for the eight-symbol alphabet?

(define lyric-tree (generate-huffman-tree
                    '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(define msg-1 '(GET A JOB))
(define msg-2 '(SHA NA NA NA NA NA NA NA NA))
(define msg-3 '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP))
(define msg-4 '(SHA BOOM))

(define lyric-binary-digit-scount
  (+ (* (length (encode msg-1 lyric-tree)) 2)
     (* (length (encode msg-2 lyric-tree)) 2)
     (length (encode msg-3 lyric-tree))
     (length (encode msg-4 lyric-tree))))

;; This piece of lyrics cost 84 digits,
;; If we use fixed length code, we at least need 3 digits to encode each word.
;; at last, we need 36 * 3 = 108 digits to encode all words.

;; Excercise 2.71:
;; Suppose we have a Huffman tree for an alphabet
;; of n symbols, and that the relative frequencies of the symbols are
;; 1, 2, 4, . . . , 2^(n - 1). Sketch the tree for n = 5; for n = 10. In such a
;; tree (for general n) how many bits are required to encode the most
;; frequent symbol? The least frequent symbol?

;; n = 5
;;        *
;;       /\
;;      *  16
;;     /\
;;    *  8
;;   / \
;;  *   4
;; /\
;;1  2

;; n = 10
#|
                  *
                 /\
                *  512
               /\
              *  256
             /\
            * 128
           /\
          *  64
         /\
        *  32
       /\
      *  16
     /\
    *  8
   / \
  *   4
 /\
1  2
|#

;; The most frequent symbol use 1 digit to encode;
;; The least frequent symbol use n - 1 digits to encode;

;; Excercise 2.72:
;; Consider the encoding procedure that you designed
;; in Exercise 2.68. What is the order of growth in the number of steps
;; needed to encode a symbol? Be sure to include the number of steps
;; needed to search the symbol list at each node encountered. To answer
;; this question in general is difficult. Consider the special case
;; where the relative frequencies of the n symbols are as described in
;; Exercise 2.71, and give the order of growth (as a function of n) of
;; the number of steps needed to encode the most frequent and least
;; frequent symbols in the alphabet.

;; accroding to the answer of Excercise 2.71,
;; the most frequent symbol need to drop down 1 floor to be find,
;; the least frequent symbol need to drop down n - 1 floor to be find.
;; So if if encoding symbol times is n, For the most frequent symbol,
;; it cost O(n), for the least symbol it cost O(n^2)