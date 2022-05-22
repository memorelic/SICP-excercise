#lang sicp

;; Excercise 1.20:
;; The process that a procedure generates is of course
;; dependent on the rules used by the interpreter. As an example,
;; consider the iterative gcd procedure given above. Suppose we
;; were to interpret this procedure using normal-order evaluation,
;; as discussed in Section 1.1.5. (The normal-order-evaluation rule
;; for if is described in Exercise 1.5.) Using the substitution method
;; (for normal order), illustrate the process generated in evaluating
;; (gcd 206 40) and indicate the remainder operations that are
;; actually performed. How many remainder operations are actually
;; performed in the normal-order evaluation of (gcd 206 40)? In
;; the applicative-order evaluation?

; 
; (gcd 206 40)
; 
; (if (= 40 0)
;     206
;     (gcd 40 (remainder 206 40)))
; 
; (gcd 40 (remainder 206 40))
; 
; (if (= (remainder 206 40) 0)
;     40
;     (gcd (remainder 206 40)
;          (remainder 40 (remainder 206 40))))
; ; 1*remainder
; (if (= 6 0)
;     40
;     (gcd (remainder 206 40)
;          (remainder 40 (remainder 206 40))))
; 
; (gcd (remainder 206 40)
;      (remainder 40 (remainder 206 40)))
; 
; (if (= (remainder 40 (remainder 206 40)) 0)
;     (remainder 206 40)
;     (gcd (remainder 40 (remainder 206 40))
;          (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; ; 1*remainder
; (if (= (remainder 40 6) 0)
;     (remainder 206 40)
;     (gcd (remainder 40 (remainder 206 40))
;          (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; ; 1*remainder
; (if (= 4 0)
;     (remainder 206 40)
;     (gcd (remainder 40 (remainder 206 40))
;          (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; 
; (gcd (remainder 40 (remainder 206 40))
;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; 
; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
;     (remainder 40 (remainder 206 40))
;     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;          (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; ; 4*remainder
; (if (= 2 0)
;     (remainder 40 (remainder 206 40))
;     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;          (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; 
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; 
; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;     (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder a  (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; ; 7*remainder
; (if (= 0 0)
;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;     (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder a  (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; 
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; ; 4*remainder
; 
; normal-order evaluation call remainder 18 times


; 
; (gcd 206 40)
; 
; (if (= 40 0)
;     206
;     (gcd 40 (remainder 206 40))) ; 1*remainder
; 
; (gcd 40 6)
; 
; (if (= 6 0)
;     40
;     (gcd 6 (remainder 40 6))) ; 1*remainder
; 
; (gcd 6 4)
; 
; (if (= 4 0)
;     6
;     (gcd 4 (remainder 6 4))) ; 1*remainder
; 
; (gcd 4 2)
; 
; (if (= 2 0)
;     4
;     (gcd 2 (remainder 4 2))) ; 1*remainder
; 
; (gcd 2 0)
; 
; (if (= 0 0)
;     2
;     (gcd 0 (remainder 2 0)))
; 
; applicative-order evaluation call remainder 4 times

(define (smallest-divisor n)
  (find-divisor n 2))

;(define (find-divisor n test-divisor)
;  (define (square x) (* x x))
;  (cond [(> (square test-divisor) n) n]
;        [(divides? test-divisor n) test-divisor]
;        [else (find-divisor n (+ test-divisor 1))]))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (define (square x) (* x x))
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m)]
        [else
         (remainder (* base (expmod base (- exp 1) m)) m)]
        ))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))

;; Exercise 1.21:
;; Use the smallest-divisor procedure to find the
;; smallest divisor of each of the following numbers: 199, 1999, 19999.

; (smallest-divisor 199)
; (smallest-divisor 1999)
; (smallest-divisor 19999)

;; Exercise 1.22:
;; Most Lisp implementations include a primitive
;; called runtime that returns an integer that specifies the amount
;; of time the system has been running (measured, for example,
;; in microseconds). The following timed-prime-test procedure,
;; when called with an integer n, prints n and checks to see if n is
;; prime. If n is prime, the procedure prints three asterisks followed
;; by the amount of time used in performing the test.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; Using this procedure, write a procedure search-for-primes that
;; checks the primality of consecutive odd integers in a specified
;; range.
;; Use your procedure to find the three smallest primes larger
;; than 1000; larger than 10,000; larger than 100,000; larger than
;; 1,000,000.
;; Note the time needed to test each prime.
;; Since the testing algorithm has order of growth of O(sqrt(n)),
;; you should expect that testing for primes around 10,000
;; should take about sqrt(10) times as long as testing for primes around 1000. Do your timing data
;; bear this out? How well do the data for 100,000 and 1,000,000
;; support the O(sqrt(n) prediction? Is your result compatible with the
;; notion that programs on your machine run in time proportional
;; to the number of steps required for the computation?

(define (search-for-primes start n)
  (cond
    [(= n 0) (newline)]
    [(even? start)
         (search-for-primes (+ start 1) n)]
    [(prime? start)
     (begin
       (timed-prime-test start)
       (search-for-primes (+ start 2) (- n 1)))]
    [else
         (search-for-primes (+ start 2) n)]))

;; 1009    --- 4ms
;; 1013    --- 2ms
;; 1019    --- 2ms
;; 10007   --- 6ms
;; 10009   --- 4ms
;; 10037   --- 5ms
;; 100003  --- 14ms
;; 100019  --- 13ms
;; 100043  --- 13ms
;; 1000003 --- 43ms
;; 1000033 --- 40ms
;; 1000037 --- 41ms

;; From our timing data, we can observe that,
;; when increasing the tested number of a factor 10,
;; the required time increases roughly of a factor 3.
;; By noting that 3 ≅ √10,
;; we can confirm both the growth prediction and the notion
;; that programs run in a time proportional to the steps required for the computation.

;; Excercise 1.23: 
;; The smallest-divisor procedure shown at the
;; start of this section does lots of needless testing: After it checks to
;; see if the number is divisible by 2 there is no point in checking to
;; see if it is divisible by any larger even numbers. This suggests that
;; the values used for test-divisor should not be 2, 3, 4, 5, 6, . . . ,
;; but rather 2, 3, 5, 7, 9, . . . . To implement this change, define a procedure
;; next that returns 3 if its input is equal to 2 and otherwise
;; returns its input plus 2. Modify the smallest-divisor procedure
;; to use (next test-divisor) instead of (+ test-divisor 1).
;; With timed-prime-test incorporating this modified version of
;; smallest-divisor, run the test for each of the 12 primes found
;; in Exercise 1.22. Since this modification halves the number of
;; test steps, you should expect it to run about twice as fast. Is this
;; expectation confirmed? If not, what is the observed ratio of the
;; speeds of the two algorithms, and how do you explain the fact that
;; it is different from 2?

(define (find-divisor n test-divisor)
  (define (square x) (* x x))
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (next test-divisor))]))

;; 1009 *** 3
;; 1013 *** 2
;; 1019 *** 2
;; 10007 *** 5
;; 10009 *** 3
;; 10037 *** 4
;; 100003 *** 10
;; 100019 *** 9
;; 100043 *** 8
;; 1000003 *** 24
;; 1000033 *** 24
;; 1000037 *** 24

;; it's improved about 113% to 164%
;; I think it could because we add a new function call, and there is a predicate dicision in it;

;; Excercise 1.24:
;; Modify the timed-prime-test procedure of
;; Exercise 1.22 to use fast-prime? (the Fermat method), and test
;; each of the 12 primes you found in that exercise. Since the Fermat
;; test has O(log n) growth, how would you expect the time to test
;; primes near 1,000,000 to compare with the time needed to test
;; primes near 1000? Do your data bear this out? Can you explain
;; any discrepancy you find?

;; 1009 *** 6
;; 1013 *** 2
;; 1019 *** 2
;; 10007 *** 7
;; 10009 *** 3
;; 10037 *** 3
;; 100003 *** 8
;; 100019 *** 4
;; 100043 *** 3
;; 1000003 *** 9
;; 1000033 *** 4
;; 1000037 *** 4

;; it almost obey O(log n) growth.

;; Excercise 1.25:
;; Alyssa P. Hacker complains that we went to a lot of
;; extra work in writing expmod. After all, she says, since we already
;; know how to compute exponentials, we could have simply written

;(define (expmod base exp m)
;  (remainder (fast-expt base exp) m))

;; Is she correct? Would this procedure serve aswell for our fast prime
;; tester? Explain.

;; 这种写法逻辑上没有问题，但是在实际计算中将会产生巨大的中间值，导致内存用尽从而无法完成计算。

;; Excercise 1.26:
;; Louis Reasoner is having great difficulty doing
;; Exercise 1.24. His fast-prime? test seems to run more slowly than
;; his prime? test. Louis calls his friend Eva Lu Ator over to help.
;; When they examine Louis’s code, they find that he has rewritten
;; the expmod procedure to use an explicit multiplication, rather than
;; calling square:

;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (* (expmod base (/ exp 2) m)
;                       (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base
;                       (expmod base (- exp 1) m))
;                    m))))

;; “I don’t see what difference that could make,” says Louis. “I do.”
;; says Eva. “By writing the procedure like that, you have transformed
;; the (log n) process into a (n) process.” Explain.

;; 这种写法将一个线性递归变为树形递归，树形递归的执行时间是指数级增长的，只是因为这里增长的指数为log2^n
;; log2^n = nlog2 => O(n)

;; Excercise 1.27:
;; Demonstrate that the Carmichael numbers listed in
;; Footnote 1.47 really do fool the Fermat test. That is, write a procedure
;; that takes an integer n and tests whether an is congruent to
;; a modulo n for every a < n, and try your procedure on the given
;; Carmichael numbers.

(define (carmichael-test n)
  (define (try-it n a)
    (cond [(= a 1) #t]
          [(not (= (expmod a n n) a)) #f]
          [else (try-it n (- a 1))]))
  (try-it n (- n 1)))

;(carmichael-test 561)  #t
;(carmichael-test 1105) #t
;(carmichael-test 1729) #t
;(carmichael-test 2465) #t
;(carmichael-test 2821) #t
;(carmichael-test 6601) #t

;; One variant of the Fermat test that cannot be fooled
;; is called the Miller-Rabin test (Miller 1976; Rabin 1980). This starts
;; from an alternate form of Fermat’s Little Theorem, which states
;; that if n is a prime number and a is any positive integer less than n,
;; then a raised to the (n - 1)-st power is congruent to 1 modulo n. To
;; test the primality of a number n by theMiller-Rabin test, we pick a
;; random number a < n and raise a to the (n - 1)-st power modulo n
;; using the expmod procedure. However, whenever we performthe
;; squaring step in expmod, we check to see if we have discovered a
;; “nontrivial square root of 1 modulo n,” that is, a number not equal
;; to 1 or n - 1 whose square is equal to 1 modulo n. It is possible to
;; prove that if such a nontrivial square root of 1 exists, then n is not
;; prime. It is also possible to prove that if n is an odd number that
;; is not prime, then, for at least half the numbers a < n, computing
;; an¡1 in this way will reveal a nontrivial square root of 1 modulo n.
;; (This is why the Miller-Rabin test cannot be fooled.) Modify the
;; expmod procedure to signal if it discovers a nontrivial square root
;; of 1, and use this to implement the Miller-Rabin test with a procedure
;; analogous to fermat-test. Check your procedure by testing
;; various known primes and non-primes. Hint: One convenient way
;; to make expmod signal is to have it return 0.

(define (remainder-square-checked x m)
  (if (and (not (or (= x 1)
                    (= x (- m 1))))
           (= (remainder (* x x) m) 1))
      0
      (remainder (* x x) m)))

(define (expmod-checked base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder-square-checked (expmod-checked base (/ exp 2) m) m)]
        [else
         (remainder (* base (expmod-checked base (- exp 1) m)) m)]
        ))

(define (Miller-Rabin-test n)
   (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (Miller-Rabin-prime?  n times)
  (cond ((= times 0) #t)
        ((Miller-Rabin-test n)
         (Miller-Rabin-prime? n (- times 1)))
        (else #f)))

(Miller-Rabin-prime? 561 9)  
(Miller-Rabin-prime? 1105 9) 
(Miller-Rabin-prime? 1729 9) 
(Miller-Rabin-prime? 2465 9) 
(Miller-Rabin-prime? 2821 9) 
(Miller-Rabin-prime? 6601 9) 