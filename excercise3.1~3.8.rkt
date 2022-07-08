#lang sicp

(define (square x) (* x x))
(define random-max 131313136)
(define random-init 0)

(define (rand-update x)
  (let ((a (expt 2 31))
        (c 1103515245)
        (m (+ random-max 1)))
    (modulo (+ (* a x) c) m)))

;; Excercise 3.1:
;; An accumulator is a procedure that is called repeatedly
;; with a single numeric argument and accumulates its arguments
;; into a sum. Each time it is called, it returns the currently
;; accumulated sum. Write a procedure make-accumulator that generates
;; accumulators, each maintaining an independent sum. The
;; input to make-accumulator should specify the initial value of the
;; sum;
;; For example:

;; (define A (make-accumulator 5))
;; (A 10) => 15
;; (A 10) => 25

(define (make-accumulator start-value)
  (lambda (plus-value)
    (begin (set! start-value (+ start-value plus-value))
           start-value)))

;; Excercise 3.2:
;; In software-testing applications, it is useful to be able
;; to count the number of times a given procedure is called during the
;; course of a computation. Write a procedure make-monitored that
;; takes as input a procedure, f, that itself takes one input. The result
;; returned by make-monitored is a third procedure, say mf, that
;; keeps track of the number of times it has been called by maintaining
;; an internal counter. If the input to mf is the special symbol
;; how-many-calls?, then mf returns the value of the counter. If
;; the input is the special symbol reset-count, then mf resets the
;; counter to zero. For any other input, mf returns the result of calling
;; f on that input and increments the counter. For instance, we could
;; make a monitored version of the sqrt procedure:

(define (make-monitored f) 
  (let ((count 0)) 
    (lambda (arg) 
      (cond [(eq? arg 'how-many-calls?) count] 
            [(eq? arg 'reset-count) (set! count 0)] 
            [else (begin (set! count (+ count 1)) 
                         (f arg))])))) 
          
  
;; Excercise 3.3:
;; Modify the make-account procedure so that it creates
;; password-protected accounts. That is, make-account should take
;; a symbol as an additional argument, as in

;; (define acc (make-account 100 'secret-password))

;; The resulting account object should process a request only if it is
;; accompanied by the password with which the account was created,
;; and should otherwise return a complaint:

;; ((acc 'secret-password 'withdraw) 40)
;; => 60
;; ((acc 'some-other-password 'deposit) 50)
;; => "Incorrect password"

;; Excercise 3.4:
;; Modify the make-account procedure of Exercise 3.3
;; by adding another local state variable so that, if an account is accessed
;; more than seven consecutive times with an incorrect password,
;; it invokes the procedure call-the-cops.


(define (make-account blance password)
  
  (let ((max-try-times 7)
        (try-times 0))

    (define (withdraw amount)
      (if (>= blance amount)
          (begin (set! blance (- blance amount))
                 blance)
          "Insufficient funds"))

    (define (deposit amount)
      (set! blance (+ blance amount)))

    (define (password-match? given-password)                         
      (eq? given-password password))                          

    (define (display-wrong-password-message useless-arg)                
      (display "Incorrect password"))                                
    
    (define (dispatch given-password mode)          
      (if (password-match? given-password)                          
          (begin
            (set! try-times 0)                          ; 成功登录之后清零计数器
            (cond ((eq? mode 'withdraw)
                   withdraw)
                  ((eq? mode 'deposit)
                   deposit)
                  (else
                   (error "Unknow request -- MAKE-ACCOUNT" mode))))
          (begin          
            (set! try-times (+ 1 try-times))            ; 进行计数
            (if (>= try-times max-try-times)
                (call-the-cops)
                display-wrong-password-message))))
    
    dispatch))

(define (call-the-cops)
  (error "You try too much times, calling the cops ..."))

;; Excercise 3.5:
;; Monte Carlo integration is a method of estimating
;; definite integrals by means of Monte Carlo simulation. Consider
;; computing the area of a region of space described by a predicate
;; P(x, y) that is true for points (x, y) in the region and false for points
;; not in the region. For example, the region contained within a circle
;; of radius 3 centered at (5, 7) is described by the predicate that tests
;; whether (x - 5)^2 + (y - 7)^2 <= 3^2. To estimate the area of the region
;; described by such a predicate, begin by choosing a rectangle that
;; contains the region. For example, a rectangle with diagonally opposite
;; corners at (2, 4) and (8, 10) contains the circle above. The
;; desired integral is the area of that portion of the rectangle that lies
;; in the region. We can estimate the integral by picking, at random,
;; points (x, y) that lie in the rectangle, and testing P(x, y) for each
;; point to determine whether the point lies in the region. If we try
;; this with many points, then the fraction of points that fall in the
;; region should give an estimate of the proportion of the rectangle
;; that lies in the region. Hence, multiplying this fraction by the area
;; of the entire rectangle should produce an estimate of the integral.

;; Implement Monte Carlo integration as a procedure estimateintegral
;; that takes as arguments a predicate P, upper and lower
;; bounds x1, x2, y1, and y2 for the rectangle, and the number of
;; trials to perform in order to produce the estimate. Your procedure
;; should use the same monte-carlo procedure that was used
;; above to estimate 1⁄4. Use your estimate-integral to produce an
;; estimate of 1⁄4 by measuring the area of a unit circle.

;; You will find it useful to have a procedure that returns a number
;; chosen at random from a given range. The following random-inrange
;; procedure implements this in terms of the random procedure
;; used in Section 1.2.6, which returns a nonnegative number
;; less than its input

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0) (/ trials-passed trials)]
          [(experiment) (iter (- trials-remaining 1) (+ trials-passed 1))]
          [else (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

(define (rect-area x1 x2 y1 y2)
  (* (abs (- x2 x1)) (abs (- y2 y1))))

(define (estimate-integral p? x1 x2 y1 y2 trials)
  (* (rect-area x1 x2 y1 y2)
     (monte-carlo trials
                  (lambda () (p? (random-in-range x1 x2)
                                 (random-in-range y1 y2))))))

(define (get-pi trials)
  (exact->inexact
   (estimate-integral (lambda (x y)
                        (< (+ (square x) (square y)) 1.0))
                      -1.0 1.0 -1.0 1.0 trials)))

;; Excercise 3.6:
;; It is useful to be able to reset a random-number generator
;; to produce a sequence starting from a given value. Design
;; a new rand procedure that is called with an argument that is either
;; the symbol generate or the symbol reset and behaves as follows:
;; (rand ’generate) produces a new random number; ((rand
;; ’reset) hnew-valuei) resets the internal state variable to the designated
;; hnew-valuei. Thus, by resetting the state, one can generate
;; repeatable sequences. These are very handy to have when testing
;; and debugging programs that use random numbers.

(define rand
  (let ((x random-init))
    (define (dispatch message)
      (cond [(eq? message 'generate)
             (begin (set! x (rand-update x))
                    x)]
            [(eq? message 'reset)
             (lambda (new-value) (set! x new-value))]))
    dispatch))

(define (rand-enumerate times)
  (if (<= times 0)
      '()
      (cons (rand 'generate) (rand-enumerate (- times 1)))))

;; Excercise 3.7:
;; Consider the bank account objects created by
;; make-account, with the password modification described in
;; Exercise 3.3. Suppose that our banking system requires the ability
;; to make joint accounts. Define a procedure make-joint that
;; accomplishes this. Make-joint should take three arguments. The
;; first is a password-protected account. The second argument must
;; match the password with which the account was defined in order
;; for the make-joint operation to proceed. The third argument
;; is a new password. Make-joint is to create an additional access
;; to the original account using the new password. For example, if
;; peter-acc is a bank account with password open-sesame, then

;(define paul-acc
;  (make-joint peter-acc 'open-sesame 'rosebud))

;; will allow one to make transactions on peter-acc using the name
;; paul-acc and the password rosebud. You may wish to modify your
;; solution to Exercise 3.3 to accommodate this new feature.

(define (make-joint origin-acc origin-passwd another-passwd)
  (lambda (given-passwd mode)
    (if (eq? given-passwd another-passwd)
        (origin-acc origin-passwd mode)
        display-wrong-another-password-message)))

(define (display-wrong-another-password-message useless-arg)
  (display "Incorrect password"))

;; Excercise 3.8:
;; When we defined the evaluation model in Section
;; 1.1.3, we said that the first step in evaluating an expression is to
;; evaluate its subexpressions. But we never specified the order in
;; which the subexpressions should be evaluated (e.g., left to right or
;; right to left). When we introduce assignment, the order in which
;; the arguments to a procedure are evaluated can make a difference
;; to the result. Define a simple procedure f such that evaluating

; (+ (f 0) (f 1))

;; will return 0 if the arguments to + are evaluated from left to right
;; but will return 1 if the arguments are evaluated from right to left.

;; This solution can't work under #lang sicp, but it does work under #lang racket
(define f
  (let ((y 1))
    (lambda (x) (set! y (* x y)) y)))