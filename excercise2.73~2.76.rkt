#lang racket
(require "getput.rkt")

(define (square x) (* x x))

(define (real-part1 z) (car z))

(define (imag-part1 z) (cdr z))

(define (magnitude1 z)
  (sqrt (+ (square (real-part1 z)) (square (imag-part1 z)))))

(define (angle1 z)
  (atan (imag-part1 z) (real-part1 z)))

(define (make-from-real-imag1 x y)
  (cons x y))

(define (make-from-mag-ang1 r a)
  (cons (* r (cos a)) (* r (sin a))))


;; --------------------------------------------------------------

(define (real-part2 z)
  (* (magnitude2 z) (cos (angle2 z))))

(define (imag-part2 z)
  (* (magnitude2 z) (sin (angle2 z))))

(define (magnitude2 z) (car z))
(define (angle2 z) (cdr z))

(define (make-from-real-imag2 x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang2 r a)
  (cons r a))

;; -------------------------------------------------------------

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; -----------------------------------------------------

(define (real-part-rect z) (car z))
(define (imag-part-rect z) (cdr z))

(define (angle-rect z)
  (atan (imag-part-rect z)
        (real-part-rect z)))

(define (magnitude-rect z)
  (sqrt (+ (square (real-part-rect z))
           (square (imag-part-rect z)))))

(define (make-from-real-imag-rect x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rect r a)
  (attach-tag 'rectangular (cons
                            (* r (cos a))
                            (* r (sin a)))))

;; -------------------------------------------------------

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; ----------------------------------------------------

(define (real-part z)
  (cond [(rectangular? z)
         (real-part-rect (contents z))]
        [(polar? z)
         (real-part-polar (contents z))]
        [else
         (error "Unknow type -- REAL-PART" z)]))

(define (imag-part z)
  (cond [(rectangular? z)
         (imag-part-rect (contents z))]
        [(polar? z)
         (imag-part-polar (contents z))]
        [else
         (error "Unknow type -- IMAG-PART" z)]))

(define (magnitude z)
  (cond [(rectangular? z)
         (magnitude-rect (contents z))]
        [(polar? z)
         (magnitude-polar (contents z))]
        [else
         (error "Unknow type -- MAGNITUDE" z)]))

(define (angle z)
  (cond [(rectangular? z)
         (angle-rect (contents z))]
        [(polar? z)
         (angle-polar (contents z))]
        [else
         (error "Unknow type -- ANGLE" z)]))

(define (make-from-real-imag x y)
  (make-from-real-imag-rect x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; -------------------------------------------------
;; Data-Directed Programming and Additivity

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; We use the list (rectangular) rather than the symbol rectangular to allow
;; for the possibility of operations with multiple arguments, not all of the
;; same type.

;; 就是说有些函数允许多个类型的参数
;; 例如我定义一个same函数，它判断一个polar和一个rect坐标是否是同一位置
;; 此时就需要填写 '(rectangular polar) 这样的。
;; 不过这个例子不太恰当，应当是同一个包内的不同类型

;; The type the constructors are installed under needn't be a list because a
;; constructor is always used to make an object of one particular type.

;; 构造函数不用列表基于一个事实：我们不可能调用同一个函数构造不同的类型的值。

(define (intall-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (real-partg z) (apply-generic 'real-part z))
(define (imag-partg z) (apply-generic 'imag-part z))
(define (magnitudeg z) (apply-generic 'magnitude z))
(define (angleg z) (apply-generic 'angle z))

(define (make-from-real-imagg x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-angg r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; Excercise 2.73:
;; Section 2.3.2 described a program that performs symbolic differentiation:

;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp) (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv (addend exp) var)
;                   (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;          (make-product (multiplier exp)
;                        (deriv (multiplicand exp) var))
;          (make-product (deriv (multiplier exp) var)
;                        (multiplicand exp))))
;        ; more rules can be added here
;        (else (error
;               "unknown expression type - DERIV" exp))))

;; We can regard this program as performing a dispatch on the type of
;; the expression to be differentiated. In this situation the “type tag”
;; of the datum is the algebraic operator symbol (such as +) and the
;; operation being performed is deriv. We can transform this program
;; into data-directed style by rewriting the basic derivative procedure
;; as

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [else
         ((get 'deriv (operator exp))
          (operands exp) var)]))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; a. Explain what was done above. Why can’twe assimilate the predicates
;;    number? and variable? into the data-directed dispatch?

;; number 和 variable两种类型并没有被我们打上tag，所以能以数据导向方式进行处理，
;; 我们用了Scheme的数字和符号类型表达这两种类型，使用内置的number?和variable?就能判断，
;; 不需要画蛇添足再加上一层了，实在要加也可以。
;; 但是这样一来，求导程序的每个包都要加上 number? 和 same-variable? 谓词,
;; 而这样的分派实际上是没有必要的。

;; b. Write the procedures for derivatives of sums and products, and
;;    the auxiliary code required to install them in the table used by
;;    the program above.

(define (install-sum-deriv-package)
  ;; internal procedures
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  (define (single-operand? l)
    (and (list? l) (= (length l) 1)))
  
  (define (addend s)
    (car s))

  (define (augend s)
    (let ((tail (cdr s)))
      (if (single-operand? tail)
          (car tail)
          (apply make-sum tail))))

  (define (make-sum a1 . a2)
    (if (single-operand? a2)
        (let ((op2 (car a2)))
          (cond [(=number? a1 0) op2]
                [(=number? op2 0) a1]
                [(and (number? a1) (number? op2)) (+ a1 op2)]
                [else (list '+ a1 op2)]))
        (cons '+ (cons a1 a2))))

  ;; interface to the rest of the system
  (put 'make-sum '+ make-sum)
  
  (put 'deriv '+
       (lambda (exp var)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))))
  "deriv-sum-pack-installed")

(define (install-product-deriv-package)
  ;; internal procedures
  (define (=number? exp num)
    (and (number? exp) (= exp num)))

  (define (single-operand? l)
    (and (list? l) (= (length l) 1)))
  
  (define (multiplier p)
    (car p))

  (define (multiplicand p)
    (let ((tail (cdr p)))
      (if (single-operand? tail)
          (car tail)
          (apply make-product tail))))
  
  (define (make-product m1 . m2)
    (if (single-operand? m2)
        (let ((op2 (car m2)))
          (cond [(or (=number? m1 0) (=number? op2 0)) 0]
                [(=number? m1 1) op2]
                [(=number? op2 1) m1]
                [(and (number? m1) (number? op2)) (* m1 op2)]
                [else (list '* m1 op2)]))
        (cons '* (cons m1 m2))))

  (define (make-sum a1 a2) ((get 'make-sum '+) a1 a2))
  
   ;; interface to the rest of the system

  (put 'make-product '* make-product)
  (put 'deriv '*
       (lambda (exp var)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))))
  "deriv-product-pack-installed")


;; c. Choose any additional differentiation rule that you like, such as
;;    the one for exponents (Exercise 2.56), and install it in this datadirected
;;    system.

(define (install-exponent-deriv-package)
  ;; internal procedures
  (define (make-exponentiation b e)
  (cond [(= e 0) 1]
        [(= e 1) b]
        [else
         (list '^ b e)]))

  (define (base exp)
    (car exp))

  (define (exponent exp)
    (cadr exp))

  (define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '^)))

  (define (make-product m1 m2) ((get 'make-product '*) m1 m2))
  ;; interface to the rest of the system
  
  (put 'deriv '^
       (lambda (exp var)
         (let ((n (exponent exp))
               (u (base exp)))
           (make-product
            n
            (make-product
             (make-exponentiation
              u
              (- n 1))
             (deriv u var))))))
  "deriv-exponent-pack-installed")

;; d. In this simple algebraic manipulator the type of an expression
;;    is the algebraic operator that binds it together. Suppose, however,
;;    we indexed the procedures in the opposite way, so that the
;;    dispatch line in deriv looked like

;    ((get (operator exp) ’deriv) (operands exp) var)

;; What corresponding changes to the derivative system are required?

;; 程序的主体无需变动，只需要修改各个package中调用put的参数的位置。
;; 例如：(put 'make-sum '+ make-sum) -> (put '+ 'make-sum make-sum)
;; make-sum过程本身不用修改。

(install-sum-deriv-package)
(install-product-deriv-package)
(install-exponent-deriv-package)

;; Excercise 2.74:
;; Insatiable Enterprises, Inc., is a highly decentralized
;; conglomerate company consisting of a large number of independent
;; divisions located all over the world. The company’s
;; computer facilities have just been interconnected by means of a
;; clever network-interfacing scheme that makes the entire network
;; appear to any user to be a single computer. Insatiable’s president,
;; in her first attempt to exploit the ability of the network to extract
;; administrative information from division files, is dismayed to discover
;; that, although all the division files have been implemented as
;; data structures in Scheme, the particular data structure used varies
;; from division to division. A meeting of division managers is hastily
;; called to search for a strategy to integrate the files that will satisfy
;; headquarters’ needs while preserving the existing autonomy of the
;; divisions.
;; Show how such a strategy can be implemented with data-directed
;; programming. As an example, suppose that each division’s personnel
;; records consist of a single file, which contains a set of records
;; keyed on employees’ names. The structure of the set varies from
;; division to division. Furthermore, each employee’s record is itself
;; a set (structured differently from division to division) that contains
;; information keyed under identifiers such as address and salary.
;; In particular:

;; a. Implement for headquarters a get-record procedure that retrieves
;;    a specified employee’s record from a specified personnel
;;    file. The procedure should be applicable to any division’s file.
;;    Explain how the individual divisions’ files should be structured.
;;    In particular, what type information must be supplied?

;; b. Implement for headquarters a get-salary procedure that
;;    returns the salary information from a given employee’s record
;;    from any division’s personnel file. How should the record be
;;    structured in order to make this operation work?


;; a) and b):

(define (subsidiary file)
  (if (pair? file)
      (car file)
      (error "Corrupted file -- SUBSIDIARY" file)))

(define (filecontent file)
  (if (pair? file)
      (cdr file)
      (error "Corrupted file -- FILECONTENT" file)))

(define (attach-subsidiary subsidiary filecontent)
  (cons subsidiary filecontent))

(define (install-SiChuan-subsidiary-package)
  
  (define (make-record id name salary)
    (list id name salary))

  (define (employee-id record)
    (car record))
  (define (employee-name record)
    (cadr record))
  (define (employee-salary record)
    (caddr record))

  ;; 将这个函数用foldr或foldl改写
  (define (add-record-to-file file record)
    (if (null? file)
        (list record)
        (append file (list record))))

  (define (make-file)
    '())

  (define (get-record file name)
    (car (filter (lambda (record) (eq? (employee-name record) name)) file)))
  (define (get-salary file name)
    (employee-salary (car (filter (lambda (record) (eq? (employee-name record) name)) file))))

  ;; interface to the rest of the system
  ;(define (tag x) (attach-subsidiary 'SiChuan x))
  ;(put 'create-file 'SiChuan (lambda () (tag (make-file))))
  ;(put 'make-record 'SiChuan make-record)
  (put 'get-record 'SiChuan get-record)
  (put 'get-salary 'SiChuan get-salary)
  ;(put 'add-record-to-file 'SiChuan add-record-to-file)
  "SiChuan-subsidiary-package-installed")

(define (install-Tokyo-subsidiary-package)
  
  (define (make-record id name salary)
    (list id name salary))

  (define (employee-id record)
    (car record))
  (define (employee-name record)
    (cadr record))
  (define (employee-salary record)
    (caddr record))

  ;; 将这个函数用foldr或foldl改写
  (define (add-record-to-file file record)
    (if (null? file)
        (list record)
        (append file (list record))))

  (define (make-file)
    '())

  (define (get-record file name)
    (car (filter (lambda (record) (eq? (employee-name record) name)) file)))
  (define (get-salary file name)
    (employee-salary (car (filter (lambda (record) (eq? (employee-name record) name)) file))))

  ;; interface to the rest of the system
  ;(define (tag x) (attach-subsidiary 'SiChuan x))
  ;(put 'create-file 'SiChuan (lambda () (tag (make-file))))
  ;(put 'make-record 'SiChuan make-record)
  (put 'get-record 'Tokyo get-record)
  (put 'get-salary 'Tokyo get-salary)
  ;(put 'add-record-to-file 'SiChuan add-record-to-file)
  "Tokyo-subsidiary-package-installed")

(define (get-record file name)
  ((get 'get-record (subsidiary file)) (filecontent file) name))
(define (get-salary file name)
  ((get 'get-salary (subsidiary file)) (filecontent file) name))

;(define (create-SiChuan-file)
;  (apply (get 'create-file 'SiChuan)))
(install-SiChuan-subsidiary-package)
(install-Tokyo-subsidiary-package)

(define SiChuan-file
  (list 'SiChuan
        (list 0 "ZhanKe" 2000)
        (list 1 "ZhuangHao" 2000)
        (list 2 "ZhouChao" 3000)))

(define Tokyo-file
  (list 'Tokyo
        (list 3 "KoJiMa" 6000)
        (list 4 "MiYaZaKi" 6000)
        (list 5 "ZhouChao" 4000)))



;; c. Implement for headquarters a find-employee-record procedure.
;;    This should search all the divisions’ files for the record of
;;    a given employee and return the record. Assume that this procedure
;;    takes as arguments an employee’s name and a list of all
;;    the divisions’ files.

(define (find-employee-record files name)
  (map (lambda (file) (get-record file name)) files))

;; d. When Insatiable takes over a new company, what changes must
;;    be made in order to incorporate the new personnel information
;;    into the central system?

;; 新公司并购后，现有系统完全不用修改，只需要将写一个新的包；
;; 增加对应的get-record和get-salary接口即可。

;; Excercise 2.75:
;; Implement the constructor make-from-mag-ang in
;; message-passing style. This procedure should be analogous to the
;; make-from-real-imag procedure given above.

(define (make-from-mag-ang-dispatch r a)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* r (cos a))]
          [(eq? op 'imag-part) (* r (sin a))]
          [(eq? op 'magnitude) r]
          [(eq? op 'angle) a]
          [else
           (error "Unknow op -- MAKE-FROM-MAG-ANG" op)]))
  dispatch)

;; Excercise 2.76:
;; As a large system with generic operations evolves,
;; new types of data objects or new operations may be needed. For
;; each of the three strategies—generic operations with explicit dispatch,
;; data-directed style, and message-passing-style—describe
;; the changes that must be made to a system in order to add new
;; types or new operations. Which organization would be most
;; appropriate for a system in which new types must often be added?
;; Which would be most appropriate for a system in which new
;; operations must often be added?

;; --- generic operations with explicit dispatch ---

;; 增加新类型的时候，我们必须修改现有通用函数，在其中增加对应tag的对应函数；
;; 此处涉及对之前代码的修改。
;; 增加新函数的时候，我们需要增加通用函数，并在其中指定每个类型的函数；
;; 此处不涉及对之前代码的修改，只有新增。

;; -- data-directed style ---

;; 增加新类型的时候，需要新增一个package，在其中指定类型tag，并将所有操作放入全局函数表中；
;; 此处不涉及对之前代码的修改,只有新增。
;; 增加新函数的时候，需要在每个package中新增函数的具体操作，并put新操作进全局函数表中。
;; 此处涉及对之前代码的修改。

;; -- message-passing style ---

;; 增加新类型的时候，只需要新增“类”的定义，并将各个操作放在类中实现；
;; 此处不涉及对之前代码的修改,只有新增。
;; 新增新函数的时候，需要在每个类中增加相应的函数。
;; 此处涉及对之前代码的修改。

;; 由此我们认为，对于经常添加数据类型的系统，可以使用data-directed或者message-pass
;; 对于经常增加方法的系统，可以使用explicit-dispatch