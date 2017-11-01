#lang racket
(require math)

;abs value function
(define (abs x)
  (if (>= x 0)
      x
      (* -1 x)))

(abs 5)
(abs -5)
(abs -99)
(abs -.99)

;factorial function
(define (fact x)
  (if(< x 0)
      "undefined"
  (if (= x 0)
      1
      (* (fact (- x 1)) x))))
 
   

(fact -5)

;fibonacci sequence function
(define (fib x)
  (if(= x 0)
     0
  (if (= x 1)
      1
      (+ (fib(- x 1)) (fib(- x 2))))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)

;set membership function
(define (member x y)
  (if(equal? (length y) 1)
     (equal? x (first y))
     (if(equal? x (first y))
        (equal? 1 1)
        (member x (rest y)))))

(member 1 '(3 4))
(member 2 '(1 2 3))

;append function
(define (my_append lhs rhs)
  (if (empty? lhs)
      rhs
      (cons (first lhs) (my_append (rest lhs) rhs))))

(my_append '(a b c d) '(e f g h))

;list reverse function
(define (myReverse l)
  (if (null? l)
     '()
     (append (myReverse (rest l)) (list (first l)))
  )
)

(myReverse '(aaa aaaa bbb ccc ddd))

;pefection function
(define (perfect? n)
  (=
   n
   (-(sum(divisors n))n)))

(perfect? 5)
(perfect? 6)

;abundance function
(define (abundant? n)
  (<
   n
   (-(sum(divisors n))n)))

(abundant? 5)
(abundant? 12)

 ;deficience function
(define (deficient? n)
  (>
   n
   (-(sum(divisors n))n)))

(deficient? 5)
(deficient? 12)
(deficient? 11)

