#lang racket
(require math)

;Math Functions

;abs value function
(define (abs x)
  (if (>= x 0)
      x
      (* -1 x)))

(displayln "absolute value function on inputs 5, -5, -99, -.99:")
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
 
 (displayln "factorial function on inputs -1, 5: ")
(fact -1)
(fact 5)

;fibonacci sequence function
(define (fib x)
  (if(= x 0)
     0
  (if (= x 1)
      1
      (+ (fib(- x 1)) (fib(- x 2))))))

(displayln "fibonacci sequence on inputs 1-8: ")
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;List Functions

;helper function
(define (greaterequalthan0 x)(>= x 0))

;filter function
(define (filter y x)
  (if (= (length x) 0)
      '()
      (if(equal? (y (first x)) #f)
         (filter y (rest x))
         (cons (first x) (filter y (rest x))))))

(displayln "filtering elements >0 on list '(-1 -2 3 4 5 6 -4 -77 9):")
(filter greaterequalthan0 '(-1 -2 3 4 5 6 -4 -77 9))

;list append function
(define (my_append lhs rhs)
  (if (empty? lhs)
      rhs
      (cons (first lhs) (my_append (rest lhs) rhs))))

(displayln "appending two lists with inputs '(a b c d) and '(e f g h): ")
(my_append '(a b c d) '(e f g h))

;list reverse function
(define (myReverse l)
  (if (null? l)
     '()
     (my_append (myReverse (rest l)) (list (first l)))
  )
)

 (displayln "reversing a list with input '(aaa aaaa bbb ccc ddd): ")
(myReverse '(aaa aaaa bbb ccc ddd))

;map function
(define (add1 x)
  (+ 1 x))

(define (my_map x y)
  (if  (empty? y)
       '()
        (cons (x (first y)) (my_map x (rest y)))))

(displayln "mapping function to a list with add 1 and list '(1 2 3 5 6 7 8 5 6): ")
(my_map add1 '(1 2 3 5 6 7 8 5 6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Set Functions


;cardinality
(define (cardinality x)
  (if (= 0 (length x))
      0
      (+ 1 (cardinality (rest x))))
  )


(displayln "cardinality of a set with set '(1 2 3 4 66 77): ")
(cardinality '(1 2 3 4 66 77))

;set membership function
(define (member? x y)
  (if(equal? (length y) 1)
     (equal? x (first y))
     (if(equal? x (first y))
        (equal? 1 1)
        (member? x (rest y)))))
(displayln "testing membership in sets, 1 in set '(3 4) and 2 in set '(1 2 3): ")
(member? '1 '(3 4))
(member? '2 '(1 2 3))

;set union function
(define(union x y)
  (remove-duplicates(my_append x y)))

(displayln "union of '(a b c d) and '(a b d) and '(b g c) and '(a g d):")
(union '(a b c) '(a b d))
(union '(b g c) '(a g d))

;insertion function
(define (insert e1 e2)
  (if (empty? e2)
      e1
        (if (member? e1 e2)
      e2
      (cons e1 e2))))

(displayln "set insertion, a in '(b c d), a in '(a b c d), and 111 in the empty set: ")
(insert 'a '(b c d))
(insert 'a '(a b c d))
(insert '111 '())

;intersection function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Required Functions


;perfection function
(define (perfect? n)
  (=
   n
   (-(sum(divisors n))n)))

(displayln "testing perfection on inputs 5 and 6:")
(perfect? 5)
(perfect? 6)

;abundance function
(define (abundant? n)
  (<
   n
   (-(sum(divisors n))n)))
 (displayln "testing abundance on inputs 5 and 12: ")
(abundant? 5)
(abundant? 12)

 ;deficience function
(define (deficient? n)
  (>
   n
   (-(sum(divisors n))n)))

(displayln "testing deficience on inputs 5, 12, 11: ")
(deficient? 5)
(deficient? 12)
(deficient? 11)

