#lang racket

(require "01-toys.rkt")
(require rackunit)

(test-case "numbers game"
  (check-true (atom? 14)))

(define (add1 n)
  (+ 1 n))

(test-case "add1"
  (check-equal? (add1 67) 68))

(define (sub1 n)
  (- n 1))

(test-case "suub1"
  (check-equal? (sub1 5) 4)

  (check-equal? (sub1 0) -1)

  (check-true (zero? 0))

  (check-false (zero? 1492)))

(define (_+ m n)
  (cond
    [(zero? m) n]
    [else (add1 (_+ n (sub1 m)))]))

(test-case "_+"
  (check-equal? (_+ 46 12) 58))

(define (_- m n)
  (cond
    [(zero? n) m]
    [else (sub1 (_- m (sub1 n)))]))

(test-case "_-"
  (check-equal? (_- 14 3) 11)

  (check-equal? (_- 17 9) 8)

  (check-equal? (_- 18 25) -7))

(define (tup? l)
  (cond
    [(null? l) #t]
    [(number? (car l)) (tup? (cdr l))]
    [else #f]))

(test-case "tup?"
  (check-true (tup? '(2 11 3 79 47 6)))

  (check-true (tup? '(8 55 5 555)))

  (check-false (tup? '(1 2 8 apple 4 3)))

  (check-false (tup? '(3 (7 4) 13 9)))

  (check-true (tup? '())))

(define (addtup l)
  (cond
    [(null? l) 0]
    [else (_+ (car l) (addtup (cdr l)))]))

(test-case "addtup"
  (check-equal? (addtup '(3 5 2 8)) 18)

  (check-equal? (addtup '(15 6 7 12 3)) 43))

(define (x n m)
  (cond
    [(zero? m) 0]
    [else (_+ n (x n (sub1 m)))]))

(test-case "x"
  (check-equal? (x 5 3) 15)

  (check-equal? (x 13 4) 52)

  (check-equal? (x 12 3) 36))

(define (tup+ tup1 tup2)
  (cond
    [(and (null? tup1) (null? tup2)) '()]
    [(null? tup1) tup2]
    [(null? tup2) tup1]
    [else (cons (_+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]))

(test-case "tup+"
  (check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))

  (check-equal? (tup+ '(2 3) '(4 6)) '(6 9))

  (check-equal? (tup+ '(3 7) '(4 6 8 1)) '(7 13 8 1))

  (check-equal? (tup+ '(4 6 8 1) '(3 7)) '(7 13 8 1)))

(define (_> n m)
  (cond
    [(zero? n) #f]
    [(zero? m) #t]
    [else (_> (sub1 n) (sub1 m))]))

(test-case "_>"
  (check-false (_> 12 133))

  (check-true (_> 120 11))

  (check-false (_> 3 3)))

(define (_< n m)
  (cond
    [(zero? m) #f]
    [(zero? n) #t]
    [else (_< (sub1 n) (sub1 m))]))

(test-case "_<"
  (check-true (_< 4 6))

  (check-false (_< 8 3))

  (check-false (_< 6 6)))

(define (= n m)
  (cond
    [(_> n m) #f]
    [(_< n m) #f]
    [else #t]))

(test-case "="
  (check-true (= 4 4))

  (check-false (= 8 3))

  (check-false (= 7 6)))

(define (pow n m)
  (cond
    [(zero? m) 1]
    [else (x n (pow n (sub1 m)))]))

(test-case "pow"
  (check-equal? (pow 1 1) 1)

  (check-equal? (pow 2 3) 8)

  (check-equal? (pow 5 3) 125))

(define (div n m)
  (cond
    [(< n m) 0]
    [else (add1 (div (_- n m) m))]))

(test-case "div"
  (check-equal? (div 15 3) 5))

(define (length lst)
  (cond
    [(null? lst) 0]
    [else (add1 (length (cdr lst)))]))

(test-case "length"
  (check-equal? (length '(ham and cheese on rye)) 5))

(define (pick n lat)
  (cond
    [(zero? (sub1 n)) (car lat)]
    [else (pick (sub1 n) (cdr lat))]))

(test-case "pick"
  (check-equal? (pick 4 '(lasgna spaghetti ravioli macaroni meatball)) 'macaroni))

(define (rempick n lat)
  (cond
    [(zero? (sub1 n)) (cdr lat)]
    [else (cons (car lat) (rempick (sub1 n) (cdr lat)))]))

(test-case "rempick"
  (check-equal? (rempick 3 '(hotdogs with hot mustard)) '(hotdogs with mustard)))

(define (no-nums lat)
  (cond
    [(null? lat) '()]
    [(number? (car lat)) (no-nums (cdr lat))]
    [else (cons (car lat) (no-nums (cdr lat)))]))

(test-case "no-nums"
  (check-equal? (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates)))

(define (all-nums lat)
  (cond
    [(null? lat) '()]
    [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
    [else (all-nums (cdr lat))]))

(test-case "all-nums"
  (check-equal? (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9)))

(define (eqan? a1 a2)
  (cond
    [(and (number? a1) (number? a2)) (= a1 a2)]
    [else (eq? a1 a2)]))

(test-case "equan?"
  (check-true (eqan? 1 1))

  (check-false (eqan? 1 2))

  (check-false (eqan? 'for 2))

  (check-true (eqan? 'for 'for)))

(define (occur n lat)
  (cond
    [(null? lat) 0]
    [(eqan? (car lat) n) (add1 (occur n (cdr lat)))]
    [else (occur n (cdr lat))]))

(test-case "occur"
  (check-equal? (occur 'one '(one two one three four one)) 3))

(define (one? n)
  (= n 1))

(test-case "one?"
  (check-true (one? 1))

  (check-false (one? 2)))

(define (rempick2 n lat)
  (cond
    [(one? n) (cdr lat)]
    [else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))]))

(test-case "rempick2"
  (check-equal? (rempick2 3 '(lemon meringue salty pie)) '(lemon meringue pie)))
