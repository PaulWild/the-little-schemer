#lang racket

(require "01-toys.rkt")
(require rackunit)

(define (rember a lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) a) (cdr lat)]
    [else (cons (car lat) (rember a (cdr lat)))]))

(test-case "rember"
  (check-equal? (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))

  (check-equal? (rember 'mint '(lamb chops and mint flavored mint jelly))
                '(lamb chops and flavored mint jelly))

  (check-equal? (rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))

  (check-equal? (rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup))

  (check-equal? (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce)))
