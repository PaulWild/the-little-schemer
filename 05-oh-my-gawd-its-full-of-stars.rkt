#lang racket

(require rackunit)
(require "01-toys.rkt")

(define (rember* a l)
  (cond
    [(null? l) '()]
    [(atom? (car l))
     (cond
       [(equal? a (car l)) (rember* a (cdr l))]
       [else (cons (car l) (rember* a (cdr l)))])]
    [else (cons (rember* a (car l)) (rember* a (cdr l)))]))

(test-case "toys"
  (check-equal? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
                '((coffee) ((tea)) (and (hick))))

  (check-equal? (rember* 'sauce '(((tomato sauce)) ((bean) suace) (and ((flying)) sauce)))
                '(((tomato)) ((bean) suace) (and ((flying))))))
