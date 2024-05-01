#lang racket

(require "01-toys.rkt")
(require rackunit)

(define (lat? l)
  (cond
    [(null? l) #t]
    [(atom? (car l)) (lat? (cdr l))]
    [else #f]))

(define (member? a lat)
  (cond
    [(null? lat) #f]
    [else (or (eq? (car lat) a) (member? a (cdr lat)))]))

(test-case "Do It, Do It Again, and Again and Again..."

  (check-true (lat? '(Jack Sprat could eat no chicken fat)))

  (check-false (lat? '((Jack) Sprat could eat no chicken fat)))

  (check-false (lat? '(Jack (Sprat could) eat no chicken fat)))

  (check-true (lat? '()))

  (check-eq? (lat? '(bacon and eggs)) #t)

  (check-true (member? 'tea '(coffee tea or millk)))

  (check-false (member? 'poached '(fried eggs and scrambled eggs))))
