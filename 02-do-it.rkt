#lang racket

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (list-of-atoms? l)
  (cond
    [(null? l) #t]
    [(atom? (car l) (list-of-atoms? (cdr l)))]
    [else #f]))
