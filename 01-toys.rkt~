#lang racket

(require rackunit)

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (length lst)
  (if (null? lst) 0
      (+ 1
         (length (cdr lst)))))

(check-true (atom? 'atom))

(check-true (atom? 'turkey))

(check-true (atom? 1492))

(check-true (atom? 'u))

(check-true (atom? '*abc$))

(check-true (list? '(atom)))

(check-true (list? '(atom turkey or)))

(check-false (list? (cons '(atom turkey) 'or)))

(check-true (list? '((atom turkey) or)))

(check-true (atom? 'xyz))

(check-true (list? '(x y z)))

(check-true (list? '((x y) z)))

(check-equal? (length '(how are you doing so far)) 6)

(check-true (list? '(((how) are) ((you) (doing so)) far)))

(check-equal? (length '(((how) are) ((you) (doing so)) far)) 3)

(check-true (list? '()))

(check-false (atom? '()))

(check-true (list? '(() () () ())))

(check-equal? (car '(a b c)) 'a)

(check-equal? (car '((a b c) x y z)) '(a b c))

(check-exn exn:fail? (lambda () (car 'hotdog)))

(check-exn exn:fail? (lambda () (car '())))

; law of car
