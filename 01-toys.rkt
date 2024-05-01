#lang racket

(require rackunit)

(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (length lst)
  (if (null? lst) 0 (+ 1 (length (cdr lst)))))

(test-case "toys"
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

  (check-exn exn:fail? (lambda () (car '()))))

(test-case "The Law of Car"
  (check-equal? (car '(((hotdogs)) (and) (pickle) relish)) '((hotdogs)))

  (check-equal? (car (car '(((hotdogs)) (and) (pickle) relish))) '(hotdogs))

  (check-equal? (car (car '(((hotdogs)) (and)))) '(hotdogs))

  (check-equal? (cdr '(a b c)) '(b c))

  (check-equal? (cdr '((a b c) x y z)) '(x y z))

  (check-equal? (cdr '(hamburger)) '())

  (check-equal? (cdr '((x) t r)) '(t r))

  (check-exn exn:fail? (lambda () (cdr 'hamburger)))

  (check-exn exn:fail? (lambda () (cdr '()))))

(test-case "The Law of Cdr"
  (check-equal? (car (cdr '((b) (x y) ((c))))) '(x y))

  (check-equal? (cdr (cdr '((b) (x y) ((c))))) '(((c))))

  (check-exn exn:fail? (lambda () (cdr (car '(a (b (c) d))))))

  ; car takes a non empty list

  ; cdr takes a non empty list

  (check-equal? (cons 'peanut '(butter and jelly)) '(peanut butter and jelly))

  (check-equal? (cons '(banana and) '(peanut butter and jelly))
                '((banana and) peanut butter and jelly))

  (check-equal? (cons '((help) this) '(is very ((hard) to learn)))
                '(((help) this) is very ((hard) to learn)))

  (check-equal? (cons '(a b (c)) '()) '((a b (c))))

  (check-equal? (cons 'a '()) '(a))

  ; (cons '((a b c)) 'b)) works in racket it returns a pair not an error

  ; (cons 'a 'b) works in racket it returns a pair not an error This is a racket thing right?
  (check-equal? (cons 'a 'b) '(a . b)))

(test-case "The Law of Cons"
  (check-equal? (cons 'a (car '((b) c d))) '(a b))

  (check-equal? (cons 'a (cdr '((b) c d))) '(a c d))

  (check-true (null? '()))

  (check-true (null? (quote ())))

  (check-false (null? '(a b c)))

  (check-false (null? 'spaghetti)))

(test-case "The Law of Null?"

  (check-true (atom? 'Harry))

  (check-false (atom? '(Harry had a heap of apples)))

  ; atom? takes one argument and is any S-expression )

  (check-true (atom? (car '(Harry had a heap of apples))))

  (check-false (atom? (cdr '(Harry had a heap of apples))))

  (check-false (atom? (cdr '(Harry))))

  (check-true (atom? (car (cdr '(swing low sweet cherry oat)))))

  (check-false (atom? (car (cdr '(swing (low sweet) cherry oat)))))

  (check-equal? 'Harry 'Harry)

  (check-true (eq? 'Harry 'Harry))

  (check-false (eq? 'margarine 'butter))

  ; eq? takes two arguments,

  ; eq? is not the same in racket as the little schemer, it can take numbers and lists,

  (check-false (eq? '() '(strawberry)))

  (check-false (eq? 6 7)))

(test-case "The Law of Eq?"

  (check-true (eq? (car '(Mary had a little lamb chop)) 'Mary))

  (check-false (eq? (cdr '(soured milk)) 'milk))

  (check-true (eq? (car '(beans beans we need jelly beans))
                   (car (cdr '(beans beans we need jekky beans))))))
