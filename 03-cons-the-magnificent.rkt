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


; firsts

(define (firsts l)
  (cond
    [(null? l) '()]
    [else (cons (car (car l)) (firsts (cdr l)))]
                         ))

(test-case "firsts"
           (check-equal? (firsts '((apple peach pumpkin) (plum pear cherry) (grape rasin pea) (bean carrot eggplent))) '(apple plum grape bean))
)

;insertR

(define (insertR new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
    [else (cons (car lat) (insertR new old (cdr lat)))]))

(test-case "insertR"
            (check-equal? (insertR 'e 'd '(a b c d f g d h)) '(a b c d e f g d h))

            (check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert)))


; insertL

(define (insertL new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new lat)]
    [else (cons (car lat) (insertL new old (cdr lat)))]))


(test-case "insertL"
            (check-equal? (insertL 'e 'd '(a b c d f g d h)) '(a b c e d f g d h))

            (check-equal? (insertL 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with topping fudge for dessert)))


; subst

(define (subst new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new (cdr lat))]
    [else (cons (car lat) (subst new old (cdr lat)))]))


(test-case "subst"
 (check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with topping for dessert)))


; multirember

(define (multirember a lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) a) (multirember a (cdr lat))]
    [else (cons (car lat) (multirember a (cdr lat)))]))

(test-case "multirember"
  (check-equal? (multirember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))

  (check-equal? (multirember 'mint '(lamb chops and mint flavored mint jelly))
                '(lamb chops and flavored jelly))

  (check-equal? (multirember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))

  (check-equal? (multirember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea and hick))

  (check-equal? (multirember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato)))


;multiinsertR

(define (multiinsertR new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat))))]
    [else (cons (car lat) (multiinsertR new old (cdr lat)))]))

(test-case "multiinsertR"
            (check-equal? (multiinsertR 'e 'd '(a b c d f g d h)) '(a b c d e f g d e h))

            (check-equal? (multiinsertR 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert)))



;multiinsertL

(define (multiinsertL new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat))))]
    [else (cons (car lat) (multiinsertL new old (cdr lat)))]))

(test-case "multiinsertL"
            (check-equal? (multiinsertL 'e 'd '(a b c d f g d h)) '(a b c e d f g e d  h))

            (check-equal? (multiinsertL 'fried 'fish '(chips and fish or fish and fried)) '(chips and fried fish or fried fish and fried))

            (check-equal? (multiinsertL 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with topping fudge for dessert)))

; multisubst

(define (multisubst new old lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) old) (cons new (multisubst new old (cdr lat)))]
    [else (cons (car lat) (multisubst new old (cdr lat)))]))


(test-case "subst"
 (check-equal? (multisubst 'topping 'fudge '(ice cream with fudge and more fudge for dessert)) '(ice cream with topping and more topping for dessert)))
