#lang plai

(define-type MisspelledAnimal
  [caml (humps number?)]
  [yacc (height number?)])


(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])

(define (parse e)
  (cond
    [(number? e) (numS e)]
    [(list? e)
     (case (first e)
       [(+) (plusS (parse (second e)) (parse (third e)))]
       [(*) (multS (parse (second e)) (parse (third e)))]
       [(-) (bminusS (parse (second e)) (parse (third e)))])]))

(define (interp a)
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    (multC (l r) (* (interp l) (interp r)))))

(define-type ArithS
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [bminusS (l ArithS?) (r ArithS?)]
  [multS (l ArithS?) (r ArithS?)])

(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

(define-type FunDefC
  [fdC (name symbol?) (arg symbol?) (body exprC)])

(test 0 (interp (numC 0)))
(test 1 (interp (plusC (numC 1) (numC 0))))
(test 6 (interp (multC (numC 2) (numC 3))))
(test 7 (interp (plusC (numC 1) (multC (numC 2) (numC 3)))))
(test 2 (interp (desugar (parse '(- 4 2)))))

  


