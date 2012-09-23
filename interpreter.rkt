#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define f-value (interp f env)])
                  (interp (closV-body f-value)
                          (extend-env (bind (closV-arg f-value)
                                            (interp a env))
                                      (closV-env f-value))))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [lamC (a b) (closV a b env)]
    [boxC (a) (boxV (interp a env))]
    [unboxC (a) (boxV-v (interp a env))]
    [seqC (b1 b2) (let ([v (interp b1 env)])
                    (interp b2 env))]
    [else (error 'interp "error")]
    ))
          

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
    [else (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r)) (numV (* (numV-n l) (numV-n r)))]
    [else (error 'num* "one argument was not a number")]))
  
(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (v : Value)])

(define (parse [e : s-expression])
  (cond
    [(s-exp-number? e) (numS (s-exp->number e))]
    [(s-exp-list? e)
     (let ([sl (s-exp->list e)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (cond
                [(empty? (rest (rest sl))) (uminusS (parse (second sl)))]
                [else (bminusS (parse (second sl)) (parse (third sl)))])]
                  
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (n) (plusC (numC 0) (multC (numC -1) (desugar n)))]))

(define-type-alias Location number)

(define-type Binding 
  [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define (lookup [for : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup "Couldn't find binding")]
    [else (if (symbol=? for (bind-name (first env))) (bind-val (first env))
              (lookup for (rest env)))]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "Couldn't find value")]
    [else (if (equal? loc (cell-location (first sto)))
              (cell-val (first sto))
              (fetch loc (rest sto)))]))

(print-only-errors true)
(test (interp (boxC (numC 18)) mt-env) (boxV (numV 18)))
(test (interp (unboxC (boxC (numC 18))) mt-env) (numV 18))

(interp
 (appC (lamC 'b
             (seqC (seqC (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b))))
                         (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b)))))
                   (unboxC (idC 'b))))
       (boxC (numC 0)))
 mt-env)

