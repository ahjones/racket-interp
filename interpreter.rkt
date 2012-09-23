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

(define (interp [expr : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [idC (n) (v*s (fetch (lookup n env) sto) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                  [v*s (v-f s-f)
                       (type-case Result (interp a env s-f)
                         [v*s (v-a s-a) 
                              (let ([where (new-loc)])
                                (interp (closV-body v-f)
                                        (extend-env (bind (closV-arg v-f)
                                                          where)
                                                    (closV-env v-f))
                                        (override-store (cell where v-a) s-a)))])])]
    [plusC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (num* v-l v-r) s-r)])])]
    [lamC (a b) (v*s (closV a b env) sto)]
    [boxC (a) (type-case Result (interp a env sto)
                [v*s (v-a s-a)
                     (let ([where (new-loc)])
                       (v*s (boxV where)
                            (override-store (cell where v-a)
                                            s-a)))])]
    [unboxC (a) (type-case Result (interp a env sto)
                  [v*s (v-a s-a)
                       (v*s (fetch (boxV-l v-a) s-a) s-a)])]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1)
                         (interp b2 env s-b1)])]
    [setboxC (b v) (type-case Result (interp b env sto)
                     [v*s (v-b s-b)
                          (type-case Result (interp v env s-b)
                            [v*s (v-v s-v)
                                 (v*s v-v
                                      (override-store (cell (boxV-l v-b)
                                                            v-v)
                                                      s-v))])])]))
          

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
  [boxV (l : Location)])

(define-type Result
  [v*s (v : Value) (s : Store)])

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

(define (update-storage [new : Storage] [sto : Store])
  (cond
    [(empty? sto) (error 'update-storage "Location didn't exist to update")]
    [(equal? (cell-location new) (cell-location (first sto))) (cons new (rest sto))]
    [else (cons (first sto) (update-storage new (rest sto)))]))

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define (interp2 [expr : ExprC]) : Value
  (v*s-v (interp expr mt-env mt-store)))

(print-only-errors true)
;(test (interp2 (boxC (numC 18))) (boxV (numV 18)))
;(test (interp2 (unboxC (boxC (numC 18)))) (numV 18))

(interp2
 (appC (lamC 'b
             (seqC (seqC (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b))))
                         (setboxC (idC 'b) (plusC (numC 1) (unboxC (idC 'b)))))
                   (unboxC (idC 'b))))
       (boxC (numC 0))))

