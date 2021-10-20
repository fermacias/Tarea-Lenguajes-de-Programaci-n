#lang play
(require "machine.rkt")
(print-only-errors #t)
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {- <s-expr> <s-expr>}
         | {with {<s-expr> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <type>} [: <type>] <expr>}
         | {<expr> <expr>}

<type> ::= Num
         | {<type> -> <type>}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s)
  (fun id targ body tbody)
  (fun-db body)
  (acc n) ;Se usa para la pregunta 3
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TFun arg ret))

#|
1. Funciones de primera clase con tipos declarados
|#

#|
parse-type: type -> Type
Parsea la gramática de tipos. En caso de error, retorna el error "Parse error"
|#
(define (parse-type s-expr)
  (match s-expr
    ['Num (TNum)]
    [(list t1 '-> t2) (TFun (parse-type t1) (parse-type t2))]
    [_ (error "Parse error")]))

#|
parse: s-expr -> Expr
Parsea la gramática completa del lje. Reemplaza el with por una aplicacion de funcion.
|#
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? s) (id s)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list with (list i ': ti e) body)
     (app (parse (list 'fun (list i ': ti) body)) (parse e))]
    [(list 'fun (list i ': ti) ': tb body)
     (fun (parse i) (parse-type ti) (parse body) (parse-type tb))]
    [(list 'fun (list i ': ti) body)
     (fun (parse i) (parse-type ti) (parse body) #f)]
    [(list f e) (app (parse f) (parse e))]
    [_ (error "Parse error")]
    ))


#|
prettify: Type -> type
Toma un tipo y lo escribe en sitaxis concreta
|#
(define (prettify t)
  (match t
    [(TNum) 'Num]
    [(TFun l r) (list (prettify l) '-> (prettify r))]))


#|
2. Verificación de Tipos
|#

#|
typeof: expr -> Type (o error)
Calcula el tipo de la expresion dada o falla cuando la expresion no es valida.
|#
(define (typeof expr [bound '()])
  (match expr
    [(num _)
     ; siempre es num
     (TNum)]
    
    [(id i)
     ; es num solo si no es free id
     (if (member (id i) bound)
         (TNum)
         (error (format "Type error: free identifier: ~a" i)))]
    
    [(add l r)
     ;  es num si l y r son num
     (define tl (typeof l bound))
     (define tr (typeof r bound))
     (if (equal? tl (TNum))
         (if (equal? tr (TNum))
             (TNum)
             (error (format "Type error in expression + position 2: expected ~a ~a ~a"
                    (prettify (TNum))
                    "found"
                    (prettify tr)))) ; si r no es num
         (error (format "Type error in expresion + position 1: expected ~a ~a ~a"
                (prettify (TNum))
                "found"
                (prettify tl))))] ; si l no es num
    
    [(sub l r)
     ; es num si l y r son nom
     (define tl (typeof l bound))
     (define tr (typeof r bound))
     (if (equal? tl (TNum))
         (if (equal? tr (TNum))
             (TNum)
             (error (format "Type error in expression - position 2: expected ~a ~a ~a"
                    (prettify (TNum))
                    "found"
                    (prettify tr)))) ; si r no es num
         (error (format "Type error in expresion - position 1: expected ~a ~a ~a"
                (prettify (TNum))
                "found"
                (prettify tl))))] ; si l no es num
    
    [(fun x tx y ty)
     (define typey (typeof y (cons x bound)))
     (if (id? x)
         (if (equal? #f ty)
             ; sin ty especificado
             (TFun tx typey)
             ; con ty especificado
             (if (equal? ty typey)
                 (TFun tx ty)
                 (error (format "Type error in expression fun position 1: expected ~a ~a ~a"
                        (prettify ty)
                        "found"
                        (prettify typey)))))
         (error "error"))] ; x no es variable
    
    [(app f a)
     (define tf (typeof f bound))
     (define ta (typeof a bound))
     (if (TFun? tf)
         (if (equal? (TFun-arg tf) ta)
             (TFun-ret tf) ; lo que entrega la funcion
             (error (format "Type error in expression app position 2: expected ~a ~a ~a"
                    (prettify (TFun-arg tf))
                    "found"
                    (prettify ta))))
         (error (format "Type error in expression app position 1: expected (?T -> ?S) found ~a"
                (prettify tf))))]
    
    [_ (error "error")]
    ))



#|
typecheck: s-expr -> Type
Retorna el tipo de un programa, en un formato legible
|#
(define (typecheck s-expr)
  (prettify (typeof (parse s-expr))))


#|
3. Compilación
|#

; ambientes : Env
; empty-env :: Env
; extended-env :: Id x Val x Env -> Env
(deftype Env
  (mtEnv)
  (aEnv x v next))

(define empty-env mtEnv)
(define extend-env aEnv)



;position :: Id x Env -> Val o error
;Indica la posicion de un id en un env solo si este pertenece a el
(define (position id env)
  (match env
    [(mtEnv) (error (format
              "Free identifier: ~a" (id-s  id)))]
    [(aEnv x v n) (if (equal? x id)
                      0
                      (+ 1 (position id n)))
                  ]))

#|
deBrujin: expr -> expr
Recorre la expresión reemplazando los índicadores no libres con los índices
de Brujin, manteniendo el scope léxico
SU SALIDA NO DEBE TENER FUN NI ID, SOLO FUN-DB Y ACC
|#
(define (deBruijn expr [env (mtEnv)])
  (match expr
    [(num n) (num n)]
    [(id x) (acc (position (id x) env))]
    [(add l r) (add (deBruijn l env) (deBruijn r env))]
    [(sub l r) (sub (deBruijn l env) (deBruijn r env))]
    [(fun x tx y ty)
     (fun-db (deBruijn y env))]
    [(app f e)
     (def (fun x _ y _) f)
     (app (deBruijn f (aEnv x e env)) (deBruijn e env))]
    ))



#|
compile: expr -> Instruction
Dada una expresion con indices De Bruijn genera un alista de instrucciones
|#
(define (compile expr)
  (match expr
    [(num n) (INT-CONST n)]
    [(acc a) (ACCESS a)]
    [(add a1 a2) (list (compile a2) (compile a1) (ADD))]
    [(sub a1 a2) (list (compile a2) (compile a1) (SUB))]
    [(app f a) (append (compile a) (list (compile f) (APPLY)))]
    [(fun-db b) (CLOSURE (append (compile b) (list (RETURN))))]))

#|
typed_compile: s-expr -> INSTRUCTION
Realiza todo el proceso de generación de código desde un programa
|#
(define (typed-compile s-expr)
  (typecheck s-expr)
  (compile (deBruijn (parse s-expr))))








