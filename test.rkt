#lang play
(require "main.rkt")
(require "machine.rkt") 

#|
PARSE-TYPE TEST
|#
(test (parse-type 'Num) (TNum))
(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test (parse-type '{{Num -> Num} -> Num}) (TFun (TFun (TNum) (TNum)) (TNum)))
(test (parse-type '{Num -> {Num -> Num}}) (TFun (TNum) (TFun (TNum) (TNum))))
(test (parse-type '{{Num -> Num} -> {Num -> Num}}) (TFun (TFun (TNum) (TNum)) (TFun (TNum) (TNum))))
(test/exn (parse-type '{->}) "Parse error")
(test/exn (parse-type '{ -> Num}) "Parse error")
(test/exn (parse-type '{Num ->}) "Parse error")


#|
PARSE TEST
|#

; Valores y operaciones basicas
(test (parse 1)
      (num 1))
(test (parse 'x)
      (id 'x))
(test (parse '{+ 1 3})
      (add (num 1) (num 3)))
(test (parse '{- 1 3})
      (sub (num 1) (num 3)))

; funciones con tipos especificados
(test (parse '{fun {x : Num} : Num {+ x 3}})
      (fun (id 'x) (TNum) (add (id 'x) (num 3)) (TNum)))

(test (parse '{fun {y : {Num -> Num}} : {Num -> Num} y})
      (fun (id 'y) (TFun (TNum) (TNum)) (id 'y) (TFun (TNum) (TNum))))

(test (parse '{fun {y : Num} : {Num -> Num} {fun {x : Num} : Num {+ x y}}})
      (fun (id 'y)
           (TNum)
           (fun (id 'x) (TNum) (add (id 'x) (id 'y)) (TNum))
           (TFun (TNum) (TNum))))

; funciones sin tipos especificados
;; entrega un num
(test (parse '{fun {x : Num} {+ x 3}})
      (fun (id 'x) (TNum) (add (id 'x) (num 3)) #f))
;; entrega una funcion
(test (parse '{fun {y : Num} : {Num -> Num} {fun {x : Num} {+ x y}}})
      (fun (id 'y)
           (TNum)
           (fun (id 'x) (TNum) (add (id 'x) (id 'y)) #f)
           (TFun (TNum) (TNum))))
;; entrega una funcion, s/ tipos
(test (parse '{fun {y : Num} {fun {x : Num} {+ x y}}})
      (fun (id 'y)
           (TNum)
           (fun (id 'x) (TNum) (add (id 'x) (id 'y)) #f)
           #f))

;; recibe una funcion
(test (parse '{fun {f : (Num -> Num)} f})
      (fun (id 'f) (TFun (TNum) (TNum)) (id 'f) #f))

;; with
(test (parse '{with {y : Num 2} {+ x y}})
      (app (fun (id 'y) (TNum) (add (id 'x) (id 'y)) #f) (num 2)))
(test (parse '{with {x : Num 5} {+ x 3}})
      (app (fun (id 'x) (TNum) (add (id 'x) (num 3)) #f) (num 5)))


;; app
(test (parse '{{fun {x : Num} x} 1})
      (app (fun (id 'x) (TNum) (id 'x) #f) (num 1)))


#|
PRETTIFY TEST
|#

(test (prettify (TNum)) 'Num)
(test (prettify (TFun (TNum) (TNum))) '(Num -> Num))
(test (prettify (TFun (TNum) (TFun (TNum) (TNum)))) '(Num -> (Num -> Num)))
(test (prettify (TFun (TFun (TNum) (TNum)) (TFun (TNum) (TNum)))) '((Num -> Num) -> (Num -> Num)))

#|
TYPEOF TEST
|#

;; test result

; funcion con ty especificado
(test (typeof (parse '{fun {x : Num} : Num 5}))
  (TFun (TNum) (TNum)))
(test (typeof (fun (id 'x) (TNum) (add (id 'x) (id 'x)) (TNum)))
      (TFun (TNum) (TNum)))

; funcion sin ty especificado
(test (typeof (parse '{fun {x : Num} x}))
  (TFun (TNum) (TNum)))

; aplicacion de funciones 
(test (typeof (parse '{{fun {x : Num} x} 1}))
      (TNum))


;; test error

; funcion con ty especificado
(test/exn (typeof (parse '{fun {{x} : Num} : Num 10})) "error") ;FIX
(test/exn (typeof (parse '{fun {x : Num} : {Num -> Num} 10}))
          "Type error in expression fun position 1: expected (Num -> Num) found Num")

; funciones sin ty especificado
(test/exn (typeof (parse '{fun {{x} : Num} 10})) "error")
(test/exn (typeof (parse '{fun{x : Num}{+ x {fun{z : Num} 1}}}))
          "Type error in expression + position 2: expected Num found (Num -> Num)")

; aplicacion de funciones
(test/exn (typeof (parse '{1 2}))
          "Type error in expression app position 1: expected (?T -> ?S) found Num")
(test/exn (typeof (parse '{{fun {x : Num} : Num {+ x x}} {fun {x : Num} : Num 5}}))
          "Type error in expression app position 2: expected Num found (Num -> Num)") ;; error
(test/exn (typeof (parse '{{fun {{x} : Num} : Num 10} 2})) "error")

; id
(test/exn (typeof (parse 'y)) "Type error: free identifier: y")
(test/exn (typeof (parse '{fun{x : Num}{+ x z}})) "Type error: free identifier: z")


#|
TYPECHECK TEST
|#
(test (typecheck '3) 'Num)
(test (typecheck  '{fun {f : Num} : Num 10})'(Num -> Num))
(test (typecheck  '{{fun {f : Num} : Num 10} 1})'Num)
(test/exn (typecheck  '{+ 2 {fun {x : Num} : Num x}})
  "Type error in expression + position 2: expected Num found (Num -> Num)")


#|
DBRUIJN TEST
|#

;; member? and position
(define env1
  (aEnv (id 'x) 1 (aEnv (id 'y) 2 (mtEnv))))
(define env2
  (aEnv (id 'x) 1 (aEnv (id 'x) 2 (mtEnv))))

(test (position (id 'x) env1) 0)
(test (position (id 'x) env2) 0)
(test (position (id 'y) env1) 1)
(test/exn (position (id 'y) env2) "Free identifier: y")


;; deBruijn
(test (deBruijn (num 3)) (num 3))
(test (deBruijn (parse '{with  {y : Num  1} {+ y 1}}))
 (app (fun-db (add (acc 0) (num 1))) (num 1)))
(test (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 0) (num 1)))) (num 5)))
(test (deBruijn (parse '{+ 1 {with {x : Num 1} {with {y : Num 2} {+ x y}}}}))
      (add (num 1) (app (fun-db (app (fun-db (add (acc 1) (acc 0))) (num 2))) (num 1))))
(test/exn (deBruijn (parse 'x)) "Free identifier: x")
(test/exn (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ z x}}})) "Free identifier: z")


;;compile
(test (compile (add (num 2) (num 1))) (list  (INT-CONST 1) (INT-CONST 2) (ADD)))
(test (compile (deBruijn (parse '{{fun {x : Num} : Num {+ x 10}} {+ 2 3}})))
      (list (INT-CONST 3) (INT-CONST 2) (ADD) (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN))) (APPLY)) )


;; typed-compile
(test (typed-compile '{{fun {x : Num} : Num
                                   {+ x 10}} {+ 2 3}})
      (list
 (INT-CONST 3)
 (INT-CONST 2)
 (ADD)
 (CLOSURE (list (INT-CONST 10) (ACCESS 0) (ADD) (RETURN)))
 (APPLY)) )

