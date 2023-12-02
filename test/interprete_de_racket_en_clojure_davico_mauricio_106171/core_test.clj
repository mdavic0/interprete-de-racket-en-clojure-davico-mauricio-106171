(ns interprete-de-racket-en-clojure-davico-mauricio-106171.core-test
  (:require [clojure.test :refer :all]
            [interprete-de-racket-en-clojure-davico-mauricio-106171.racket :refer :all]))

; user=> (proteger-bool-en-str "(or #f #t)")
; "(or %f %t)"
; user=> (proteger-bool-en-str "(and (or #f #t) #t)")
; "(and (or %f %t) %t)"
; user=> (proteger-bool-en-str "")
; ""
(deftest test-proteger-bool-en-str
  (testing "Proteger bool en str: Cambia, en una cadena, #t por %t y #f por %f"
    (is (= (proteger-bool-en-str "(or #f #t)") "(or %f %t)"))
    (is (= (proteger-bool-en-str "(and (or #f #t) #t)") "(and (or %f %t) %t)"))
    (is (= (proteger-bool-en-str "") ""))
  )
)

; user=> (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)")))
; (and (or #F #f #t #T) #T)
; user=> (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") )
; (and (or #F #f #t #T) #T)
(deftest test-restaurar-bool
  (testing "Restaurar bool cambia, en un codigo leido con read-string, %t por #t y %f por #f."
    (is (= (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)"))) (list (symbol "and") (list (symbol "or") (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))))
    (is (= (restaurar-bool (read-string "(and (or %F %f %t %T) %T)")) (list (symbol "and") (list (symbol "or") (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))))
  )
)

; user=> (verificar-parentesis "(hola 'mundo")
; 1
; user=> (verificar-parentesis "(hola '(mundo)))")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")
; -1
; user=> (verificar-parentesis "(hola '(mundo) )")
; 0
(deftest test-verificar-parentesis
  (testing "Verificar paréntesis"
    (is (= (verificar-parentesis "(hola 'mundo") 1))
    (is (= (verificar-parentesis "(hola '(mundo)))") -1))
    (is (= (verificar-parentesis "(hola '(mundo) () 6) 7)") -1))
    (is (= (verificar-parentesis "(hola '(mundo) () 6) 7) 9)") -1))
    (is (= (verificar-parentesis "(hola '(mundo) )") 0))
  )
)

; user=> (buscar 'c '(a 1 b 2 c 3 d 4 e 5))
; 3
; user=> (buscar 'f '(a 1 b 2 c 3 d 4 e 5))
; (;ERROR: unbound variable: f)
(deftest test-buscar
  (testing "Buscar clave existente en ambiente"
    (is (= (buscar 'c '(a 1 b 2 c 3 d 4 e 5)) 3))
    (is (= (buscar 'a '(a 1 b 2 c 3 d 4 e 5)) 1)))

  (testing "Buscar clave inexistente en ambiente"
    (is (= (buscar 'f '(a 1 b 2 c 3 d 4 e 5)) (cons (symbol ";ERROR:") (list 'unbound (symbol "variable:") 'f))))
    (is (= (buscar 'x '(a 1 b 2 c 3 d 4 e 5)) (cons (symbol ";ERROR:") (list 'unbound (symbol "variable:") 'x)))))

  (testing "Buscar en ambiente vacío"
    (is (= (buscar 'a '()) (cons (symbol ";ERROR:") (list 'unbound (symbol "variable:") 'a)))))

  (testing "Buscar en ambiente con valores nulos"
    (is (= (buscar 'b '(a nil b nil c nil)) nil))
  )
)

; user=> (error? (list (symbol ";ERROR:") 'mal 'hecho))
; true
; user=> (error? (list 'mal 'hecho))
; false
; user=> (error? (list (symbol ";WARNING:") 'mal 'hecho))
; true
(deftest test-error?
  (testing "Lista que empieza con ;ERROR: es un error"
    (is (= (error? (list (symbol ";ERROR:") 'mal 'hecho)) true))
  )
    
  (testing "Lista que empieza con ;WARNING: es un error"
    (is (= (error? (list (symbol ";WARNING:") 'mal 'hecho)) true))
  )

  (testing "Lista que no empieza con ;ERROR: ni ;WARNING: no es un error"
    (is (= (error? (list 'mal 'hecho)) false))
  )
)

; user=> (fnc-append '( (1 2) (3) (4 5) (6 7)))
; (1 2 3 4 5 6 7)
; user=> (fnc-append '( (1 2) 3 (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg 3)
; user=> (fnc-append '( (1 2) A (4 5) (6 7)))
; (;ERROR: append: Wrong type in arg A)
(deftest test-fnc-append
  (testing "fnc-append: Concatena listas"
    (is (= (fnc-append '( (1 2) (3) (4 5) (6 7))) '(1 2 3 4 5 6 7)))
    (is (= (fnc-append '( (1 2) 3 (4 5) (6 7))) (cons (symbol ";ERROR:") (list (symbol "append:") 'Wrong 'type 'in 'arg 3))))
    (is (= (fnc-append '( (1 2) A (4 5) (6 7))) (cons (symbol ";ERROR:") (list (symbol "append:") 'Wrong 'type 'in 'arg 'A))))
  )
) 

; user=> (fnc-equal? ())
; #t
; user=> (fnc-equal? '(A))
; #t
; user=> (fnc-equal? '(A a))
; #t
; user=> (fnc-equal? '(A a A))
; #t
; user=> (fnc-equal? '(A a A a))
; #t
; user=> (fnc-equal? '(A a A B))
; #f
; user=> (fnc-equal? '(1 1 1 1))
; #t
; user=> (fnc-equal? '(1 1 2 1))
; #f
(deftest test-fnc-equal?
  (testing "fnc-equal?: Compara elementos en la lista (case-sensitive)"
    (is (= (fnc-equal? '()) (symbol "#t")))
    (is (= (fnc-equal? '(A)) (symbol "#t")))
    (is (= (fnc-equal? '(A a)) (symbol "#t")))
    (is (= (fnc-equal? '(A a A)) (symbol "#t")))
    (is (= (fnc-equal? '(A a A a)) (symbol "#t")))
    (is (= (fnc-equal? '(A a A B)) (symbol "#f")))
    (is (= (fnc-equal? '(1 1 1 1)) (symbol "#t")))
    (is (= (fnc-equal? '(1 1 2 1)) (symbol "#f")))
  )
)

; user=> (fnc-sumar ())
; 0
; user=> (fnc-sumar '(3))
; 3
; user=> (fnc-sumar '(3 4))
; 7
; user=> (fnc-sumar '(3 4 5))
; 12
; user=> (fnc-sumar '(3 4 5 6))
; 18
; user=> (fnc-sumar '(A 4 5 6))
; (;ERROR: +: Wrong type in arg1 A)
; user=> (fnc-sumar '(3 A 5 6))
; (;ERROR: +: Wrong type in arg2 A)
; user=> (fnc-sumar '(3 4 A 6))
; (;ERROR: +: Wrong type in arg2 A)
(deftest test-fnc-sumar
  (testing "fnc-sumar: Suma los elementos de la lista"
    (is (= (fnc-sumar '()) 0))
    (is (= (fnc-sumar '(3)) 3))
    (is (= (fnc-sumar '(3 4)) 7))
    (is (= (fnc-sumar '(3 4 5)) 12))
    (is (= (fnc-sumar '(3 4 5 6)) 18))
    (is (= (fnc-sumar '(A 4 5 6)) (cons (symbol ";ERROR:") (list (symbol "+:") 'Wrong 'type 'in 'arg1 'A))))
    (is (= (fnc-sumar '(3 A 5 6)) (cons (symbol ";ERROR:") (list (symbol "+:") 'Wrong 'type 'in 'arg2 'A))))
    (is (= (fnc-sumar '(3 4 A 6)) (cons (symbol ";ERROR:") (list (symbol "+:") 'Wrong 'type 'in 'arg2 'A))))
  )
)

; user=> (fnc-restar ())
; (;ERROR: -: Wrong number of args given)
; user=> (fnc-restar '(3))
; -3
; user=> (fnc-restar '(3 4))
; -1
; user=> (fnc-restar '(3 4 5))
; -6
; user=> (fnc-restar '(3 4 5 6))
; -12
; user=> (fnc-restar '(A 4 5 6))
; (;ERROR: -: Wrong type in arg1 A)
; user=> (fnc-restar '(3 A 5 6))
; (;ERROR: -: Wrong type in arg2 A)
; user=> (fnc-restar '(3 4 A 6))
; (;ERROR: -: Wrong type in arg2 A)
(deftest test-fnc-restar
  (testing "fnc-restar: Resta los elementos de una lista"
    (is (= (fnc-restar '()) (cons (symbol ";ERROR:") (list (symbol "-:") 'Wrong 'number 'of 'args 'given))))
    (is (= (fnc-restar '(3)) -3))
    (is (= (fnc-restar '(3 4)) -1))
    (is (= (fnc-restar '(3 4 5)) -6))
    (is (= (fnc-restar '(3 4 5 6)) -12))
    (is (= (fnc-restar '(A 4 5 6)) (cons (symbol ";ERROR:") (list (symbol "-:") 'Wrong 'type 'in 'arg1 'A))))
    (is (= (fnc-restar '(3 A 5 6)) (cons (symbol ";ERROR:") (list (symbol "-:") 'Wrong 'type 'in 'arg2 'A))))
    (is (= (fnc-restar '(3 4 A 6)) (cons (symbol ";ERROR:") (list (symbol "-:") 'Wrong 'type 'in 'arg2 'A))))
  )
)

; user=> (fnc-menor ())
; #t
; user=> (fnc-menor '(1))
; #t
; user=> (fnc-menor '(1 2))
; #t
; user=> (fnc-menor '(1 2 3))
; #t
; user=> (fnc-menor '(1 2 3 4))
; #t
; user=> (fnc-menor '(1 2 2 4))
; #f
; user=> (fnc-menor '(1 2 1 4))
; #f
; user=> (fnc-menor '(A 1 2 4))
; (;ERROR: <: Wrong type in arg1 A)
; user=> (fnc-menor '(1 A 1 4))
; (;ERROR: <: Wrong type in arg2 A)
; user=> (fnc-menor '(1 2 A 4))
; (;ERROR: <: Wrong type in arg2 A)
(deftest test-fnc-menor
  (testing "fnc-menor: Devuelve #t si los numeros de una lista estan en orden estrictamente creciente; si no, #f."
    (is (= (fnc-menor ()) (symbol "#t")))
    (is (= (fnc-menor '(1)) (symbol "#t")))
    (is (= (fnc-menor '(1 2)) (symbol "#t")))
    (is (= (fnc-menor '(1 2 3)) (symbol "#t")))
    (is (= (fnc-menor '(1 2 3 4)) (symbol "#t")))
    (is (= (fnc-menor '(1 2 2 4)) (symbol "#f")))
    (is (= (fnc-menor '(1 2 1 4)) (symbol "#f")))
    (is (= (fnc-menor '(A 1 2 4)) (cons (symbol ";ERROR:") (list (symbol "<:") 'Wrong 'type 'in 'arg1 'A))))
    (is (= (fnc-menor '(1 A 1 4)) (cons (symbol ";ERROR:") (list (symbol "<:") 'Wrong 'type 'in 'arg2 'A))))
    (is (= (fnc-menor '(1 2 A 4)) (cons (symbol ";ERROR:") (list (symbol "<:") 'Wrong 'type 'in 'arg2 'A))))
  )
)

; user=> (fnc-mayor ())
; #t
; user=> (fnc-mayor '(1))
; #t
; user=> (fnc-mayor '(2 1))
; #t
; user=> (fnc-mayor '(3 2 1))
; #t
; user=> (fnc-mayor '(4 3 2 1))
; #t
; user=> (fnc-mayor '(4 2 2 1))
; #f
; user=> (fnc-mayor '(4 2 1 4))
; #f
; user=> (fnc-mayor '(A 3 2 1))
; (;ERROR: >: Wrong type in arg1 A)
; user=> (fnc-mayor '(3 A 2 1))
; (;ERROR: >: Wrong type in arg2 A)
; user=> (fnc-mayor '(3 2 A 1))
; (;ERROR: >: Wrong type in arg2 A)
(deftest test-fnc-mayor
  (testing "fnc-mayor: Devuelve #t si los numeros de una lista estan en orden estrictamente decreciente; si no, #f."
    (is (= (fnc-mayor ()) (symbol "#t")))
    (is (= (fnc-mayor '(1)) (symbol "#t")))
    (is (= (fnc-mayor '(2 1)) (symbol "#t")))
    (is (= (fnc-mayor '(3 2 1)) (symbol "#t")))
    (is (= (fnc-mayor '(4 3 2 1)) (symbol "#t")))
    (is (= (fnc-mayor '(4 2 2 1)) (symbol "#f")))
    (is (= (fnc-mayor '(4 2 1 4)) (symbol "#f")))
    (is (= (fnc-mayor '(A 3 2 1)) (cons (symbol ";ERROR:") (list (symbol ">:") 'Wrong 'type 'in 'arg1 'A))))
    (is (= (fnc-mayor '(3 A 2 1)) (cons (symbol ";ERROR:") (list (symbol ">:") 'Wrong 'type 'in 'arg2 'A))))
    (is (= (fnc-mayor '(3 2 A 1)) (cons (symbol ";ERROR:") (list (symbol ">:") 'Wrong 'type 'in 'arg2 'A))))
  )
)

; user=> (fnc-mayor-o-igual ())
; #t
; user=> (fnc-mayor-o-igual '(1))
; #t
; user=> (fnc-mayor-o-igual '(2 1))
; #t
; user=> (fnc-mayor-o-igual '(3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 3 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 2 1))
; #t
; user=> (fnc-mayor-o-igual '(4 2 1 4))
; #f
; user=> (fnc-mayor-o-igual '(A 3 2 1))
; (;ERROR: >=: Wrong type in arg1 A)
; user=> (fnc-mayor-o-igual '(3 A 2 1))
; (;ERROR: >=: Wrong type in arg2 A)
; user=> (fnc-mayor-o-igual '(3 2 A 1))
; (;ERROR: >=: Wrong type in arg2 A)
(deftest test-fnc-mayor-o-igual
  (testing "fnc-mayor-o-igual: Devuelve #t si los numeros de una lista estan en orden decreciente; si no, #f."
    (is (= (fnc-mayor-o-igual ()) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(1)) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(2 1)) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(3 2 1)) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(4 3 2 1)) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(4 2 2 1)) (symbol "#t")))
    (is (= (fnc-mayor-o-igual '(4 2 1 4)) (symbol "#f")))
    (is (= (fnc-mayor-o-igual '(A 3 2 1)) (cons (symbol ";ERROR:") (list (symbol ">=:") 'Wrong 'type 'in 'arg1 'A))))
    (is (= (fnc-mayor-o-igual '(3 A 2 1)) (cons (symbol ";ERROR:") (list (symbol ">=:") 'Wrong 'type 'in 'arg2 'A))))
    (is (= (fnc-mayor-o-igual '(3 2 A 1)) (cons (symbol ";ERROR:") (list (symbol ">=:") 'Wrong 'type 'in 'arg2 'A))))
  )
)

; user=> (evaluar-escalar 32 '(x 6 y 11 z "hola"))
; (32 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar "chau" '(x 6 y 11 z "hola"))
; ("chau" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'y '(x 6 y 11 z "hola"))
; (11 (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'z '(x 6 y 11 z "hola"))
; ("hola" (x 6 y 11 z "hola"))
; user=> (evaluar-escalar 'n '(x 6 y 11 z "hola"))
; ((;ERROR: unbound variable: n) (x 6 y 11 z "hola"))

(deftest test-evaluar-escalar
  (testing "evaluar-escalar: Evalua un escalar en un ambiente"
    (is (= (evaluar-escalar 32 '(x 6 y 11 z "hola")) '(32 (x 6 y 11 z "hola"))))
    (is (= (evaluar-escalar "chau" '(x 6 y 11 z "hola")) '("chau" (x 6 y 11 z "hola"))))
    (is (= (evaluar-escalar 'y '(x 6 y 11 z "hola")) '(11 (x 6 y 11 z "hola"))))
    (is (= (evaluar-escalar 'z '(x 6 y 11 z "hola")) '("hola" (x 6 y 11 z "hola"))))
    (is (= (evaluar-escalar 'n '(x 6 y 11 z "hola")) (list (cons (symbol ";ERROR:") (list 'unbound (symbol "variable:") 'n)) '(x 6 y 11 z "hola"))))
  )
)

; user=> (leer-entrada)
; (hola
; mundo)
; "(hola mundo)"
; user=> (leer-entrada)
; 123
; "123"
; user=> (leer-entrada)
; (+ 1 3) 3)
; ;WARNING: unexpected ")"#<input-port 0>
; "(+ 1 3) 3)"

;; Agegro tests automaticos para leer-entrada usando un wrapper que internamente
;; llama a leer-entrada
;; FUNCION PARA TESTEAR leer-entrada de forma automática
(defn testing-leer-entrada [entrada-esperada entrada-simulada]
  (with-in-str entrada-simulada
    (let [resultado (leer-entrada)]
      resultado)))

(deftest test-leer-entrada
  (testing "leer-entrada: Lee una entrada de la consola"
    (is (= (testing-leer-entrada "(hola mundo)" "(hola\nmundo)\n") "(hola mundo)"))
    (is (= (testing-leer-entrada "123" "123\n") "123"))
    ;; Dejo comentado este caso ya que printea un WARNING en la consola
    ;; (is (= (testing-leer-entrada "(+ 1 3) 3)" "(+ 1 3) 3)\n") "(+ 1 3) 3)"))
    ;; (is (= (leer-entrada) "(+ 1 3) 3)"))
  )
)

; user=> (actualizar-amb '(a 1 b 2 c 3) 'd 4)
; (a 1 b 2 c 3 d 4)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b 4)
; (a 1 b 4 c 3)
; user=> (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))
; (a 1 b 2 c 3)
; user=> (actualizar-amb () 'b 7)
; (b 7)
(deftest test-actualizar-amb
  (testing "actualizar-amb: Actualiza un ambiente"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'd 4) '(a 1 b 2 c 3 d 4)))
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b 4) '(a 1 b 4 c 3)))
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho)) '(a 1 b 2 c 3)))
    (is (= (actualizar-amb () 'b 7) '(b 7)))
  )
)

;; FUNCION PARA TESTEAR fnc-read de forma automática
;; Utiliza el wrapper de leer-entrada para simular la entrada de datos
(defn testing-fnc-read
  "Devuelve la lectura de un elemento de Racket desde la terminal/consola."
  [list] 
  (cond
    (empty? list) (read-string (testing-leer-entrada "(hola mundo)" "(hola\nmundo)\n"))
    (= (count list) 1) (generar-mensaje-error :io-ports-not-implemented 'read)
    :else (generar-mensaje-error :wrong-number-args-prim-proc 'read)
  )
)
; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read '(1))
; (;ERROR: read: Use of I/O ports not implemented)
; user=> (fnc-read '(1 2))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
; user=> (fnc-read '(1 2 3))
; (;ERROR: Wrong number of args given #<primitive-procedure read>)
(deftest test-fnc-read
  (testing "fnc-read: Lee una entrada de la consola"
    (is (= (testing-fnc-read ()) '(hola mundo)))
    (is (= (fnc-read '(1)) (cons (symbol ";ERROR:") (list (symbol "read:") 'Use 'of 'I/O 'ports 'not 'implemented))))
    (is (= (fnc-read '(1 2)) (cons (symbol ";ERROR:") (list 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") 'read>))))
    (is (= (fnc-read '(1 2 3)) (cons (symbol ";ERROR:") (list 'Wrong 'number 'of 'args 'given (symbol "#<primitive-procedure") 'read>))))
  )
)


; user=> (evaluar-define '(define x 2) '(x 1))                         -> COND 4
; (#<void> (x 2))
; user=> (evaluar-define '(define (f x) (+ x 1)) '(x 1))               -> else
; (#<void> (x 1 f (lambda (x) (+ x 1))))
; user=> (evaluar-define '(define) '(x 1))                              -> COND 1
; ((;ERROR: define: missing or extra expression (define)) (x 1))
; user=> (evaluar-define '(define x) '(x 1))                            -> COND 1
; ((;ERROR: define: missing or extra expression (define x)) (x 1))
; user=> (evaluar-define '(define x 2 3) '(x 1))                        -> COND 3
; ((;ERROR: define: missing or extra expression (define x 2 3)) (x 1))
; user=> (evaluar-define '(define ()) '(x 1))                           -> COND 1  
; ((;ERROR: define: missing or extra expression (define ())) (x 1))
; user=> (evaluar-define '(define () 2) '(x 1))                         -> COND 2
; ((;ERROR: define: bad variable (define () 2)) (x 1))
; user=> (evaluar-define '(define 2 x) '(x 1))                          -> COND 2
; ((;ERROR: define: bad variable (define 2 x)) (x 1))
(deftest test-evaluar-define 
  (testing "evaluar-define: Evalua una expresion de tipo define"
    (is (= (evaluar-define '(define x 2) '(x 1)) (list (symbol "#<void>") '(x 2))))
    (is (= (evaluar-define '(define (f x) (+ x 1)) '(x 1)) (list (symbol "#<void>") '(x 1 f (lambda (x) (+ x 1))))))
    (is (= (evaluar-define '(define) '(x 1)) (list (cons (symbol ";ERROR:") (list (symbol "define:") 'missing 'or 'extra 'expression '(define))) '(x 1))))
    (is (= (evaluar-define '(define x) '(x 1)) (list (cons (symbol ";ERROR:") (list (symbol "define:") 'missing 'or 'extra 'expression '(define x))) '(x 1))))
    (is (= (evaluar-define '(define x 2 3) '(x 1)) (list (cons (symbol ";ERROR:") (list (symbol "define:") 'missing 'or 'extra 'expression '(define x 2 3))) '(x 1))))
    (is (= (evaluar-define '(define ()) '(x 1)) (list (cons (symbol ";ERROR:") (list (symbol "define:") 'missing 'or 'extra 'expression '(define ()))) '(x 1))))
    (is (= (evaluar-define '(define () 2) '(x 1)) (list (cons (symbol ";ERROR:") (list (symbol "define:") 'bad 'variable '(define () 2))) '(x 1))))
    (is (= (evaluar-define '(define 2 x) '(x 1)) (list (cons (symbol ";ERROR:") (list (symbol "define:") 'bad 'variable '(define 2 x))) '(x 1))))
  )
)

; user=> (evaluar-if '(if 1 2) '(n 7))
; (2 (n 7))
; user=> (evaluar-if '(if 1 n) '(n 7))
; (7 (n 7))
; user=> (evaluar-if '(if 1 n 8) '(n 7))
; (7 (n 7))
; user=> (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<void> (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f")))
; (8 (n 7 #f #f))
; user=> (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f")))
; (#<void> (n 9 #f #f))
; user=> (evaluar-if '(if) '(n 7))
; ((;ERROR: if: missing or extra expression (if)) (n 7))
; user=> (evaluar-if '(if 1) '(n 7))
; ((;ERROR: if: missing or extra expression (if 1)) (n 7))
(deftest test-evaluar-if
  (testing "evaluar-if: Evalua una expresion de tipo if"
    (is (= (evaluar-if '(if 1 2) '(n 7)) '(2 (n 7))))
    (is (= (evaluar-if '(if 1 n) '(n 7)) '(7 (n 7))))
    (is (= (evaluar-if '(if 1 n 8) '(n 7)) '(7 (n 7))))
    (is (= (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f"))) (list (symbol "#<void>") (list 'n '7 (symbol "#f") (symbol "#f")))))
    (is (= (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f"))) (list 8 (list 'n 7 (symbol "#f") (symbol "#f")))))
    (is (= (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f"))) (list (symbol "#<void>") (list 'n 9 (symbol "#f") (symbol "#f")))))
    (is (= (evaluar-if '(if) '(n 7)) (list (cons (symbol ";ERROR:") (list (symbol "if:") 'missing 'or 'extra 'expression '(if))) '(n 7))))
    (is (= (evaluar-if '(if 1) '(n 7)) (list (cons (symbol ";ERROR:") (list (symbol "if:") 'missing 'or 'extra 'expression '(if 1))) '(n 7))))
  )
)

; user=> (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#t (#f #f #t #t))
; user=> (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (7 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (5 (#f #f #t #t))
; user=> (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
; (#f (#f #f #t #t))
(deftest test-evaluar-or
  (testing "evaluar-or: Evalua una expresion de tipo or"
    (is (= (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list 5 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
    (is (= (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))) (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))))
  )	
)


; user=> (evaluar-set! '(set! x 1) '(x 0))
; (#<void> (x 1))
; user=> (evaluar-set! '(set! x 1) '())
; ((;ERROR: unbound variable: x) ())
; user=> (evaluar-set! '(set! x) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x)) (x 0))
; user=> (evaluar-set! '(set! x 1 2) '(x 0))
; ((;ERROR: set!: missing or extra expression (set! x 1 2)) (x 0))
; user=> (evaluar-set! '(set! 1 2) '(x 0))
; ((;ERROR: set!: bad variable 1) (x 0))
(deftest test-evaluar-set!
  (testing "evaluar-set!: Evalua una expresion de tipo set!"
    (is (= (evaluar-set! '(set! x 1) '(x 0)) (list (symbol "#<void>") '(x 1))))
    (is (= (evaluar-set! '(set! x 1) '()) (list (cons (symbol ";ERROR:") (list 'unbound (symbol "variable:") 'x)) '())))
    (is (= (evaluar-set! '(set! x) '(x 0)) (list (cons (symbol ";ERROR:") (list (symbol "set!:") 'missing 'or 'extra 'expression '(set! x))) '(x 0))))
    (is (= (evaluar-set! '(set! x 1 2) '(x 0)) (list (cons (symbol ";ERROR:") (list (symbol "set!:") 'missing 'or 'extra 'expression '(set! x 1 2))) '(x 0))))
    (is (= (evaluar-set! '(set! 1 2) '(x 0)) (list (cons (symbol ";ERROR:") (list (symbol "set!:") 'bad 'variable '1)) '(x 0))))
  )
)



;; ################## MI SALIDA

;; *****************************************************
;; *                    Racket 2023                    *
;; * Demo de definicion y uso de variables y funciones *
;; *****************************************************

;; Definicion de variables
;; -----------------------
;; > (define u 'u)
;; > (define v 'v)
;; > (define w 'w)

;; Las variables ahora estan en el ambiente.
;; Evaluandolas se obtienen sus valores:
;; > u
;; (quote u)
;; > v
;; (quote v)
;; > w
;; (quote w)

;; Una vez definida una variable, con set! se le puede
;; cambiar el valor:
;; > (define n 0)
;; > n
;; 0
;; > (set! n 17)
;; > n
;; 17

;; Definicion de funciones
;; -----------------------
;; > (define (sumar a b) (+ a b))
;; > (define (restar a b) (- a b))

;; Las funciones ahora estan en el ambiente.
;; es posible aplicarlas a valores formando expresiones
;; que evaluadas generan resultados:
;; > (sumar 3 5)
;; 8
;; > (restar 12 5)
;; 7

;; Racket es un lenguaje de ambito lexico (lexically scoped):
;; > (define x 1)
;; > (define (g y) (+ x y))
;; > (define (f x) (g 2))
;; > (f 5)
;; 3
;; [En TLC-Lisp -dynamically scoped- daria 7 en lugar de 3]

;; Aplicacion de funciones anonimas [lambdas]
;; ------------------------------------------

;; Lambda con cuerpo simple:
;; > ((lambda (y) (+ 1 y)) 15)
;; 16

;; Lambda con cuerpo multiple:
;; > ((lambda (y) (display "hola!") (newline) (+ 1 y)) 5)
;; hola!
;; 6

;; Lambda con cuerpo multiple y efectos colaterales [side effects]:
;; > ((lambda (a b c) (set! u a) (set! v b) (set! w c)) 1 2 3)
;; #<void>

;; Los nuevos valores de las variables modificadas:
;; > u
;; 1
;; > v
;; 2
;; > w
;; 3

;; Aplicacion parcial:
;; > (((lambda (x) (lambda (y) (- x y))) 8) 3)
;; 5

;; El mismo ejemplo anterior, ahora definiendo una funcion:
;; > (define p (lambda (x) (lambda (y) (- x y))))
;; > (p 8)
;; (lambda (y) (- 8 y))
;; > ((p 8) 3)
;; 5

;; Definicion de funciones recursivas [recorrido lineal]
;; -----------------------------------------------------

;; Funcion recursiva con efecto colateral
;; [deja en la variable d la cantidad de pares]:
;; > (define (recorrer l)
;;     (recorrer2 l 0))
;; > (define d 0)
;; > (define (recorrer2 l i)
;;     (cond
;;       ((null? (cdr l)) (set! d (+ 1 d)) (list (car l) i))
;;       (%t (display (list (car l) i)) (set! d (+ i 1)) (newline) (recorrer2 (cdr l) d))))
;; > (recorrer '(a b c d e f))
;; (a 0)
;; (b 1)
;; (c 2)
;; (d 3)
;; (e 4)
;; (f 5)
;; > d
;; 6

;; Definicion de funciones recursivas [recorrido "a todo nivel"]
;; -------------------------------------------------------------

;; Existencia de un elemento escalar en una lista:
;; > (define (existe? a l)
;;     (cond
;;       ((null? l) %f)
;;       ((not (list? (car l))) (or (equal? a (car l)) (existe? a (cdr l))))
;;       (else (or (existe? a (car l)) (existe? a (cdr l))))))
;; > (existe? 'c '(a ((b) ((d c) a) e f)))
;; FILTERED EXPR: : #f
;; #f
;; > (existe? 'g '(a ((b) ((d c) a) e f)))
;; FILTERED EXPR: : #f
;; #f

;; Eliminacion de un elemento de una lista:
;; > (define (eliminar dat li)
;;     (cond
;;       ((null? li) li)
;;       ((equal? dat (car li)) (eliminar dat (cdr li)))
;;       ((list? (car li)) (cons (eliminar dat (car li)) (eliminar dat (cdr li))))
;;       (else (cons (car li) (eliminar dat (cdr li))))))
;; > (eliminar 'c '(a ((b) ((d c) a) c f)))
;; (a ((b) ((d) a) f))
;; > (eliminar '(1 2 3) '(a ((b) (((1 2 3) c) a) c f)))
;; (a ((b) ((c) a) c f))

;; Profundidad de una lista:
;; > (define (profundidad lista)
;;     (if (or (not (list? lista)) (null? lista)) 0
;;         (if (> (+ 1 (profundidad (car lista))) (profundidad (cdr lista)))
;;             (+ 1 (profundidad (car lista)))
;;             (profundidad (cdr lista)))))
;; > (profundidad '((2 3)(3 ((7))) 5))
;; (if (> (+ 1 (profundidad (car (quote ((2 3) (3 ((7))) 5))))) (profundidad (cdr (quote ((2 3) (3 ((7))) 5))))) (+ 1 (profundidad (car (quote ((2 3) (3 ((7))) 5))))) (profundidad (cdr (quote ((2 3) (3 ((7))) 5)))))
;; [el valor esperado es 4]

;; "Planchado" de una lista:
;; > (define (planchar li)
;;     (cond
;;       ((null? li) '())
;;       ((list? (car li)) (append (planchar (car li)) (planchar (cdr li))))
;;       (else (cons (car li) (planchar (cdr li))))))
;; > (planchar '((2 3)(3 ((7))) 5))
;; (2 3 3 7 5)

;; Definicion de funciones para "ocultar" la recursividad en la programacion funcional
;; -----------------------------------------------------------------------------------

;; FILTRAR [selecciona de una lista los elementos que cumplan con una condicion dada]:
;; > (define (filtrar f l)
;;     (cond
;;       ((null? l) '())
;;       ((f (car l)) (cons (car l) (filtrar f (cdr l))))
;;       (else (filtrar f (cdr l)))))
;; > (filtrar (lambda (x) (> x 0)) '(5 0 2 -1 4 6 0 8))
;; (5 2 4 6 8)

;; REDUCIR [reduce una lista aplicando de a pares una funcion dada]:
;; > (define (reducir f l)
;;     (if (null? (cdr l))
;;         (car l)
;;         (f (car l) (reducir f (cdr l)))))
;; > (reducir (lambda (x y) (if (> x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))
;; ((lambda (x y) (if (> x 0) (cons x y) y)) (car (quote (5 0 2 -1 4 6 0 8 ()))) (reducir (lambda (x y) (if (> x 0) (cons x y) y)) (cdr (quote (5 0 2 -1 4 6 0 8 ())))))

;; MAPEAR [aplica a cada elemento de una lista una funcion dada]:
;; > (define (mapear op l)
;;     (if (null? l)
;;         '()
;;         (cons (op (car l)) (mapear op (cdr l)))))
;; > (mapear (lambda (x) (if (equal? x 0) 'z x)) '(5 0 2 -1 4 6 0 8))
;; (cons ((lambda (x) (if (equal? x 0) (quote z) x)) (car (quote (5 0 2 -1 4 6 0 8)))) (mapear (lambda (x) (if (equal? x 0) (quote z) x)) (cdr (quote (5 0 2 -1 4 6 0 8)))))

;; TRANSPONER [transpone una lista de listas]:
;; > (define (transponer m)
;;     (if (null? (car m))
;;         '()
;;         (cons (mapear car m) (transponer (mapear cdr m)))))
;; > (transponer '((a b c) (d e f) (g h i)))
;; (cons (mapear car (quote ((a b c) (d e f) (g h i)))) (transponer (mapear cdr (quote ((a b c) (d e f) (g h i))))))

;; IOTA [retorna una lista con los primeros n numeros naturales]:
;; > (define (iota n)
;;     (if (< n 1)
;;          '()
;;          (auxiota 1 n)))
;; > (define (auxiota i n)
;;     (if (equal? i n)
;;         (list n)
;;         (cons i (auxiota (+ i 1) n))))
;; > (iota 10)
;; (auxiota 1 10)

;; Funciones implementadas usando las funciones anteriores
;; -------------------------------------------------------

;; Sumatoria de los primeros n numeros naturales:
;; > (define (sumatoria n) (reducir + (iota n)))
;; > (sumatoria 100)
;; ((quote +) (car (quote (auxiota 1 100))) (reducir (quote +) (cdr (quote (auxiota 1 100)))))
;; [El valor esperado es 5050]

;; Eliminacion de los elementos repetidos en una lista simple:
;; > (define (eliminar-repetidos li)
;;     (reverse (reducir (lambda (x y) (if (existe? x y) y (cons x y))) (reverse (cons '() li)))))
;; > (eliminar-repetidos '(a b c d e f g d c h b i j))
;; ((reducir (lambda (x y) (if (existe? x y) y (cons x y))) (cdr (quote (j i b h c d g f e d c b a ())))) (car (quote (j i b h c d g f e d c b a ()))) (lambda (x y) (if (existe? x y) y (cons x y))))

;; Seleccion del enesimo elemento de una lista dada:
;; > (define (seleccionar n li)
;;     (if (or (< n 1) (> n (length li)))
;;         '()
;;         (car (car (filtrar (lambda (x) (equal? n (car (cdr x)))) (transponer (list li (iota (length li)))))))))
;; > (seleccionar 5 '(a b c d e f g h i j))
;; (car (car (filtrar (lambda (x) (equal? 5 (car (cdr x)))) (transponer (list (quote (a b c d e f g h i j)) (iota (length (quote (a b c d e f g h i j)))))))))

;; Aplicacion de todas las funciones de una lista a un elemento dado:
;; > (define (aplicar-todas lf x)
;;     (mapear (lambda (f) (f x)) lf))
;; > (aplicar-todas (list length cdr car) '((3 2 1)(9 8)(7 6)(5 4)))
;; (cons ((lambda (f) (f (quote ((3 2 1) (9 8) (7 6) (5 4))))) (car (quote (length cdr car)))) (mapear (lambda (f) (f (quote ((3 2 1) (9 8) (7 6) (5 4))))) (cdr (quote (length cdr car)))))

;; Entrada de datos y salida del interprete
;; ----------------------------------------

;; Carga de datos desde la terminal/consola:
;; > (define r 0)
;; > (define (cargar-r)
;;     (display "->r: ")(set! r (read))(display "r*2: ")(display (+ r r))(newline))
;; > (cargar-r)
;; ->r: 7
;; READED: : 7
;; r*2: 14

;; Para ver el ambiente [no funciona en Racket v8.10]: (env)
;; Para salir del interprete: (exit)


;; ;; ################## SALIDA ESPERADA
;; *****************************************************
;; *                    Racket 2023                    *
;; * Demo de definicion y uso de variables y funciones *
;; *****************************************************

;; Definicion de variables
;; -----------------------
;; > (define u 'u)
;; > (define v 'v)
;; > (define w 'w)

;; Las variables ahora estan en el ambiente.
;; Evaluandolas se obtienen sus valores:
;; > u
;; u
;; > v
;; v
;; > w
;; w

;; Una vez definida una variable, con set! se le puede
;; cambiar el valor:
;; > (define n 0)
;; > n
;; 0
;; > (set! n 17)
;; > n
;; 17

;; Definicion de funciones
;; -----------------------
;; > (define (sumar a b) (+ a b))
;; > (define (restar a b) (- a b))

;; Las funciones ahora estan en el ambiente.
;; es posible aplicarlas a valores formando expresiones
;; que evaluadas generan resultados:
;; > (sumar 3 5)
;; 8
;; > (restar 12 5)
;; 7

;; Racket es un lenguaje de ambito lexico (lexically scoped):
;; > (define x 1)
;; > (define (g y) (+ x y))
;; > (define (f x) (g 2))
;; > (f 5)
;; 3
;; [En TLC-Lisp -dynamically scoped- daria 7 en lugar de 3]

;; Aplicacion de funciones anonimas [lambdas]
;; ------------------------------------------

;; Lambda con cuerpo simple:
;; > ((lambda (y) (+ 1 y)) 15)
;; 16

;; Lambda con cuerpo multiple:
;; > ((lambda (y) (display "hola!") (newline) (+ 1 y)) 5)
;; hola!
;; 6

;; Lambda con cuerpo multiple y efectos colaterales [side effects]:
;; > ((lambda (a b c) (set! u a) (set! v b) (set! w c)) 1 2 3)
;; #<void>

;; Los nuevos valores de las variables modificadas:
;; > u
;; 1
;; > v
;; 2
;; > w
;; 3

;; Aplicacion parcial:
;; > (((lambda (x) (lambda (y) (- x y))) 8) 3)
;; 5

;; El mismo ejemplo anterior, ahora definiendo una funcion:
;; > (define p (lambda (x) (lambda (y) (- x y))))
;; > (p 8)
;; #<procedure:...les/Racket/demo.rkt:119:22>
;; > ((p 8) 3)
;; 5

;; Definicion de funciones recursivas [recorrido lineal]
;; -----------------------------------------------------

;; Funcion recursiva con efecto colateral
;; [deja en la variable d la cantidad de pares]:
;; > (define (recorrer l)
;;     (recorrer2 l 0))
;; > (define d 0)
;; > (define (recorrer2 l i)
;;     (cond
;;       ((null? (cdr l)) (set! d (+ 1 d)) (list (car l) i))
;;       (#t (display (list (car l) i)) (set! d (+ i 1)) (newline) (recorrer2 (cdr l) d))))
;; > (recorrer '(a b c d e f))
;; (a 0)
;; (b 1)
;; (c 2)
;; (d 3)
;; (e 4)
;; (f 5)
;; > d
;; 6

;; Definicion de funciones recursivas [recorrido "a todo nivel"]
;; -------------------------------------------------------------

;; Existencia de un elemento escalar en una lista:
;; > (define (existe? a l)
;;     (cond
;;       ((null? l) #f)
;;       ((not (list? (car l))) (or (equal? a (car l)) (existe? a (cdr l))))
;;       (else (or (existe? a (car l)) (existe? a (cdr l))))))
;; > (existe? 'c '(a ((b) ((d c) a) e f)))
;; #t
;; > (existe? 'g '(a ((b) ((d c) a) e f)))
;; #f

;; Eliminacion de un elemento de una lista:
;; > (define (eliminar dat li)
;;     (cond
;;       ((null? li) li)
;;       ((equal? dat (car li)) (eliminar dat (cdr li)))
;;       ((list? (car li)) (cons (eliminar dat (car li)) (eliminar dat (cdr li))))
;;       (else (cons (car li) (eliminar dat (cdr li))))))
;; > (eliminar 'c '(a ((b) ((d c) a) c f)))
;; (a ((b) ((d) a) f))
;; > (eliminar '(1 2 3) '(a ((b) (((1 2 3) c) a) c f)))
;; (a ((b) ((c) a) c f))

;; Profundidad de una lista:
;; > (define (profundidad lista)
;;     (if (or (not (list? lista)) (null? lista)) 0
;;         (if (> (+ 1 (profundidad (car lista))) (profundidad (cdr lista)))
;;             (+ 1 (profundidad (car lista)))
;;             (profundidad (cdr lista)))))
;; > (profundidad '((2 3)(3 ((7))) 5))
;; 4
;; [el valor esperado es 4]

;; "Planchado" de una lista:
;; > (define (planchar li)
;;     (cond
;;       ((null? li) '())
;;       ((list? (car li)) (append (planchar (car li)) (planchar (cdr li))))
;;       (else (cons (car li) (planchar (cdr li))))))
;; > (planchar '((2 3)(3 ((7))) 5))
;; (2 3 3 7 5)

;; Definicion de funciones para "ocultar" la recursividad en la programacion funcional
;; -----------------------------------------------------------------------------------

;; FILTRAR [selecciona de una lista los elementos que cumplan con una condicion dada]:
;; > (define (filtrar f l)
;;     (cond
;;       ((null? l) '())
;;       ((f (car l)) (cons (car l) (filtrar f (cdr l))))
;;       (else (filtrar f (cdr l)))))
;; > (filtrar (lambda (x) (> x 0)) '(5 0 2 -1 4 6 0 8))
;; (5 2 4 6 8)

;; REDUCIR [reduce una lista aplicando de a pares una funcion dada]:
;; > (define (reducir f l)
;;     (if (null? (cdr l))
;;         (car l)
;;         (f (car l) (reducir f (cdr l)))))
;; > (reducir (lambda (x y) (if (> x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))
;; (5 2 4 6 8)

;; MAPEAR [aplica a cada elemento de una lista una funcion dada]:
;; > (define (mapear op l)
;;     (if (null? l)
;;         '()
;;         (cons (op (car l)) (mapear op (cdr l)))))
;; > (mapear (lambda (x) (if (equal? x 0) 'z x)) '(5 0 2 -1 4 6 0 8))
;; (5 z 2 -1 4 6 z 8)

;; TRANSPONER [transpone una lista de listas]:
;; > (define (transponer m)
;;     (if (null? (car m))
;;         '()
;;         (cons (mapear car m) (transponer (mapear cdr m)))))
;; > (transponer '((a b c) (d e f) (g h i)))
;; ((a d g) (b e h) (c f i))

;; IOTA [retorna una lista con los primeros n numeros naturales]:
;; > (define (iota n)
;;     (if (< n 1)
;;          '()
;;          (auxiota 1 n)))
;; > (define (auxiota i n)
;;     (if (equal? i n)
;;         (list n)
;;         (cons i (auxiota (+ i 1) n))))
;; > (iota 10)
;; (1 2 3 4 5 6 7 8 9 10)

;; Funciones implementadas usando las funciones anteriores
;; -------------------------------------------------------

;; Sumatoria de los primeros n numeros naturales:
;; > (define (sumatoria n) (reducir + (iota n)))
;; > (sumatoria 100)
;; 5050
;; [El valor esperado es 5050]

;; Eliminacion de los elementos repetidos en una lista simple:
;; > (define (eliminar-repetidos li)
;;     (reverse (reducir (lambda (x y) (if (existe? x y) y (cons x y))) (reverse (cons '() li)))))
;; > (eliminar-repetidos '(a b c d e f g d c h b i j))
;; (a b c d e f g h i j)

;; Seleccion del enesimo elemento de una lista dada:
;; > (define (seleccionar n li)
;;     (if (or (< n 1) (> n (length li)))
;;         '()
;;         (car (car (filtrar (lambda (x) (equal? n (car (cdr x)))) (transponer (list li (iota (length li)))))))))
;; > (seleccionar 5 '(a b c d e f g h i j))
;; e

;; Aplicacion de todas las funciones de una lista a un elemento dado:
;; > (define (aplicar-todas lf x)
;;     (mapear (lambda (f) (f x)) lf))
;; > (aplicar-todas (list length cdr car) '((3 2 1)(9 8)(7 6)(5 4)))
;; (4 ((9 8) (7 6) (5 4)) (3 2 1))

;; Entrada de datos y salida del interprete
;; ----------------------------------------

;; Carga de datos desde la terminal/consola:
;; > (define r 0)
;; > (define (cargar-r)
;;     (display "->r: ")(set! r (read))(display "r*2: ")(display (+ r r))(newline))
;; > (cargar-r)
;; ->r: 7
;; r*2: 14

;; Para ver el ambiente [no funciona en Racket v8.10]: (env)
;; Para salir del interprete: (exit)
