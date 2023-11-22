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
    (is (= (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)"))) "(and (or #F #f #t #T) #T)"))
    (is (= (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") ) "(and (or #F #f #t #T) #T)"))
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
