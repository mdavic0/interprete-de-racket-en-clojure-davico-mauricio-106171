(ns interprete-de-racket-en-clojure-davico-mauricio-106171.core-test
  (:require [clojure.test :refer :all]
            [interprete-de-racket-en-clojure-davico-mauricio-106171.racket :refer :all]))

(deftest test-proteger-bool-en-str
  (testing "Proteger bool en str: Cambia, en una cadena, #t por %t y #f por %f"
    (is (= (proteger-bool-en-str "(or #f #t)") "(or %f %t)"))
    (is (= (proteger-bool-en-str "(and (or #f #t) #t)") "(and (or %f %t) %t)"))
    (is (= (proteger-bool-en-str "") ""))
  )
)

(deftest test-verificar-parentesis
  (testing "Verificar paréntesis"
    (is (= (verificar-parentesis "(hola 'mundo") 1))
    (is (= (verificar-parentesis "(hola '(mundo)))") -1))
    (is (= (verificar-parentesis "(hola '(mundo) () 6) 7)") -1))
    (is (= (verificar-parentesis "(hola '(mundo) () 6) 7) 9)") -1))
    (is (= (verificar-parentesis "(hola '(mundo) )") 0))
  )
)

;; (deftest test-actualizar-amb
;;   (testing "Actualizar ambiente con clave existente"
;;     (is (= (actualizar-amb '(a 1 b 2 c 3) 'b 4) '(a 1 b 4 c 3)))
;;   )

;;   (testing "Actualizar ambiente con clave inexistente"
;;     (is (= (actualizar-amb '(a 1 b 2 c 3) 'e 5) '(a 1 b 2 c 3 e 5)))
;;     (is (= (actualizar-amb '(a 1 b 2 c 3) 'f 6) '(a 1 b 2 c 3 f 6)))
;;   )
  
;;   (testing "Actualizar ambiente con valor de error"
;;     (is (= (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho)) '(a 1 b 2 c 3)))
;;   )
  
;;   (testing "Actualizar ambiente vacío"
;;     (is (= (actualizar-amb () 'b 7) '(b 7)))
;;     (is (= (actualizar-amb () 'c (list (symbol ";ERROR:") 'error 'mensaje)) '()))
;;   )
;; )

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

(deftest test-restaurar-bool
  (testing "Restaurar bool cambia, en un codigo leido con read-string, %t por #t y %f por #f."
    (is (= (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)"))) "(and (or #F #f #t #T) #T)"))
    (is (= (restaurar-bool (read-string "(and (or %F %f %t %T) %T)") ) "(and (or #F #f #t #T) #T)"))
  )
)

(deftest test-fnc-append
  (testing "fnc-append: Concatena listas"
    (is (= (fnc-append '( (1 2) (3) (4 5) (6 7))) '(1 2 3 4 5 6 7)))
    (is (= (fnc-append '( (1 2) 3 (4 5) (6 7))) (cons (symbol ";ERROR:") (list (symbol "append:") 'Wrong 'type 'in 'arg 3))))
    (is (= (fnc-append '( (1 2) A (4 5) (6 7))) (cons (symbol ";ERROR:") (list (symbol "append:") 'Wrong 'type 'in 'arg 'A))))
  )
) 