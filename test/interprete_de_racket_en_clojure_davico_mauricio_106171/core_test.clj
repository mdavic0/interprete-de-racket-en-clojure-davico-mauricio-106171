(ns interprete-de-racket-en-clojure-davico-mauricio-106171.core-test
  (:require [clojure.test :refer :all]
            [interprete-de-racket-en-clojure-davico-mauricio-106171.racket :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))
  )
)

(deftest test-proteger-bool-en-str
  (testing "Proteger bool en str: Cambia, en una cadena, #t por %t y #f por %f"
    (is (= (proteger-bool-en-str "(or #f #t)") "(or %f %t)"))
    (is (= (proteger-bool-en-str "(and (or #f #t) #t)") "(and (or %f %t) %t)"))
    (is (= (proteger-bool-en-str "") ""))
  )
)
