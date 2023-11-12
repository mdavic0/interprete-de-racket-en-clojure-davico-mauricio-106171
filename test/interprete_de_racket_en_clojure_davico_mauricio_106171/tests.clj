(ns proyecto.core-test
  (:require [clojure.test :refer :all]
            [proyecto.core :refer :all]))

(deftest calcular-test

  (testing "Varias aserciones"
      (is (= 4 (calcular 3)))
      (is (= 5 (calcular 4)))
      (is (= 6 (calcular 5)))
  )
)

(deftest saludar-test

  (testing "Test del valor de retorno"
      (is (= nil (saludar "Diego")))
      (is (= nil (saludar nil)))
      (is (= nil (saludar "Pablo")))
  )

  (testing "Test del efecto colateral"
      (is (= "Hola Diego\n" (with-out-str (saludar "Diego"))))
  )
) 
