(defproject interprete-de-racket-en-clojure-davico-mauricio-106171 "0.1.0-SNAPSHOT"
  :description "75.14 / 95.48 Lenguajes Formales
  
  Intérprete de Racket en Clojure
  "
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main ^:skip-aot interprete-de-racket-en-clojure-davico-mauricio-106171.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
