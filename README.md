# 75.14 / 95.48 Lenguajes Formales

## Interprete de racket en Clojure

* Davico Mauricio, Padrón: 106171.

## Usage

.\lein run

Cargar demo.rkt
`(enter! "demo.rkt")`

Esto carga el ambiente de demo y ademas hace display de lo que va evaluando, deteniendose en: 

![demo](/resources/demo.png)

Al ingresar un valor para r:

![Alt text](/resources/valor-r.png)

Para volver: `(enter! #f)`

Para salir: `(exit)`

Cargar jarras.rkt

`(enter! "jarras.rkt")`

![Prompt](/resources/prompt-esperado.png)

Para volver: `(enter! #f)`

Para probar que se cargo bien, llamar funciones definidas en jarras.rkt :

![Bien cargado](/resources/propar-que-se-cargo-bien.png)


Para probar jarras.rkt:

`(breadth-first bc)`

En nuestro racket:
![Result](/resources/resultado-esperado-propio.png)

En racket original
![Resultado esperado](/resources/resultado-esperado.png)

Para salir: 
`(exit)`
![Salir](/resources/salir.png)


Ejemplo define: 
![Alt text](/resources/ej-define.png)
## Tests

.\lein test


## License

Copyright © 2023 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
