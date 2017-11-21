# remolacha compiler

Remolacha es un lenguaje de programación orientado a objetos. Este proyecto es el compilador de remolacha a C++ para la materia de 
Parseo y Generación de Código de la Universidad Nacional de Quilmes.

Compilando el código
--------------------

Puede utilizar GHC y la plataforma haskell para compilar, pero sugerimos utilizar
Haskell Stack compilar la aplicación. Siga los siguientes pasos

1. [Instalar Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clonar este repositorio en una carpeta en su equipo
3. Abrir una terminal en dicha carpeta y ejecutar `stack setup`
5. En la misma terminal ejecutar `stack build happy`

Su sistema habrá descargado todas las dependencias necesarias para compilar
el código fuente. En la misma terminal puede ejecutar ahora
```
stack build
```
Esto compilará el código fuente y generará un archivo ejecutable para su
plataforma. Recuerde que si realiza alguna modificación al código, deberá
volver a ejecutar este comando para el correcto funcionamiento del mismo.

Corriendo la aplicación
-----------------------

Una vez construida la aplicación puede utilizar el binario generado directamente,
o puede continuar utilizando stack para mayor comodidad.

La aplicación espera una serie de argumentos para poder ejecutarse correctamente.
Puede ver los argumentos esperados ejecutando el programa con el flag `-?` o `--help`. 
No obstante siempre imprime el AST del programa parseado.

Utilice stack de la siguiente forma para ver la ayuda de la aplicación:

```
stack exec remolacha-exe -- -?
```

El primer doble guion indica que los argumentos deben ser pasados a la aplicación
ejecutada en lugar de a stack, luego de este simbolo puede colocar tantos parametros
de la aplicación como le parezca.

Los posibles parámetros que puede dar a la aplicación son:

* -? --help          (Muestra el mensaje de ayuda)
* -v --version       (Muestra la versión de la aplicación)
* -i --file\[=\[input\]\]  (programa Remolacha)
* -o --file\[=\[input\]\]  (archivo de salida)

El archivo de salida es opcional, y en caso de ser nulo imprimirá por la salida estándar el resultado de la compilación.

Corriendo los tests de la aplicación
------------------------------------

Si desea correr los tests puede ejecutar los mismos mediante el comando:

```
stack test
```

Esto evaluará distintos recorridos sobre el AST de Remolacha, sobre los archivos encontrados en la carpeta `test/input`.
