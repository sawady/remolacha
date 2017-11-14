programa
| /* EMPTY */     => Nil
| clase programa  => Cons($1, $2)

clase
| "class" ID locales metodos => DefClass($2, $3, $4)

locales
| /* EMPTY */        => Nil
| "local" ID locales => Cons($2, $3)

metodos
| /* EMPTY */     => Nil
| metodo metodos  => Cons($1, $2)

metodo
| "def" ID parametros bloque => DefMethod($2, $3, $4)

parametros
| "(" parametros1 ")" => $2

parametros1
| /* EMPTY */      => Nil
| ID masParametros => Cons($1, $2)

masParametros
| /* EMPTY */          => Nil
| "," ID masParametros => Cons($2, $3)

bloque
| /* EMPTY */      => Nil
| expresion bloque => Cons($1, $2)

expresion
| "set" ID "=" expresion => Set($2, $4)
| atomo expresion_cont   => $2[$1]

expresion_cont
| /* EMPTY */                      => _
| "." ID argumentos expresion_cont => $4[Send(_, $2, $3)]

atomo
| ID                  => Variable($1)
| STRING              => ConstantString($1)
| NUM                 => ConstantNumber($1)
| "self"              => Self
| "new" ID            => New($2)
| "(" expresion ")"   => $2

argumentos
| "(" argumentos1 ")" => $2

argumentos1
| /* EMPTY */             => Nil
| expresion masArgumentos => Cons($1, $2)

masArgumentos
| /* EMPTY */                 => Nil
| "," expresion masArgumentos => Cons($2, $3)