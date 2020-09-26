# Fire Link

**Fire Link** es un lenguaje de programación basado en el _lore_ de la serie de videojuegos *Dark Souls*, diseñado para la cadena de electivas de área Lenguajes de Programación II y III (CI-4721, CI-4722), durante los trimestres Septiembre - Diciembre 2019 y Enero - Marzo 2020 en la Universidad Simón Bolívar.

## Autores

- Germán Robayo (14-10924)
- Andrés Ignacio Torres (14-11082)

## Descripción General del Lenguaje

**Fire Link** es un lenguaje de programación imperativo no orientado a objetos.

## Identificadores

En **Fire Link**, un identificador válido es toda palabra que comienza con una letra minúscula, está formada posteriormente por letras, números y guiones bajos (`_`) y no es una palabra reservada por el lenguaje, o un alias (`knight`) previamente declarado.

Formalmente, es toda palabra de la forma `[a-z][A-Za-z0-9_]*` que no sea a su vez una palabra reservada por el lenguaje.

## Declaradores

En **Fire Link**, existen dos tipos de declaradores para los tipos de datos:

- `const` para declarar identificadores cuyos valores no pueden ser reasignados, y que requiere de una inicialización junto a su declaración.
- `var` para declarar identificadores cuyos valores pueden o no ser reasignados, y que no requiere de una inicialización al momento de declararse.

La *sintaxis* de declaración sin asignación es la siguiente:

```firelink
<declarador de valor> <identificador> of type <declarador de tipo>
```

La *sintaxis* de declaración con asignación es la siguiente (obligatoria para constantes, opcional para variables):

```firelink
<declarador de valor> <identificador> of type <declarador de tipo> [<<= <valor>]
```

## Tipos de Datos

En **FireLink** se soportan los siguientes tipos de datos:

### Escalares

Los tipos de datos escalares son los tipos más primitivos que soporta el lenguaje, que pueden retornarse a través de funciones.

#### Entero

El tipo de dato _entero_ representa un número entero almacenado en complemento a dos. Este tipo cuenta con dos representaciones distintas, cada una con su declarador de tipo.

- `small humanity`, entero de 16 bits, entre -32768 y 32767.
- `big humanity`, entero de 32 bits, entre -2147483648 y 2147483647.

Adicionalmente, el declarador de tipo `humanity` es un alias para `big humanity`.

Los literales enteros son de la forma `[-]{0,1}[0-9]+` en expresión regular.

El valor por defecto de un entero es `0`.

##### Operadores para los enteros

Nuestro lenguaje soporta todos los operadores básicos de enteros:

- `+`: adición de dos `humanity`s, retornando un `humanity`.
- `-`: substracción de dos `humanity`s, retornando un `humanity`.
- `*`: multiplicación de dos `humanity`s, retornando un `humanity`.
- `/`: división de dos `humanity`s, retornando un `humanity`.
- `%`: resto de la división de dos `humanity`s, retornando un `humanity`.
- `-`: operador menos unario una `humanity`, retornando un `humanity`.
- `lt`: operador _menor que_, que retorna un `bonfire`.
- `gt`: operador _mayor que_, que retorna un `bonfire`.
- `lte`: operador _menor que_, que retorna un `bonfire`.
- `gte`: operador _mayor que_, que retorna un `bonfire`.
- `eq`: operador _igual_, que retorna un `bonfire`.
- `neq`: operador _desigual_, que retorna un `bonfire`.

#### Bonfire

El tipo de dato _bonfire_ representa un valor de verdad con posibilidad de incertidumbre, es decir, con tres posibilidades. Su declarador de tipo es `bonfire`.

Los literales bonfire son los siguientes:

- `lit` para un valor de verdad.
- `unlit` para un valor de falsedad.
- `undiscovered` para un valor de incertidumbre.

El valor por defecto de un _bonfire_ es `undiscovered`.

##### Operadores para los bonfires

El tipo de datos _bonfire_ soporta los siguientes operadores:

- `and`: `a and b`
- `or`: `a or b`
- `eq`: `a eq b`
- `neq`: `a neq b`
- `not`: `not a`

Los resultados de aplicar estos operadores se muestran a continuación, en distintas tablas.

##### Tabla de Valores para el Operador *not*

A continuación, los resultados de aplicar el operador _not_ a distintos valores del tipo `bonfire`.

| **not** |         |
|---------|---------|
| lit     | unlit   |
| undiscovered | undiscovered |
| unlit   | lit     |

##### Tabla de Valores para el Operador *and*

A continuación, los resultados de aplicar el operador _and_ a distintos valores del tipo `bonfire`.

| **and** | lit     | undiscovered | unlit   |
|---------|---------|---------|---------|
| lit     | lit     | undiscovered   | unlit   |
| undiscovered | undiscovered   | undiscovered     | undiscovered   |
| unlit   | unlit   | undiscovered   | unlit     |

##### Tabla de Valores para el Operador *or*

A continuación, los resultados de aplicar el operador _or_ a distintos valores del tipo `bonfire`.

| **eq**  | lit     | undiscovered | unlit   |
|---------|---------|---------|---------|
| lit     | lit     | lit   | lit   |
| undiscovered | lit   | undiscovered     | undiscovered   |
| unlit   | lit   | undiscovered   | unlit     |

##### Tabla de Valores para el Operador *eq*

A continuación, los resultados de aplicar el operador _eq_ a distintos valores del tipo `bonfire`.

| **eq**  | lit     | undiscovered | unlit   |
|---------|---------|---------|---------|
| lit     | lit     | undiscovered | unlit   |
| undiscovered | undiscovered | lit     | undiscovered |
| unlit   | unlit   | undiscovered | lit     |

##### Tabla de Valores para el Operador *neq*

A continuación, los resultados de aplicar el operador _neq_ a distintos valores del tipo `bonfire`.

| **neq** | lit     | undiscovered | unlit   |
|---------|---------|---------|---------|
| lit     | unlit   | undiscovered | lit     |
| undiscovered | undiscovered | unlit   | undiscovered |
| unlit   | lit     | undiscovered | unlit   |

#### Punto flotante

El tipo de dato _punto flotante_ representa un número decimal con signo de precisión doble almacenado según el estándar _IEEE 754-2019_. Su declarador de tipo es _hollow_.

Los literales punto flotante son de la forma `[-]{0,1}[0-9]+([.][0-9]+){0,1}` en expresión regular.

El valor por defecto de un punto flotante es `0.0`.

##### Operadores para los flotantes

Nuestro lenguaje soporta todos los operadores básicos de enteros:

- `+`: adición de dos `humanity`s, retornando un `humanity`.
- `-`: substracción de dos `humanity`s, retornando un `humanity`.
- `*`: multiplicación de dos `humanity`s, retornando un `humanity`.
- `/`: división de dos `humanity`s, retornando un `humanity`.
- `-`: operador menos unario una `humanity`, retornando un `humanity`.
- `lt`: operador _menor que_, que retorna un `bonfire`.
- `gt`: operador _mayor que_, que retorna un `bonfire`.
- `lte`: operador _menor que_, que retorna un `bonfire`.
- `gte`: operador _mayor que_, que retorna un `bonfire`.
- `eq`: operador _igual_, que retorna un `bonfire`.
- `neq`: operador _desigual_, que retorna un `bonfire`.

#### Caracter

El tipo de dato _caracter_ representa un caracter. Este tipo cuenta con una representación:.

- `sign`, caracter con codificación ASCII (ocupa 8 bits).

Los literales de caracter están encerrados entre el caracter `|`. Adicionalmente, el lenguaje debe reconocer los siguientes caracteres especiales:

- Caracter nulo `|\0|`
- Salto de línea `|\n|`
- Tabulación `|\t|`
- `|` `|\||`

El valor por defecto de un caracter es el caracter nulo (`|\0|`).

##### Funciones de los caracteres

Se debe implementar la siguiente función especial en el preludio de **FireLink**.

- `ascii_of`: Retorna el código ascii de la variable (como `humanity`).

### Colección

Los tipos colección permiten incorporar una noción de orden en datos al lenguaje.

#### Cadena de Caracteres

Representa una cadena colección de cero, uno o varios caracteres, almacenados contiguamente, de longitud conocida al momento de su declaración. Su declarador de tipo es `<n>-miracle` donde `n` es un literal entero o una expresión de tipo entero.

Los literales de cadena de caracteres están encerrados entre el caracter `@` y sus elementos no están separados entre sí por ningún caracter.

##### Operadores de las cadenas de caracteres

Se cuenta con el siguiente operador:

- `>-<`: toma un `<n>-miracle` y un `<m>-miracle` y los concatena, retornando un `<n+m>-miracle`.

##### Funciones de las cadenas de caracteres

Se deben implementar las siguientes funciones especiales en el preludio de **FireLink**.

- `ascii_of`: Retorna un arreglo de `humanity`s, representando los códigos asciis de cada caracter en el input.
- `size`: `size a` retorna un `humanity` con la cantidad de caracteres que tenga la cadena.

#### Arreglos

Representa una colección de valores del mismo tipo, almacenados contiguamente, de longitud conocida al momento de su declaración. Su declarador de tipo es `<n>-chest of type <tipo>`, donde `n` es un literal entero o una expresión de tipo entero y `tipo` es un declarador de tipo (que puede ser a su vez otro arreglo).

Los literales de arreglo están encerrados entre el caracter de inicio `<$` y el caracter de fin `$>`, y sus elementos están separados entre sí por una coma (`,`), seguida de cero, uno o varios espacios (` `). No se admite una coma entre el último elemento y el `$>` de cierre.

Se puede acceder al valor en la posición `i` de un arreglo `a` con la sintaxis `a<$i$>`.

Además, se debe proveer el operador `>-<` que concatena dos arreglos. El tipo de dato que encierra cada arreglo debe ser el mismo, aunque no está sujeto a que ambos arreglos sean del mismo tamaño. No hay coerción de tipos con el tipo que encierra
cada arreglo.

Finalmente, se debe proveer el operador unario `size` que retorna un `humanity` con la cantidad de elementos que tenga el arreglo en el momento.

#### Conjuntos

Representa una colección de valores del msimo tipo que no admite elementos repetidos. Su declarador de tipo es `armor of type <tipo>`, donde `tipo` es un declarador de tipo escalar.

Los literales de arreglo están encerrados entre el caracter de inicio `{$` y el caracter de fin `$}`, y sus elementos están separados entre sí por una coma (`,`), seguida de cero, uno o varios espacios (` `). No se admite una coma entre el último elemento y el `$}` de cierre.

Se deben proveer los siguientes operadores sobre conjuntos:

- `union`: `a union b` retorna un `armor` con todos los elementos que estén en `a` o en `b`.
- `intersect`: `a intersect b` retorna un `armor` con todos los elementos que estén en `a` y en `b` al mismo tiempo.
- `diff`: `a diff b` retorna un `armor` con todos los elementos que estén en `a` y no estén en `b`.
- `size`: `size a` retorna un `humanity` con la cantidad de elementos que tenga el conjunto en el momento.

Las operaciones binarias entre conjuntos y la asignación de literales a variables no hacen coerción de tipos automática.
Es decir, sí `a` es de tipo `armor of type type1` y `b` es de tipo `armor of type type2`, no se puede realizar `a op b`
si `type1 != type2` inclusive si hay alguna coerción de tipos entre `type1` y `type2`.

### Estructurados

Los tipos de datos estructurados dan cierta noción de estructura al lenguaje y son los siguientes.

#### Registro

Representa un agrupamiento de distintas _propiedades_ (donde una _propiedad_ es un valor de un tipo determinado) bajo un mismo nombre en un bloque de memoria, cada propiedad con su identificador y valor, almacenados contiguamente. Su declarador de tipo es `bezel`.

La sintaxis de declaración de un registro es la siguiente (con indentación agregada para mayor legibilidad):

```firelink
bezel {
  <nombre 1> of type <tipo 1>,
  <nombre 2> of type <tipo 2>,
  <nombre 3> of type <tipo 3>,
  ...
  <nombre n> of type <tipo n>
}
```

Donde `n` es la cantidad de _propiedades_ distintas del registro declarado, cada una con un nombre *único* para el mismo registro, independientemente de los tipos.

El acceso a una propiedad de nombre `prop` de un registro `a` se realiza mediante la sintaxis `a~>prop`.

Los literales de registros (`bezel literals`) son expresiones de la siguiente forma (con indentación agregada para mayor legibilidad):

```firelink
{
  <nombre 1> <<= <valor 1>,
  <nombre 2> <<= <valor 2>,
  ...
  <nombre n> <<= <valor n>
}
```

Donde `n` es la cantidad de _propiedades_ del tipo registro al cuál pertenece, cada una con una expresión (valor) asignada. Estos literales, para considerarse válidos, deben cumplir que:

- Tienen exactamente la misma cantidad de propiedades que el tipo registro al cual se están asignando
- Todos los nombres del tipo registro determinado están presentes en el literal de registro (sin importar el orden)
- Los tipos de las expresiones corresponden al tipo al cuál está asociado su nombre en el tipo registro determinado

#### Unión

Representa un valor que puede ser de algún tipo entre varios, bajo un mismo nombre en un mismo bloque de memoria. Su declarador de tipo es `link`.

La sintaxis de declaración de una unión es la siguiente (con indentación agregada para mayor legibilidad):

```firelink
link {
  <nombre 1> of type <tipo 1>,
  <nombre 2> of type <tipo 2>,
  <nombre 3> of type <tipo 3>,
  ...
  <nombre n> of type <tipo n>
}
```

Donde `n` es la cantidad de tipos de la unión, cada una con un nombre *único* para el mismo registro, independientemente de los tipos.

Los literales de uniones (`link literals`) son expresiones de la siguiente forma (con indentación agregada para mayor legibilidad):

```firelink
{
  <nombre> <<= <valor>
}
```

Donde `nombre` corresponde a _alguno_ de los nombres válidos del tipo unión al cuál pertenece, y valor es una expresión cuyo tipo equivale al tipo asociado con el nombre en el tipo unión. No se considerarán válidos literales de uniones que tengan más de un par nombre/valor, que tengan un nombre que no exista en el tipo unión, o cuya expresión pertenezca a un tipo incompatible con el tipo asociado al nombre declarado.

Note que intentar acceder al valor de un atributo inactivo de una unión (es decir: un atributo que no es el último inicializado, o cualquier atributo en el caso de una unión que no ha sido inicializada nunca) genera un error a tiempo de ejecución.

##### Funciones de las uniones

Se debe implementar la siguiente función especial en el preludio de **FireLink**.

- `is_active`: Recibe un atributo de una unión (`link`) y retorna un `bonfire` de la siguiente manera:
  - `lit` si el atributo es el atributo activo de la unión
  - `unlit` si el atributo NO es el atributo activo de la unión
  - `undiscovered` si ningún valor ha sido asignado aún a algún atributo de la unión

Un ejemplo de su uso sería el siguiente (con algunas libertades en la sintaxis):

```firelink

-- Asumimos que tenemos la siguiente declaración, aún sin inicializar
var x of type link {
  a of type humanity,
  b of type humanity,
  c of type bonfire
}

-- Antes de asignar algún valor
is_active x~>a -- undiscovered
is_active x~>b -- undiscovered
is_active x~>c -- undiscovered

-- Asignamos
x~>a <<= 3

-- Verificamos el atributo activo
is_active x~>a -- lit
is_active x~>b -- unlit
is_active x~>c -- unlit

-- Reasignamos
x~>c <<= lit

-- Verificamos el atributo activo
is_active x~>a -- unlit
is_active x~>b -- unlit
is_active x~>c -- lit
```

### Especiales

#### Valor nulo

El tipo unitario `abyss` tiene un único valor (`abyss`), utilizado para indicar que un apuntador no tiene a quién referenciar, es decir, no está haciendo referencia a un valor válido.

#### Apuntador al Heap

Representa la dirección en la que un valor de un tipo dado se encuentra almacenado. Su declarador de tipo es `arrow to <tipo>`, donde `tipo` es un declarador de tipo escalar.

Todo apuntador al heap tiene los siguientes operadores:

- `aim a`, que reserva cantidad de memoria suficiente para almacenar un valor del tipo del apuntador en el heap.
- `throw a`, que derreferencia el valor almacenado en esa ubicación de la memoria.
- `recover a`, que elimina la memoria reservada para un apuntador ya reservado y le asigna el valor nulo.

El valor por defecto de un apuntador es el valor nulo, `abyss`.

#### Aliases de Tipo

Un alias de tipo permite referenciar un tipo determinado de manera más sencilla para el programador. Su declarador de tipo es `knight`. Solo pueden existir de manera global en un programa. La sintaxis de creación de un alias de tipo es la siguiente:

```firelink
knight <identificador> <tipo>
```

Donde `identificador` es un identificador válido para el alias de tipo, y `tipo` corresponde al tipo al que hará alias. Por ejemplo:

```firelink
knight onion 10-chest of type 5-chest of type 15-miracle

...

var i of type knight
```

Se puede declarar una **lista** de `n` aliases de tipo, utilizando la siguiente sintaxis:

```firelink
requiring help of
  <alias de tipo 1>,
  <alias de tipo 2>,
  ...
  <alias de tipo n>
help received
```

Es importante resaltar que, una vez que se declara un alias de tipo con un identificador dado, no puede utilizarse como un identificador de constantes, variables, funciones o procedimientos más adelante.

#### Paréntesis

Las expresiones (valores) pueden agruparse en paréntesis (`()`) por motivos de precedencia. Al momento de evaluar una expresión con paréntesis, se evaluará en primer lugar la expresión dentro de un paréntesis antes de evaluar las operaciones fuera de ellas; la expresión resultante dentro de los paréntesis será el valor que se reemplazará por completo en lugar de la expresión parentizada.

## Instrucciones y Control de Flujo

### Comentarios

Un programa puede contener comentarios en cualquier lugar. Los comentarios inician con `--` y abarcan el resto de la línea, hasta el próximo salto de línea. Sólo se admiten comentarios de una sola línea.

```firelink
-- Esto es un comentario.

hello ashen one

-- Esto simula un bloque de comentarios
-- de muchas líneas,
-- ya van tres.

farewell ashen one

```firelink

### Programa

Un programa tiene la siguiente sintaxis:

```firelink
hello ashen one

[<lista de alias de tipo>]

[<declaraciones de funciones y/o procedimientos>]

<bloque de instrucciones principal>

farewell ashen one
```

Es obligatorio declarar un bloque de instrucciones principal, análogo a un procedimiento `main`. El bloque de instrucciones principal corresponde a un bloque de instrucciones que no esté asociado a la declaración de alguna función o procedimiento. No debe existir más de un (1) bloque de instrucciones principal.

### Bloque de Declaraciones de Variables

Un bloque de declaraciones de variables incluye una lista de una o varias declaraciones de variable, opcionalmente con su inicialización (en el caso de `var`, ya que los `const` siempre deben ser inicializados).

La sintaxis de un bloque de declaraciones es la siguiente (con indentación opcional para facilitar la legibilidad):

```firelink
with
  <declaracion 1>,
  <declaracion 2>,
  ...
  <declaracion n>
in your inventory
```

Donde _declaracion 1_, _declaracion 2_, _declaracion n_ son declaraciones de tipo según la sintaxis indicada anteriormente.

Por ejemplo,

```firelink
with
  var dragon of type humanity,
  var fire of type hollow
in your inventory
```

### Bloque de Instrucciones

Un bloque de instrucciones incluye opcionalmente un bloque de declaraciones de variables, válidas dentro del alcance del bloque, junto a una o varias instrucciones, debidamente concatenadas con el operador de secuenciación de instrucciones. En cualquier momento se puede abrir un bloque de instrucciones, y puede haber bloques de instrucciones arbitrariamente anidados. Se permitirá redeclarar variables entre bloques.

La sintaxis de un bloque de instrucciones es la siguiente (con indentación opcional para facilitar la legibilidad):

```firelink
traveling somewhere
[<bloque de declaraciones>]
  <lista de instruciones>
you died
```

### Secuenciación

Para concatenar dos instrucciones `I1` e `I2`, de modo que `I1` se ejecute antes de `I2` en secuencia, se deben separar ambas con el operador `\`, quedando:

```firelink
I1 \
I2
```

No se permitirá colocar el operador de secuenciación al final de la última instrucción de un bloque de instrucciones.

### Asignación

Una asignación consiste en almacenar un valor en una variable de algún tipo determinado.

La sintaxis de asignación de una variable previamente declarada de identificador `a` es la siguiente:

```firelink
a <<= <valor>
```

### Declaración de Función

Es un bloque aislado de código con una secuencia no nula de instrucciones a ejecutar que adicionalmente tiene un tipo de retorno asociado. Toda función requiere de un identificador único y válido, un tipo de retorno y opcionalmente una serie de parámetros con su identificador local y su tipo.

La sintaxis para declarar una función es la siguiente:

```firelink
invocation <identificador>
[requesting
  <tipo de parametro 1> <identificador 1> of type <tipo 1>,
  <tipo de parametro 2> <identificador 2> of type <tipo 2>,
  ...
  <tipo de parametro n> <identificador n> of type <tipo n>
]
with skill of type <tipo de retorno>
  <bloque de instrucciones>
after this return to your world
```

Donde el tipo de parámetro es:

- `val` para parámetros por valor
- `ref` para parámetros por referencia

Dentro de la función, se puede utilizar la siguiente sintaxis para retornar una expresión que debe corresponder con el tipo de retorno de la función.

```firelink
go back with <expresion>
```

No es posible utilizar esta instrucción con una expresión fuera de una función.

Las funciones soportan correcursión.

El _alcance_ de la lista de instrucciones inmediatas a la declaración de la función es el mismo que el de la declaración de sus argumentos, por lo tanto no se puede ocultar un argumento mediante la declaración de otra variable. Además, a pesar de abrir alcances adicionales dentro de una función, el chequeador de tipos no permitirá ocultar el argumento por declarar otra variable.

Por ejemplo, el siguiente fragmento de código es inválido:

```firelink
invocation fun
requesting
  val x of type sign -- esta variable fue declarada en el scope 'a'
with skill of type humanity
  traveling somewhere
  with
    var x of type <5>-miracle -- aquí seguimos en el mismo scope 'a'
                                -- esto es un error :x:
  in your inventory
    go back with 42
  you died

after this return to your world
```

De igual forma, el siguiente fragmento sigue siendo inválido:

```firelink
invocation fun
requesting
  val x of type sign -- esta variable fue declarada en el scope 'a'
with skill of type humanity

  traveling somewhere

    while the lit covenant is active:
      traveling somewhere
      with
        var x of type humanity -- esta variable fue declarada en el scope 'b' (b != a)
                               -- a pesar de esto, sigue siendo inválido :x:
      in your inventory
        go back with 42
      you died
    covenant left

  you died

after this return to your world
```

#### Llamada a Funciones

Dentro de un bloque de instrucciones, se puede realizar una llamada a una función de la siguiente manera:

```firelink
summon <nombre de la función>
[granting
  <expresion 1>,
  <expresion 2>,
  ...
  <expresion n>
  to the knight
]
```

Donde cada _expresion i_ corresponde a un argumento a pasar a la función, que debe corresponder con el tipo declarado. Si la función no recibe parámetros, el bloque que inicia con _granting_ no es requerido.

La llamada a la función evalúa a una expresión que puede ser asignada a una variable o utilizada en alguna otra expresión.

### Declaración de Procedimientos

Es un bloque aislado de código con una secuencia no nula de instrucciones a ejecutar que no tiene un tipo de retorno asociado. Todo procedimiento requiere de un identificador único y válido, y opcionalmente una serie de parámetros con su identificador local y su tipo.

La sintaxis para declarar un procedimiento en la siguiente:

```firelink
spell <identificador>
[requesting
  <tipo de parametro 1> <identificador 1> of type <tipo 1>,
  <tipo de parametro 2> <identificador 2> of type <tipo 2>,
  ...
  <tipo de parametro n> <identificador n> of type <tipo n>
  to the estus flask
]
  <bloque de instrucciones>
ashen estus flask consumed
```

Donde el tipo de parámetro es:

- `val` para parámetros por valor
- `ref` para parámetros por referencia

Dentro del procedimiento, se puede utilizar la siguiente sintaxis para retornar y detener la ejecución del procedimiento inmediatamente.

```firelink
go back
```

No es posible utilizar la instrucción `go back` dentro de una función; sin embargo, se puede utilizar en el cuerpo principal del programa para detener la ejecución inmediatamente.

El manejo del scope de los procedimientos es análogo al de las funciones (previamente descrito). Los procedimientos de igual forma soportan correcursión.

#### Llamada a Procedimientos

Dentro de un bloque de instrucciones, se puede realizar una llamada a un procedimiento de la siguiente manera:

```firelink
cast <nombre de la función>
[offering
  <expresion 1>,
  <expresion 2>,
  ...
  <expresion n>
  to the estus flask
]
```

Donde cada _expresion i_ corresponde a un argumento a pasar a la función, que debe corresponder con el tipo declarado. Si la función no recibe parámetros, el bloque que inicia con _offering_ no es requerido.

La llamada a la función NO evalúa a ninguna expresión.

### Imprimir

Para imprimir una expresión a la salida estándar, se puede usar el siguiente procedimiento:

```firelink
with orange soapstone say <expresión>
```

Únicamente se pueden imprimir expresiones de los siguientes tipos:

- Big / Small humanity
- Bonfire
- Hollow
- Sign
- <n>-Miracle (para cualquier tamaño `n` válido)

### Leer

Para leer una expresión y almacenarla en una variable, se puede usar el siguiente procedimiento:

```firelink
transpose into <expresión>
```

La expresión debe evaluar a un LVALUE adecuado (id, elementos de contenedores o estructuras, o referencias a través de apuntadores) y debe corresponder con una variable ya declarada de los siguientes tipos:

- Big / Small humanity
- Bonfire
- Hollow
- Sign
- <n>-Miracle (para cualquier tamaño `n` válido, donde `n` será el tamaño máximo del buffer al leer, ignorando el exceso de haber)

### Selección

El lenguaje poseee estructuras condicionales, de tipo if/elseif/else. La sintaxis es la siguiente:

```firelink
trust your inventory
<condición 1>:
  <bloque de instrucciones 1>
[<condición 2>:
  <bloque de instrucciones 2>]
...
[<condición n>:
  <bloque de instrucciones n>]
[liar!:
  <bloque de instrucciones para el else>]
inventory closed
```

El token `liar!` es equivalente al token `else` de otros lenguajes de programación. Las condiciones deben ser expresiones que evaluen a un tipo de dato `bonfire` (3-booleano).

### Selección por casos

El lenguaje poseee estructuras condicionales, de tipo switch. La sintaxis es la siguiente:

```firelink
enter dungeon with <expresión>:
<expresión 1>:
  <bloque de instrucciones 1>
[<expresión 2>:
  <bloque de instrucciones 2>]
...
[<expresión n>:
  <bloque de instrucciones n>]
[empty dungeon:
  <bloque de instrucciones para el else>]
dungeon exited
```

El token `empty dungeon` es equivalente al token `default` de otros lenguajes de programación. No es requerido finalizar un bloque de instrucciones con un token especial (similar al `break` de otros lenguajes), ya que no se permite desbordar un caso hacia otro.

### Iteraciones acotadas

Las iteraciones acotadas permiten repetir una secuencia no vacía de instrucciones por una cantidad fija de iteraciones, conocida antes de la primera iteración.

La sintaxis para una iteración acotada es la siguiente:

```firelink
upgrading <identificador de la variable de iteración> with <paso> soul[s] until level <cota superior>
  <bloque de instrucciones>
max level reached
```

Donde _identificador de la variable de iteración_ corresponde al identificador de una variable de tipo _humanity_ previamente declarada e inicializada, y _paso_ es la cantidad que será adicionada automáticamente a la variable al finalizar una iteración.

Por ejemplo:

```firelink
upgrading i with 1 soul until level 20
  traveling somewhere
    with orange soapstone say i \
    with orange soapstone say |\n|
  you died
max level reached
```

Dentro del bloque de instrucciones, no se permitirá modificar directamente la variable de iteración. La cota superior es una cota no alcanzada; es decir, la iteración no se ejecutará para el valor de la cota superior.

Es importante resaltar que intentar declarar una variable nueva con el mismo nombre de una variable de iteración que esté viva se considerará un error semántico; es decir: no se puede oscurecer (_shadow_) una variable de iteración bajo ninguna circunstancia.

Adicionalmente, justo antes de la primera iteración se hace una copia dura tanto de la cota superior como del incremento, de modo que cualquier modificación a estas expresiones **no** será tomada en cuenta durante las iteraciones (permitiendo que sean _realmente_ iteraciones acotadas).

Al terminar la iteración, la variable de iteración correspondiente tendrá el valor resultante de la última comparación con la guardia al romper el ciclo (en el caso del ejemplo, `i <<= 20`.)

### Iteraciones por estructuras

Las iteraciones por estructuras permiten repetir una secuencia no vacía de instrucciones asignando en una variable un elemento distinto de una estructura iterable en cada iteración.

Las estructuras iterables son: listas y conjuntos. Sin embargo, durante el desarrollo del compilador, se dará prioridad a la iteración por listas, pudiendo no contar con la iteración por conjuntos.

La sintaxis para una iteración por estructura es la siguiente:

```firelink
repairing <identificador de la variable de iteración> with titanite from <estructura>
  <bloque de instrucciones>
weaponry repaired
```

Donde _identificador de la variable de iteración_ corresponde al identificador de una variable previamente declarada (ignorando cualquier valor previo), y _estructura_ corresponde a la estructura iterable de la que se tomarán los elementos.

Por ejemplo:

```firelink
repairing i with titanite from biglist
  traveling somewhere
    with orange soapstone say i \
    with orange soapstone say |\n|
  you died
weaponry repaired
```

Dentro del bloque de instrucciones, no se permitirá modificar directamente la variable de iteración.

Es importante resaltar que intentar declarar una variable nueva con el mismo nombre de una variable de iteración que esté viva se considerará un error semántico; es decir: no se puede oscurecer (_shadow_) una variable de iteración bajo ninguna circunstancia.

Al terminar la iteración, la variable de iteración correspondiente tendrá el último valor utilizado dentro del ciclo.

### Iteraciones condicionadas

Las iteraciones condicionadas permiten repetir una secuencia no vacía de instrucciones mientras se cumpla una condición de tipo `bonfire`, evaluada antes de la primera iteración y luego de cada una de las iteraciones realizadas, mientras el valor de la expresión `bonfire` corresponda a un valor de verdad.

La sintaxis para una iteración condicionada es la siguiente:

```firelink
while the <expresion de tipo bonfire> covenant is active:
  <bloque de instrucciones>
covenant left
```

Donde _expresion de tipo bonfire_ corresponde a la condición de iteración que será evaluada como se refirió anteriormente.

Por ejemplo:

```firelink
while the b covenant is active:
  traveling somewhere
    b <<= unlit
  you died
covenant left
```

## Peso Máximo

**Fire Link** restringe el tamaño máximo (o _peso máximo_) de las variables en memoria estática y de pila a tiempo de ejecución, a una cantidad de _bytes_ conocida al compilar. Al momento de compilar, el compilador especificará una opción para escoger una cantidad entera que corresponde a la cantidad máxima (en bytes) de uso de memoria estática y de pila (ambas sumadas) que se permitirá al programa. En caso de sobrepasar el peso máximo durante la ejecución, el programa abortará y se avisará al usuario que se ha sobrepasado el mismo y por cuánta diferencia. Por defecto, se establecerá un *peso máximo* de **1000000 (1e6) bytes**.

## Cantidad Máxima de Funciones y Procedimientos

**Fire Link** restringe la cantidad máxima de armas (funciones y procedimientos) que pueden definirse en el programa a tiempo de compilación, a una cantidad máxima de funciones y procedimientos combinados. Al momento de compilar, el compilador especificará una opción para escoger una cantidad entera que corresponde a la cantidad máxima (en unidades) de funciones y procedimientos (ambos sumados) que se permitirá al programa. En caso de sobrepasar la cantidad máxima en compilación, el programa no compilará y el compilador avisará al usuario que se ha sobrepasado el máximo y por cuanta diferencia. Por defecto, se establecerá una *cantidad máxima* de **8 funciones y procedimientos**.

## Cantidad Máxima de Llamadas a Funciones y Procedimientos

**Fire Link** restringe la cantidad máxima de llamadas a funciones y procedimientos que pueden realizarse a tiempo de ejecución, a una cantidad máxima. Al momento de compilar, el compilador especificará una opción para escoger una cantidad entera que corresponde a la cantidad máxima (en unidades) de llamadas a funciones y procedimientos (ambos sumados) que se permitirá al programa. En caso de sobrepasar la cantidad máxima en ejecución, el programa abortará y se avisará al usuario que se ha sobrepasado el mismo. Este límite no incluirá llamadas recursivas de una misma función. Por defecto, se establecerá una *cantidad máxima* de **40 llamadas funciones y procedimientos**.
