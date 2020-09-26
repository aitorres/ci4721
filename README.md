# Fire Link

[![Build Status](https://travis-ci.org/aitorres/firelink.svg?branch=master)](https://travis-ci.org/aitorres/firelink)

**Fire Link** es un lenguaje de programación basado en el _lore_ de la serie de videojuegos *Dark Souls*, diseñado para la cadena de electivas de área Lenguajes de Programación II y III (CI-4721, CI-4722), durante los trimestres Septiembre - Diciembre 2019 y Enero - Marzo 2020 en la Universidad Simón Bolívar.

## Autores

- Germán Robayo (14-10924)
- Andrés Ignacio Torres (14-11082)

## Especificación

Puedes encontrar la especificación del lenguaje en español [aquí](docs/spec.md).

## Uso

Tras compilar con `stack build`, se puede ejecutar el compilador de la siguiente manera:

```bash
stack run -- <archivo> [<flag>]
```

Donde:

- `archivo`: es la ruta a un archivo `.souls` con el código a compilar
- `flag`: es una opción de configuración válida, entre las que se encuentran:
  - `-s` o `--symtable`: imprime la tabla de símbolos, si el archivo contiene un programa válido
  - `-p` o `--program`: imprime el programa parseado, si el archivo contiene un programa válido
  - `-f` o `--frontend`: imprime ambas opciones superiores: la tabla de símbolos y el programa parseado, si el archivo contiene un programa válido
  - `-t` o `--tac`: imprime la representación intermedia en código de tres direcciones (TAC) del programa, si el archivo contiene un programa válido
  - `-b` o `--blocks`: imprime los bloques básicos del código de tres direcciones (TAC), si el archivo contiene un programa válido
  - `-g` o `--graph`: imprime el grafo de flujo asociado al programa, si el archivo contiene un programa válido
  - `-c` o `--target-code`: imprime el código final en MIPS asociado al programa, si el archivo contiene un programa válido

Si no se pasa ningún _flag_, el compilador imprime todo lo génerado: tabla de símbolos, programa, código intermedio, bloques básicos y grafo de flujo.

## Ejecución

En el repositorio se incluye un pequeño script en `bash` para ejecutar en vivo un programa válido escrito en `FireLink`. Para poder ejecutarlo, `spim` debe estar instalado en la máquina y poder ejecutarse desde el `$PATH`.

Para ejecutarlo, se puede utilizar:

```bash
./runfirelink <archivo.souls>
```

Si el archivo contiene un programa válido en FireLink, se ejecutará. En caso contrario, se reportará el error respectivo en la salida estandar.
