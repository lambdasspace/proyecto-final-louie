# proyecto-final-louie

El proyecto consiste en una implementación del algoritmo DPLL en Haskell. Dicho algoritmo es un método de satisfacibilidad booleana utilizado para encontrar soluciones a problemas de restricción, como el Sudoku.
## Formato de entrada 
El programa toma una cadena con el siguiente formato

```
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _
```
Donde cada '\_' puede ser reemplazado por una pista en el tablero de Sudoku inicial

## Uso 
Para ejecutar el programa es necesario compilarlo, lo cual se puede hacer con el siguiente comando

```
ghc --make dpll.hs
```
El programa se ejecuta en la línea de comandos y puede utilizarse de dos formas

Entrada estándar: El usuario puede ingresar directamente el Sudoku incompleto en la línea de comandos. El programa leerá el Sudoku de un archivo con el formato mencionado y mostrará la solución correspondiente.
    ```
    cat ./sudoku_tests/sudoku1.txt | ./dpll
    ```

Ruta de archivo: El usuario puede proporcionar la ruta de un archivo que contenga el Sudoku incompleto con el formato correcto. El programa leerá el archivo, procesará el Sudoku y mostrará la solución en la línea de comandos.
    ```
    ./dpll ./sudoku_tests/sudoku1.txt
    ```
El sudoku resuelto es mostrado junto con el Sudoku incompleto inicial mediante la salida estándard, por ejemplo

```
_ 9 _ _ _ _ _ _ 2
_ _ _ _ 9 _ 4 6 3
3 _ 6 _ _ 8 1 _ _
6 _ _ 9 _ _ 3 _ _
9 _ _ 8 _ 2 _ _ 1
_ _ 2 _ _ 7 _ _ 5
_ _ 3 5 _ _ 7 _ 4
5 1 7 _ 8 _ _ _ _
4 _ _ _ _ _ _ 1 _

Solucion
1 9 4 3 7 6 8 5 2
7 5 8 2 9 1 4 6 3
3 2 6 4 5 8 1 9 7
6 7 1 9 4 5 3 2 8
9 4 5 8 3 2 6 7 1
8 3 2 1 6 7 9 4 5
2 6 3 5 1 9 7 8 4
5 1 7 6 8 4 2 3 9
4 8 9 7 2 3 5 1 6
```
    
