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
    
    
