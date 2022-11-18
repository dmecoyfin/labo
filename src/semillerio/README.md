# Experimentos Colectivos 2022 - Semillerio

Este directorio contiene todo el código necesario para ejecutar el experimento colectivo "Semillerio" de la tanda de Experimentos Colectivos 2022 de la materia DMEyF - Maestria DM - UBA

**Autores** (Agregarse debajo en la medida que aporten)

Tomas Delvechio
Manuel Medina

Experimento NN: Semillerios como forma de lidiar al azar de los experimentos

## Hipótesis Experimental del problema NN #

¿Puede ofrecernos un ensamble superador la endogamia de modelos predictivos?

## Sesgos cognitivos

Para ser honesto, creo que esto va a funcionar según la discusión en clase. Sin embargo, sigo siendo escéptico a si es la endogamia de modelos lo que ofrece el meta modelo superador, o es casualmente LGB el que tiene esta propiedad.

Realmente deseo que con una cantidad de semillas pequeña el modelo sea altamente competitivo para dejar esa cuestión atrás, o al menos, que sea un efecto mas de los modelos generados.

## Revisión de Bibliografía

Kuncheva, L. I., & Whitaker, C. J. (2003). Measures of diversity in classifier ensembles and their  relationship with the ensemble accuracy. *Machine learning*, *51*(2), 181-207.

> Although there are proven connections between diversity and accuracy in some special cases, **our results raise some doubts about the usefulness of diversity measures** in building classifier ensembles in real-life pattern recognition problems.

No se leyó el paper completo.

## Diseño experimental

* Tomar el setup del modelo del ganador de la C3 (salteando la OB de la cual se tomaran los valores de la mejor iteración)
* Tomar Julio de 2021 como Private, pero del dataset de la Competencia Final.
* A partir del script de C3 `z991_ZZ_lightgbm.r`, modificar `ksemilla` por un array de 50 semillas. Por cada iteración:
  * Entreno el modelo final
  * Predicción sobre 202107 (1)
  * Agregar las predicciones al semillerio, promediar y calcular predicción para semilla i-esima. (2)
  * Para un punto de corte de 11k envios, guardar:
    * Ganancia total para Semilla i en el mes 202107. (calculado en paso (1))
    * Ganancia del ensemble para Semilla i en mismo mes (calculado en paso (2))

**Dudas del diseño**

* ¿Es necesario reproducir el experimento ganador de la C3? ¿O se puede hacer un pequeño pipeline con una OB corta?
* ¿Es correcto usar 202107 para la prueba?
* ¿Es recomendable no explorar diferente envíos? 11k parece razonable.
* ¿Es correcto tener 2 predicciones? ¿La semilla "sola" y el Ensemble promediado? Es para reproducir el gráfico de la clase, con los puntos individuales y la curva del semillerio.

**Scripts a ejecutar**

Todo sobre Dataset de la C4, pero con PARAMS y training strategy de competencia 3.

- `src/competencia3/z906_reparar_dataset.r`
- `src/competencia3/z914_corregir_drifting.r`
- `src/competencia3/z925_FE_historia.r`
- `src/competencia3/z931_training_strategy_under.r`
- Reutilizar Archivo BO_log.txt del ganador de la C3 (Agustín Diez) (Subido al bucket y replicado en este repo)
  - `src/semillerio/HT9410_semillerio/BO_log.txt`

- `src/semillerio/z1291_SM_semillero_lgbm_under.r`

## Limitaciones



## Resultados



## Discusión



## Conclusiones



## Futuros Problemas



## Anexo

