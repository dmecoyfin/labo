#Comparando modelos predictivos
#se muestran tres conjuntos de hiperparametros buenos A,B y C  y se pide ordenarlos del mejor al peor
#se deben utilizar sus propias semillas

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

ksemillas  <- c( 954011, 657863, 124739, 761213, 857957) #reemplazar por las propias semillas

#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
          by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia  <- function( semilla, param_basicos )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(7,3), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !

  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   control= param_basicos )  #aqui van los parametros del arbol

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 


  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  0.025,
                                         ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ),
                                         0 ) )]

  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3

  return( ganancia_test_normalizada )
}
#------------------------------------------------------------------------------

ArbolesMontecarlo  <- function( semillas,  param_basicos )
{
  ganancias  <- mcmapply( ArbolEstimarGanancia, 
                          semillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                          MoreArgs= list( param_basicos),  #aqui paso el segundo parametro
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )  #se puede subir a 5 si posee Linux o Mac OS

  #el vector de las ganancias
  return(  unlist(ganancias) )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\jesia\\Desktop\\4_DMEyF\\")  #Establezco el Working Directory
#cargo los datos

#cargo los datos
dataset  <- fread("./datasets/competencia1_2022.csv")

#trabajo solo con los datos con clase, es decir 202101
dataset  <- dataset[ clase_ternaria!= "" ]


paramA  <- list( "cp"=         -1,  #complejidad minima
                 "minsplit"=  300,  #minima cantidad de registros en un nodo para hacer el split
                 "minbucket"= 150,  #minima cantidad de registros en una hoja
                 "maxdepth"=    6 ) #profundidad máxima del arbol

paramB  <- list( "cp"=          0,  #complejidad minima
                 "minsplit"=   15,  #minima cantidad de registros en un nodo para hacer el split
                 "minbucket"=   5,  #minima cantidad de registros en una hoja
                 "maxdepth"=   10 ) #profundidad máxima del arbol

paramC  <- list( "cp"=         -1,  #complejidad minima
                 "minsplit"=   50,  #minima cantidad de registros en un nodo para hacer el split
                 "minbucket"=  16,  #minima cantidad de registros en una hoja
                 "maxdepth"=    6 ) #profundidad máxima del arbol


#calculo el vector de 5 ganancias de cada uno de los param
ganA  <- ArbolesMontecarlo( ksemillas, paramA )
ganB  <- ArbolesMontecarlo( ksemillas, paramB )
ganC  <- ArbolesMontecarlo( ksemillas, paramC )

#imprimo la media de las ganancias
cat( mean(ganA), mean(ganB), mean(ganC), "\n" )


