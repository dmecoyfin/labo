#Este script demora varias horas en correr
#se sugiere solo entender el codigo e interpretar los resultados que genera
#los resultados quedan en el repositorio GitHub  /exp/ST1005

rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")
require("parallel")

require("primes")      #para obtener numeros primos que seran semillas
require("ggplot2")     #para grafiucar las densidades

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
                          mc.cores= 16 )  #se puede subir a 5 si posee Linux o Mac OS

  #media de las ganancias
  return(  unlist(ganancias) )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory
#cargo los datos

#cargo los datos
dataset  <- fread("./datasetsOri/uba/competencia1_2022.csv")

#trabajo solo con los datos con clase, es decir 202101
dataset  <- dataset[ clase_ternaria!= "" ]



#defino los modelos que voy a ccmparar
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



#creo un vector con 100 numeros primos tomados al azar del intervalo [ 100k, 1M ]
kcantidad_semillas <- 100   #cantidad de semillas con las que voy a trabajar
set.seed( 17 )               #seteo inicial para replicabilidad

primos  <- generate_primes(min=100000, max=1000000)   #genero TODOS los numeros primos entre 100k y 1M
vsemillas  <- sample(primos)[ 1:kcantidad_semillas ]  #me quedo con kcantidad_semillas primos al azar


#calculo las ganancias
ganA  <- ArbolesMontecarlo( vsemillas, paramA )
ganB  <- ArbolesMontecarlo( vsemillas, paramB )
ganC  <- ArbolesMontecarlo( vsemillas, paramC )

#---------------------------------------
#calculos directos

#primero, probabilidad que la ganancia de un modelo sea mayor a la del otro
sum( ganA > ganB ) / length(ganA)
# 0.75   el 75% de las veces  el modelo A performa mejor que el modelo B

sum( ganA > ganC ) / length(ganA)
# 0.59   el 59% de las veces  el modelo A performa mejor que el modelo C
# es decir A y C performan igual

sum( ganC > ganB ) / length(ganA)
# 0.74   el 74% de las veces  el modelo C performa mejor que el modelo B


#luego  media diferencias
mean( ganA - ganB )
# 1500347   sin dudas  el modelo A es superior a B

mean( ganA - ganC )
# 363560   

mean( ganC - ganB )
# 1136787    sin dudas  el modelo C es superior a B


#-------------------------------
#finalmente  test  Wilcoxon
wilcox.test( ganA, ganB ) 
#        Wilcoxon rank sum test with continuity correction

# data:  ganA and ganB
# W = 173731, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# como p-value <  0.05    ganA es distinto a ganB

wilcox.test( ganA, ganC ) 
#        Wilcoxon rank sum test with continuity correction

#data:  ganA and ganC
#W = 137073, p-value = 0.008202
#alternative hypothesis: true location shift is not equal to 0


#cuantas corridas hacen falta para saber que  A es mejor que C ?
for(  hasta  in  5:100 )
{
  w  <- wilcox.test( ganA[1:hasta], ganC[1:hasta] )
  cat(  hasta, "\t",  w$p.value, "\n" )
}



#---------------------------------------
#grafico las desnsidades

#primero, creo la carpeta donde van los resultados
dir.create( "./exp/" ) 
dir.create( "./exp/ST2005" ) 

#primero creo un dataset con los resultados

#cada vector a un pequeño dataset
tblA  <-  as.data.table( list( "modelo"= "A", "gan"= ganA) )
tblB  <-  as.data.table( list( "modelo"= "B", "gan"= ganB) )
tblC  <-  as.data.table( list( "modelo"= "C", "gan"= ganC) )

#concateno los datasets
tbl  <- rbind( tblA, tblB, tblC )


fwrite( tbl,
        file= "./exp/ST2005/tresmodelos.txt",
        sep=  "\t" )

#grafico
pdf("./exp/ST2005/tres_modelos.pdf")

grafico  <- ggplot( tbl, aes(x=gan, fill=modelo)) + geom_density(alpha=0.25) 
print(grafico)

dev.off()

