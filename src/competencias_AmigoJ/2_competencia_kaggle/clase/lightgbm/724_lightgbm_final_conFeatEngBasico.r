# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "KA7240_v2_featEngBasico"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.0400328452#0.0280015981   #0.0142501265
PARAM$finalmodel$num_iterations    <-    95#328  #615
PARAM$finalmodel$num_leaves        <-   535#1015  #784
PARAM$finalmodel$min_data_in_leaf  <-   584#5542  #5628
PARAM$finalmodel$feature_fraction  <-     0.3450513931# 0.7832319551  #0.8382482539
PARAM$finalmodel$semilla           <- 954011

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd("C:/Users/jesia/Desktop/4_DMEyF/")   #setwd( "~/buckets/b1" )

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)
#Feature engineering
#dataset[ , ctrx_quarter_bool :=  ifelse( ctrx_quarter>14, 1, 0 ) ]
dataset[ , mcuenta_corriente := (mcuenta_corriente_adicional + mcuenta_corriente)]
dataset[ , cprestamos := (cprestamos_personales + cprestamos_prendarios + cprestamos_hipotecarios)]
dataset[ , mprestamos := (mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios)]
dataset[ , ccomisiones := (ccomisiones_mantenimiento + ccomisiones_otras)]
dataset[ , mcomisiones := (mcomisiones_mantenimiento + mcomisiones_otras)]
dataset[ , ctarjetas_transacciones := (ctarjeta_visa_transacciones + ctarjeta_master_transacciones)]

#dataset[,crtx_quarter:=NULL]
dataset[,mcuenta_corriente_adicional:=NULL]
dataset[,cprestamos_personales:=NULL]
dataset[,cprestamos_prendarios:=NULL]
dataset[,cprestamos_hipotecarios:=NULL]
dataset[,mprestamos_personales:=NULL]
dataset[,mprestamos_prendarios:=NULL]
dataset[,mprestamos_hipotecarios:=NULL]
dataset[,ccomisiones_mantenimiento:=NULL]
dataset[,ccomisiones_otras:=NULL]
dataset[,mcomisiones_mantenimiento:=NULL]
dataset[,mcomisiones_otras:=NULL]
dataset[,ctarjeta_visa_transacciones:=NULL]
dataset[,ctarjeta_master_transacciones:=NULL]

#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                      )
)

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 12000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  
  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )
