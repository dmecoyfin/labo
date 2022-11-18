# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ1292_ganancias_semillerio"
PARAM$exp_input <- "ZZ9410_semillerio"

#PARAM$corte <- 11000 # cantidad de envios
PARAM$cortes  <- seq( from=  7000,
                      to=    14000,
                      by=        250 )
# FIN Parametros del script

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"

# creo la carpeta donde va el experimento
dir.create(paste0(base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

# Cargar las semillas usadas para levantar las ganancias en el orden que fueron calculadas
#arch_future <- paste0(base_dir, "exp/", PARAM$exp_input, "/ksemillas.csv")
#ksemillas <- read.csv(arch_future, header = TRUE)$x

path_experimento_semillerio <- paste0(base_dir, "exp/", PARAM$exp_input)
archivos <- list.files(path = path_experimento_semillerio, pattern = "_resultados.csv")

# Esto es MUY dependiente del formato del nombre de los experimentos, se puede romper muy facil
ksemillas <- strtoi(sapply(strsplit(archivos, "_"), "[", 3))

# Levantar dataset C4
# leo el dataset a partir del cual voy a calcular las ganancias
arch_dataset <- paste0(base_dir, "datasets/competenciaFINAL_2022.csv.gz")
dataset <- fread(arch_dataset)

dataset_julio <- dataset[foto_mes == 202107]
rm(dataset)

dataset_julio[, clase_real := ifelse(clase_ternaria == "BAJA+2", 1, 0)]
# Nos quedamos con las 2 columnas que nos resultan relevantes
dataset_julio <- dataset_julio[, .(numero_de_cliente, clase_real)]

calcularGanancia <- function(real, predicho) {
  tb_comparacion <- merge(real, predicho)
  # Estoy seguro que tiene que existir una forma menos horrible de escribir la siguiente expresión
  return (tb_comparacion[, sum(ifelse(clase_real == 1 & Predicted == 1, 78000, ifelse(clase_real == 0 & Predicted == 1, -2000, 0)))])
}

tb_ganancias <- data.table(semillas = ksemillas)
#tb_ganancias[, individual := 0]
#tb_ganancias[, semillerio := 0]

# Tabla que contendrá los rankings de todos los clientes para todas las semillas
tb_ranking_semillerio <- data.table(numero_de_cliente = dataset_julio[, numero_de_cliente])

for (archivo in archivos) {
  
  ksemilla <- strtoi(sapply(strsplit(archivo, "_"), "[", 3))
  
  # cols: numero_de_cliente,foto_mes,prob,rank
  tb_prediccion <- fread(paste0(path_experimento_semillerio, '/', archivo))


  setorder(tb_prediccion, numero_de_cliente)
  setorder(tb_ranking_semillerio, numero_de_cliente)

  # repara bug en z1292, si se fixea ahi, esto no genera problemas
  tb_prediccion[, rank := frank(-prob, ties.method = "random")]
  
  # Generamos predicción del semillerio
  tb_ranking_semillerio[, paste0("rank_", ksemilla) := tb_prediccion$rank]
  
  # Generamos predicción individual
  setorder(tb_prediccion, -prob)
  
  
  # Esta es la predicción del semillerio para la semilla i-esima
  tb_prediccion_semillerio <- data.table(
    tb_ranking_semillerio[, list(numero_de_cliente)],
    prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
  )
  setorder(tb_prediccion_semillerio, prediccion) # Esto es un ranking, entonces de menor a mayor
  
  
  for (corte in PARAM$cortes)
  {
    nom_col_ind = paste0("individual_",sprintf("%d", corte))
    nom_col_sem = paste0("semillerio_",sprintf("%d", corte))
    tb_prediccion[, Predicted := 0]
    tb_prediccion[1:corte, Predicted := 1L]
    tb_prediccion_semillerio[, Predicted := 0]
    tb_prediccion_semillerio[1:corte, Predicted := 1L]
    tb_ganancias[semillas == ksemilla, nom_col_ind] = calcularGanancia(dataset_julio, tb_prediccion)
    tb_ganancias[semillas == ksemilla, nom_col_sem] = calcularGanancia(dataset_julio, tb_prediccion_semillerio)
  }
  
  
}

pdf("semillerio_vs_individuales_pc.pdf")
secuencia <- seq(from = 1, to = length(tb_ganancias$semilla))
for (corte in PARAM$cortes)
{
  nom_corte_ind = paste0("individual_",sprintf("%d", corte))
  nom_corte_sem = paste0("semillerio_",sprintf("%d", corte))
  yminimo <- min(tb_ganancias[, get(nom_corte_ind)]) - 0.005 * min(tb_ganancias[, get(nom_corte_ind)])
  ymaximo <- max(tb_ganancias[, get(nom_corte_ind)]) + 0.005 * max(tb_ganancias[, get(nom_corte_ind)])
  plot(secuencia, tb_ganancias[, get(nom_corte_sem)],
       type = "l",
       col = "red",
       ylim = c(yminimo, ymaximo),
       xlab = "Semillas",
       ylab = "Ganancia total Julio 2021",
       main = paste0("Experimento Semillerio - ",sprintf("%d", corte)," envio")
  )
  points(secuencia, tb_ganancias[, get(nom_corte_ind)], col = "blue")
  mean_gan_ind = as.integer(mean(tb_ganancias[, get(nom_corte_ind)]))
  abline(h=mean_gan_ind, col = "green")
  last_gan_sem = tb_ganancias[length(ksemillas), get(nom_corte_sem)]
  legend("bottomleft",
         inset = .05,
         c(paste0("Ensemble Semillerio: ", sprintf("%d", last_gan_sem)), 
           paste0("Media semillas sueltas: ", sprintf("%d", mean_gan_ind)),
           "Semillas sueltas"),
         fill = c("red", "green", "blue"),
         horiz = FALSE
  )

  message(" Pto Corte : ", corte, " -  Dif entre semilla e avg ind: ", max(tb_ganancias[, get(nom_corte_sem)]) - mean_gan_ind )
  message(" Pto Corte : ", corte, " -  Dif entre min semilla e avg ind: ", min(tb_ganancias[, get(nom_corte_sem)]) - mean_gan_ind )

}


dev.off()
