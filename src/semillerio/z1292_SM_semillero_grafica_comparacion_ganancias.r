# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ1292_ganancias_semillerio"
PARAM$exp_input <- "ZZ9410_semillerio"

PARAM$corte <- 11000 # cantidad de envios
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
tb_ganancias[, individual := 0]
tb_ganancias[, semillerio := 0]

# Tabla que contendrá los rankings de todos los clientes para todas las semillas
tb_ranking_semillerio <- data.table(numero_de_cliente = dataset_julio[, numero_de_cliente])
tb_prediccion_semillerio_acumulado <- data.table(numero_de_cliente = dataset_julio[, numero_de_cliente])

#set.seed(12341)
#archivos <- sample(archivos)

for (archivo in archivos) {

    ksemilla <- strtoi(sapply(strsplit(archivo, "_"), "[", 3))

    # cols: numero_de_cliente,foto_mes,prob,rank
    tb_prediccion <- fread(paste0(path_experimento_semillerio, "/", archivo))
    # repara bug en z1292, si se fixea ahi, esto no genera problemas
    #ties.method = c("average", "first", "last", "random", "max", "min"))
    tb_prediccion[, rank := frank(-prob, ties.method = "random")]

    # Generamos predicción del semillerio
    tb_ranking_semillerio[, paste0("rank_", ksemilla) := tb_prediccion$rank]

    tb_prediccion_semillerio_acumulado[, paste0("prediccion_ind_", ksemilla) := tb_prediccion$rank]

    #if (ncol(tb_prediccion_semillerio_acumulado) == 2) {
    #    # Esta es la predicción del semillerio para la semilla i-esima
    #    tb_prediccion_semillerio <- data.table(
    #        tb_ranking_semillerio[, list(numero_de_cliente)],
    #        # prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
    #        prediccion = tb_prediccion$rank
    #    )
    #} else {
    #    # Esta es la predicción del semillerio para la semilla i-esima
    #    tb_prediccion_semillerio <- data.table(
    #        tb_ranking_semillerio[, list(numero_de_cliente)],
    #        # prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
    #        (
    #            tb_prediccion_semillerio_acumulado[, ncol(tb_prediccion_semillerio_acumulado) - 1, with = FALSE] +
    #            tb_prediccion_semillerio_acumulado[, ncol(tb_prediccion_semillerio_acumulado), with = FALSE]
    #        ) / 2 # excluye el numero_de_cliente del cálculo de la media
    #    )
    #}
    #colnames(tb_prediccion_semillerio) <- c("numero_de_cliente", "prediccion")
    #tb_prediccion_semillerio_acumulado[, paste0("prediccion_acc_", ksemilla) := tb_prediccion$rank]

    # Generamos predicción individual
    setorder(tb_prediccion, -prob)
    tb_prediccion[, Predicted := 0]
    tb_prediccion[1:PARAM$corte, Predicted := 1L]

    # Esta es la predicción del semillerio para la semilla i-esima
    tb_prediccion_semillerio <- data.table(
        tb_ranking_semillerio[, list(numero_de_cliente)],
        prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
    )
    tb_prediccion_semillerio_acumulado[, paste0("prediccion_acc_", ksemilla) := tb_prediccion_semillerio$prediccion]
    setorder(tb_prediccion_semillerio, prediccion) # Esto es un ranking, entonces de menor a mayor
    tb_prediccion_semillerio[, Predicted := 0]
    tb_prediccion_semillerio[1:PARAM$corte, Predicted := 1L]

    tb_ganancias[semillas == ksemilla]$individual <- calcularGanancia(dataset_julio, tb_prediccion)
    tb_ganancias[semillas == ksemilla]$semillerio <- calcularGanancia(dataset_julio, tb_prediccion_semillerio)

    message("Para la semilla ", ksemilla, " se obtiene ganancia individual de ", calcularGanancia(dataset_julio, tb_prediccion))
}

fwrite(tb_prediccion_semillerio_acumulado, "predictor_acumulado.csv", sep = ",")

pdf("semillerio_vs_individuales.pdf")
secuencia <- seq(from = 1, to = length(tb_ganancias$semilla))
yminimo <- min(tb_ganancias$individual) - 0.005 * min(tb_ganancias$individual)
ymaximo <- max(tb_ganancias$individual) + 0.005 * max(tb_ganancias$individual)
plot(secuencia, tb_ganancias$semillerio,
    type = "l",
    col = "red",
    ylim = c(yminimo, ymaximo),
    xlab = "Semillas",
    ylab = "Ganancia total Julio 2021",
    main = "Experimento Semillerio - 11000 envios"
)
points(secuencia, tb_ganancias$individual, col = "blue")
abline(h=mean(tb_ganancias$individual), col = "green")
legend("bottomleft",
    inset = .05,
    c("Ensemble Semillerio", "Semillas sueltas", "Media semillas sueltas"),
    fill = c("red", "blue", "green"),
    horiz = FALSE
)
dev.off()
