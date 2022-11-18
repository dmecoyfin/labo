# Esto es un fix interno del experimento, no es necesario que NADIE corra este script

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ9410_semillerio"
PARAM$exp_input <- "ZZ9410_semillerio"
# FIN Parametros del script

options(error = function() {
    traceback(20)
    options(error = NULL)
    stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"

# No hace falta crear carpeta porque el fix es sobre los mismos archivos que levanta
setwd(paste0(base_dir, "exp/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

path_experimento_semillerio <- paste0(base_dir, "exp/", PARAM$exp_input)
archivos <- list.files(path = path_experimento_semillerio, pattern = "_resultados.csv")

for (archivo in archivos) {

    # cols: numero_de_cliente,foto_mes,prob,rank
    tb_prediccion <- fread(paste0(path_experimento_semillerio, "/", archivo))

    # backup del archivo
    fwrite(tb_prediccion, paste0(path_experimento_semillerio, "/", archivo, ".backup"), sep = ",")

    # repara bug en z1292, si se fixea ahi, esto no genera problemas
    tb_prediccion[, rank := frank(-prob, ties.method = "random")]
    fwrite(tb_prediccion, paste0(path_experimento_semillerio, "/", archivo), sep = ",")

}
