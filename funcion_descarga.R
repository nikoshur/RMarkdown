########### Creacion de una funcion propia para la descarga de archivos del directorio de CHELSA #############

#' Esta funcion ha reutilizado part del código de otra función, 'get_chelsa', que tenia ciertos
#' problemas en cuanto a nombre de directorios, ya que estos han cambiado desde que esta
#' funcion fue definida y otros errores, derivados del método de descarga así como de la definición
#' de las rutas, mediant la librería glue, que no funcionan correctamente.

#' @param period de tipo character, que define el periodo que queremos descargar, siendo las opciones posibles UNA en c("past", "present", "future").

#' @param layer tipo numérico, seleccionar las variables bioclimáticas. se pueden seleccionar hasta 19, siendo por defecto todas estas,
#' para descargar una en específico usar {layer = 1}

#' @param model_string de tipo character, define que tipo de modelo climático se necesita, para periodos PASADOS o FUTUROS, por lo que
#' solo esta disponible para este tipo de archivos

#' @param scenario_string de tipo character, define el escenario. Las opciones disponibles son:"rcp26", "rcp45", "rcp60", "rcp85", para escenatios FUTUROS,
#' y "pmip3" para escenarios pasados.
#'
#' @param future_years de tipo character, que define el periodo de escenario futuro, por lo que solo se podrá usar para descargar este tipo de archivos
#' las opciones disponibles son "2041-2060" y "2061-2080".

#' @param output_dir de tipo character, define el directorio para guardar los archivos, siendo por defecto el getwd()


#' @examples ejemplos de descarga de datos de distintos tipos:

#'          presente <- chelsa_download(period = "present",layer = 2)

#'          pasado <- chelsa_download(period = "past", layer = 2, model_string = "CCSM4")

#'          futuro <- chelsa_download(period = "future", layer = 2, future_years = "2041-2060", scenario_string = "rcp26", model_string = "BNU-ESM")


#' @import Es necesario tener y cargar la libreria "raster" 
#' install.packages("raster")
#' library(raster)



# definicion de la funcion y sus argumentos
chelsa_download <- function(period = "present", layer = 1:19, model_string,
                            scenario_string, future_years, output_dir) {
  
  # directorio base de CHELSA, que contiene todos los archivos, cambiando esta variable, si CHELSA cambia el nombre de sus directorios, 
  # como ha pasado anteriormente, el código posterior funcionará perfectamente, siempre y cuando los nombres o estructura de nombres no 
  # sean cambiabos
  directorio_base <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/"
  
  # se transforma la lista de numero de variables a una lista que vaya de 01 a 19 con tipo character 
  layerf <- sprintf("%02d", layer)
  stopifnot(layerf %in% sprintf("%02d", 1:19))
  
  
  ##### COMPROBACION DE VARIABLES #####
  
  # se comprueba que los datos introducidos en la funcion sean los correctos
  stopifnot(layer %in% 1:19, period %in% c("past", "present", "future"))
  
  # se establece el directorio por defecto
  if (missing(output_dir))
  {
    output_dir <- getwd()
  } else {
    dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
  }
  
  
  
  
  ##### PASADO #####
  
  # definición de las operaciones para descargar archivos de variables bioclimáticas pasadas
  if (period == "past") {
    
    # se comprueba que los datos introducidos sean los correctos
    stopifnot(model_string %in% c("CCSM4", "CNRM-CM5", "FGOALS-g2", "IPSL-CM5A-LR",
                                  "MIROC-ESM", "MPI-ESM-P", "MRI-CGCM3"))
    
    # se establece el escenario por defecto
    if (missing(scenario_string))
    {
      cat("Argument scenario_string missing. Assuming pmip3 scenario", "\n")
      scenario_string <- "pmip3"
    }
    
    # se establece la carpeta donde se guardarán los archivos
    path <- paste0(normalizePath(output_dir), "/past/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    # bucle que itera sobre los datos introducidos
    for (i in layerf) {
      
      for (model_s in model_string) {
        
        # se establece el nombre que tiene que tener segun lo introducido
        archive_name <- paste("CHELSA_PMIP_", model_s, "_BIO_", i, ".tif", sep = "")
        
        # se establece la url exacta que contiene el archivo
        layer_url <- paste(directorio_base, "pmip3/bioclim/", archive_name, sep = "")

        # se establece con que nombre se guardará
        file_path <- paste0(path, archive_name)
        
        # se comprueba que no exista ya el archivo
        if (!file.exists(file_path)) {
          
          # se descarga, siendo muy importante el modo (wb), de no ser asi los archivos descargados no podran ser
          # manejados
          download.file(layer_url, destfile = file_path, mode = "wb")
        }
      }
    }
    # se devuelve un stack de los raster dentro del directorio donde se han guardado las descargas
    return(stack(list.files(path, full.names = TRUE)))
  }
  
  
  
  
  
  ##### PRESENTE #####
  
  # ***las opraciones se estructuran de la misma manera que para el caso de las variables pasadas***
  
  # definición de las operaciones para descargar archivos de variables bioclimáticas en la actualidad
  if (period == "present"){
    
    # crear directorio contenedor
    path <- paste0(normalizePath(output_dir), "/present/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    # iterar sobre las variables a descargar
    for (m in layerf) {
      
      # nombre
      archive_name <- paste("CHELSA_bio10_", m, ".tif", sep = "")
      
      # dirección exacta
      layer_url <- paste(directorio_base, "bioclim/integer/", archive_name, sep = "")
      
      # nombre
      file_path <- paste0(path, archive_name)
      
      if (!file.exists(file_path))
      {
        # descarga
        download.file(layer_url, destfile = file_path, mode = "wb")
      }
    }
    return(stack(list.files(path, full.names = TRUE)))
  }
  
  
  
  
  
  ##### FUTURO #####
  
  # ***las opraciones se estructuran de la misma manera que para el caso de las variables pasadas***
  
  # definición de las operaciones para descargar archivos predictivos sobre las variables bioclimaticas
  if (period == "future") {
    
    # comprobacion de la correcion en los datos introducidos
    stopifnot(future_years %in% c("2041-2060", "2061-2080"), scenario_string %in% c("rcp26", "rcp45", "rcp60", "rcp85"),
              model_string %in% c("ACCESS1-0", "BNU-ESM", "CCSM4", "CESM1-BGC", "CESM1-CAM5", "CMCC-CMS", "CMCC-CM",
                                  "CNRM-CM5", "CSIRO-Mk3-6-0", "CanESM2", "FGOALS-g2", "FIO-ESM", "GFDL-CM3", "GFDL-ESM2G",
                                  "GFDL-ESM2M", "GISS-E2-H-CC", "GISS-E2-H", "GISS-E2-R-CC", "GISS-E2-R", "HadGEM2-AO", "HadGEM2-CC",
                                  "IPSL-CM5A-LR", "IPSL-CM5A-MR", "MIROC-ESM-CHEM", "MIROC-ESM", "MIROC5", "MPI-ESM-LR",
                                  "MPI-ESM-MR", "MRI-CGCM3", "MRI-ESM1", "NorESM1-M", "bcc-csm1-1", "inmcm4"))
    
    path <- paste0(normalizePath(output_dir), "/future/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    # iteracion sobre los datos introducidos, primero en los años futuros, luego sobre los escenarios, sobre los modelos y por 
    # ultimo sobre las variables
    
    for (future_y in future_years) # Loop over the future years
    {
      for (scenario_s in scenario_string) # Loop over RCP scenarios
      {
        for (model_s in model_string) # Loop over climate models
        {
          for (i in layer) # Loop over bioclim layers
          {
            
            archive_name <- paste("CHELSA_bio_mon_", model_s, "_", scenario_s, "_r1i1p1_g025.nc_", as.character(i),
                                  "_", future_y, "_V1.2.tif", sep = "")
            
            layer_url <- paste(directorio_base, "cmip5/", future_y, "/bio/", archive_name, sep = "")
            
            file_path <- paste0(path, archive_name)
            
            
            if (!file.exists(file_path))
            {
              download.file(layer_url, destfile = file_path, mode = "wb")
              
            }
          } 
        } 
      }
    } 
    return(stack(list.files(path, full.names = TRUE)))
  }
}




