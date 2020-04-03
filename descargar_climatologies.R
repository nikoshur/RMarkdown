#' Creacion de una funcion propia para la descarga de archivos climatológicos del directorio de CHELSA #####

#'  Esta función solo sirve para descargar datos climáticos del directorio de CHELSA, siendo éstos 4:
#'  precipitación, temperaturas medias, temperaturas máximas y temperaturas mínimas, para cada mes del año.

#' modelo general de circulacion!!
#' 
#' hacer una media con varias predicciones y una medida de error!!
#' 
#' el escenario coger el que se quiera, pero solo 1
  
# definicion de la funcion y sus argumentos

chelsa_climate <- function(period, variable, meses = 1:12, model_string,
                            scenario_string, future_years, output_dir) {
  
  directorio_base <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/"
  
  stopifnot(meses %in% 1:12, period %in% c("present", "future", "past"),
            variable %in% c("prec", "temp", "tmax", "tmin"))
  
  # se transforma la lista de numero de variables a una lista que vaya de 01 a 19 con tipo character 
  mesesf <- sprintf("%02d", meses)
  stopifnot(mesesf %in% sprintf("%02d", 1:12))
  
  
  # se establece el directorio por defecto
  if (missing(output_dir))
  {
    output_dir <- getwd()
  } else {
    dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
  }
  
  
  ########### PASADO #############
  
  if (period == "past") {
    
    path <- paste0(normalizePath(output_dir), "/mensuales_pasado_", variable, "/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    stopifnot(model_string %in% c("CCSM4", "CNRM-CM5", "FGOALS-g2", "IPSL-CM5A-LR",
                                  "MIROC-ESM", "MPI-ESM-P", "MRI-CGCM3"))
    
    # es necesario incluir las pequeñas diferencias en los nombre de cada variable,
    # para el caso, algunas van numeradas de 1 a 12 o otras de 01 a 12.
    if (variable == "temp") {
      meses_seleccion = mesesf
    } else { meses_seleccion = meses }
    
    
    # igual que en el caso anterior, es una inclusión de una excepción
    # que existe para la variable de temperatura media
    if (variable!= "temp") {
      bonus = "_1"
    } else { bonus = "" }
    
    
    # se reenombra la variable porque en este caso, le han puesto otro nombre a los
    # archivos
    if (variable == "temp") {
      variable = "tmean"
    }
    
    
    # bucle que itera sobre los datos introducidos
    for (i in meses_seleccion) {
      
      for (model_s in model_string) {
        
        # se establece el nombre que tiene que tener segun lo introducido
        archive_name <- paste("CHELSA_PMIP_", model_s,"_", variable, "_", i, bonus, ".tif", sep = "")
        
        # se establece la url exacta que contiene el archivo
        layer_url <- paste(directorio_base, "pmip3/", variable, "/", archive_name, sep = "")
        
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
  
  
  
  
  ####### PRESENTE ###########
  
  if (period == "present") {
    
    # crear directorio contenedor
    path <- paste0(normalizePath(output_dir), "/mensuales_presente_", variable, "/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    # igual que anteriormente, esto es el control de las expeciones que hay en 
    # determinados nombres
    
    if (variable != "prec") {
      bonus = "_1979-2013"
      bonus2 = "temp/integer/"
    } else {
      bonus = ""
      bonus2 = ""
      }
    
    
    # algunas variables cambian de nombre en los archivos y aquí, de nuevo,
    # se controlan estas excepciones
    
    if (variable == "prec"){
      variable2 = variable
    }
    if (variable == "temp"){
      variable2 = "temp10"
    }
    if (variable == "tmax"){
      variable2 = "tmax10"
    }
    if (variable == "tmin"){
      variable2 = "tmin10"
    }

  
    # iterar sobre los meses a descargar
    for (m in mesesf) {
      
      # nombre
      archive_name <- paste("CHELSA_", variable2, "_", m, bonus, "_V1.2_land.tif", sep = "")
      
      # dirección exacta
        
      layer_url <- paste(directorio_base, "climatologies/",bonus2, variable, "/", archive_name, sep = "")
      
      # nombre de destino
      file_path <- paste0(path, archive_name)
      
      if (!file.exists(file_path))
      {
        # descarga
        download.file(layer_url, destfile = file_path, mode = "wb")
      }
    }
    return(stack(list.files(path, full.names = TRUE)))
  }
  
  
  
  ######### FUTURO #########
  
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
    
    path <- paste0(normalizePath(output_dir), "/mensuales_futuro_", variable, "_", model_string, "/", sep = "")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    
    # control de excepciones, igual que anteriormente
    if (variable == "prec"){
      bonus=""
    } else { bonus = "_V1.2" }
    
    
    # control de excepciones, igual que anteriormente
    if (variable == "prec"){
      variable2 = "pr"
    }
    if (variable == "temp"){
      variable2 = "tas"
    }
    if (variable == "tmax"){
      variable2 = "tasmax"
    }
    if (variable == "tmin"){
      variable2 = "tasmin"
    }
    
    
    for (future_y in future_years) # iteracion sobre los años futuros introducidos
    {
      for (scenario_s in scenario_string) # iteracion sobre los escenarios RCP 
      {
        for (model_s in model_string) # iteracion sobre los modelos climaticos
        {
          for (i in meses) # iteracion sobre los meses de interes
          {

            archive_name <- paste("CHELSA_", variable2, "_mon_", model_s, "_", scenario_s, "_r1i1p1_g025.nc_", as.character(i),
                                  "_", future_y, bonus, ".tif", sep = "")
            
            layer_url <- paste(directorio_base, "cmip5/", future_y, "/", variable, "/", archive_name, sep = "")
            
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
