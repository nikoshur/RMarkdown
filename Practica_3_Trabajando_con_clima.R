Andalucia <- peninsula %>%
  dplyr::select ("NAMEUNIT", "geometry") %>% 
  filter (NAMEUNIT == "Andalucía")

st_write(Andalucia, paste(directorio, "andalucia.shp", sep = "\\"))


## Práctica 3: trabajando con variables climáticas
## Fecha: 13/03/2020
## Autores: Adrián Vicioso Matrat y Nikolai Shurupov


#### Creacion de una funcion propia para la descarga de archivos del directorio de CHELSA ####
### Esta funcion esta construida en funcion de otra funcion, get_chelsa, que tenia ciertos
### problemas en cuanto a nombre de directorios, ya que estos han cambiado desde que esta
### funcion fue definida, así

#' @param period Character. Which time period to download for climate layers. One in c("past", "present", "future").

#' @param layer Numeric. Select which bioclim layer(s) is/are downloaded.
#' You can choose to download any of 19 bioclimatic layers. Default is all layers (1:19).
#' To download bioclim layer 1, use \code{layer = 1}, or use \code{layer = 1:19} to download all layers as a rasterstack. . See details \strong{ADD LATER!}

#'
#' @param model_string Character. Which climatic model to download for past or future period. Only available if \code{period} is one of c("past", "future").
#' See \code{climatedata::check_models()} for available options.
#'
#' @param scenario_string Character. Which climate scenario to download. Available options are c("rcp26", "rcp45", "rcp60", "rcp85", "pmip3"). RCP scenarios are only available if \code{period} = "future", and PMIP3 is available if \code{period} = "past".
#'
#' @param future_years Character. Which time period to download for future scenario.
#' Available options are c("2041-2060", "2061-2080") for years 2050 and 2070.
#' @param output_dir Character. Path indicating the directory in which the downloaded files will be stored. Default is the current working directory.
#' @return Raster* object or NULL. See \code{return_raster} argument.
#' @export
#'
#' @examples
#' @importFrom fs file_temp
#' @importFrom glue glue
#' @importFrom raster raster stack
#' @importFrom archive archive_extract



chelsa_download <- function(tipo = "present", layer = 1:19, period, model_string,
                            scenario_string, future_years, output_dir) {
  
  layerf <- sprintf("%02d", layer)
  # Check if input is correct
  stopifnot(layerf %in% sprintf("%02d", 1:19))
  
  
  stopifnot(layer %in% 1:19, period %in% c("past", "present", "future"))
  
  if (missing(output_dir))
  {
    output_dir <- getwd()
  } else {
    dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
  }
  
  directorio_base <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/"
  
  if (period == "past")
  {
    stopifnot(model_string %in% c("CCSM4", "CNRM-CM5", "FGOALS-g2", "IPSL-CM5A-LR",
                                  "MIROC-ESM", "MPI-ESM-P", "MRI-CGCM3"))
    
    if (missing(scenario_string))
    {
      cat("Argument scenario_string missing. Assuming pmip3 scenario", "\n")
      scenario_string <- "pmip3"
    }
    
    path <- paste0(normalizePath(output_dir), "/past/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    for (i in layerf) {
      
      for (model_s in model_string){
        
        archive_name <- glue::glue("CHELSA_PMIP_{model_s}_BIO_{i}.tif")
        layer_url <- glue::glue("hhttps://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/pmip3/bioclim/{out_layer}")
        file_path <- paste0(path, out_layer)
        
        if (!file.exists(file_path)) {
          
          download.file(layer_url, file_path)
        }
      }
    }
    return(stack(list.files(path, full.names = TRUE)))
  }
  
  if (tipo == "present"){
    
    path <- paste0(normalizePath(output_dir), "/present/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    for (m in layerf) {
        
        file <- glue:glue("CHELSA_bio10_{m}.tif")

        layer_url <- glue::glue("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/{out_layer}")
        
        file_path <- paste0(path, file)
        
        if (!file.exists(file_path))
        {
          download.file(layer_url, file_path)
        }
    }
    return(stack(list.files(path, full.names = TRUE)))
  }
  
  if (period == "future") {
    stopifnot(future_years %in% c("2041-2060", "2061-2080"), scenario_string %in% c("rcp26", "rcp45", "rcp60", "rcp85"),
              model_string %in% c("ACCESS1-0", "BNU-ESM", "CCSM4", "CESM1-BGC", "CESM1-CAM5", "CMCC-CMS", "CMCC-CM",
                                  "CNRM-CM5", "CSIRO-Mk3-6-0", "CanESM2", "FGOALS-g2", "FIO-ESM", "GFDL-CM3", "GFDL-ESM2G",
                                  "GFDL-ESM2M", "GISS-E2-H-CC", "GISS-E2-H", "GISS-E2-R-CC", "GISS-E2-R", "HadGEM2-AO", "HadGEM2-CC",
                                  "IPSL-CM5A-LR", "IPSL-CM5A-MR", "MIROC-ESM-CHEM", "MIROC-ESM", "MIROC5", "MPI-ESM-LR",
                                  "MPI-ESM-MR", "MRI-CGCM3", "MRI-ESM1", "NorESM1-M", "bcc-csm1-1", "inmcm4"))
    
    path <- paste0(normalizePath(output_dir), "/future/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    for (future_y in future_years) # Loop over the future years
    {
      for (scenario_s in scenario_string) # Loop over RCP scenarios
      {
        for (model_s in model_string) # Loop over climate models
        {
          for (i in layer) # Loop over bioclim layers
          {

            layer_name <- glue::glue("CHELSA_bio_mon_{model_s}_{scenario_s}_r1i1p1_g025.nc_{i}_{future_y}_V1.2.tif")

            layer_url <- glue::glue("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/cmip5/{future_y}/bio/{layer_name}")

            file_path <- paste0(path, layer_name)
            
            if (!file.exists(file_path))
            {
              download.file(layer_url, file_path)
            }
          } # Bioclim layer closing
        } # Model string closing
      } # Scenario string closing
    } # Future years closing
    return(stack(list.files(path, full.names = TRUE)))
  }
}
  


#funciones de paloma:
y <- "1979-2013"; 
for (m in 1:12)
{
  mm<-ifelse(nchar(m) < 2,
             paste("0", m, sep=""),m)
  #mean 
  file <- paste("CHELSA_temp10_", mm,
                "_", y, "_V1.2_land.tif", sep="")
  download.file(paste("wsl.ch/lud/chelsa/data/climatologies/temp/integer/temp", file,sep=""),
                destfile=paste("CheslaClimate/", file,sep=""), mode="wb")}



## Function to download chelsa data for future scenarios ####  
download.chelsa.future<-function(yr, scenario,model){
  for (m in 1:12){
    #mean 
    file <- paste("CHELSA_tas_mon_",model,"_",scenario,"_r1i1p1_g025.nc_", m, "_", yr, "_V1.2.tif", sep="")
    download.file(paste("wsl.ch/lud/chelsa/data/cmip5/2041-2060/temp", file,sep=""), 
                  destfile=paste("ChelsaClimate/", file,sep=""), mode="wb")}
}

##### creación de una función de descarga propia ####

## es una función inicial, que se encargará de descargar los datos de CHELSA, pues
## no existe ninguna otra que vaya bien


##################################copia de la funcion get_chelsa#############################################


#### Note to self (27.06.2018) -- remove raster package functions and replace fs function with base (to keep it lightweight)

#' Get CHELSA climate data
#'
#' Download data from CHELSA climatologies. This function retrieves files from the server via dowload request.
#'
#' @param type Character. Currently only "bioclim".
#' @param layer Numeric. Select which bioclim layer(s) is/are downloaded.
#' You can choose to download any of 19 bioclimatic layers. Default is all layers (1:19).
#' To download bioclim layer 1, use \code{layer = 1}, or use \code{layer = 1:19} to download all layers as a rasterstack. . See details \strong{ADD LATER!}
#' @param period Character. Which time period to download for climate layers. One in c("past", "current", "future").
#'
#' @param model_string Character. Which climatic model to download for past or future period. Only available if \code{period} is one of c("past", "future").
#' See \code{climatedata::check_models()} for available options.
#'
#' @param scenario_string Character. Which climate scenario to download. Available options are c("rcp26", "rcp45", "rcp60", "rcp85", "pmip3"). RCP scenarios are only available if \code{period} = "future", and PMIP3 is available if \code{period} = "past".
#'
#' @param future_years Character. Which time period to download for future scenario.
#' Available options are c("2041-2060", "2061-2080") for years 2050 and 2070.
#' @param output_dir Character. Path indicating the directory in which the downloaded files will be stored. Default is the current working directory.
#' @return Raster* object or NULL. See \code{return_raster} argument.
#' @export
#'
#' @examples
#' output_dir <- milkunize("Projects/Crete/data-raw/Chelsa")
#' chelsa_bioclim <- get_chelsa(layer = 1:19, output_dir = output_dir)
#' @importFrom fs file_temp
#' @importFrom glue glue
#' @importFrom raster raster stack
#' @importFrom archive archive_extract

get_chelsa <- function(type = "bioclim", layer = 1:19, period, model_string, scenario_string, future_years,
                       output_dir)
{
  # Argument checking - fail if one of 19 layers isn't requested
  stopifnot(layer %in% 1:19, type == "bioclim", period %in% c("past", "current", "future"))
  
  if (missing(output_dir))
  {
    output_dir <- getwd()
  } else {
    dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
  }
  
  if (period == "future")
  {
    stopifnot(future_years %in% c("2041-2060", "2061-2080"), scenario_string %in% c("rcp26", "rcp45", "rcp60", "rcp85"),
              model_string %in% c("ACCESS1-0", "BNU-ESM", "CCSM4", "CESM1-BGC", "CESM1-CAM5", "CMCC-CMS", "CMCC-CM",
                                  "CNRM-CM5", "CSIRO-Mk3-6-0", "CanESM2", "FGOALS-g2", "FIO-ESM", "GFDL-CM3", "GFDL-ESM2G",
                                  "GFDL-ESM2M", "GISS-E2-H-CC", "GISS-E2-H", "GISS-E2-R-CC", "GISS-E2-R", "HadGEM2-AO", "HadGEM2-CC",
                                  "IPSL-CM5A-LR", "IPSL-CM5A-MR", "MIROC-ESM-CHEM", "MIROC-ESM", "MIROC5", "MPI-ESM-LR",
                                  "MPI-ESM-MR", "MRI-CGCM3", "MRI-ESM1", "NorESM1-M", "bcc-csm1-1", "inmcm4"))
  }
  
  
  
  
  # Check if files already exist in the folder
  # if (load_old)
  # {
  #   if (length(list.files(output_dir, pattern = "CHELSA_bio10_*.*.tif", full.names = TRUE)) > 0)
  #   {
  #     raster_stack <- raster::stack(list.files(output_dir, pattern = "CHELSA_bio10_*.*.tif", full.names = TRUE))
  #     return(raster_stack)
  #   }
  # }
  
  layerf <- sprintf("%02d", layer)
  # Check if input is correct
  stopifnot(layerf %in% sprintf("%02d", 1:19))
  
  # Fork to download bioclim data for last glacial maximum (past data)
  if (period == "past")
  {
    stopifnot(model_string %in% c("CCSM4", "CNRM-CM5", "FGOALS-g2", "IPSL-CM5A-LR",
                                  "MIROC-ESM", "MPI-ESM-P", "MRI-CGCM3"))
    
    if (missing(scenario_string))
    {
      cat("Argument scenario_string missing. Assuming pmip3 scenario", "\n")
      scenario_string <- "pmip3"
    }
    
    path <- paste0(normalizePath(output_dir), "/past/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    
    for (i in layerf) # Loop over bioclim layers
    {
      for (model_s in model_string)
      {
        # layer_url <- paste0("https://www.wsl.ch/lud/chelsa/data/pmip3/bioclim/CHELSA_PMIP_CCSM4_bio_", i, ".7z")
        
        out_layer <- glue::glue("CHELSA_PMIP_{model_s}_BIO_{i}.tif")
        layer_url <- glue::glue("https://www.wsl.ch/lud/chelsa/data/pmip3/bioclim/{out_layer}")
        file_path <- paste0(path, out_layer)
        
        if (!file.exists(file_path))
        {
          download.file(layer_url, file_path)
        }
        
        # Extract archive
        # archive::archive_extract(temporary_file, dir = output_dir)
        
      }
      
      # Delete temporary files if tmp_keep argument
      # if (!tmp_keep)
      # {
      #   fs::file_delete(temporary_file)
      # }
    }
    return(stack(list.files(path, full.names = TRUE)))
  }
  # Loop over layers - download, unzip and remove zipped file (only bioclim for now)
  if (period == "current")
  {
    path <- paste0(normalizePath(output_dir), "/current/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    for (i in layerf)
    {
      # layer_url <- paste0("https://www.wsl.ch/lud/chelsa/data/bioclim/integer/CHELSA_bio10_", i, ".tif")
      out_layer <- glue::glue("CHELSA_bio10_{i}.tif")
      layer_url <- glue::glue("https://www.wsl.ch/lud/chelsa/data/bioclim/integer/{out_layer}")
      # temporary_file <- fs::file_temp(ext = ".tif", tmp_dir = temp_dir)
      file_path <- paste0(path, out_layer)
      
      if (!file.exists(file_path))
      {
        download.file(layer_url, file_path)
      }
      # download.file(layer_url, file_path)
      
      # Extract archive
      # archive::archive_extract(temporary_file, dir = output_dir)
      
    }
    return(stack(list.files(path, full.names = TRUE)))
    
  }
  
  # Download CHELSA climate data for future years
  if (period == "future")
  {
    
    path <- paste0(normalizePath(output_dir), "/future/")
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    
    for (future_y in future_years) # Loop over the future years
    {
      for (scenario_s in scenario_string) # Loop over RCP scenarios
      {
        for (model_s in model_string) # Loop over climate models
        {
          for (i in layer) # Loop over bioclim layers
          {
            # New version of CHELSA future data comes as tif file...
            
            # layer_name <- paste0("CHELSA_bio_mon_", model_s, "_", scenario_s, "_r1i1p1_g025.nc_",
            #                      i, "_", future_y, ".tif")
            # https://www.wsl.ch/lud/chelsa/data/cmip5/2061-2080/bio/CHELSA_bio_mon_ACCESS1-0_rcp45_r1i1p1_g025.nc_1_2061-2080_V1.2.tif
            layer_name <- glue::glue("CHELSA_bio_mon_{model_s}_{scenario_s}_r1i1p1_g025.nc_{i}_{future_y}_V1.2.tif")
            
            # layer_url <- paste0("https://www.wsl.ch/lud/chelsa/data/cmip5/", future_y, "/bio/", layer_name)
            layer_url <- glue::glue("https://www.wsl.ch/lud/chelsa/data/cmip5/{future_y}/bio/{layer_name}")
            # "CHELSA_bio_mon_", model_s, "_", scenario_s, "_r1i1p1_g025.nc_",
            # i, "_", future_y, ".7z")
            # print(layer_url)
            # temporary_file <- fs::file_temp(ext = ".7z", tmp_dir = temp_dir)
            
            # file_name_out <- paste0(normalizePath(output_dir), "/", layer_name)
            file_path <- paste0(path, layer_name)
            
            if (!file.exists(file_path))
            {
              download.file(layer_url, file_path)
            }
            
            # download.file(layer_url, file_path)
            
            # Extract archive
            # archive::archive_extract(temporary_file, dir = output_dir)
            
            # if (!tmp_keep)
            # {
            #   fs::file_delete(temporary_file)
            # }
          } # Bioclim layer closing
        } # Model string closing
      } # Scenario string closing
    } # Future years closing
    return(stack(list.files(path, full.names = TRUE)))
    
  } # Period closing
}

#####################################################################################################
#####################################################################################################

#decargar variables de chelsa
output_dir <- getwd()

variables <- get_chelsa(type = "bioclim", layer = 1:19, period = 'future',future_years = "2041-2060",
                        scenario_string = "rcp45",  output_dir)

x11()
plot(variables)
  
library(dismo)
library(XML)
library(maptools)
library(jsonlite)
library(dplyr)
library(rgdal)

glimpse(variables)

url <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/CHELSA_bio10_01.tif"

install.packages("remotes")
remotes::install_github("MirzaCengic/climatedata")
devtools::install_github("MirzaCengic/climatedata", force = TRUE)

library(remotes)
library(climatedata)
chelsa_bioclim <- get_chelsa(layer = 1:19, output_dir = "C:/Users/Niko/Desktop/Programacion_avanzada/Practicas/RMarkdown/RMarkdown_MTIG")

devtools::install_github("mirzacengic/climatedata")
devtools::install_github("jimhester/archive")

library(climatedata)

install.packages("digest")
