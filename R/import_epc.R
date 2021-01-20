# Read in EPC Data
# Settings ---------------------------------------------------------------

# Setup ---------------------------------------------------------------

if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/")){
  onedrive <- "E:/Users/earmmor/OneDrive - University of Leeds"
} else if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/")){
  onedrive <- "E:/OneDrive - University of Leeds"
} else {
  onedrive <- "D:/OneDrive - University of Leeds"
}

library(dplyr)
library(readr)
source("R/translate_welsh.R", encoding="UTF-8")
source("R/funtions.R", encoding="UTF-8")

dir.create(file.path(tempdir(),"epc"))
unzip(file.path(onedrive,"Data/CREDS Data/EPC Certificates/all-domestic-certificates.zip"), 
      exdir = file.path(tempdir(),"epc"))

files <- list.files(file.path(tempdir(),"epc"), recursive = TRUE, full.names = TRUE)
files_certs <- files[grepl("certificates.csv", files)]
files_reccs <- files[grepl("recommendations.csv", files)]


# Import Loop -------------------------------------------------------------

certs <- list()

for(i in 1:349){
  message(files_certs[i])
  sub2 <- readr::read_csv(files_certs[i], col_types = col_types)
  
  # Dump unneeded columns
  sub2$CONSTITUENCY <- NULL
  sub2$CONSTITUENCY_LABEL <- NULL
  sub2$ADDRESS <- NULL
  
  # Clean up some of those names
  sub2$SOLAR_WATER_HEATING_FLAG <- yn2logical(sub2$SOLAR_WATER_HEATING_FLAG)
  sub2$MAINS_GAS_FLAG <- yn2logical(sub2$MAINS_GAS_FLAG)
  sub2$FLAT_TOP_STOREY <- yn2logical(sub2$FLAT_TOP_STOREY)
  
  #round numbers where decimals are errors
  sub2$ENERGY_CONSUMPTION_POTENTIAL <- as.integer(sub2$ENERGY_CONSUMPTION_POTENTIAL)
  sub2$LIGHTING_COST_CURRENT <- as.integer(sub2$LIGHTING_COST_CURRENT)
  sub2$LIGHTING_COST_POTENTIAL <- as.integer(sub2$LIGHTING_COST_POTENTIAL)
  sub2$HEATING_COST_CURRENT <- as.integer(sub2$HEATING_COST_CURRENT)
  sub2$HEATING_COST_POTENTIAL <- as.integer(sub2$HEATING_COST_POTENTIAL)
  sub2$HOT_WATER_COST_CURRENT <- as.integer(sub2$HOT_WATER_COST_CURRENT)
  sub2$HOT_WATER_COST_POTENTIAL <- as.integer(sub2$HOT_WATER_COST_POTENTIAL)
  
  sub2$MAIN_HEATING_CONTROLS[sub2$MAIN_HEATING_CONTROLS == "%%MAINHEATCONTROL%%"] <- NA
  sub2$MAIN_HEATING_CONTROLS <- as.integer(sub2$MAIN_HEATING_CONTROLS)
  
  # Clean missign data flats
  sub2$FLOOR_LEVEL[sub2$FLOOR_LEVEL == "NO DATA!"] <- NA
  sub2$FLOOR_LEVEL[sub2$FLOOR_LEVEL == "NODATA!"] <- NA
  sub2$ENERGY_TARIFF[sub2$ENERGY_TARIFF == "NO DATA!"] <- NA
  sub2$ENERGY_TARIFF[sub2$ENERGY_TARIFF == "INVALID!"] <- NA
  sub2$GLAZED_TYPE[sub2$GLAZED_TYPE == "NO DATA!"] <- NA
  sub2$GLAZED_TYPE[sub2$GLAZED_TYPE == "INVALID!"] <- NA
  sub2$GLAZED_AREA[sub2$GLAZED_AREA == "NO DATA!"] <- NA
  sub2$FLOOR_ENERGY_EFF[sub2$FLOOR_ENERGY_EFF == "NO DATA!"] <- NA
  sub2$FLOOR_ENERGY_EFF[sub2$FLOOR_ENERGY_EFF == "N/A"] <- NA
  sub2$FLOOR_ENV_EFF[sub2$FLOOR_ENV_EFF == "NO DATA!"] <- NA
  sub2$FLOOR_ENV_EFF[sub2$FLOOR_ENV_EFF == "N/A"] <- NA
  sub2$ROOF_ENERGY_EFF[sub2$ROOF_ENERGY_EFF == "N/A"] <- NA
  sub2$ROOF_ENV_EFF[sub2$ROOF_ENV_EFF == "N/A"] <- NA
  sub2$HEAT_LOSS_CORRIDOOR[sub2$HEAT_LOSS_CORRIDOOR == "NO DATA!"] <- NA
  sub2$MECHANICAL_VENTILATION[sub2$MECHANICAL_VENTILATION == "NO DATA!"] <- NA
  sub2$MAIN_FUEL[sub2$MAIN_FUEL == "NO DATA!"] <- NA
  sub2$MAIN_FUEL[sub2$MAIN_FUEL == "INVALID!"] <- NA
  sub2$HOT_WATER_ENERGY_EFF[sub2$HOT_WATER_ENERGY_EFF == "N/A"] <- NA
  sub2$HOT_WATER_ENV_EFF[sub2$HOT_WATER_ENV_EFF == "N/A"] <- NA
  sub2$MAINHEAT_ENERGY_EFF[sub2$MAINHEAT_ENERGY_EFF == "N/A"] <- NA
  sub2$MAINHEAT_ENV_EFF[sub2$MAINHEAT_ENV_EFF == "N/A"] <- NA
  sub2$MAINHEATC_ENERGY_EFF[sub2$MAINHEATC_ENERGY_EFF == "N/A"] <- NA
  sub2$MAINHEATC_ENV_EFF[sub2$MAINHEATC_ENV_EFF == "N/A"] <- NA
  sub2$LIGHTING_ENERGY_EFF[sub2$LIGHTING_ENERGY_EFF == "N/A"] <- NA
  sub2$LIGHTING_ENV_EFF[sub2$LIGHTING_ENV_EFF == "N/A"] <- NA
  sub2$WINDOWS_ENERGY_EFF[sub2$WINDOWS_ENERGY_EFF == "N/A"] <- NA
  sub2$WINDOWS_ENV_EFF[sub2$WINDOWS_ENV_EFF == "N/A"] <- NA
  sub2$WALLS_ENERGY_EFF[sub2$WALLS_ENERGY_EFF == "N/A"] <- NA
  sub2$WALLS_ENV_EFF[sub2$WALLS_ENV_EFF == "N/A"] <- NA

  sub2$SHEATING_ENERGY_EFF[sub2$SHEATING_ENERGY_EFF == "N/A"] <- NA
  sub2$SHEATING_ENV_EFF[sub2$SHEATING_ENV_EFF == "N/A"] <- NA
  
  sub2$CURRENT_ENERGY_RATING[sub2$CURRENT_ENERGY_RATING == "INVALID!"] <- NA
  sub2$POTENTIAL_ENERGY_RATING[sub2$POTENTIAL_ENERGY_RATING == "INVALID!"] <- NA
  
  # If all NAs then make logical to reduce memory use
  if(all(is.na(sub2$SHEATING_ENERGY_EFF))){
    sub2$SHEATING_ENERGY_EFF <- NA
  }
  
  if(all(is.na(sub2$SHEATING_ENV_EFF))){
    sub2$SHEATING_ENV_EFF <- NA
  }

  certs[[i]] <- sub2
}

certs <- bind_rows(certs)

saveRDS(certs, "epc_all_raw.Rds")

