# Read in EPC Data
onedrive <- "E:/OneDrive - University of Leeds/"
library(dplyr)
library(readr)
source("R/translate_welsh.R")
#dir.create("tmp2")
#unzip(file.path(onedrive,"CREDS Data/EPC Certificates/all-domestic-certificates.zip"), exdir = "tmp2")

files <- list.files("tmp2", recursive = TRUE, full.names = TRUE)
files_certs <- files[grepl("certificates.csv", files)]
files_reccs <- files[grepl("recommendations.csv", files)]

# Fucntions

yn2logical <- function(vec){
  vec2 <- sapply(vec, function(i){
    if(is.na(i)){
      return(NA)
    } else if (i == "Y"){
      return(TRUE)
    } else if(i == "N"){
      return(FALSE)
    } else{
      stop(paste0("Unknown value ",i))
    }
  })
}


# read in certs
certs <- list()


# cols

col_types = cols(.default = col_character(),
 LMK_KEY = col_double(),
 BUILDING_REFERENCE_NUMBER = col_double(),
 CURRENT_ENERGY_RATING	= col_factor(c("A","B","C","D","E","F","G"), TRUE), 
 POTENTIAL_ENERGY_RATING = col_factor(c("A","B","C","D","E","F","G"), TRUE),
 CURRENT_ENERGY_EFFICIENCY = col_integer(),
 POTENTIAL_ENERGY_EFFICIENCY = col_integer(),
 PROPERTY_TYPE = col_factor(c("Flat","House","Maisonette","Bungalow","Park home"), FALSE),
 BUILT_FORM = col_factor(c("Detached","Semi-Detached","Mid-Terrace",
                           "Enclosed Mid-Terrace","NO DATA!",
                           "End-Terrace","Enclosed End-Terrace" ), FALSE),
 INSPECTION_DATE = col_date(),
 LOCAL_AUTHORITY = col_factor(),
 CONSTITUENCY = col_factor(),
 COUNTY = col_factor(),
 LODGEMENT_DATE = col_date(),
 ENVIRONMENT_IMPACT_CURRENT = col_integer(),
 ENVIRONMENT_IMPACT_POTENTIAL = col_integer(),
 ENERGY_CONSUMPTION_CURRENT = col_integer(),
 ENERGY_CONSUMPTION_POTENTIAL = col_double(),
 CO2_EMISSIONS_CURRENT = col_double(),
 CO2_EMISS_CURR_PER_FLOOR_AREA = col_double(),
 CO2_EMISSIONS_POTENTIAL = col_double(),
 LIGHTING_COST_CURRENT = col_double(),
 LIGHTING_COST_POTENTIAL = col_double(),
 HEATING_COST_CURRENT = col_double(),
 HEATING_COST_POTENTIAL = col_double(),
 HOT_WATER_COST_CURRENT = col_double(),
 HOT_WATER_COST_POTENTIAL = col_double(),
 TOTAL_FLOOR_AREA = col_double(),
 ENERGY_TARIFF = col_factor(c("standard tariff","24 hour","off-peak 7 hour","Single",
                              "dual","Unknown","dual (24 hour)","off-peak 10 hour",
                              "off-peak 18 hour","NO DATA!","INVALID!"), FALSE),
 MAINS_GAS_FLAG = col_factor(c("Y","N"), FALSE),
   FLAT_TOP_STOREY = col_factor(c("Y","N"), FALSE),
  FLAT_STOREY_COUNT	= col_integer(),
  MAIN_HEATING_CONTROLS = col_integer(),
  MULTI_GLAZE_PROPORTION = col_integer(),
  GLAZED_TYPE	= col_factor(c("NO DATA!",
                             "INVALID!",
                             "double glazing installed before 2002",
                             "double glazing installed during or after 2002",
                             "double glazing, unknown install date",
                             "not defined",
                             "single glazing",
                             "secondary glazing",
                             "triple glazing",
                             "double, known data",
                             "triple, known data"), FALSE),
 
  GLAZED_AREA	= col_factor(c("Much Less Than Typical","Less Than Typical","Normal",
                             "More Than Typical","Much More Than Typical","NO DATA!"), TRUE),
  EXTENSION_COUNT = col_integer(),
  NUMBER_HABITABLE_ROOMS = col_integer(),
  NUMBER_HEATED_ROOMS = col_integer(),
  LOW_ENERGY_LIGHTING = col_integer(),
  NUMBER_OPEN_FIREPLACES = col_integer(),
  HOT_WATER_ENERGY_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  HOT_WATER_ENV_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  FLOOR_ENERGY_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","NO DATA!","N/A"), TRUE),
  FLOOR_ENV_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","NO DATA!","N/A"), TRUE),
  
  WINDOWS_ENERGY_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  WINDOWS_ENV_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  WALLS_ENERGY_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  WALLS_ENV_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  ROOF_ENERGY_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  ROOF_ENV_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  MAINHEAT_ENERGY_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  MAINHEAT_ENV_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  MAINHEATC_ENERGY_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  MAINHEATC_ENV_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  LIGHTING_ENERGY_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  LIGHTING_ENV_EFF = col_factor(c("Very Good","Good","Average","Poor","Very Poor","N/A"), TRUE),
  WIND_TURBINE_COUNT = col_integer(),
  HEAT_LOSS_CORRIDOOR = col_factor(c("NO DATA!","unheated corridor","heated corridor","no corridor"), FALSE),
  UNHEATED_CORRIDOR_LENGTH = col_double(),
  FLOOR_HEIGHT = col_double(),
  PHOTO_SUPPLY = col_integer(),
  SOLAR_WATER_HEATING_FLAG = col_factor(c("N","Y"), FALSE),
  MECHANICAL_VENTILATION = col_factor(c("NO DATA!","natural","mechanical, extract only","mechanical, supply and extract"), FALSE)
)

for(i in 8:10){
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
  
  # If all NAs then make logical to reduce memory use
  if(all(is.na(sub2$SHEATING_ENERGY_EFF))){
    sub2$SHEATING_ENERGY_EFF <- NA
  }
  
  if(all(is.na(sub2$SHEATING_ENV_EFF))){
    sub2$SHEATING_ENV_EFF <- NA
  }
  
  #long strings
  
  # look for welsh in |
  splitwelsh <- function(x){
    if(grepl("|",x, fixed = TRUE)){
      y <- strsplit(x,"|", fixed = TRUE)
      y <- y[[1]]
      if(length(y) == 2){
        return(y[1])
      } else if(length(y) %% 2 == 0){
        return(paste(y[seq(1,length(y) - 1, 2)], collapse = ""))
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  }
  
  sub2$FLOOR_DESCRIPTION <- sapply(sub2$FLOOR_DESCRIPTION,splitwelsh)
  sub2$FLOOR_DESCRIPTION <- sapply(sub2$FLOOR_DESCRIPTION,translatewelsh)
  
  sub2$FLOOR_DESCRIPTION <- gsub("Average thermal transmittance ","",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("= ","",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("-","",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("W/m?K","W/m2K",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("W/m²K","W/m2K",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("W/mA?K","W/m2K",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("W/mÂ²K","W/m2K",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("W/m&#0178;K","W/m2K",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("W/m??K","W/m2K",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  
  sub2$FLOOR_DESCRIPTION <- gsub("0.1 W/m2K","0.10 W/m2K",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("0.2 W/m2K","0.20 W/m2K",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("0.3 W/m2K","0.30 W/m2K",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  
  sub2$FLOOR_DESCRIPTION <- gsub("uninsulated","no insulation",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION <- gsub("  "," ",sub2$FLOOR_DESCRIPTION, fixed = TRUE)
  sub2$FLOOR_DESCRIPTION[sub2$FLOOR_DESCRIPTION == "SAP05:Floor"] <- NA
  sub2$FLOOR_DESCRIPTION[sub2$FLOOR_DESCRIPTION == "0 W/m2K"] <- "0.00 W/m2K"
  sub2$FLOOR_DESCRIPTION[sub2$FLOOR_DESCRIPTION == "Other premises below"] <- "(other premises below)"
  #sub2$FLOOR_DESCRIPTION[sub2$FLOOR_DESCRIPTION == "Solet, dim inswleiddio (rhagdybiaeth)"] <- "Solid, no insulation (assumed)"
  sub2$FLOOR_DESCRIPTION[sub2$FLOOR_DESCRIPTION == ", no insulation (assumed)"] <- "no insulation (assumed)"
  #sub2$FLOOR_DESCRIPTION[sub2$FLOOR_DESCRIPTION == "Solet, wedi?i inswleiddio (rhagdybiaeth)"] <- "Solid, insulated (assumed)"
  sub2$FLOOR_DESCRIPTION[sub2$FLOOR_DESCRIPTION == ", insulated (assumed)"] <- "insulated (assumed)"
  #sub2$FLOOR_DESCRIPTION[sub2$FLOOR_DESCRIPTION == "Crog, dim inswleiddio (rhagdybiaeth)"] <- "Suspended, no insulation (assumed)"
  
  FLOOR_DESCRIPTION <- c("(another dwelling below)",
                         "(other premises below)",
                         "(Same dwelling below) insulated (assumed)",
                         
                         paste0(format(seq(0.00, 5.00, 0.01),digits = 2)," W/m2K"),
                         "Solid,",
                         "Solid, insulated",
                         "Solid, insulated (assumed)",
                         "Solid, limited insulation (assumed)",
                         "Solid, no insulation (assumed)",
                         "Suspended,",
                         "Suspended, insulated",
                         "Suspended, insulated (assumed)",
                         "Suspended, limited insulation (assumed)",
                         "Suspended, no insulation (assumed)",
                         "To external air, insulated",
                         "To external air, insulated (assumed)",
                         "To external air, no insulation (assumed)",
                         "To external air, limited insulation (assumed)",
                         "To unheated space, insulated",
                         "To unheated space, insulated (assumed)",
                         "To unheated space, no insulation (assumed)",
                         "To unheated space, limited insulation (assumed)",
                         "Conservatory",
                         "no insulation (assumed)",
                         "insulated (assumed)",
                         "limited insulation (assumed)",
                         NA)
  
  

  
    
  
  if(all(sub2$FLOOR_DESCRIPTION %in% FLOOR_DESCRIPTION)){
    sub2$FLOOR_DESCRIPTION <- factor(sub2$FLOOR_DESCRIPTION, levels = FLOOR_DESCRIPTION)
  } else{
    err <- unique(sub2$FLOOR_DESCRIPTION)
    err <- err[!err %in% FLOOR_DESCRIPTION]
    stop(paste0("FLOOR_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
  }
  
  sub2$WALLS_DESCRIPTION <- sapply(sub2$WALLS_DESCRIPTION, splitwelsh)
  sub2$WALLS_DESCRIPTION <- sapply(sub2$WALLS_DESCRIPTION, translatewelsh)
  
  sub2$WALLS_DESCRIPTION <- gsub("Average thermal transmittance ","",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("= ","",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("W/m?K","W/m2K",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("W/m²K","W/m2K",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("W/mA?K","W/m2K",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("W/mÂ²K","W/m2K",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("W/m&#0178;K","W/m2K",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("W/m??K","W/m2K",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  
  sub2$WALLS_DESCRIPTION <- gsub("0.1 W/m2K","0.10 W/m2K",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("0.2 W/m2K","0.20 W/m2K",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("0.3 W/m2K","0.30 W/m2K",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("-","",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("  "," ",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION <- gsub("Granite or whin,","Granite or whinstone,",sub2$WALLS_DESCRIPTION, fixed = TRUE)
  sub2$WALLS_DESCRIPTION[sub2$WALLS_DESCRIPTION == "0 W/m2K"] <- "0.00 W/m2K"
  sub2$WALLS_DESCRIPTION[sub2$WALLS_DESCRIPTION == "1 W/m2K"] <- "1.00 W/m2K"
  #sub2$WALLS_DESCRIPTION[sub2$WALLS_DESCRIPTION == "Waliau ceudod, fel y'u hadeiladwyd, dim inswleiddio (rhagdybiaeth)"] <- "Cavity wall, as built, no insulation (assumed)"
  #sub2$WALLS_DESCRIPTION[sub2$WALLS_DESCRIPTION == "Waliau ceudod, ceudod wedi?i lenwi"] <- "Cavity wall, filled cavity"
  #sub2$WALLS_DESCRIPTION[sub2$WALLS_DESCRIPTION == "Waliau ceudod, fel y?u hadeiladwyd, dim inswleiddio (rhagdybiaeth)"] <- "Cavity wall, as built, no insulation (assumed)"
  #sub2$WALLS_DESCRIPTION[sub2$WALLS_DESCRIPTION == "Briciau solet, fel y?u hadeiladwyd, dim inswleiddio (rhagdybiaeth)"] <- "Solid brick, as built, no insulation (assumed)"
  
  
  
  WALLS_DESCRIPTION <- c(paste0(format(seq(0.00, 5.00, 0.01),digits = 2)," W/m2K"),
                         "Cavity wall, as built, insulated (assumed)",
                         "Cavity wall, as built, no insulation (assumed)",
                         "Cavity wall, as built, partial insulation (assumed)",
                         "Cavity wall, filled cavity",
                         "Cavity wall, with internal insulation",
                         "Cavity wall, with external insulation",
                         "Cavity wall, filled cavity and internal insulation",
                         "Cavity wall,",
                         "Cavity wall, filled cavity and external insulation",
                         "Cavity wall, no insulation (assumed)",
                         "Cavity wall, insulated (assumed)",
                         "Cavity wall, partial insulation (assumed)",
                         "Cob, with internal insulation",
                         "Cob, with external insulation",
                         "Cob, filled cavity",
                         "Granite or whinstone, as built, insulated (assumed)",
                         "Granite or whinstone, as built, no insulation (assumed)",
                         "Granite or whinstone, with internal insulation",
                         "Granite or whinstone, with external insulation",
                         "Sandstone or limestone, as built, partial insulation (assumed)",
                         "Sandstone or limestone, as built, no insulation (assumed)", 
                         "Sandstone or limestone, with external insulation",
                         "Sandstone or limestone, with internal insulation",
                         "Sandstone or limestone, as built, insulated (assumed)",
                         "Sandstone, as built, insulated (assumed)",
                         "Sandstone, as built, no insulation (assumed)",
                         "Sandstone, with internal insulation",
                         "Sandstone, with external insulation",
                         "Sandstone, as built, partial insulation (assumed)",
                         "Solid brick, as built, insulated (assumed)",
                         "Solid brick, as built, no insulation (assumed)",
                         "Solid brick, with external insulation",
                         "Solid brick, with internal insulation",
                         "Solid brick, as built, partial insulation (assumed)",
                         "System built, as built, insulated (assumed)",
                         "System built, as built, no insulation (assumed)",
                         "System built, with external insulation",
                         "System built, with internal insulation",
                         "System built, as built, partial insulation (assumed)",
                         "System built, filled cavity",
                         "Timber frame, filled cavity",
                         "Timber frame, as built, insulated (assumed)",
                         "Timber frame, as built, no insulation (assumed)",
                         "Timber frame, as built, partial insulation (assumed)",
                         "Timber frame, with additional insulation",
                         "Timber frame, with internal insulation",
                         "Timber frame, with external insulation",
                         "Park home wall, as built",
                         "Cob, as built",
                         "SAP05:Walls",
                         NA)
  
  
  if(all(sub2$WALLS_DESCRIPTION %in% WALLS_DESCRIPTION)){
    sub2$WALLS_DESCRIPTION <- factor(sub2$WALLS_DESCRIPTION, levels = WALLS_DESCRIPTION)
  } else{
    err <- unique(sub2$WALLS_DESCRIPTION)
    err <- err[!err %in% WALLS_DESCRIPTION]
    stop(paste0("WALLS_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
  }
  
  sub2$ROOF_DESCRIPTION <- sapply(sub2$ROOF_DESCRIPTION, splitwelsh)
  sub2$ROOF_DESCRIPTION <- sapply(sub2$ROOF_DESCRIPTION, translatewelsh)
  
  sub2$ROOF_DESCRIPTION <- gsub("Average thermal transmittance ","",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("= ","",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("W/m?K","W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("W/m²K","W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("W/mA?K","W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("W/mÂ²K","W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("W/m&#0178;K","W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("W/m??K","W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  
  sub2$ROOF_DESCRIPTION <- gsub("0.1 W/m2K","0.10 W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("0.2 W/m2K","0.20 W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("0.3 W/m2K","0.30 W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("0.4 W/m2K","0.40 W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("0.5 W/m2K","0.50 W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("0.6 W/m2K","0.60 W/m2K",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("  "," ",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("0mm loft insulation","0 mm loft insulation",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("2mm loft insulation","2 mm loft insulation",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("5mm loft insulation","5 mm loft insulation",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("0+mm loft insulation","0+ mm loft insulation",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("Pitched, 0 mm loft insulation","Pitched, no insulation",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("Pitched, loft insulation","Pitched, insulated",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  sub2$ROOF_DESCRIPTION <- gsub("insulation(assumed)","insulation (assumed)",sub2$ROOF_DESCRIPTION, fixed = TRUE)
  
  sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "Pitched, *** INVALID INPUT Code : 57 *** loft insulation"] <- "Pitched"
  sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "Pitched,"] <- "Pitched"
  sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "Flat,"] <- "Flat"
  sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "0 W/m2K"] <- "0.00 W/m2K"
  sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "1 W/m2K"] <- "1.00 W/m2K"
  sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "2.3 W/m2K"] <- "2.30 W/m2K"
  sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "Other premises above"] <- "(other premises above)"
  #sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "Ar oleddf, dim inswleiddio"] <- "Pitched, no insulation"
  #sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "Ar oleddf, 300+ mm mm o inswleiddio yn y llofft"] <- "Pitched, 300+ mm loft insulation"
  sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "Pitched, >=300 mm loft insulation"] <- "Pitched, 300+ mm loft insulation"
  #sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "0.13 W/m2K|Trawsyriannedd thermol cyfartalog 0.13 W/m2K"] <- "0.13 W/m2K"
  sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "NaN W/m2K"] <- NA
  
  #sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "Ar oleddf, 100 mm o inswleiddio yn y llofft"] <- "Pitched, 100 mm loft insulation"
  #sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "To gwellt, gydag inswleiddio ychwanegol"] <- "Thatched, with additional insulation"
  #sub2$ROOF_DESCRIPTION[sub2$ROOF_DESCRIPTION == "Ar oleddf, 50 mm o inswleiddio yn y llofft"] <- "Pitched, 50 mm loft insulation"
  
  
  ROOF_DESCRIPTION <- c("(another dwelling above)",
                        "(other premises above)",
                        paste0(format(seq(0.00, 5.00, 0.01),digits = 2)," W/m2K"),
                        "Flat, insulated","Flat, insulated (assumed)",
                        "Flat, limited insulation",
                        "Flat, limited insulation (assumed)",
                        "Flat, no insulation","Flat, no insulation (assumed)",
                        "Flat",
                        "Pitched",
                        "Pitched, mm loft insulation",
                        "Pitched, 1 mm loft insulation",
                        "Pitched, 12 mm loft insulation",
                        "Pitched, 25 mm loft insulation",
                        "Pitched, 50 mm loft insulation",
                        "Pitched, 75 mm loft insulation",
                        "Pitched, 100 mm loft insulation",
                        "Pitched, 150 mm loft insulation",
                        "Pitched, 200 mm loft insulation",
                        "Pitched, 250 mm loft insulation",
                        "Pitched, 270 mm loft insulation",
                        "Pitched, 300 mm loft insulation",
                        "Pitched, 300+ mm loft insulation",
                        "Pitched, 350 mm loft insulation",
                        "Pitched, 400 mm loft insulation",
                        "Pitched, 400+ mm loft insulation",
                        "Pitched, insulated",
                        "Pitched, insulated (assumed)",
                        "Pitched, insulated at rafters",
                        "Pitched, limited insulation",
                        "Pitched, limited insulation (assumed)",
                        "Pitched, no insulation",
                        "Pitched, no insulation (assumed)",
                        "Pitched, Unknown loft insulation",
                        "Roof room(s), ceiling insulated",
                        "Roof room(s), insulated",
                        "Roof room(s), insulated (assumed)",
                        "Roof room(s), limited insulation",
                        "Roof room(s), limited insulation (assumed)",
                        "Roof room(s), no insulation",
                        "Roof room(s), no insulation (assumed)",
                        "Roof room(s), thatched",
                        "Roof room(s), thatched with additional insulation",
                        "Thatched, with additional insulation",
                        "Thatched",
                        "SAP05:Roof",
                        NA)
  
  
  if(all(sub2$ROOF_DESCRIPTION %in% ROOF_DESCRIPTION)){
    sub2$ROOF_DESCRIPTION <- factor(sub2$ROOF_DESCRIPTION, levels = ROOF_DESCRIPTION)
  } else{
    err <- unique(sub2$ROOF_DESCRIPTION)
    err <- err[!err %in% ROOF_DESCRIPTION]
    stop(paste0("ROOF_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
    
  }
  
  sub2$MAIN_FUEL <- gsub(" - this is for backwards compatibility only and should not be used"," (unknown)",sub2$MAIN_FUEL, fixed = TRUE)
  sub2$MAIN_FUEL[sub2$MAIN_FUEL == "To be used only when there is no heating/hot-water system"] <- "no heating/hot-water system"
  
  MAIN_FUEL <- c("anthracite",
                 "biogas (community)",
                 "biomass (community)",
                 "biomass (unknown)",
                 "bulk wood pellets",
                 "dual fuel - mineral + wood",
                 "electricity (unknown)", 
                 "electricity (community)",
                 "electricity (not community)",
                 "Electricity: electricity, unspecified tariff",
                 "house coal (not community)",
                 "house coal (unknown)",
                 "LPG (unknown)",
                 "LPG (not community)",
                 "LPG (community)",
                 "LPG special condition",
                 "bottled LPG",
                 "mains gas (unknown)",
                 "mains gas (community)",
                 "mains gas (not community)",
                 "oil (unknown)",
                 "oil (community)",
                 "oil (not community)",
                 "smokeless coal",
                 "no heating/hot-water system",
                 "wood chips",
                 "wood logs",
                 "B30D (community)",
                 NA)
  
  if(all(sub2$MAIN_FUEL %in% MAIN_FUEL)){
    sub2$MAIN_FUEL <- factor(sub2$MAIN_FUEL, levels = MAIN_FUEL)
  } else{
    err <- unique(sub2$MAIN_FUEL)
    err <- err[!err %in% MAIN_FUEL]
    stop(paste0("MAIN_FUEL unknown values: ",paste(err, collapse = "  ")))
    
  }
  
  sub2$LIGHTING_DESCRIPTION <- sapply(sub2$LIGHTING_DESCRIPTION,splitwelsh)
  sub2$LIGHTING_DESCRIPTION <- gsub("Low energy lighting in ","",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  sub2$LIGHTING_DESCRIPTION <- gsub("% fixed outlets","",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  sub2$LIGHTING_DESCRIPTION <- gsub("% of fixed outlets","",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  sub2$LIGHTING_DESCRIPTION <- gsub("all fixed outlets","100",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  sub2$LIGHTING_DESCRIPTION <- gsub("No low energy lighting","0",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  sub2$LIGHTING_DESCRIPTION <- gsub("No Low energy lighting","0",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  sub2$LIGHTING_DESCRIPTION <- gsub("SAP05:Lighting","",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  sub2$LIGHTING_DESCRIPTION <- gsub("Goleuadau ynni-isel mewn ","",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  sub2$LIGHTING_DESCRIPTION <- gsub("% o'r mannau gosod","",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  sub2$LIGHTING_DESCRIPTION <- gsub("% o?r mannau gosod","",sub2$LIGHTING_DESCRIPTION, fixed = TRUE)
  
  sub2$LIGHTING_DESCRIPTION[sub2$LIGHTING_DESCRIPTION == "Dim goleuadau ynni-isel"] <- "0"
  sub2$LIGHTING_DESCRIPTION[sub2$LIGHTING_DESCRIPTION == "Low energy lighting 100% 100"] <- "100"
  sub2$LIGHTING_DESCRIPTION[sub2$LIGHTING_DESCRIPTION == "Goleuadau ynni-isel ym mhob un o'r mannau gosod"] <- "100"
  #sub2$LIGHTING_DESCRIPTION[sub2$LIGHTING_DESCRIPTION == "Goleuadau ynni-isel mewn 40% o'r mannau gosod"] <- "40"
  
  LIGHTING_DESCRIPTION <- as.integer(as.numeric(sub2$LIGHTING_DESCRIPTION))
  
  if(anyNA(LIGHTING_DESCRIPTION)){
    err <- sub2$LIGHTING_DESCRIPTION[is.na(LIGHTING_DESCRIPTION)]
    err <- unique(err)
    if(any(err != "")){
      stop(paste0("LIGHTING_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
    }
  } 
  
  sub2$LIGHTING_DESCRIPTION <- LIGHTING_DESCRIPTION
  
  sub2$MAINHEATCONT_DESCRIPTION <- sapply(sub2$MAINHEATCONT_DESCRIPTION, translatewelsh)
  sub2$MAINHEATCONT_DESCRIPTION <- gsub("least 2","least two",sub2$MAINHEATCONT_DESCRIPTION, fixed = TRUE)
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Flat rate charging, no stat control of room temperature"] <- "Flat rate charging, no thermostatic control of room temperature"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Flat rate charging, programmer, no room thermostat"] <- "Flat rate charging, programmer no room thermostat"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Charging system linked to the use of community heating, prog and TRVs"] <- "Charging system linked to use of community heating, programmer and TRVs"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Charging system linked to the use of community heating, programmer and TRVs"] <- "Charging system linked to use of community heating, programmer and TRVs"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "No time or thermostatic control of room temp"] <- "No time or thermostatic control of room temperature"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Not applicable (boiler provides DHW only)"] <- "Not relevant (supplies DHW only)"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Flat rate charging, no thermostatic control of room temperature"] <- "Flat rate charging, no thermostatic control"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Flat rate charging, programmer and room stat"] <- "Flat rate charging, no thermostatic control"
  #sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Rhaglennydd, dim thermostat ystafell"] <- "Programmer, no room thermostat"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Programmer, roomstat and TRVs"] <- "Programmer, room thermostat and TRVs"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Delayed start stat & program & TRV's"] <- "Delayed start thermostat, programmer and TRVs"
  #sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Rhaglennydd, thermostat ystafell a TRVs"] <- "Programmer, room thermostat and TRVs"
  
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Charging system linked to use of community heating, programmer?and TRVs"] <- "Charging system linked to use of community heating, programmer and TRVs"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Flat rate charging*, programmer and TRVs"] <- "Flat rate charging, programmer and TRVs"
  #sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Rhaglennydd, TRVs a falf osgoi"] <- "Programmer, TRVs and bypass"
  #sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "TRVs a falf osgoi"] <- "TRVs and bypass"
  
  #sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Thermostat ystafell yn unig"] <- "Room thermostat only"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Celect control"] <- "Celect-type controls"
  sub2$MAINHEATCONT_DESCRIPTION[sub2$MAINHEATCONT_DESCRIPTION == "Charging system linked to use of community heating, programmer and at least two room stats"] <- "Charging system linked to use of community heating, programmer and at least two room thermostats"
  
  MAINHEATCONT_DESCRIPTION = c("Appliance thermostats",
                               "Appliance thermostat and programmer",
                              "Automatic charge control",
                              "Charging system linked to use of community heating, programmer and at least two room thermostats",
                              "Charging system linked to use of community heating, programmer and room thermostat",
                              "Charging system linked to use of community heating, programmer and TRVs",
                              "Charging system linked to use of community heating, room thermostat only",
                              "Charging system linked to use of community heating, TRVs",
                              "Controls for high heat retention storage heaters",
                              "Charging system linked to use of communit heating, programmer and room thermostat",
                              "Delayed start thermostat, programmer and TRVs",
                              "Flat rate charging, programmer and at least two room thermostats",
                              "Flat rate charging, programmer and room thermostat",
                              "Flat rate charging, programmer no room thermostat",
                              "Flat rate charging, programmer and TRVs",
                              "Flat rate charging, room thermostat only",
                              "Flat rate charging, TRVs",
                              "Flat rate charging, no thermostatic control",
                              "Manual charge control",
                              "No thermostatic control of room temperature",
                              "No time or thermostatic control of room temperature",
                              "None",
                              "Not relevant (supplies DHW only)",
                              "Programmer and appliance thermostats",
                              "Programmer and at least two room thermostats",
                              "Programmer and at least two room thermostats including a delayed start thermostat",
                              "Programmer and room thermostat",
                              "Programmer and room thermostats",
                              "Programmer, no room thermostat",
                              "Programmer, room thermostat and TRVs",
                              "Programmer, TRVs and boiler energy manager",
                              "Programmer, TRVs and bypass",
                              "Programmer, TRVs and flow switch",
                              "Room thermostat only",
                              "Room thermostats only",
                              "Time and temperature zone control",
                              "Temperature zone control",
                              "TRVs and bypass",
                              "Unit charging, programmer and TRVs",
                              "SAP05:Main-Heating-Controls",
                              "Celect-type controls",
                              NA)
  
  
  if(all(sub2$MAINHEATCONT_DESCRIPTION %in% MAINHEATCONT_DESCRIPTION)){
    sub2$MAINHEATCONT_DESCRIPTION <- factor(sub2$MAINHEATCONT_DESCRIPTION, levels = MAINHEATCONT_DESCRIPTION)
  } else{
    err <- unique(sub2$MAINHEATCONT_DESCRIPTION)
    err <- err[!err %in% MAINHEATCONT_DESCRIPTION]
    stop(paste0("MAINHEATCONT_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
    
  }
  
  sub2$MAINHEAT_DESCRIPTION <- sapply(sub2$MAINHEAT_DESCRIPTION, splitwelsh)
  sub2$MAINHEAT_DESCRIPTION <- sapply(sub2$MAINHEAT_DESCRIPTION, translatewelsh)
  
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Portable electric heating assumed for most rooms"] <- "Portable electric heaters assumed for most rooms"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler & underfloor, LPG"] <- "Boiler and underfloor heating, LPG"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler & underfloor, mains gas"] <- "Boiler and underfloor heating, mains gas"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Warm air, Electricaire"] <- "Warm air, electric"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Community, community"] <- "Community scheme"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler, dual fuel (mineral and wood)"] <- "Boiler and radiators, dual fuel (mineral and wood)" # Guess these are the same
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler & underfloor, wood pellets"] <- "Boiler and radiators, wood pellets"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler and radiators, house coal"] <- "Boiler and radiators, coal"
  #sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Bwyler a rheiddiaduron, nwy prif gyflenwad"] <- "Boiler and radiators, mains gas"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler and radiators, dual fuel appliance (mineral and wood)"] <- "Boiler and radiators, dual fuel (mineral and wood)"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler & underfloor, wood logs"] <- "Boiler and underfloor heating, wood logs"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler & underfloor, oil"] <- "Boiler and underfloor heating, oil"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Room heaters, electricity"] <- "Room heaters, electric"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "No system present: electric heaters assumed, mains gas"] <- "No system present: electric heaters assumed"
  #sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler and radiators, |Bwyler a rheiddiaduron, |mains gas|nwy prif gyflenwad"] <- "Boiler and radiators, mains gas"                                                              
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler and radiators, lpg (bottled)"] <- "Boiler and radiators, bottled LPG"                                                                          
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Electric ceiling, electric"] <- "Electric ceiling heating"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Air source heat pump, , radiators, electric"] <- "Air source heat pump, radiators, electric"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler and radiators,"] <- "Boiler and radiators"
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Air source heat pump , electric"] <- "Air source heat pump, electric"
  
  sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Boiler, underfloor, oil"] <- "Boiler and underfloor heating, oil"
  #sub2$MAINHEAT_DESCRIPTION[sub2$MAINHEAT_DESCRIPTION == "Bwyler a rheiddiaduron, LPG"] <- "Boiler and radiators, LPG"
  
  
  MAINHEAT_DESCRIPTION = c(", electric",
                          "Air source heat pump, electric",
                          "Air source heat pump, radiators, electric",
                          "Air source heat pump, radiators, mains gas",
                          "Air source heat pump, Systems with radiators, electric",
                          "Air source heat pump, Underfloor heating, pipes in screed above insulation, electric",
                          "Air source heat pump, Underfloor heating, pipes in insulated timber floor, electric",
                          "Air source heat pump, Underfloor heating and radiators, pipes in screed above insulation, electric",
                          "Air source heat pump, underfloor, electric",
                          "Air source heat pump, warm air, electric",
                          "Air source heat pump, fan coil units, electric",
                          "Boiler and radiators",
                          "Boiler and radiators, anthracite",
                          "Boiler and radiators, coal",
                          "Boiler and radiators, dual fuel (mineral and wood)",
                          "Boiler and radiators, electric",
                          "Boiler and radiators, LPG",
                          "Boiler and radiators, bottled LPG",
                          "Boiler and radiators, mains gas",
                          "Boiler and radiators, bottled gas",
                          "Boiler and radiators, oil",
                          "Boiler and radiators, wood chips",
                          "Boiler and radiators, wood logs",
                          "Boiler and radiators, wood pellets",
                          "Boiler and radiators, smokeless fuel",
                          "Boiler and underfloor heating, electric",
                          "Boiler and underfloor heating, LPG",
                          "Boiler and underfloor heating, mains gas",
                          "Boiler and underfloor heating, bottled gas",
                          "Boiler and underfloor heating, oil",
                          "Boiler and underfloor heating, wood pellets",
                          "Boiler and underfloor heating, wood logs",
                          "Boiler and underfloor heating, dual fuel (mineral and wood)",
                          "Community scheme",
                          "Community scheme with CHP",
                          "Community scheme, mains gas",
                          "Community scheme, radiators, mains gas",
                          "Community scheme with CHP and mains gas",
                          "Community heat pump, electric",
                          "Community heat pump",
                          "Electric storage heaters",
                          "Electric storage heaters, radiators",
                          "Electric underfloor heating",
                          "Electric ceiling heating",
                          "Ground source heat pump, Underfloor heating, pipes in screed above insulation, electric",
                          "Ground source heat pump, Underfloor heating, pipes in insulated timber floor, electric",
                          "Ground source heat pump, underfloor, electric",
                          "Ground source heat pump, underfloor, mains gas",
                          "Ground source heat pump, warm air, electric",
                          "Ground source heat pump, radiators, electric",
                          "Ground source heat pump, fan coil units, electric",
                          "No system present: electric heaters assumed",
                          "Portable electric heaters",
                          "Portable electric heaters assumed for most rooms",
                          "Room heaters, anthracite",
                          "Room heaters, coal",
                          "Room heaters, oil",
                          "Room heaters, dual fuel (mineral and wood)",
                          "Room heaters, electric",
                          "Room heaters, radiators, electric",
                          "Room heaters, smokeless fuel",
                          "Room heaters, wood logs",
                          "Room heaters, mains gas",
                          "Room heaters, bottled LPG",
                          "Room heaters, LPG",
                          "Warm air, electric",
                          "Warm air, mains gas",
                          "Warm air, oil",
                          "Water source heat pump, radiators, electric",
                          "Water source heat pump, underfloor, electric",
                          "Micro-cogeneration, mains gas",
                          "SAP05:Main-Heating")
  
  
  
  
  
  if(all(sub2$MAINHEAT_DESCRIPTION %in% MAINHEAT_DESCRIPTION)){
    sub2$MAINHEAT_DESCRIPTION <- factor(sub2$MAINHEAT_DESCRIPTION, levels = MAINHEAT_DESCRIPTION)
  } else{
    err <- unique(sub2$MAINHEAT_DESCRIPTION)
    err <- err[!err %in% MAINHEAT_DESCRIPTION]
    stop(paste0("MAINHEAT_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
    
  }
  
  sub2$WINDOWS_DESCRIPTION <- sapply(sub2$WINDOWS_DESCRIPTION, splitwelsh)
  sub2$WINDOWS_DESCRIPTION <- sapply(sub2$WINDOWS_DESCRIPTION, translatewelsh)
  
  
  sub2$WINDOWS_DESCRIPTION[sub2$WINDOWS_DESCRIPTION == "Full secondary glazing"] <- "Fully secondary glazing"
  #sub2$WINDOWS_DESCRIPTION[sub2$WINDOWS_DESCRIPTION == "Gwydrau dwbl llawn"] <- "Fully double glazed"
  sub2$WINDOWS_DESCRIPTION[sub2$WINDOWS_DESCRIPTION == "Full triple glazing"] <- "Fully triple glazed"
  sub2$WINDOWS_DESCRIPTION[sub2$WINDOWS_DESCRIPTION == "Fully secondary glazed"] <- "Fully secondary glazing"
  #sub2$WINDOWS_DESCRIPTION[sub2$WINDOWS_DESCRIPTION == "Fully double glazed|Gwydrau dwbl llawn"] <- "Fully double glazed"
  sub2$WINDOWS_DESCRIPTION[sub2$WINDOWS_DESCRIPTION == "Single glazedSingle glazing"] <- "Single glazed"
  #sub2$WINDOWS_DESCRIPTION[sub2$WINDOWS_DESCRIPTION == "Gwydrau dwbl rhannol"] <- "Partial double glazing"
  
  WINDOWS_DESCRIPTION = c("High performance glazing",
                          
                          "Fully double glazed",
                          "Mostly double glazing",
                          "Partial double glazing",
                          "Some double glazing",
                          
                          "Single glazed",
                          "Single and multiple glazing",
                          
                          "Multiple glazing throughout",
                          "Partial multiple glazing",
                          "Some multiple glazing",
                          "Mostly multiple glazing",
                          
                          "Fully triple glazed",
                          "Partial triple glazing",
                          "Mostly triple glazing",
                          "Some triple glazing",
                          
                          "Some secondary glazing",
                          "Partial secondary glazing",
                          "Secondary glazing",
                          "Fully secondary glazing",
                          "Mostly secondary glazing",
                          
                          "SAP05:Windows",
                          "Single glazeddouble glazing",
                          "Single glazedsecondary glazing",
                          NA
                          )
  
  
  if(all(sub2$WINDOWS_DESCRIPTION %in% WINDOWS_DESCRIPTION)){
    sub2$WINDOWS_DESCRIPTION <- factor(sub2$WINDOWS_DESCRIPTION, levels = WINDOWS_DESCRIPTION)
  } else{
    err <- unique(sub2$WINDOWS_DESCRIPTION)
    err <- err[!err %in% WINDOWS_DESCRIPTION]
    stop(paste0("WINDOWS_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
    
  }
  
  # Welsh!
  sub2$HOTWATER_DESCRIPTION <- sapply(sub2$HOTWATER_DESCRIPTION, splitwelsh)
  sub2$HOTWATER_DESCRIPTION <- sapply(sub2$HOTWATER_DESCRIPTION, translatewelsh)
  
  #sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION == "O'r brif system, adfer gwres nwyon ffliw"] <- "From main system, flue gas heat recovery"
  sub2$HOTWATER_DESCRIPTION <- gsub("cylinderstat","cylinder thermostat",sub2$HOTWATER_DESCRIPTION, fixed = TRUE)
  sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION == "community scheme"] <- "Community scheme"
  sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION ==  "From main system, no cylinder thermostat, no cylinder thermostat"] <- "From main system, no cylinder thermostat"
  sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION ==  "From community scheme"] <- "Community scheme"
  sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION ==  "community scheme, no cylinder thermostat"] <- "Community scheme, no cylinder thermostat"
 # sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION ==  "O'r brif system"] <- "From main system"
  #sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION ==  "O?r brif system"] <- "From main system"
  sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION ==  "No system present?electric immersion assumed"] <- "No system present: electric immersion assumed"
  #sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION ==  "From main system |O'r brif system"] <- "From main system"
  sub2$HOTWATER_DESCRIPTION[sub2$HOTWATER_DESCRIPTION ==  "From main system "] <- "From main system"
   
  HOTWATER_DESCRIPTION = c("Community scheme",
                           "Community scheme with CHP",
                           "Community scheme, no cylinder thermostat",
                           "Community scheme, plus solar",
                           "Electric immersion, off-peak",
                           "Electric immersion, off-peak, plus solar",
                           "Electric immersion, off-peak, no cylinder thermostat",
                           "Electric immersion, dual tariff",
                           "Electric immersion, standard tariff",
                           "Electric immersion, standard tariff, no cylinder thermostat",
                           "Electric immersion, standard tariff, plus solar",
                           "Electric instantaneous at point of use",
                           "Electric instantaneous at point of use, no cylinder thermostat",
                           "Electric instantaneous at point of use, plus solar",
                           "Electric heat pump",
                           "Electric heat pump, plus solar, no cylinder thermostat",
                           "Electric heat pump for water heating only",
                           "Electric heat pump for water heating only, no cylinder thermostat",
                           "Electric heat pump for water heating only, plus solar",
                           "Electric multipoint",
                           "From main system",
                           "From main system, standard tariff",
                           "From main system, flue gas heat recovery",
                           "From main system, flue gas heat recovery, waste water heat recovery", 
                           "From main system, no cylinder thermostat",
                           "From main system, no cylinder thermostat, plus solar",
                           "From main system, plus solar",
                           "From main system, plus solar, flue gas heat recovery",
                           "From main system, plus solar, no cylinder thermostat",
                           "From main system, waste water heat recovery",
                           "From secondary system",
                           "From secondary system, no cylinder thermostat",
                           "From secondary system, plus solar, no cylinder thermostat",
                           "Single-point gas water heater, standard tariff",
                           "Point gas water heater, no cylinder thermostat",
                           "Gas boiler/circulator",
                           "Gas boiler/circulator, no cylinder thermostat",
                           "Gas boiler/circulator, plus solar",
                           "Gas multipoint",
                           "Gas multipoint, plus solar",
                           "Gas multipoint, no cylinder thermostat",
                           "Gas range cooker, no cylinder thermostat",  
                           "Gas range cooker",
                           "Gas instantaneous at point of use",
                           "No system present: electric immersion assumed",
                           "No system present: electric immersion assumed, plus solar",
                           "No system present: electric immersion assumed, no cylinder thermostat",
                           ", no cylinder thermostat",
                           "Oil boiler/circulator",
                           "Oil range cooker",
                           "Oil range cooker, no cylinder thermostat",
                           "Solid fuel range cooker",
                           "Solid fuel range cooker, no cylinder thermostat",
                           "Solid fuel range cooker, no cylinder thermostat, plus solar",
                           "Solid fuel boiler/circulator",
                           "Solid fuel boiler/circulator, no cylinder thermostat",
                           "SAP05:Hot-Water")
  
  if(all(sub2$HOTWATER_DESCRIPTION %in% HOTWATER_DESCRIPTION)){
    sub2$HOTWATER_DESCRIPTION <- factor(sub2$HOTWATER_DESCRIPTION, levels = HOTWATER_DESCRIPTION)
  } else{
    err <- unique(sub2$HOTWATER_DESCRIPTION)
    err <- err[!err %in% HOTWATER_DESCRIPTION]
    stop(paste0("HOTWATER_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
  }
  
  
  sub2$FLOOR_LEVEL[sub2$FLOOR_LEVEL == "ground floor"] <- "Ground"
  
  FLOOR_LEVEL = c("Basement","Ground","1st","mid floor",
                  "2nd","3rd",
                  paste0(c(4:20,24:30),"th"),
                  "21st","22nd","23rd",
                  "21st or above","top floor", NA)
  
  if(all(sub2$FLOOR_LEVEL %in% FLOOR_LEVEL)){
    sub2$FLOOR_LEVEL <- factor(sub2$FLOOR_LEVEL, levels = FLOOR_LEVEL)
  } else{
    err <- unique(sub2$FLOOR_LEVEL)
    err <- err[!err %in% FLOOR_LEVEL]
    stop(paste0("FLOOR_LEVEL unknown values: ",paste(err, collapse = "  ")))
  }
  
  sub2$SECONDHEAT_DESCRIPTION <- sapply(sub2$SECONDHEAT_DESCRIPTION,splitwelsh)
  sub2$SECONDHEAT_DESCRIPTION <- sapply(sub2$SECONDHEAT_DESCRIPTION,translatewelsh)
  
  sub2$SECONDHEAT_DESCRIPTION[sub2$SECONDHEAT_DESCRIPTION == "Room heaters, lpg"] <- "Room heaters, LPG"
  sub2$SECONDHEAT_DESCRIPTION[sub2$SECONDHEAT_DESCRIPTION == "Dim"] <- "None"
  sub2$SECONDHEAT_DESCRIPTION[sub2$SECONDHEAT_DESCRIPTION == "Portable electric heaters(assumed)"] <- "Portable electric heaters (assumed)"
  #sub2$SECONDHEAT_DESCRIPTION[sub2$SECONDHEAT_DESCRIPTION == "Gwresogyddion ystafell, nwy prif gyflenwad"] <- "Room heaters, mains gas"
  sub2$SECONDHEAT_DESCRIPTION[sub2$SECONDHEAT_DESCRIPTION == "Room heaters, (null)"] <- "Room heaters,"
  
  
  
  
  
  
  SECONDHEAT_DESCRIPTION = c("None",
                            "Portable electric heaters",
                            "Portable electric heaters (assumed)",
                            "Gas/LPG boiler pre-1998, with fan-assisted flue, gas",
                            "Gas/LPG boiler pre-1998 with balanced or open-flue, gas",
                            "Electric Underfloor Heating (Standard tariff), electric",
                            "Room heaters,",
                            "Room heaters, B30K",
                            "Room heaters, bioethanol",
                            "Room heaters, oil",
                            "Room heaters, electric",
                            "Room heaters, coal",
                            "Room heaters, dual fuel (mineral and wood)", 
                            "Room heaters, wood pellets",
                            "Room heaters, wood logs",
                            "Room heaters, wood chips",
                            "Room heaters, smokeless fuel",
                            "Room heaters, LPG",
                            "Room heaters, bottled LPG",
                            "Room heaters, mains gas",
                            "Room heaters, bottled gas",
                            "Room heaters, anthracite",
                            "SAP05:Secondary-Heating",
                            NA)
  
  if(all(sub2$SECONDHEAT_DESCRIPTION %in% SECONDHEAT_DESCRIPTION)){
    sub2$SECONDHEAT_DESCRIPTION <- factor(sub2$SECONDHEAT_DESCRIPTION, levels = SECONDHEAT_DESCRIPTION)
  } else{
    err <- unique(sub2$SECONDHEAT_DESCRIPTION)
    err <- err[!err %in% SECONDHEAT_DESCRIPTION]
    stop(paste0("SECONDHEAT_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
  }
  
  sub2$TRANSACTION_TYPE[sub2$TRANSACTION_TYPE == "not recorded"] <- "unknown"
  sub2$TRANSACTION_TYPE[sub2$TRANSACTION_TYPE == "none of the above"] <- "unknown"
  sub2$TRANSACTION_TYPE <- gsub(" - this is for backwards compatibility only and should not be used","",sub2$TRANSACTION_TYPE, fixed = TRUE)
  
  TRANSACTION_TYPE = c("new dwelling",
                      "rental (social)",
                      "rental (private)",
                      "ECO assessment",
                      "marketed sale",
                      "non marketed sale",
                      "assessment for green deal",
                      "RHI application",
                      "rental",
                      "FiT application",
                      "following green deal",
                      "Stock Condition Survey",
                      "unknown")
  
  
  
  if(all(sub2$TRANSACTION_TYPE %in% TRANSACTION_TYPE)){
    sub2$TRANSACTION_TYPE <- factor(sub2$TRANSACTION_TYPE, levels = TRANSACTION_TYPE)
  } else{
    err <- unique(sub2$TRANSACTION_TYPE)
    err <- err[!err %in% TRANSACTION_TYPE]
    stop(paste0("TRANSACTION_TYPE unknown values: ",paste(err, collapse = "  ")))
  }
  
  warnings()
  certs[[i]] <- sub2
}

certs <- bind_rows(certs)


