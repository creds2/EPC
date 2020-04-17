# Read in EPC Data
onedrive <- "E:/OneDrive - University of Leeds/"
library(dplyr)
library(readr)
source("R/translate_welsh.R")
source("R/funtions.R")
#dir.create("tmp2")
#unzip(file.path(onedrive,"CREDS Data/EPC Certificates/all-domestic-certificates.zip"), exdir = "tmp2")

files <- list.files("tmp2", recursive = TRUE, full.names = TRUE)
files_certs <- files[grepl("certificates.csv", files)]
files_reccs <- files[grepl("recommendations.csv", files)]


# Functions ---------------------------------------------------------------

fix_wm2k <- function(x){
  
  x <- gsub("²","2", x)
  x <- gsub("W/m?K","W/m2K", x, fixed = TRUE)
  x <- gsub("W/mA?K","W/m2K", x, fixed = TRUE)
  x <- gsub("W/mÂ2K","W/m2K", x, fixed = TRUE)
  x <- gsub("W/m&#0178;K","W/m2K", x, fixed = TRUE)
  x <- gsub("W/m??K","W/m2K", x, fixed = TRUE)
  
  
  if(grepl("W/m2K", x)){
    y <- strsplit(x," ")[[1]]
    if(length(y) != 2){
      stop(paste0("Don't know how to process ",x))
    }
    if(nchar(y[1]) != 4){
      y[1] <- format(as.numeric(y[1]), digits = 2, nsmall = 2)
    }
    x <- paste0(y[1]," ",y[2])
  }
  
  return(x)
}

validate <- function(vals, nm){
  if(all(certs[[nm]] %in% vals)){
    certs[[nm]] <- factor(certs[[nm]], levels = vals)
    assign('certs',certs,envir=.GlobalEnv)
  } else {
    err <- unique(certs[[nm]])
    err <- err[!err %in% vals]
    err <- err[order(err)]
    print(err)
    stop(paste0("Unknown values in ",nm))
  }
}


yn2logical <- function(vec){
  vec2 <- pbapply::pbsapply(vec, function(i){
    if(is.na(i)){
      return(NA)
    } else if (i == "Y"){
      return(TRUE)
    } else if(i == "N"){
      return(FALSE)
    } else{
      stop(paste0("Unknown value ",i))
    }
  }, USE.NAMES = FALSE)
}


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


td_FLOOR_DESCRIPTION <- function(from,to){
  certs$FLOOR_DESCRIPTION[certs$FLOOR_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

sub_ROOF_DESCRIPTION <- function(from,to){
  certs$ROOF_DESCRIPTION <- gsub(from,to,certs$ROOF_DESCRIPTION, fixed = TRUE)
  assign('certs',certs,envir=.GlobalEnv)
}

td_ROOF_DESCRIPTION <- function(from,to){
  certs$ROOF_DESCRIPTION[certs$ROOF_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

td_MAINHEAT_DESCRIPTION <- function(from,to){
  certs$MAINHEAT_DESCRIPTION[certs$MAINHEAT_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

sub_MAINHEAT_DESCRIPTION <- function(from,to){
  certs$MAINHEAT_DESCRIPTION <- gsub(from,to,certs$MAINHEAT_DESCRIPTION, fixed = TRUE)
  assign('certs',certs,envir=.GlobalEnv)
}

sub_FLOOR_DESCRIPTION <- function(from,to){
  certs$FLOOR_DESCRIPTION <- gsub(from,to,certs$FLOOR_DESCRIPTION, fixed = TRUE)
  assign('certs',certs,envir=.GlobalEnv)
}

td_HOTWATER_DESCRIPTION <- function(from,to){
  certs$HOTWATER_DESCRIPTION[certs$HOTWATER_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

td_SECONDHEAT_DESCRIPTION <- function(from,to){
  certs$SECONDHEAT_DESCRIPTION[certs$SECONDHEAT_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

sub_WALLS_DESCRIPTION <- function(from,to){
  certs$WALLS_DESCRIPTION <- gsub(from,to,certs$WALLS_DESCRIPTION, fixed = TRUE)
  assign('certs',certs,envir=.GlobalEnv)
}

td_WALLS_DESCRIPTION <- function(from,to){
  certs$WALLS_DESCRIPTION[certs$WALLS_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

# Import Loop -------------------------------------------------------------

certs <- list()

for(i in 22:50){
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

cl <- parallel::makeCluster(5)
parallel::clusterExport(
  cl = cl,
  varlist = c("translatewelsh", "splitwelsh", "fix_wm2k"),
  envir = environment()
)


# long strings


# FLOOR DeSCRIPTION -------------------------------------------------------

certs$FLOOR_DESCRIPTION <- pbapply::pbsapply(certs$FLOOR_DESCRIPTION,splitwelsh, cl = cl, USE.NAMES = FALSE)
certs$FLOOR_DESCRIPTION <- pbapply::pbsapply(certs$FLOOR_DESCRIPTION,translatewelsh, cl = cl, USE.NAMES = FALSE)


sub_FLOOR_DESCRIPTION("Average thermal transmittance ","")
sub_FLOOR_DESCRIPTION("Trawsyriannedd thermol cyfartalog ","")
sub_FLOOR_DESCRIPTION("= ","")
sub_FLOOR_DESCRIPTION("-","")
sub_FLOOR_DESCRIPTION("  "," ")

certs$FLOOR_DESCRIPTION <- pbapply::pbsapply(certs$FLOOR_DESCRIPTION,fix_wm2k, cl = cl, USE.NAMES = FALSE)

sub_FLOOR_DESCRIPTION("uninsulated","no insulation")

td_FLOOR_DESCRIPTION("Other premises below","(other premises below)")
td_FLOOR_DESCRIPTION(", no insulation (assumed)","no insulation (assumed)")
td_FLOOR_DESCRIPTION(", insulated (assumed)","insulated (assumed)")
td_FLOOR_DESCRIPTION(", limited insulation (assumed)","limited insulation (assumed)")

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
                       "To unheated space,",
                       "To unheated space, insulated",
                       "To unheated space, insulated (assumed)",
                       "To unheated space, no insulation (assumed)",
                       "To unheated space, limited insulation (assumed)",
                       "Conservatory",
                       "no insulation (assumed)",
                       "insulated",
                       "insulated (assumed)",
                       "limited insulation (assumed)",
                       "SAP05:Floor",
                       NA)



validate(FLOOR_DESCRIPTION, "FLOOR_DESCRIPTION")


# WALLS_DESCRIPTION -------------------------------------------------------

certs$WALLS_DESCRIPTION <- pbapply::pbsapply(certs$WALLS_DESCRIPTION, splitwelsh, cl = cl, USE.NAMES = FALSE)
certs$WALLS_DESCRIPTION <- pbapply::pbsapply(certs$WALLS_DESCRIPTION, translatewelsh, cl = cl, USE.NAMES = FALSE)

sub_WALLS_DESCRIPTION("Average thermal transmittance ","")
sub_WALLS_DESCRIPTION("Trawsyriannedd thermol cyfartalog ","")
sub_WALLS_DESCRIPTION("-","")
sub_WALLS_DESCRIPTION("  "," ")
sub_WALLS_DESCRIPTION("= ","")

certs$WALLS_DESCRIPTION <- pbapply::pbsapply(certs$WALLS_DESCRIPTION,fix_wm2k, cl = cl, USE.NAMES = FALSE)

sub_WALLS_DESCRIPTION("Granite or whin,","Granite or whinstone,")

td_WALLS_DESCRIPTION("Cob, Internal","Cob, with internal insulation")


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
                       "Cob, as Built",
                       "Cob, with internal insulation",
                       "Cob, with external insulation",
                       "Cob, filled cavity",
                       "Granite or whinstone,",
                       "Granite or whinstone, as built, insulated (assumed)",
                       "Granite or whinstone, as built, no insulation (assumed)",
                       "Granite or whinstone, with internal insulation",
                       "Granite or whinstone, with external insulation",
                       "Granite or whinstone, as built, partial insulation (assumed)",
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
                       "Sandstone, filled cavity",
                       "Solid brick, filled cavity",
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
                       "System built, filled cavity and internal insulation",
                       "Timber frame, filled cavity",
                       "Timber frame, filled cavity and internal insulation",
                       "Timber frame, filled cavity and external insulation",
                       "Timber frame, as built, insulated (assumed)",
                       "Timber frame, as built, no insulation (assumed)",
                       "Timber frame, as built, partial insulation (assumed)",
                       "Timber frame, with additional insulation",
                       "Timber frame, with internal insulation",
                       "Timber frame, with external insulation",
                       "Park home wall, as built",
                       "Park home wall, with external insulation",
                       "Park home wall, with internal insulation",
                       "Cob, as built",
                       "SAP05:Walls",
                       NA)


validate(WALLS_DESCRIPTION, "WALLS_DESCRIPTION")


# ROOF_DESCRIPTION --------------------------------------------------------

certs$ROOF_DESCRIPTION <- pbapply::pbsapply(certs$ROOF_DESCRIPTION, splitwelsh, cl = cl, USE.NAMES = FALSE)
certs$ROOF_DESCRIPTION <- pbapply::pbsapply(certs$ROOF_DESCRIPTION, translatewelsh, cl = cl, USE.NAMES = FALSE)

sub_ROOF_DESCRIPTION("Average thermal transmittance ","")
sub_ROOF_DESCRIPTION("Trawsyriannedd thermol cyfartalog ","")
sub_ROOF_DESCRIPTION("= ","")
sub_ROOF_DESCRIPTION("  "," ")

certs$ROOF_DESCRIPTION <- pbapply::pbsapply(certs$ROOF_DESCRIPTION,fix_wm2k, cl = cl, USE.NAMES = FALSE)


sub_ROOF_DESCRIPTION("0mm loft insulation","0 mm loft insulation")
sub_ROOF_DESCRIPTION("2mm loft insulation","2 mm loft insulation")
sub_ROOF_DESCRIPTION("5mm loft insulation","5 mm loft insulation")
sub_ROOF_DESCRIPTION("0+mm loft insulation","0+ mm loft insulation")
sub_ROOF_DESCRIPTION("Pitched, 0 mm loft insulation","Pitched, no insulation")
sub_ROOF_DESCRIPTION("Pitched, loft insulation","Pitched, insulated")
sub_ROOF_DESCRIPTION("insulation(assumed)","insulation (assumed)")
sub_ROOF_DESCRIPTION(" mm mm "," mm ")

td_ROOF_DESCRIPTION("Pitched, *** INVALID INPUT Code : 57 *** loft insulation","Pitched")
td_ROOF_DESCRIPTION("Pitched,","Pitched")
td_ROOF_DESCRIPTION("Flat,","Flat")
td_ROOF_DESCRIPTION("Other premises above","(other premises above)")
td_ROOF_DESCRIPTION("Pitched, >=300 mm loft insulation","Pitched, 300+ mm loft insulation")
td_ROOF_DESCRIPTION("NaN W/m2K",NA)
td_ROOF_DESCRIPTION("(other dwelling above)","(another dwelling above)")
td_ROOF_DESCRIPTION("Roof room(s), thatched with additional insulation","Roof room(s), thatched, with additional insulation")

ROOF_DESCRIPTION <- c("(another dwelling above)",
                      "(other premises above)",
                      paste0(format(seq(0.00, 5.00, 0.01),digits = 2)," W/m2K"),
                      "18.00 W/m2K",
                      "Flat, insulated","Flat, insulated (assumed)",
                      "Flat, limited insulation",
                      "Flat, limited insulation (assumed)",
                      "Flat, no insulation","Flat, no insulation (assumed)",
                      "Flat",
                      "Pitched",
                      "Pitched, mm loft insulation",
                      "Pitched, 1 mm loft insulation",
                      "Pitched, 11 mm loft insulation",
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
                      "Pitched, Flat Roof Insulation loft insulation",
                      "Roof room(s)",
                      "Roof room(s), ceiling insulated",
                      "Roof room(s), insulated",
                      "Roof room(s), insulated (assumed)",
                      "Roof room(s), limited insulation",
                      "Roof room(s), limited insulation (assumed)",
                      "Roof room(s), no insulation",
                      "Roof room(s), no insulation (assumed)",
                      "Roof room(s), thatched",
                      
                      "Roof room(s), thatched, with additional insulation",
                      "Thatched, with additional insulation",
                      "Thatched",
                      "SAP05:Roof",
                      NA)

validate(ROOF_DESCRIPTION, "ROOF_DESCRIPTION")


# MAIN_FUEL ---------------------------------------------------------------

certs$MAIN_FUEL <- gsub(" - this is for backwards compatibility only and should not be used"," (unknown)",certs$MAIN_FUEL, fixed = TRUE)
certs$MAIN_FUEL[certs$MAIN_FUEL == "To be used only when there is no heating/hot-water system"] <- "no heating/hot-water system"
certs$MAIN_FUEL[certs$MAIN_FUEL == "coal (community)"] <- "house coal (community)"

MAIN_FUEL <- c("anthracite",
               "appliances able to use mineral oil or liquid biofuel",
               "biogas (community)",
               "biogas - landfill (unknown)",
               "biomass (community)",
               "biomass (unknown)",
               "rapeseed oil",
               "Community heating schemes: heat from boilers - biomass",
               "Community heating schemes: waste heat from power stations",
               "bulk wood pellets",
               "dual fuel - mineral + wood",
               "electricity (unknown)", 
               "electricity (community)",
               "electricity (not community)",
               "Electricity: electricity, unspecified tariff",
               "house coal (community)",
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
               "wood pellets in bags for secondary heating",
               "waste combustion (unknown)",
               "waste combustion (community)",
               "B30D (community)",
               "B30K (not community)",
               NA)

validate(MAIN_FUEL,"MAIN_FUEL")

# MAINHEATCONT_DESCRIPTION ------------------------------------------------

td_MAINHEATCONT_DESCRIPTION <- function(from,to){
  certs$MAINHEATCONT_DESCRIPTION[certs$MAINHEATCONT_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

certs$MAINHEATCONT_DESCRIPTION <- pbapply::pbsapply(certs$MAINHEATCONT_DESCRIPTION, translatewelsh, cl = cl, USE.NAMES = FALSE)
certs$MAINHEATCONT_DESCRIPTION <- gsub("least 2","least two",certs$MAINHEATCONT_DESCRIPTION, fixed = TRUE)
certs$MAINHEATCONT_DESCRIPTION <- gsub("communit heating","community heating",certs$MAINHEATCONT_DESCRIPTION, fixed = TRUE)

td_MAINHEATCONT_DESCRIPTION("Flat rate charging, no stat control of room temperature","Flat rate charging, no thermostatic control of room temperature")
td_MAINHEATCONT_DESCRIPTION("Flat rate charging, programmer, no room thermostat","Flat rate charging, programmer no room thermostat")
td_MAINHEATCONT_DESCRIPTION("Charging system linked to the use of community heating, prog and TRVs","Charging system linked to use of community heating, programmer and TRVs")
td_MAINHEATCONT_DESCRIPTION("Charging system linked to the use of community heating, programmer and TRVs","Charging system linked to use of community heating, programmer and TRVs")
td_MAINHEATCONT_DESCRIPTION("No time or thermostatic control of room temp","No time or thermostatic control of room temperature")
td_MAINHEATCONT_DESCRIPTION("Not applicable (boiler provides DHW only)","Not relevant (supplies DHW only)")
td_MAINHEATCONT_DESCRIPTION("Flat rate charging, no thermostatic control of room temperature","Flat rate charging, no thermostatic control")
td_MAINHEATCONT_DESCRIPTION("Flat rate charging, programmer and room stat","Flat rate charging, no thermostatic control")
td_MAINHEATCONT_DESCRIPTION("Programmer, roomstat and TRVs","Programmer, room thermostat and TRVs")
td_MAINHEATCONT_DESCRIPTION("Delayed start stat & program & TRV's","Delayed start thermostat, programmer and TRVs")
td_MAINHEATCONT_DESCRIPTION("Charging system linked to use of community heating, programmer?and TRVs","Charging system linked to use of community heating, programmer and TRVs")
td_MAINHEATCONT_DESCRIPTION("Flat rate charging*, programmer and TRVs","Flat rate charging, programmer and TRVs")
td_MAINHEATCONT_DESCRIPTION(c("Celect control","Celect controls"),"Celect-type controls")
td_MAINHEATCONT_DESCRIPTION("Charging system linked to use of community heating, programmer and at least two room stats","Charging system linked to use of community heating, programmer and at least two room thermostats")
td_MAINHEATCONT_DESCRIPTION("2207 Time and temperature zone control","Time and temperature zone control")
td_MAINHEATCONT_DESCRIPTION("Charging system linked to use of community heating, programmerand room thermostat","Charging system linked to use of community heating, programmer and room thermostat")
td_MAINHEATCONT_DESCRIPTION("Programmer and at least two room thermostat","Programmer and at least two room thermostats")
td_MAINHEATCONT_DESCRIPTION("Flat rate charging TRVs","Flat rate charging, TRVs")
td_MAINHEATCONT_DESCRIPTION("Thermostat, programmer and TRVs","Programmer, room thermostat and TRVs")

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
                             "Delayed start thermostat and programmer",
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

validate(MAINHEATCONT_DESCRIPTION, "MAINHEATCONT_DESCRIPTION")


# MAINHEAT_DESCRIPTION ----------------------------------------------------

certs$MAINHEAT_DESCRIPTION <- pbapply::pbsapply(certs$MAINHEAT_DESCRIPTION, splitwelsh, cl = cl, USE.NAMES = FALSE)
certs$MAINHEAT_DESCRIPTION <- pbapply::pbsapply(certs$MAINHEAT_DESCRIPTION, translatewelsh, cl = cl, USE.NAMES = FALSE)

sub_MAINHEAT_DESCRIPTION("&","and")
sub_MAINHEAT_DESCRIPTION("house coal","coal")
sub_MAINHEAT_DESCRIPTION("Boiler and underfloor,","Boiler and underfloor heating,")


td_MAINHEAT_DESCRIPTION("Portable electric heating assumed for most rooms","Portable electric heaters assumed for most rooms")
td_MAINHEAT_DESCRIPTION("Warm air, Electricaire","Warm air, electric")
td_MAINHEAT_DESCRIPTION("Community, community","Community scheme")
td_MAINHEAT_DESCRIPTION("Boiler, dual fuel (mineral and wood)","Boiler and radiators, dual fuel (mineral and wood)") # Guess these are the same
td_MAINHEAT_DESCRIPTION("Boiler and radiators, house coal","Boiler and radiators, coal")
td_MAINHEAT_DESCRIPTION("Boiler and radiators, dual fuel appliance (mineral and wood)","Boiler and radiators, dual fuel (mineral and wood)")
td_MAINHEAT_DESCRIPTION("Room heaters, electricity","Room heaters, electric")
td_MAINHEAT_DESCRIPTION(c("No system present: electric heaters assumed, mains gas","No system present: electric heaters assumed, electric"),"No system present: electric heaters assumed")
td_MAINHEAT_DESCRIPTION("Boiler and radiators, lpg (bottled)","Boiler and radiators, bottled LPG"   )                                                                       
td_MAINHEAT_DESCRIPTION("Electric ceiling, electric","Electric ceiling heating")
td_MAINHEAT_DESCRIPTION("Air source heat pump, , radiators, electric","Air source heat pump, radiators, electric")
td_MAINHEAT_DESCRIPTION("Boiler and radiators,","Boiler and radiators")
td_MAINHEAT_DESCRIPTION("Air source heat pump , electric","Air source heat pump, electric")
td_MAINHEAT_DESCRIPTION("Boiler, underfloor, oil","Boiler and underfloor heating, oil")
td_MAINHEAT_DESCRIPTION("Electric ceiling heating, electric","Electric ceiling heating")
td_MAINHEAT_DESCRIPTION("Boiler and radiators, heat from boilers - mains gas","Boiler and radiators, mains gas")
td_MAINHEAT_DESCRIPTION("Boiler and radiators, bulk LPG","Boiler and radiators, LPG")
td_MAINHEAT_DESCRIPTION("Air source heat pump, , electric","Air source heat pump, electric")
td_MAINHEAT_DESCRIPTION("Ground source heat pump , electric","Ground source heat pump, electric")
td_MAINHEAT_DESCRIPTION("Ground source heat pump, Systems with radiators, electric","Ground source heat pump, radiators, electric")
td_MAINHEAT_DESCRIPTION("Room heaters","Room heaters,")
td_MAINHEAT_DESCRIPTION("Water source heat pump, Warm air, electric","Water source heat pump, warm air, electric")
td_MAINHEAT_DESCRIPTION("Boiler and underfloor, electric","Boiler and underfloor heating, electric")
td_MAINHEAT_DESCRIPTION("Boiler, underfloor, LPG","Boiler and underfloor heating, LPG")
td_MAINHEAT_DESCRIPTION("Boiler, coal","Boiler and radiators, coal")
td_MAINHEAT_DESCRIPTION("Boiler, underfloor, dual fuel (mineral and wood)","Boiler and underfloor heating, dual fuel (mineral and wood)")
td_MAINHEAT_DESCRIPTION("Warm air heat pump, electric","Warm air, heat pump, electric")
td_MAINHEAT_DESCRIPTION("Ground source heat pump, , electric","Ground source heat pump, electric")
td_MAINHEAT_DESCRIPTION(c("Portable electric heating assumed for most rooms, electric","Portable electric heating assumed for most rooms"),"Portable electric heaters assumed for most rooms")
td_MAINHEAT_DESCRIPTION("Electric storage, electric","Electric storage heaters")
td_MAINHEAT_DESCRIPTION("Electric ceiling","Electric ceiling heating")
td_MAINHEAT_DESCRIPTION("Air source heat pump fan coil units, electric","Air source heat pump, fan coil units, electric")
td_MAINHEAT_DESCRIPTION("Boiler, smokeless fuel","Boiler and radiators, smokeless fuel")
td_MAINHEAT_DESCRIPTION("Community scheme with CHP, waste combustion and waste combustion","Community scheme, waste combustion")
td_MAINHEAT_DESCRIPTION("Ground-to-water heat pump with auxiliary heater (electric) source heat pump, radiators, electric","Ground source heat pump, radiators, electric")
td_MAINHEAT_DESCRIPTION("Air source heat pump, Warm air, electric","Air source heat pump, warm air, electric")
td_MAINHEAT_DESCRIPTION("Ground source heat pump, radiators, electricity","Ground source heat pump, radiators, electric")
td_MAINHEAT_DESCRIPTION("Boiler and radiators, bulk wood pellets","Boiler and radiators, wood pellets")


MAINHEAT_DESCRIPTION = c(", electric",
                         ", gas",
                         ", wood pellets",
                         "Air source heat pump, electric",
                         "Air source heat pump, radiators, electric",
                         "Air source heat pump, radiators, mains gas",
                         "Air source heat pump, Systems with radiators, electric",
                         "Air source heat pump, Underfloor heating, pipes in screed above insulation, electric",
                         "Air source heat pump, Underfloor heating, pipes in insulated timber floor, electric",
                         "Air source heat pump, Underfloor heating, pipes in concrete slab, electric",
                         "Air source heat pump, Underfloor heating and radiators, pipes in screed above insulation, electric",
                         "Air source heat pump, Underfloor heating and radiators, pipes in insulated timber floor, electric",
                         "Air source heat pump, underfloor, electric",
                         "Air source heat pump, warm air, electric",
                         
                         "Air source heat pump, fan coil units, electric",
                         
                         "Boiler and radiators",
                         "Boiler and radiators, glo",
                         "Boiler and radiators, anthracite",
                         "Boiler and radiators, B30K",
                         "Boiler and radiators, biomass",
                         "Boiler and radiators, liquid biofuel",
                         "Boiler and radiators, coal",
                         "Boiler and radiators, dual fuel (mineral and wood)",
                         "Boiler and radiators, dual fuel appliance",
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
                         "Boiler and underfloor heating, coal",
                         "Boiler and underfloor heating, B30K",
                         "Boiler and underfloor heating, anthracite",
                         "Boiler and underfloor heating, electric",
                         "Boiler and underfloor heating, LPG",
                         "Boiler and underfloor heating, bottled LPG",
                         "Boiler and underfloor heating, mains gas",
                         "Boiler and underfloor heating, bottled gas",
                         "Boiler and underfloor heating, oil",
                         "Boiler and underfloor heating, wood pellets",
                         "Boiler and underfloor heating, wood logs",
                         "Boiler and underfloor heating, wood chips",
                         "Boiler and underfloor heating, dual fuel (mineral and wood)",
                         "Boiler and underfloor heating, smokeless fuel",
                         "Community scheme",
                         "Community scheme, biomass",
                         "Community scheme with CHP",
                         "Community scheme with CHP and waste combustion",
                         "Community scheme, mains gas",
                         "Community scheme, oil",
                         "Community scheme, radiators, mains gas",
                         "Community scheme with CHP and mains gas",
                         "Community scheme, waste combustion",
                         "Community heat pump, electric",
                         "Community heat pump, electric and mains gas",
                         "Community heat pump",
                         "Electric storage heaters",
                         "Electric storage heaters, radiators",
                         "Electric underfloor heating",
                         "Electric ceiling heating",
                         "Electric ceiling heating, radiators, electric",
                         "Electric ceiling heating, underfloor, electric",
                         "Electric heat pumps, electric",
                         "Exhaust air MEV source heat pump, Systems with radiators, electric",
                         "Ground source heat pump, Underfloor heating, pipes in screed above insulation, electric",
                         "Ground source heat pump, Underfloor heating, pipes in insulated timber floor, electric",
                         "Ground source heat pump, Underfloor heating and radiators, pipes in concrete slab, electric",
                         "Ground source heat pump, Underfloor heating and radiators, pipes in insulated timber floor, electric",
                         "Ground source heat pump, Underfloor heating and radiators, pipes in screed above insulation, electric",
                         "Ground source heat pump, underfloor, electric",
                         "Ground source heat pump, underfloor, mains gas",
                         "Ground source heat pump, underfloor, LPG",
                         "Ground source heat pump, warm air, electric",
                         "Ground source heat pump, radiators, electric",
                         "Ground source heat pump, radiators, mains gas",
                         "Ground source heat pump, fan coil units, electric",
                         
                         "Ground source heat pump, electric",
                         "No system present: electric heaters assumed",
                         "Portable electric heaters",
                         
                         "Portable electric heaters assumed for most rooms",
                         "Room heaters,",
                         "Room heaters, anthracite",
                         "Room heaters, coal",
                         "Room heaters, oil",
                         "Room heaters, dual fuel",
                         "Room heaters, dual fuel (mineral and wood)",
                         "Room heaters, electric",
                         "Room heaters, radiators, electric",
                         "Room heaters, smokeless fuel",
                         "Room heaters, wood logs",
                         "Room heaters, wood chips",
                         "Room heaters, wood pellets",
                         "Room heaters, mains gas",
                         "Room heaters, bottled gas",
                         "Room heaters, bottled LPG",
                         "Room heaters, LPG",
                         "Room heaters, radiators, oil",
                         "Solid-fuel boiler, solid fuel",
                         "Warm air,",
                         "Warm air, electric",
                         "Warm air, mains gas",
                         "Warm air, oil",
                         "Warm air, heat pump, electric",
                         "Warm air, LPG",
                         "Water source heat pump, radiators, electric",
                         "Water source heat pump, underfloor, electric",
                         "Water source heat pump, warm air, electric",
                         "Water source heat pump, radiators, mains gas",
                         "Micro-cogeneration, mains gas",
                         "SAP05:Main-Heating")


validate(MAINHEAT_DESCRIPTION, "MAINHEAT_DESCRIPTION")

# WINDOWS_DESCRIPTION -----------------------------------------------------

certs$WINDOWS_DESCRIPTION <- pbapply::pbsapply(certs$WINDOWS_DESCRIPTION, splitwelsh, cl = cl, USE.NAMES = FALSE)
certs$WINDOWS_DESCRIPTION <- pbapply::pbsapply(certs$WINDOWS_DESCRIPTION, translatewelsh, cl = cl, USE.NAMES = FALSE)
certs$WINDOWS_DESCRIPTION <- gsub("  "," ",certs$WINDOWS_DESCRIPTION)
certs$WINDOWS_DESCRIPTION <- gsub("glazed","glazing",certs$WINDOWS_DESCRIPTION)
certs$WINDOWS_DESCRIPTION <- gsub("Fully","Full",certs$WINDOWS_DESCRIPTION)

certs$WINDOWS_DESCRIPTION[certs$WINDOWS_DESCRIPTION == "Single glazingSingle glazing"] <- "Single glazing"
certs$WINDOWS_DESCRIPTION[certs$WINDOWS_DESCRIPTION == "Solid, no insulation (assumed)"] <- NA

WINDOWS_DESCRIPTION = c("High performance glazing",
                        
                        "Full double glazing",
                        "Mostly double glazing",
                        "Partial double glazing",
                        "Some double glazing",
                        "double glazing",
                        
                        "Single glazing",
                        "Some Single glazing",
                        "Single and multiple glazing",
                        
                        "Multiple glazing throughout double glazing", 
                        
                        "Multiple glazing throughout",
                        "Partial multiple glazing",
                        "Some multiple glazing",
                        "Mostly multiple glazing",
                        
                        "Full triple glazing",
                        "Partial triple glazing",
                        "Mostly triple glazing",
                        "Some triple glazing",
                        
                        "Some secondary glazing",
                        "Partial secondary glazing",
                        "Secondary glazing",
                        "Full secondary glazing",
                        "Mostly secondary glazing",
                        
                        "SAP05:Windows",
                        "Single glazeddouble glazing",
                        "Single glazedsecondary glazing",
                        "Single glazingdouble glazing",
                        "Single glazingsecondary glazing",
                        "Single glazingtriple glazing",
                        "Partial multiple glazingdouble glazing",
                        
                        NA
)

validate(WINDOWS_DESCRIPTION, "WINDOWS_DESCRIPTION")


# HOTWATER_DESCRIPTION ----------------------------------------------------

certs$HOTWATER_DESCRIPTION <- pbapply::pbsapply(certs$HOTWATER_DESCRIPTION, splitwelsh, cl = cl, USE.NAMES = FALSE)
certs$HOTWATER_DESCRIPTION <- pbapply::pbsapply(certs$HOTWATER_DESCRIPTION, translatewelsh, cl = cl, USE.NAMES = FALSE)

certs$HOTWATER_DESCRIPTION <- gsub("cylinderstat","cylinder thermostat",certs$HOTWATER_DESCRIPTION, fixed = TRUE)
certs$HOTWATER_DESCRIPTION <- gsub("no cylinder thermostat, no cylinder thermostat","no cylinder thermostat",certs$HOTWATER_DESCRIPTION, fixed = TRUE)

td_HOTWATER_DESCRIPTION("community scheme",
                        "Community scheme")
td_HOTWATER_DESCRIPTION( "From community scheme","Community scheme")
td_HOTWATER_DESCRIPTION( "community scheme, no cylinder thermostat",
                         "Community scheme, no cylinder thermostat")
td_HOTWATER_DESCRIPTION( c("No system present?electric immersion assumed",
                           "No hot water system present - electric immersion assumed",
                           "No system present : electric immersion assumed"),
                         "No system present: electric immersion assumed")
td_HOTWATER_DESCRIPTION( "From main system ",
                         "From main system")
td_HOTWATER_DESCRIPTION("Electric immersion, standard tariff ","Electric immersion, standard tariff")
td_HOTWATER_DESCRIPTION("From main system , no cylinder thermostat","From main system, no cylinder thermostat")
td_HOTWATER_DESCRIPTION("From main system, flue gas heat recovery, plus solar","From main system, plus solar, flue gas heat recovery")
td_HOTWATER_DESCRIPTION("From secondary heater, no cylinder thermostat","From secondary system, no cylinder thermostat")
td_HOTWATER_DESCRIPTION("SAP:Hot-Water","SAP05:Hot-Water")


HOTWATER_DESCRIPTION = c("Back boiler (hot water only), gas",
                         "Community scheme",
                         "Community scheme with CHP",
                         "Community scheme, no cylinder thermostat",
                         "Community scheme, no cylinder thermostat, plus solar",
                         "Community scheme, plus solar",
                         "Community scheme, waste water heat recovery",
                         "Community heat pump",
                         "Electric immersion, off-peak",
                         "Electric immersion, off-peak, plus solar",
                         "Electric immersion, off-peak, no cylinder thermostat",
                         "Electric immersion, off-peak, flue gas heat recovery, waste water heat recovery",
                         "Electric immersion, dual tariff",
                         "Electric immersion, standard tariff",
                         "Electric immersion, standard tariff, no cylinder thermostat",
                         "Electric immersion, standard tariff, plus solar",
                         "Electric immersion, standard tariff, flue gas heat recovery, waste water heat recovery",
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
                         "From main system, plus solar, waste water heat recovery",
                         "From main system, waste water heat recovery",
                         "From second main heating system",
                         "From secondary system",
                         "From secondary system, no cylinder thermostat",
                         "From secondary system, plus solar, no cylinder thermostat",
                         "From secondary system, plus solar",
                         "Heat pump",
                         "Single-point gas water heater, standard tariff",
                         "Single-point gas water heater",
                         "Single-point gas water heater, off-peak",
                         "Point gas water heater, no cylinder thermostat",
                         
                         "Gas boiler/circulator",
                         "Gas boiler/circulator, no cylinder thermostat",
                         "Gas boiler/circulator, no cylinder thermostat, plus solar",
                         "Gas boiler/circulator, plus solar",
                         "Gas multipoint",
                         "Gas multipoint, plus solar",
                         "Gas multipoint, no cylinder thermostat",
                         "Gas range cooker, no cylinder thermostat",  
                         "Gas range cooker",
                         "Gas range cooker, plus solar",
                         "Gas instantaneous at point of use",
                         "Gas instantaneous at point of use, plus solar",
                         "No system present: electric immersion assumed",
                         "No system present: electric immersion assumed, plus solar",
                         "No system present: electric immersion assumed, no cylinder thermostat",
                         ", no cylinder thermostat",
                         "Oil boiler/circulator",
                         "Oil boiler/circulator, no cylinder thermostat",
                         "Oil boiler/circulator, plus solar",
                         "Oil range cooker",
                         "Oil range cooker, no cylinder thermostat",
                         "Oil range cooker, plus solar, no cylinder thermostat",
                         "Oil range cooker, plus solar",
                         "Solid fuel range cooker",
                         "Solid fuel range cooker, plus solar",
                         "Solid fuel range cooker, no cylinder thermostat",
                         "Solid fuel range cooker, no cylinder thermostat, plus solar",
                         "Solid fuel boiler/circulator",
                         "Solid fuel boiler/circulator, no cylinder thermostat",
                         "Solid fuel boiler/circulator, plus solar",
                         "Solid fuel boiler/circulator, plus solar, no cylinder thermostat",
                         "SAP05:Hot-Water")

validate(HOTWATER_DESCRIPTION, "HOTWATER_DESCRIPTION")


# FLOOR_LEVEL -------------------------------------------------------------

certs$FLOOR_LEVEL[certs$FLOOR_LEVEL == "ground floor"] <- "Ground"

FLOOR_LEVEL = c("Basement","Ground","1st","mid floor",
                "2nd","3rd",
                paste0(c(4:20,24:30),"th"),
                "21st","22nd","23rd",
                "21st or above","top floor", NA)

validate(FLOOR_LEVEL, "FLOOR_LEVEL")


# SECONDHEAT_DESCRIPTION --------------------------------------------------

certs$SECONDHEAT_DESCRIPTION <- pbapply::pbsapply(certs$SECONDHEAT_DESCRIPTION, splitwelsh, cl = cl, USE.NAMES = FALSE)
certs$SECONDHEAT_DESCRIPTION <- pbapply::pbsapply(certs$SECONDHEAT_DESCRIPTION, translatewelsh, cl = cl, USE.NAMES = FALSE)

td_SECONDHEAT_DESCRIPTION("Room heaters, lpg","Room heaters, LPG")
td_SECONDHEAT_DESCRIPTION("Dim","None")
td_SECONDHEAT_DESCRIPTION("Portable electric heaters(assumed)","Portable electric heaters (assumed)")

td_SECONDHEAT_DESCRIPTION("Room heaters, (null)","Room heaters,")
td_SECONDHEAT_DESCRIPTION("Room heaters, bulk LPG","Room heaters, LPG")
td_SECONDHEAT_DESCRIPTION("Room heaters, bulk wood pellets","Room heaters, wood pellets")
td_SECONDHEAT_DESCRIPTION(",",NA)
td_SECONDHEAT_DESCRIPTION("Room heaters, heating oil","Room heaters, oil")

SECONDHEAT_DESCRIPTION = c("None",
                           "Portable electric heaters",
                           "Portable electric heaters (assumed)",
                           "Gas/LPG boiler pre-1998, with fan-assisted flue, gas",
                           "Gas/LPG boiler pre-1998 with balanced or open-flue, gas",
                           "Gas/LPG boiler 1998 or later, gas",
                           "Gas/LPG CPSU, gas",
                           "Electric Underfloor Heating (Standard tariff), electric",
                           "Room heaters,",
                           "Room heaters, B30K",
                           "Room heaters, bioethanol",
                           "Room heaters, oil",
                           "Room heaters, electric",
                           "Room heaters, coal",
                           "Room heaters, dual fuel (mineral and wood)",
                           "Room heaters, dual fuel",
                           "Room heaters, wood pellets",
                           "Room heaters, wood logs",
                           "Room heaters, wood chips",
                           "Room heaters, smokeless fuel",
                           "Room heaters, LPG",
                           "Room heaters, LNG",
                           "Room heaters, bottled LPG",
                           "Room heaters, mains gas",
                           "Room heaters, bottled gas",
                           "Room heaters, anthracite",
                           "Room heaters, smokeless Fuel",
                           "SAP05:Secondary-Heating",
                           NA)

validate(SECONDHEAT_DESCRIPTION, "SECONDHEAT_DESCRIPTION")

# TRANSACTION_TYPE --------------------------------------------------------

certs$TRANSACTION_TYPE[certs$TRANSACTION_TYPE == "not recorded"] <- "unknown"
certs$TRANSACTION_TYPE[certs$TRANSACTION_TYPE == "none of the above"] <- "unknown"
certs$TRANSACTION_TYPE <- gsub(" - this is for backwards compatibility only and should not be used","",certs$TRANSACTION_TYPE, fixed = TRUE)

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

validate(TRANSACTION_TYPE, "TRANSACTION_TYPE")

# LIGHTING_DESCRIPTION ---------------------------------------------------------------


certs$LIGHTING_DESCRIPTION <- pbapply::pbsapply(certs$LIGHTING_DESCRIPTION,splitwelsh, cl = cl, USE.NAMES = FALSE)
certs$LIGHTING_DESCRIPTION <- gsub("Low energy lighting in ","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("% fixed outlets","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("% of fixed outlets","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("all fixed outlets","100",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("No low energy lighting","0",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("No Low energy lighting","0",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("SAP05:Lighting","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("Goleuadau ynni-isel mewn ","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("% o'r mannau gosod","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("% o?r mannau gosod","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)

certs$LIGHTING_DESCRIPTION[certs$LIGHTING_DESCRIPTION == "Dim goleuadau ynni-isel"] <- "0"
certs$LIGHTING_DESCRIPTION[certs$LIGHTING_DESCRIPTION == "Low energy lighting 100% 100"] <- "100"
certs$LIGHTING_DESCRIPTION[certs$LIGHTING_DESCRIPTION == "Goleuadau ynni-isel ym mhob un o'r mannau gosod"] <- "100"

LIGHTING_DESCRIPTION <- as.integer(as.numeric(certs$LIGHTING_DESCRIPTION))

if(anyNA(LIGHTING_DESCRIPTION)){
  err <- certs$LIGHTING_DESCRIPTION[is.na(LIGHTING_DESCRIPTION)]
  err <- unique(err)
  if(any(err != "")){
    stop(paste0("LIGHTING_DESCRIPTION unknown values: ",paste(err, collapse = "  ")))
  }
} 

certs$LIGHTING_DESCRIPTION <- LIGHTING_DESCRIPTION


# Finish Up ---------------------------------------------------------------


parallel::stopCluster(cl)

