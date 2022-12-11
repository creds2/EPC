fix_wm2k <- function(x, check = FALSE){
  
  if(grepl("w",x)){
    x <- gsub("²","2", x)
    x <- gsub("w/m?k","w/m2k", x, fixed = TRUE)
    x <- gsub("w/ma?k","w/m2k", x, fixed = TRUE)
    x <- gsub("w/mâ2k","w/m2k", x, fixed = TRUE)
    x <- gsub("w/mand#0178;k","w/m2k", x, fixed = TRUE)
    x <- gsub("w/m??k","w/m2k", x, fixed = TRUE)
    x <- gsub("-","", x, fixed = TRUE)
    
    if(grepl("w/m2k", x)){
      y <- strsplit(x," ")[[1]]
      if(length(y) != 2){
        if(check){
          stop(paste0("Don't know how to process ",x))
        } else {
          return(x)
        }
      }
      if(nchar(y[1]) != 4){
        yn <- as.numeric(y[1])
        if(is.nan(yn)){
          if(check){
            stop(paste0("Don't know how to process ",y[1]))
          } else {
            return(x)
          }
        }
        y[1] <- format(as.numeric(y[1]), digits = 2, nsmall = 2)
      }
      x <- paste0(y[1]," ",y[2])
    }
  }
  
  
  return(x)
}


standardclean <- function(x){
  x <- tolower(x)
  x <- gsub("????????????????????????????????????????????????????",", ", x, fixed = TRUE)
  x <- gsub("house coal","coal", x, fixed = TRUE)
  x <- gsub("&","and", x, fixed = TRUE)
  x <- gsub(", , ",", ", x, fixed = TRUE)
  x <- gsub(" , ",", ", x, fixed = TRUE)
  x <- gsub("average thermal transmittance ","", x, fixed = TRUE)
  x <- gsub("trawsyriannedd thermol cyfartalog ","", x, fixed = TRUE)
  x <- gsub("= ","", x, fixed = TRUE)
  x <- gsub("- "," ", x, fixed = TRUE)
  x <- gsub("  "," ", x, fixed = TRUE)
  x <- gsub("fully","full", x, fixed = TRUE)
  x <- gsub("electricity","electric", x, fixed = TRUE)
  
  
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
    }
  }
  return(x)
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

td_mainheat_description <- function(from,to){
  certs$MAINHEAT_DESCRIPTION[certs$MAINHEAT_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

sub_mainheat_description <- function(from,to){
  certs$MAINHEAT_DESCRIPTION <- gsub(from,to,certs$MAINHEAT_DESCRIPTION, fixed = TRUE)
  assign('certs',certs,envir=.GlobalEnv)
}

sub_FLOOR_DESCRIPTION <- function(from,to){
  certs$FLOOR_DESCRIPTION <- gsub(from,to,certs$FLOOR_DESCRIPTION, fixed = TRUE)
  assign('certs',certs,envir=.GlobalEnv)
}

sub_HOTWATER_DESCRIPTION <- function(from,to){
  certs$HOTWATER_DESCRIPTION <- gsub(from,to,certs$HOTWATER_DESCRIPTION, fixed = TRUE)
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


td_MAINHEATCONT_DESCRIPTION <- function(from,to){
  certs$MAINHEATCONT_DESCRIPTION[certs$MAINHEATCONT_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

sub_MAINHEATCONT_DESCRIPTION <- function(from,to){
  certs$MAINHEATCONT_DESCRIPTION <- gsub(from,to,certs$MAINHEATCONT_DESCRIPTION, fixed = TRUE)
  assign('certs',certs,envir=.GlobalEnv)
}


sub_MAIN_FUEL <- function(from,to){
  certs$MAIN_FUEL <- gsub(from,to,certs$MAIN_FUEL, fixed = TRUE)
  assign('certs',certs,envir=.GlobalEnv)
}

td_MAIN_FUEL <- function(from,to){
  certs$MAIN_FUEL[certs$MAIN_FUEL %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}


sub_WINDOWS_DESCRIPTION <- function(from,to){
  certs$WINDOWS_DESCRIPTION <- gsub(from,to,certs$WINDOWS_DESCRIPTION, fixed = TRUE)
  assign('certs',certs,envir=.GlobalEnv)
}

td_WINDOWS_DESCRIPTION <- function(from,to){
  certs$WINDOWS_DESCRIPTION[certs$WINDOWS_DESCRIPTION %in% from] <- to
  assign('certs',certs,envir=.GlobalEnv)
}

col_types = cols(.default = col_character(),
                 LMK_KEY = col_double(),
                 BUILDING_REFERENCE_NUMBER = col_double(),
                 CURRENT_ENERGY_RATING	= col_factor(c("A","B","C","D","E","F","G","INVALID!"), TRUE), 
                 POTENTIAL_ENERGY_RATING = col_factor(c("A","B","C","D","E","F","G","INVALID!"), TRUE),
                 CURRENT_ENERGY_EFFICIENCY = col_integer(),
                 POTENTIAL_ENERGY_EFFICIENCY = col_integer(),
                 PROPERTY_TYPE = col_factor(c("Flat","House","Maisonette","Bungalow","Park home"), FALSE),
                 BUILT_FORM = col_factor(c("Detached","Semi-Detached","Mid-Terrace",
                                           "Enclosed Mid-Terrace","NO DATA!",
                                           "End-Terrace","Enclosed End-Terrace" ), FALSE),
                 INSPECTION_DATE = col_date(),
                 #LOCAL_AUTHORITY = col_factor(),
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
                 #MAIN_HEATING_CONTROLS = col_integer(),
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