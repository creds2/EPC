library(dplyr)
library(sf)

# Get most recent EPC
certs <- readRDS("epc_all_clean.Rds")

# dropcol <- c("LMK_KEY","ADDRESS1","ADDRESS2","ADDRESS3",
#              "LOCAL_AUTHORITY","COUNTY","LODGEMENT_DATE",
#              "ENVIRONMENT_IMPACT_CURRENT",
#              "ENVIRONMENT_IMPACT_POTENTIAL","ENERGY_CONSUMPTION_CURRENT","ENERGY_CONSUMPTION_POTENTIAL",
#              "CO2_EMISSIONS_CURRENT","CO2_EMISS_CURR_PER_FLOOR_AREA", "CO2_EMISSIONS_POTENTIAL",
#              "LIGHTING_COST_CURRENT","LIGHTING_COST_POTENTIAL","HEATING_COST_CURRENT",
#              "HEATING_COST_POTENTIAL","HOT_WATER_COST_CURRENT","HOT_WATER_COST_POTENTIAL",
#              "MAINS_GAS_FLAG","FLOOR_LEVEL","FLAT_TOP_STOREY","FLAT_STOREY_COUNT","MAIN_HEATING_CONTROLS",
#              "MAIN_HEATING_CONTROLS","MULTI_GLAZE_PROPORTION","GLAZED_TYPE",
#              "GLAZED_AREA","EXTENSION_COUNT","NUMBER_HABITABLE_ROOMS","NUMBER_HEATED_ROOMS",
#              "FLOOR_DESCRIPTION","FLOOR_ENV_EFF","WINDOWS_ENV_EFF","WALLS_ENV_EFF","SHEATING_ENV_EFF",
#              "ROOF_ENV_EFF","LIGHTING_ENV_EFF",
#              "WIND_TURBINE_COUNT","HEAT_LOSS_CORRIDOOR","HEAT_LOSS_CORRIDOOR",
#              "UNHEATED_CORRIDOR_LENGTH","FLOOR_HEIGHT",
#              "MECHANICAL_VENTILATION","LOCAL_AUTHORITY_LABEL","NUMBER_OPEN_FIREPLACES",
#              "WALLS_DESCRIPTION", "SHEATING_ENERGY_EFF")

# certs <- certs[,!names(certs) %in% dropcol]
certs <- certs[order(certs$INSPECTION_DATE, decreasing = TRUE),]
certs <- certs[!duplicated(certs$BUILDING_REFERENCE_NUMBER),]

source("../../creds2/CarbonCalculator/R/secure_path.R")

dir.create("tmp")
unzip(paste0(paste0(substr(secure_path,1,39),"Postcodes/codepo_20210208/codepo_gpkg_gb.zip")),
      exdir = "tmp")
postcodes <- read_sf("tmp/data/codepo_gb.gpkg")
unlink("tmp",recursive = TRUE)
postcodes <- postcodes[,c("Postcode")]
postcodes$Postcode <- gsub(" ","",postcodes$Postcode)

postcodeold <- readRDS("D:/OneDrive - University of Leeds/Data/Postcodes/code_point_open.Rds")
postcodeold$postcode <- gsub(" ","",postcodeold$postcode)
postcodeold <- postcodeold["postcode"]
names(postcodeold) <- c("Postcode","geom")
st_geometry(postcodeold) <- "geom"
postcodeold <- postcodeold[!postcodeold$Postcode %in% postcodes$Postcode,]
postcodes <- rbind(postcodes, postcodeold)

certs$POSTCODE_clean <- gsub(" ","",certs$POSTCODE) 

summary(certs$POSTCODE_clean %in% postcodes$Postcode)

# 33916 invalid postcodes

dir.create("tmp")
unzip(paste0(substr(secure_path,1,39),"OA Bounadries/GB_LSOA_2011_clipped.zip"),
      exdir = "tmp")
lsoa <- read_sf("tmp/infuse_lsoa_lyr_2011_clipped.shp")
unlink("tmp",recursive = TRUE)

lsoa <- lsoa["geo_code"]
names(lsoa) <- c("LSOA11","geometry")
nrow(postcodes)
postcodes <- st_join(postcodes, lsoa)
nrow(postcodes)
postcodes <- postcodes[!duplicated(postcodes$Postcode),]
postcodes <- st_drop_geometry(postcodes)
postcodes$quality <- NULL

certs <- left_join(certs, postcodes, by = c("POSTCODE_clean" = "Postcode"))
certs$buidling_type <- paste0(certs$BUILT_FORM," ",certs$PROPERTY_TYPE)
certs$fuel <- gsub(" \\(not community\\)","",certs$MAIN_FUEL)
certs$fuel <- gsub(" \\(unknown\\)","",certs$fuel)
certs$fuel <- gsub(" \\(community\\)","",certs$fuel)

certs$FLOOR_ENERGY_EFF[is.na(certs$FLOOR_ENERGY_EFF)] <- "NO DATA!"

certs$WINDOWS_ENERGY_EFF <- as.character(certs$WINDOWS_ENERGY_EFF)
certs$WINDOWS_ENERGY_EFF[is.na(certs$WINDOWS_ENERGY_EFF)] <- "NO DATA!"
certs$WINDOWS_ENERGY_EFF <- as.factor(certs$WINDOWS_ENERGY_EFF)

certs$WALLS_ENERGY_EFF <- as.character(certs$WALLS_ENERGY_EFF)
certs$WALLS_ENERGY_EFF[is.na(certs$WALLS_ENERGY_EFF)] <- "NO DATA!"
certs$WALLS_ENERGY_EFF <- as.factor(certs$WALLS_ENERGY_EFF)

certs$ROOF_ENERGY_EFF <- as.character(certs$ROOF_ENERGY_EFF)
certs$ROOF_ENERGY_EFF[is.na(certs$ROOF_ENERGY_EFF)] <- "NO DATA!"
certs$ROOF_ENERGY_EFF <- as.factor(certs$ROOF_ENERGY_EFF)

certs$MAINHEAT_ENERGY_EFF <- as.character(certs$MAINHEAT_ENERGY_EFF)
certs$MAINHEAT_ENERGY_EFF[is.na(certs$MAINHEAT_ENERGY_EFF)] <- "NO DATA!"
certs$MAINHEAT_ENERGY_EFF <- as.factor(certs$MAINHEAT_ENERGY_EFF)

certs$MAINHEATC_ENERGY_EFF <- as.character(certs$MAINHEATC_ENERGY_EFF)
certs$MAINHEATC_ENERGY_EFF[is.na(certs$MAINHEATC_ENERGY_EFF)] <- "NO DATA!"
certs$MAINHEATC_ENERGY_EFF <- as.factor(certs$MAINHEATC_ENERGY_EFF)

certs$PHOTO_SUPPLY[is.na(certs$PHOTO_SUPPLY)] <- 0

# Flag Roofs and Floors
certs$FLOOR_ENERGY_EFF <- as.character(certs$FLOOR_ENERGY_EFF)
certs$FLOOR_ENERGY_EFF <- ifelse((certs$FLOOR_ENERGY_EFF %in% c("NO DATA!","N/A")) & 
                (certs$FLOOR_DESCRIPTION %in% c("(another dwelling below)", "(other premises below)")),
                                 "dwelling below",certs$FLOOR_ENERGY_EFF)

certs$ROOF_ENERGY_EFF <- as.character(certs$ROOF_ENERGY_EFF)
certs$ROOF_ENERGY_EFF <- ifelse((certs$ROOF_ENERGY_EFF %in% c("NO DATA!","N/A")) & 
                (certs$ROOF_DESCRIPTION %in% c("(another dwelling above)", "(other premises above)")),
              "dwelling above",certs$ROOF_ENERGY_EFF)


cert_summ <- certs %>%
  group_by(LSOA11) %>%
  summarise(epc_total = n(),
            epc_newbuild = length(TRANSACTION_TYPE[TRANSACTION_TYPE == "new dwelling"]),
            epc_A = length(CURRENT_ENERGY_RATING[CURRENT_ENERGY_RATING == "A"]),
            epc_B = length(CURRENT_ENERGY_RATING[CURRENT_ENERGY_RATING == "B"]),
            epc_C = length(CURRENT_ENERGY_RATING[CURRENT_ENERGY_RATING == "C"]),
            epc_D = length(CURRENT_ENERGY_RATING[CURRENT_ENERGY_RATING == "D"]),
            epc_E = length(CURRENT_ENERGY_RATING[CURRENT_ENERGY_RATING == "E"]),
            epc_F = length(CURRENT_ENERGY_RATING[CURRENT_ENERGY_RATING == "F"]),
            epc_G = length(CURRENT_ENERGY_RATING[CURRENT_ENERGY_RATING == "G"]),
            epc_other = length(CURRENT_ENERGY_RATING[is.na(CURRENT_ENERGY_RATING)]),
            epc_score_avg = mean(CURRENT_ENERGY_EFFICIENCY, na.rm = TRUE),
            type_house_semi = length(buidling_type[buidling_type == "Semi-Detached House"]),
            type_house_midterrace = length(buidling_type[buidling_type %in% c("Enclosed Mid-Terrace House", "Mid-Terrace House")]),
            type_house_endterrace = length(buidling_type[buidling_type %in% c("Enclosed End-Terrace House", "End-Terrace House")]),
            type_house_detached = length(buidling_type[buidling_type == "Detached House"]),
            type_flat = length(buidling_type[grepl("Flat",buidling_type)]),
            type_bungalow_semi = length(buidling_type[buidling_type == "Semi-Detached Bungalow"]),
            type_bungalow_midterrace = length(buidling_type[buidling_type %in% c("Enclosed Mid-Terrace Bungalow", "Mid-Terrace Bungalow")]),
            type_bungalow_endterrace = length(buidling_type[buidling_type %in% c("Enclosed End-Terrace Bungalow", "End-Terrace Bungalow")]),
            type_bungalow_detached = length(buidling_type[buidling_type == "Detached Bungalow"]),
            type_maisonette = length(buidling_type[grepl("Maisonette",buidling_type)]),
            type_parkhome = length(buidling_type[grepl("Park home",buidling_type)]),
            floor_area_avg = round(mean(TOTAL_FLOOR_AREA, na.rm = TRUE)),
            low_energy_light = round(mean(LOW_ENERGY_LIGHTING, na.rm = TRUE)),
            
            floor_verygood = length(FLOOR_ENERGY_EFF[FLOOR_ENERGY_EFF == "Very Good"]),
            floor_good = length(FLOOR_ENERGY_EFF[FLOOR_ENERGY_EFF == "Good"]),
            floor_average = length(FLOOR_ENERGY_EFF[FLOOR_ENERGY_EFF == "Average"]),
            floor_poor = length(FLOOR_ENERGY_EFF[FLOOR_ENERGY_EFF == "Poor"]),
            floor_verypoor = length(FLOOR_ENERGY_EFF[FLOOR_ENERGY_EFF == "Very Poor"]),
            floor_below = length(FLOOR_ENERGY_EFF[FLOOR_ENERGY_EFF == "dwelling below"]),
            floor_other = length(FLOOR_ENERGY_EFF[!FLOOR_ENERGY_EFF %in% c("Very Good","Good","Average","Poor","Very Poor","dwelling below")]),
            
            window_verygood = length(WINDOWS_ENERGY_EFF[WINDOWS_ENERGY_EFF == "Very Good"]),
            window_good = length(WINDOWS_ENERGY_EFF[WINDOWS_ENERGY_EFF == "Good"]),
            window_average = length(WINDOWS_ENERGY_EFF[WINDOWS_ENERGY_EFF == "Average"]),
            window_poor = length(WINDOWS_ENERGY_EFF[WINDOWS_ENERGY_EFF == "Poor"]),
            window_verypoor = length(WINDOWS_ENERGY_EFF[WINDOWS_ENERGY_EFF == "Very Poor"]),
            window_other = length(WINDOWS_ENERGY_EFF[!WINDOWS_ENERGY_EFF %in% c("Very Good","Good","Average","Poor","Very Poor")]),
            
            wall_verygood = length(WALLS_ENERGY_EFF[WALLS_ENERGY_EFF == "Very Good"]),
            wall_good = length(WALLS_ENERGY_EFF[WALLS_ENERGY_EFF == "Good"]),
            wall_average = length(WALLS_ENERGY_EFF[WALLS_ENERGY_EFF == "Average"]),
            wall_poor = length(WALLS_ENERGY_EFF[WALLS_ENERGY_EFF == "Poor"]),
            wall_verypoor = length(WALLS_ENERGY_EFF[WALLS_ENERGY_EFF == "Very Poor"]),
            wall_other = length(WALLS_ENERGY_EFF[!WALLS_ENERGY_EFF %in% c("Very Good","Good","Average","Poor","Very Poor")]),
            
            roof_verygood = length(ROOF_ENERGY_EFF[ROOF_ENERGY_EFF == "Very Good"]),
            roof_good = length(ROOF_ENERGY_EFF[ROOF_ENERGY_EFF == "Good"]),
            roof_average = length(ROOF_ENERGY_EFF[ROOF_ENERGY_EFF == "Average"]),
            roof_poor = length(ROOF_ENERGY_EFF[ROOF_ENERGY_EFF == "Poor"]),
            roof_verypoor = length(ROOF_ENERGY_EFF[ROOF_ENERGY_EFF == "Very Poor"]),
            roof_above = length(ROOF_ENERGY_EFF[ROOF_ENERGY_EFF == "dwelling above"]),
            roof_other = length(ROOF_ENERGY_EFF[!ROOF_ENERGY_EFF %in% c("Very Good","Good","Average","Poor","Very Poor","dwelling above")]),
            
            mainheat_verygood = length(MAINHEAT_ENERGY_EFF[MAINHEAT_ENERGY_EFF == "Very Good"]),
            mainheat_good = length(MAINHEAT_ENERGY_EFF[MAINHEAT_ENERGY_EFF == "Good"]),
            mainheat_average = length(MAINHEAT_ENERGY_EFF[MAINHEAT_ENERGY_EFF == "Average"]),
            mainheat_poor = length(MAINHEAT_ENERGY_EFF[MAINHEAT_ENERGY_EFF == "Poor"]),
            mainheat_verypoor = length(MAINHEAT_ENERGY_EFF[MAINHEAT_ENERGY_EFF == "Very Poor"]),
            mainheat_other = length(MAINHEAT_ENERGY_EFF[!MAINHEAT_ENERGY_EFF %in% c("Very Good","Good","Average","Poor","Very Poor")]),
            
            mainheatdesc_gasboiler = length(MAINHEAT_DESCRIPTION[MAINHEAT_DESCRIPTION %in% c("boiler, underfloor heating, mains gas", "boiler, radiators, mains gas") ]),
            mainheatdesc_oilboiler = length(MAINHEAT_DESCRIPTION[MAINHEAT_DESCRIPTION %in% c("boiler, underfloor heating, oil","boiler, radiators, oil")]),
            mainheatdesc_storageheater = length(MAINHEAT_DESCRIPTION[grepl("electric storage heaters",MAINHEAT_DESCRIPTION)]),
            mainheatdesc_portableheater = length(MAINHEAT_DESCRIPTION[grepl("electric heaters",MAINHEAT_DESCRIPTION)]),
            mainheatdesc_roomheater = length(MAINHEAT_DESCRIPTION[grepl("room heaters",MAINHEAT_DESCRIPTION)]),
            mainheatdesc_heatpump = length(MAINHEAT_DESCRIPTION[grepl("heat pump",MAINHEAT_DESCRIPTION)]),
            mainheatdesc_community = length(MAINHEAT_DESCRIPTION[grepl("community",MAINHEAT_DESCRIPTION)]),
            
            mainfuel_mainsgas = length(fuel[fuel == "mains gas"]),
            mainfuel_electric = length(fuel[grepl("electric",fuel)]),
            mainfuel_oil = length(fuel[fuel == "oil" | grepl("b30",fuel)] ),
            mainfuel_coal = length(fuel[grepl("coal",fuel) | grepl("anthracite",fuel)]),
            mainfuel_lpg = length(fuel[grepl("lpg",fuel)]),
            mainfuel_biomass = length(fuel[grepl("wood",fuel) | grepl("biomass",fuel)]),
            
            mainheatcontrol_verygood = length(MAINHEATC_ENERGY_EFF[MAINHEATC_ENERGY_EFF == "Very Good"]),
            mainheatcontrol_good = length(MAINHEATC_ENERGY_EFF[MAINHEATC_ENERGY_EFF == "Good"]),
            mainheatcontrol_average = length(MAINHEATC_ENERGY_EFF[MAINHEATC_ENERGY_EFF == "Average"]),
            mainheatcontrol_poor = length(MAINHEATC_ENERGY_EFF[MAINHEATC_ENERGY_EFF == "Poor"]),
            mainheatcontrol_verypoor = length(MAINHEATC_ENERGY_EFF[MAINHEATC_ENERGY_EFF == "Very Poor"]),
            mainheatcontrol_other = length(MAINHEATC_ENERGY_EFF[!MAINHEATC_ENERGY_EFF %in% c("Very Good","Good","Average","Poor","Very Poor")]),
            
            has_solarpv = length(PHOTO_SUPPLY[PHOTO_SUPPLY > 0 ]), 
            has_solarthermal = length(SOLAR_WATER_HEATING_FLAG[SOLAR_WATER_HEATING_FLAG]),
            
            )

cert_summ$type_other <- cert_summ$epc_total - rowSums(cert_summ[,grepl("type_",names(cert_summ))])
cert_summ$mainheatdesc_other <- cert_summ$epc_total - rowSums(cert_summ[,grepl("mainheatdesc_",names(cert_summ))])

cert_summ <- cert_summ[,c("LSOA11","epc_total","epc_newbuild",
                          "epc_A","epc_B","epc_C","epc_D","epc_E","epc_F","epc_G","epc_other",
                          "epc_score_avg",
                          "type_house_semi","type_house_midterrace","type_house_endterrace",
                          "type_house_detached","type_flat","type_bungalow_semi",
                          "type_bungalow_midterrace","type_bungalow_endterrace",
                          "type_bungalow_detached","type_maisonette","type_parkhome","type_other",
                          "floor_area_avg","low_energy_light",
                          "floor_verygood","floor_good","floor_average","floor_poor","floor_verypoor","floor_below","floor_other",
                          "window_verygood","window_good","window_average","window_poor","window_verypoor","window_other",
                          "wall_verygood","wall_good","wall_average","wall_poor","wall_verypoor","wall_other",
                          "roof_verygood","roof_good","roof_average","roof_poor","roof_verypoor","roof_above","roof_other",
                          "mainheat_verygood","mainheat_good","mainheat_average","mainheat_poor","mainheat_verypoor","mainheat_other",
                          "mainheatdesc_gasboiler","mainheatdesc_oilboiler","mainheatdesc_storageheater",
                          "mainheatdesc_portableheater","mainheatdesc_roomheater","mainheatdesc_heatpump",
                          "mainheatdesc_community","mainheatdesc_other",
                          "mainfuel_mainsgas","mainfuel_electric","mainfuel_oil","mainfuel_coal",
                          "mainfuel_lpg","mainfuel_biomass",
                          "mainheatcontrol_verygood","mainheatcontrol_good","mainheatcontrol_average","mainheatcontrol_poor","mainheatcontrol_verypoor","mainheatcontrol_other",
                          "has_solarpv","has_solarthermal")]


saveRDS(cert_summ, "epc_lsoa_summary.Rds")

