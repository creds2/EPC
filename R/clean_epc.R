ncores = 30
certs <- readRDS("epc_all_raw.Rds")

library(future)
library(future.apply)

future::plan("future::multisession", workers = ncores)


# long strings

# Time consuming parts ----------------------------------------------------

certs$FLOOR_DESCRIPTION <- future_sapply(certs$FLOOR_DESCRIPTION,splitwelsh,  USE.NAMES = FALSE)
certs$FLOOR_DESCRIPTION <- future_sapply(certs$FLOOR_DESCRIPTION,standardclean,  USE.NAMES = FALSE)
certs$FLOOR_DESCRIPTION <- future_sapply(certs$FLOOR_DESCRIPTION,translatewelsh,  USE.NAMES = FALSE)
certs$FLOOR_DESCRIPTION <- future_sapply(certs$FLOOR_DESCRIPTION,fix_wm2k,  USE.NAMES = FALSE)

certs$WALLS_DESCRIPTION <- future_sapply(certs$WALLS_DESCRIPTION, splitwelsh,  USE.NAMES = FALSE)
certs$WALLS_DESCRIPTION <- future_sapply(certs$WALLS_DESCRIPTION, standardclean,  USE.NAMES = FALSE)
certs$WALLS_DESCRIPTION <- future_sapply(certs$WALLS_DESCRIPTION, translatewelsh,  USE.NAMES = FALSE)
certs$WALLS_DESCRIPTION <- future_sapply(certs$WALLS_DESCRIPTION,fix_wm2k,  USE.NAMES = FALSE)

certs$ROOF_DESCRIPTION <- future_sapply(certs$ROOF_DESCRIPTION, splitwelsh,  USE.NAMES = FALSE)
certs$ROOF_DESCRIPTION <- future_sapply(certs$ROOF_DESCRIPTION, standardclean,  USE.NAMES = FALSE)
certs$ROOF_DESCRIPTION <- future_sapply(certs$ROOF_DESCRIPTION, translatewelsh,  USE.NAMES = FALSE)
certs$ROOF_DESCRIPTION <- future_sapply(certs$ROOF_DESCRIPTION,fix_wm2k,  USE.NAMES = FALSE)

certs$MAINHEATCONT_DESCRIPTION <- future_sapply(certs$MAINHEATCONT_DESCRIPTION, standardclean,  USE.NAMES = FALSE)
certs$MAINHEATCONT_DESCRIPTION <- future_sapply(certs$MAINHEATCONT_DESCRIPTION, translatewelsh,  USE.NAMES = FALSE)

certs$MAINHEAT_DESCRIPTION <- future_sapply(certs$MAINHEAT_DESCRIPTION, splitwelsh,  USE.NAMES = FALSE)
certs$MAINHEAT_DESCRIPTION <- future_sapply(certs$MAINHEAT_DESCRIPTION, standardclean,  USE.NAMES = FALSE)
certs$MAINHEAT_DESCRIPTION <- future_sapply(certs$MAINHEAT_DESCRIPTION, translatewelsh,  USE.NAMES = FALSE)

certs$HOTWATER_DESCRIPTION <- future_sapply(certs$HOTWATER_DESCRIPTION, splitwelsh,  USE.NAMES = FALSE)
certs$HOTWATER_DESCRIPTION <- future_sapply(certs$HOTWATER_DESCRIPTION, standardclean,  USE.NAMES = FALSE)
certs$HOTWATER_DESCRIPTION <- future_sapply(certs$HOTWATER_DESCRIPTION, translatewelsh,  USE.NAMES = FALSE)

certs$SECONDHEAT_DESCRIPTION <- future_sapply(certs$SECONDHEAT_DESCRIPTION, splitwelsh,  USE.NAMES = FALSE)
certs$SECONDHEAT_DESCRIPTION <- future_sapply(certs$SECONDHEAT_DESCRIPTION, standardclean,  USE.NAMES = FALSE)
certs$SECONDHEAT_DESCRIPTION <- future_sapply(certs$SECONDHEAT_DESCRIPTION, translatewelsh,  USE.NAMES = FALSE)

certs$LIGHTING_DESCRIPTION <- future_sapply(certs$LIGHTING_DESCRIPTION,splitwelsh,  USE.NAMES = FALSE)
certs$LIGHTING_DESCRIPTION <- future_sapply(certs$LIGHTING_DESCRIPTION,standardclean,  USE.NAMES = FALSE)

certs$WINDOWS_DESCRIPTION <- future_sapply(certs$WINDOWS_DESCRIPTION, splitwelsh,  USE.NAMES = FALSE)
certs$WINDOWS_DESCRIPTION <- future_sapply(certs$WINDOWS_DESCRIPTION, standardclean,  USE.NAMES = FALSE)
certs$WINDOWS_DESCRIPTION <- future_sapply(certs$WINDOWS_DESCRIPTION, translatewelsh,  USE.NAMES = FALSE)

certs$FLOOR_LEVEL <- future_sapply(certs$FLOOR_LEVEL, standardclean,  USE.NAMES = FALSE)

certs$TRANSACTION_TYPE <- future_sapply(certs$TRANSACTION_TYPE, standardclean,  USE.NAMES = FALSE)

certs$MAIN_FUEL <- future_sapply(certs$MAIN_FUEL, standardclean,  USE.NAMES = FALSE)



# FLOOR DeSCRIPTION -------------------------------------------------------

sub_FLOOR_DESCRIPTION("uninsulated","no insulation")

td_FLOOR_DESCRIPTION("other premises below","(other premises below)")
td_FLOOR_DESCRIPTION(", no insulation (assumed)","no insulation (assumed)")
td_FLOOR_DESCRIPTION(", insulated (assumed)","insulated (assumed)")
td_FLOOR_DESCRIPTION(", limited insulation (assumed)","limited insulation (assumed)")
td_FLOOR_DESCRIPTION("above unheated space","to unheated space,")
td_FLOOR_DESCRIPTION("to unheated space, limited insulation (assumed)insulated","to unheated space, limited insulation (assumed)")
td_FLOOR_DESCRIPTION("sap05:floor",NA)

FLOOR_DESCRIPTION <- c("(another dwelling below)",
                       "(other premises below)",
                       "(same dwelling below) insulated (assumed)",
                       
                       paste0(format(seq(0.00, 5.00, 0.01),digits = 2)," w/m2k"),
                       paste0(format(c(7.37, 18.00, 127.35),digits = 2)," w/m2k"),
                       "0.089 w/m2k",
                       "103.90 w/m2k",
                       "16.72 w/m2k",
                       "25.42 w/m2k",
                       "5.50 w/m2k",
                       "7.37 w/m2k",
                       "solid,",
                       "solid, insulated",
                       "solid, insulated (assumed)",
                       "solid, limited insulation (assumed)",
                       "solid, no insulation (assumed)",
                       "suspended,",
                       "suspended, insulated",
                       "suspended, insulated (assumed)",
                       "suspended, limited insulation (assumed)",
                       "suspended, no insulation (assumed)",
                       "to external air, insulated",
                       "to external air, insulated (assumed)",
                       "to external air, no insulation (assumed)",
                       "to external air, limited insulation (assumed)",
                       "to unheated space,",
                       "to unheated space, insulated",
                       "to unheated space, insulated (assumed)",
                       "to unheated space, no insulation (assumed)",
                       "to unheated space, limited insulation (assumed)",
                       "conservatory",
                       "no insulation (assumed)",
                       "insulated",
                       "insulated (assumed)",
                       "limited insulation (assumed)",
                       NA)


validate(FLOOR_DESCRIPTION, "FLOOR_DESCRIPTION")


# WALLS_DESCRIPTION -------------------------------------------------------

sub_WALLS_DESCRIPTION("granite or whin,","granite or whinstone,")

td_WALLS_DESCRIPTION("cob, internal","cob, with internal insulation")
td_WALLS_DESCRIPTION("cobbuilt","cob,")
td_WALLS_DESCRIPTION("sap05:walls" ,NA)
td_WALLS_DESCRIPTION("cavity wall, as built, insulated (assumed)as built, no insulation (assumed)" ,"cavity wall, as built, insulated (assumed)")
td_WALLS_DESCRIPTION("system built, external","system built, with external insulation")

WALLS_DESCRIPTION <- c(paste0(format(seq(0.00, 5.00, 0.01),digits = 2)," w/m2k"),
                       paste0(format(c(23.31, 6.26, 62.37, 68.50,9.00),digits = 3)," w/m2k"),
                       ", as built, no insulation (assumed)",
                       ", with external insulation",
                       "32.34 w/m2k",
                       "9.49 w/m2k", 
                       "6.26 w/m2k", 
                       "9.00 w/m2k", 
                       "cavity wall,",
                       "cavity wall, as built, insulated (assumed)",
                       "cavity wall, as built, no insulation (assumed)",
                       "cavity wall, as built, partial insulation (assumed)",
                       "cavity wall, filled cavity",
                       "cavity wall, filled cavity and external insulation",
                       "cavity wall, filled cavity and internal insulation",
                       "cavity wall, insulated (assumed)",
                       "cavity wall, no insulation (assumed)",
                       "cavity wall, partial insulation (assumed)",
                       "cavity wall, with external insulation",
                       "cavity wall, with internal insulation",
                       "cob,",        
                       "cob, as built",
                       "cob, as built",
                       "cob, filled cavity",
                       "cob, filled cavity and internal insulation",
                       "cob, with external insulation",
                       "cob, with internal insulation",
                       "granite or whinstone,",
                       "granite or whinstone, as built, insulated (assumed)",
                       "granite or whinstone, as built, no insulation (assumed)",
                       "granite or whinstone, as built, partial insulation (assumed)",
                       "granite or whinstone, filled cavity",
                       "granite or whinstone, with external insulation",
                       "granite or whinstone, with internal insulation",
                       "park home wall, as built",
                       "park home wall, with external insulation",
                       "park home wall, with internal insulation",
                       "sandstone or limestone, as built, insulated (assumed)",
                       "sandstone or limestone, as built, no insulation (assumed)",
                       "sandstone or limestone, as built, partial insulation (assumed)",
                       "sandstone or limestone, with external insulation",
                       "sandstone or limestone, with internal insulation",
                       "sandstone,",
                       "sandstone, as built, insulated (assumed)",
                       "sandstone, as built, no insulation (assumed)",
                       "sandstone, as built, partial insulation (assumed)",
                       "sandstone, filled cavity",
                       "sandstone, with external insulation",
                       "sandstone, with internal insulation",
                       "solid brick, as built, insulated (assumed)",
                       "solid brick, as built, no insulation (assumed)",
                       "solid brick, as built, partial insulation (assumed)",
                       "solid brick, filled cavity",
                       "solid brick, with external insulation",
                       "solid brick, with internal insulation",
                       "system built, as built, insulated (assumed)",
                       "system built, as built, no insulation (assumed)",
                       "system built, as built, partial insulation (assumed)",
                       "system built, filled cavity",
                       "system built, filled cavity and external insulation",
                       "system built, filled cavity and internal insulation",
                       "system built, with external insulation",
                       "system built, with internal insulation",
                       "timber frame, as built, insulated (assumed)",
                       "timber frame, as built, no insulation (assumed)",
                       "timber frame, as built, partial insulation (assumed)",
                       "timber frame, filled cavity",
                       "timber frame, filled cavity and external insulation",
                       "timber frame, filled cavity and internal insulation",
                       "timber frame, with additional insulation",
                       "timber frame, with external insulation",
                       "timber frame, with internal insulation",
                       NA  )

validate(WALLS_DESCRIPTION, "WALLS_DESCRIPTION")

# ROOF_DESCRIPTION --------------------------------------------------------

sub_ROOF_DESCRIPTION("0mm loft insulation","0 mm loft insulation")
sub_ROOF_DESCRIPTION("2mm loft insulation","2 mm loft insulation")
sub_ROOF_DESCRIPTION("5mm loft insulation","5 mm loft insulation")
sub_ROOF_DESCRIPTION("0+mm loft insulation","0+ mm loft insulation")
sub_ROOF_DESCRIPTION("pitched, 0 mm loft insulation","pitched, no insulation")
sub_ROOF_DESCRIPTION("pitched, loft insulation","pitched, insulated")
sub_ROOF_DESCRIPTION("insulation(assumed)","insulation (assumed)")
sub_ROOF_DESCRIPTION(" mm mm "," mm ")
sub_ROOF_DESCRIPTION("don't know","unknown")
sub_ROOF_DESCRIPTION("yes loft insulation","unknown loft insulation")
sub_ROOF_DESCRIPTION("joists loft insulation","unknown loft insulation")
sub_ROOF_DESCRIPTION("thatchedinsulated","thatched insulated")

td_ROOF_DESCRIPTION("pitched, *** invalid input code : 57 *** loft insulation","pitched")
td_ROOF_DESCRIPTION("pitched","pitched,")
td_ROOF_DESCRIPTION("flat","flat,")
td_ROOF_DESCRIPTION("other premises above","(other premises above)")
td_ROOF_DESCRIPTION("pitched, >=300 mm loft insulation","pitched, 300+ mm loft insulation")
td_ROOF_DESCRIPTION(c("nan w/m2k","sap05:roof"),NA)
td_ROOF_DESCRIPTION("(other dwelling above)","(another dwelling above)")
td_ROOF_DESCRIPTION("roof room(s), thatched with additional insulation","roof room(s), thatched, with additional insulation")
td_ROOF_DESCRIPTION("roof room(s), insulated at rafters","roof room(s), insulated")
td_ROOF_DESCRIPTION("roof room(s)","roof room(s),")
td_ROOF_DESCRIPTION("pitched, 0 mm loft insulation","pitched, no insulation")
td_ROOF_DESCRIPTION("pitched, mm loft insulation","pitched, unknown loft insulation")
td_ROOF_DESCRIPTION("pitched, loft insulation","pitched, insulated")

ROOF_DESCRIPTION <- c("(another dwelling above)",
                      "(other premises above)",
                      paste0(format(seq(0.00, 5.05, 0.01),digits = 2)," w/m2k"),
                      "127.35 w/m2k",
                      "18.00 w/m2k",
                      "47.01 w/m2k",
                      "flat, insulated",
                      "flat, insulated (assumed)",
                      "flat, limited insulation",
                      "flat, limited insulation (assumed)",
                      "flat, no insulation","flat, no insulation (assumed)",
                      "flat,",
                      "pitched,",
                      "pitched, 1 mm loft insulation",
                      "pitched, 11 mm loft insulation",
                      "pitched, 12 mm loft insulation",
                      "pitched, 25 mm loft insulation",
                      "pitched, 50 mm loft insulation",
                      "pitched, 75 mm loft insulation",
                      "pitched, 100 mm loft insulation",
                      "pitched, 150 mm loft insulation",
                      "pitched, 150+ mm loft insulation",
                      "pitched, 200 mm loft insulation",
                      "pitched, 250 mm loft insulation",
                      "pitched, 270 mm loft insulation",
                      "pitched, 300 mm loft insulation",
                      "pitched, 300+ mm loft insulation",
                      "pitched, 350 mm loft insulation",
                      "pitched, 400 mm loft insulation",
                      "pitched, 400+ mm loft insulation",
                      "pitched, insulated",
                      "pitched, insulated (assumed)",
                      "pitched, insulated at rafters",
                      "pitched, limited insulation",
                      "pitched, limited insulation (assumed)",
                      "pitched, no insulation",
                      "pitched, no insulation (assumed)",
                      "pitched, unknown loft insulation",
                      "pitched, flat roof insulation loft insulation",
                      "roof room(s),",
                      "roof room(s), ceiling insulated",
                      "roof room(s), insulated",
                      "roof room(s), insulated (assumed)",
                      "roof room(s), limited insulation",
                      "roof room(s), limited insulation (assumed)",
                      "roof room(s), no insulation",
                      "roof room(s), no insulation (assumed)",
                      "roof room(s), thatched",
                      
                      "roof room(s), thatched, with additional insulation",
                      "thatched, with additional insulation",
                      "thatched",
                      "thatched insulated at rafters",
                      "thatched insulated (assumed)",
                      NA)

validate(ROOF_DESCRIPTION, "ROOF_DESCRIPTION")


# MAIN_FUEL ---------------------------------------------------------------

sub_MAIN_FUEL(" this is for backwards compatibility only and should not be used"," (unknown)")
td_MAIN_FUEL("to be used only when there is no heating/hot-water system","no heating/hot-water system")
td_MAIN_FUEL("community heating schemes: heat from boilers biomass","community heating schemes: heat from boilers - biomass")

MAIN_FUEL <- c("anthracite",
               "appliances able to use mineral oil or liquid biofuel",
               "biogas (community)",
               "biogas landfill (unknown)",
               "biomass (community)",
               "biomass (unknown)",
               "bioethanol",
               "biodiesel from any biomass source",
               "biodiesel from used cooking oil only",
               "rapeseed oil",
               "community heating schemes: heat from boilers - biomass",
               "community heating schemes: waste heat from power stations",
               "community heating schemes: heat from heat pump",
               "bulk wood pellets",
               "dual fuel mineral + wood",
               "electric (unknown)", 
               "electric (community)",
               "electric (not community)",
               "electric: electric, unspecified tariff",
               "coal (community)",
               "coal (not community)",
               "coal (unknown)",
               "lpg (unknown)",
               "lpg (not community)",
               "lpg (community)",
               "lpg special condition",
               "bottled lpg",
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
               "b30d (community)",
               "b30k (not community)",
               NA)

validate(MAIN_FUEL,"MAIN_FUEL")

# MAINHEATCONT_DESCRIPTION ------------------------------------------------

sub_MAINHEATCONT_DESCRIPTION("least 2","least two")
sub_MAINHEATCONT_DESCRIPTION("communit heating","community heating")
sub_MAINHEATCONT_DESCRIPTION("charging linked to use,","charging system linked to use of community heating,")

td_MAINHEATCONT_DESCRIPTION("flat rate charging, no stat control of room temperature","flat rate charging, no thermostatic control of room temperature")
td_MAINHEATCONT_DESCRIPTION("flat rate charging, programmer, no room thermostat","flat rate charging, programmer no room thermostat")
td_MAINHEATCONT_DESCRIPTION("charging system linked to the use of community heating, prog and trvs","charging system linked to use of community heating, programmer and trvs")
td_MAINHEATCONT_DESCRIPTION("charging system linked to the use of community heating, programmer and trvs","charging system linked to use of community heating, programmer and trvs")
td_MAINHEATCONT_DESCRIPTION("no time or thermostatic control of room temp","no time or thermostatic control of room temperature")
td_MAINHEATCONT_DESCRIPTION("not applicable (boiler provides dhw only)","not relevant (supplies dhw only)")
td_MAINHEATCONT_DESCRIPTION("flat rate charging, no thermostatic control of room temperature","flat rate charging, no thermostatic control")
td_MAINHEATCONT_DESCRIPTION("flat rate charging, programmer and room stat","flat rate charging, no thermostatic control")
td_MAINHEATCONT_DESCRIPTION("programmer, roomstat and trvs","programmer, room thermostat and trvs")
td_MAINHEATCONT_DESCRIPTION("delayed start stat & program & trv's","delayed start thermostat, programmer and trvs")
td_MAINHEATCONT_DESCRIPTION("charging system linked to use of community heating, programmer?and trvs","charging system linked to use of community heating, programmer and trvs")
td_MAINHEATCONT_DESCRIPTION("flat rate charging*, programmer and trvs","flat rate charging, programmer and trvs")
td_MAINHEATCONT_DESCRIPTION(c("celect control","celect controls"),"celect-type controls")
td_MAINHEATCONT_DESCRIPTION("charging system linked to use of community heating, programmer and at least two room stats","charging system linked to use of community heating, programmer and at least two room thermostats")
td_MAINHEATCONT_DESCRIPTION("2207 time and temperature zone control","time and temperature zone control")
td_MAINHEATCONT_DESCRIPTION(c("charging system linked to use of community heating, programmerand room thermostat","charging system linked to use of communit heating, programmer and room thermostat"),"charging system linked to use of community heating, programmer and room thermostat")
td_MAINHEATCONT_DESCRIPTION("programmer and at least two room thermostat","programmer and at least two room thermostats")
td_MAINHEATCONT_DESCRIPTION("flat rate charging trvs","flat rate charging, trvs")
td_MAINHEATCONT_DESCRIPTION("thermostat, programmer and trvs","programmer, room thermostat and trvs")
td_MAINHEATCONT_DESCRIPTION("celect-type controls","celect-type controls")
td_MAINHEATCONT_DESCRIPTION("no time or thermostatic control of temperature","no time or thermostatic control of room temperature")
td_MAINHEATCONT_DESCRIPTION("programmer, no thermostat","programmer, no room thermostat")
td_MAINHEATCONT_DESCRIPTION(c("sap05:main-heating-controls",
                              "sap:main-heating-controls",
                              "????????????????",
                              ", ??????",
                              "???????????????????????????????????",
                              "????????????????????????????????????????????????"),NA)
td_MAINHEATCONT_DESCRIPTION("thermostat and programmer","programmer and room thermostat")
td_MAINHEATCONT_DESCRIPTION("trv's, program and flow switch","programmer, trvs and flow switch")
td_MAINHEATCONT_DESCRIPTION("appliance thermostat","appliance thermostats")
td_MAINHEATCONT_DESCRIPTION("delayed start stat and program and trv's","delayed start thermostat, programmer and trvs")
td_MAINHEATCONT_DESCRIPTION("charging system linked to community heating use, programmer and trvs","charging system linked to community heating, programmer and trvs")
td_MAINHEATCONT_DESCRIPTION("dim","none")

MAINHEATCONT_DESCRIPTION = c("appliance thermostats",
                             "appliance thermostat and programmer",
                             "automatic charge control",
                             "charging system linked to use of community heating, programmer",
                             "charging system linked to use of community heating, programmer and at least two room thermostats",
                             "charging system linked to use of community heating, programmer and room thermostat",
                             "charging system linked to use of community heating, programmer and trvs",
                             "charging system linked to use of community heating, room thermostat only",
                             "charging system linked to use of community heating, trvs",
                             "controls for high heat retention storage heaters",
                             "delayed start thermostat, programmer and trvs",
                             "delayed start thermostat and programmer",
                             "delayed start programmer and at least two room thermostats",
                             "flat rate charging, programmer and at least two room thermostats",
                             "flat rate charging, programmer and room thermostat",
                             "flat rate charging, programmer no room thermostat",
                             "flat rate charging, programmer and trvs",
                             "flat rate charging, room thermostat only",
                             "flat rate charging, trvs",
                             "flat rate charging, no thermostatic control",
                             "manual charge control",
                             "no thermostatic control of room temperature",
                             "no time or thermostatic control of room temperature",
                             "none",
                             "not relevant (supplies dhw only)",
                             "programmer and appliance thermostats",
                             "programmer and at least two room thermostats",
                             "programmer and at least two room thermostats including a delayed start thermostat",
                             "programmer and delayed start thermostat",
                             "programmer and room thermostat",
                             "programmer and room thermostats",
                             "programmer, no room thermostat",
                             "programmer, room thermostat and trvs",
                             "programmer, trvs and boiler energy manager",
                             "programmer, trvs and bypass",
                             "programmer, trvs and flow switch",
                             "room thermostat only",
                             "room thermostats only",
                             "time and temperature zone control",
                             "time and temperature zone control by suitable arrangement of plumbing and electrical services",
                             "temperature zone control",
                             "trvs and bypass",
                             "unit charging, programmer and trvs",
                             "celect-type controls",
                             NA)

validate(MAINHEATCONT_DESCRIPTION, "MAINHEATCONT_DESCRIPTION")


# MAINHEAT_DESCRIPTION ----------------------------------------------------

sub_mainheat_description("ratiators","radiators")
sub_mainheat_description(", underfloor,",", underfloor heating,")
sub_mainheat_description("sourceheat","source heat")
sub_mainheat_description("boiler, underfloor,","boiler, underfloor heating,")
sub_mainheat_description("boiler and underfloor,","boiler, underfloor heating,")
sub_mainheat_description("full double glazed,","")
sub_mainheat_description(" and underfloor heating",", underfloor heating")
sub_mainheat_description(" and radiators",", radiators")
sub_mainheat_description(" bulk "," ")
sub_mainheat_description("solar assisted source","solar assisted")
sub_mainheat_description("solar-assisted","solar assisted")
sub_mainheat_description("boiler and ,","boiler and,")
sub_mainheat_description("secondary wood pellets","wood pellets")

sub_mainheat_description(c("full double glazed",
                           "full secondary glazing",
                           "partial double glazing",
                           "single glazed"),"")



td_mainheat_description("portable electric heating assumed for most rooms","portable electric heaters assumed for most rooms")
td_mainheat_description("warm air, electricaire","warm air, electric")
td_mainheat_description("community, community","community scheme")
td_mainheat_description("boiler, dual fuel (mineral and wood)","boiler, radiators, dual fuel (mineral and wood)") # guess these are the same

td_mainheat_description(c("no system present: electric heaters assumed, mains gas",
                          "no system present: electric heaters assumed, electric",
                          "no system present: electric heaters assumed, electicity",
                          "no system present: electric heaters assumed, radiators, electric"
),"no system present: electric heaters assumed")

td_mainheat_description("boiler, radiators, lpg (bottled)","boiler, radiators, bottled lpg"   )                                                                       
td_mainheat_description("electric ceiling, electric","electric ceiling heating")

td_mainheat_description("air source heat pump , electric","air source heat pump, electric")
td_mainheat_description("boiler, underfloor, oil","boiler, underfloor heating, oil")
td_mainheat_description("electric ceiling heating, electric","electric ceiling heating")
td_mainheat_description("boiler, radiators, heat from boilers - mains gas","boiler, radiators, mains gas")
td_mainheat_description("boiler, radiators, bulk lpg","boiler, radiators, lpg")
td_mainheat_description("air source heat pump, , electric","air source heat pump, electric")
td_mainheat_description("ground source heat pump , electric","ground source heat pump, electric")
td_mainheat_description("ground source heat pump, systems with radiators, electric","ground source heat pump, radiators, electric")

td_mainheat_description("water source heat pump, warm air, electric","water source heat pump, warm air, electric")
td_mainheat_description("boiler and underfloor, electric","boiler, underfloor heating, electric")
td_mainheat_description("boiler, underfloor, lpg","boiler, underfloor heating, lpg")
td_mainheat_description("boiler, coal","boiler, radiators, coal")
td_mainheat_description("boiler, underfloor, dual fuel (mineral and wood)","boiler, underfloor heating, dual fuel (mineral and wood)")
td_mainheat_description("warm air heat pump, electric","warm air, heat pump, electric")
td_mainheat_description("ground source heat pump, , electric","ground source heat pump, electric")
td_mainheat_description(c("portable electric heating assumed for most rooms, electric","portable electric heating assumed for most rooms"),"portable electric heaters assumed for most rooms")
td_mainheat_description("electric storage, electric","electric storage heaters")
td_mainheat_description("electric ceiling","electric ceiling heating")
td_mainheat_description("air source heat pump fan coil units, electric","air source heat pump, fan coil units, electric")
td_mainheat_description("boiler, smokeless fuel","boiler, radiators, smokeless fuel")
td_mainheat_description("community scheme with chp, waste combustion and waste combustion","community scheme, waste combustion")
td_mainheat_description("ground-to-water heat pump with auxiliary heater (electric) source heat pump, radiators, electric","ground source heat pump, radiators, electric")
td_mainheat_description("air source heat pump, warm air, electric","air source heat pump, warm air, electric")
td_mainheat_description("ground source heat pump, radiators, electricity","ground source heat pump, radiators, electric")
td_mainheat_description("boiler, radiators, bulk wood pellets","boiler, radiators, wood pellets")
td_mainheat_description("air source heat pump, radiators, electricity","air source heat pump, radiators, electric")
td_mainheat_description("air source heat pump, systems with radiators, electric","air source heat pump, radiators, electric")
td_mainheat_description("boiler, radiators, electricity","boiler, radiators, electric")
td_mainheat_description("boiler, radiators, main wood pellets","boiler, radiators, wood pellets")
td_mainheat_description("community scheme, wood pellets and mains gas","community scheme, mains gas and wood pellets")
td_mainheat_description(c(", underfloor heating, electric",", electric underfloor heating"),"electric underfloor heating")

td_mainheat_description(c("boiler, radiators, dual fuel appliance","boiler, radiators, dual fuel appliance (mineral and wood)"),"boiler, radiators, dual fuel (mineral and wood)")
td_mainheat_description("electric boiler, electric","boiler, electric")
td_mainheat_description(c("sap05:main-heating",""),NA)
td_mainheat_description("boiler, radiators","boiler, radiators,")

td_mainheat_description("boiler, radiators, heat from boilers mains gas","boiler, radiators, mains gas")
td_mainheat_description("boiler, underfloor heating, dual fuel appliance","boiler, underfloor heating, dual fuel (mineral and wood)")
td_mainheat_description("community heat pump","community heat pump,")
td_mainheat_description("community scheme with chp, mains gas","community scheme with chp and mains gas")
td_mainheat_description("oil boiler, oil","boiler, radiators, oil")

td_mainheat_description(c("radiator heating, heat from boilers gas",
                          "radiator heating, mains gas"),"boiler, radiators, mains gas")

td_mainheat_description(c("community heap pump",
                          "community heat pump, heat pump"),"community heap pump,")

td_mainheat_description(c("community heap pump",
                          "community heat pump, heat pump"),"community heap pump,")

td_mainheat_description("community heat pump, underfloor heating, heat pump","community heat pump, underfloor heating")

td_mainheat_description("community scheme, biomass and mains gas","community scheme, mains gas and biomass")

td_mainheat_description("electric storage heaters, underfloor","electric storage heaters, underfloor heating")

td_mainheat_description("room heaters","room heaters,")

td_mainheat_description("room heaters, dual fuel appliance","room heaters, dual fuel")
td_mainheat_description("room heaters, main wood pellets","room heaters, wood pellets")




MAINHEAT_DESCRIPTION = c(", electric",
                         ", gas",
                         ", mains gas",
                         ", oil",
                         ", underfloor, electric",
                         ", wood pellets",
                         " warm air, electric",
                         ", radiators, electric",
                         ", underfloor",                                                                                
                         ", warm air, electric" ,
                         "underfloor heating, mains gas",
                         "air source heat pump",
                         "air source heat pump, mains gas",
                         "air source heat pump, radiators, b30k",
                         "air source heat pump, radiators, lpg",
                         "air source heat pump, underfloor heating, b30k",
                         "air source heat pump, underfloor heating, bioethanol",
                         "air source heat pump, underfloor heating, bottled lpg",
                         "air source heat pump, electric",
                         "air source heat pump, radiators, oil" ,
                         "air source heat pump, fan coil units, electric",
                         "air source heat pump, fan coil units, mains gas",
                         "air source heat pump, radiators,",
                         "air source heat pump, radiators, electric",
                         "air source heat pump, radiators, mains gas",
                         "air source heat pump, underfloor heating, radiators, pipes in concrete slab, electric",
                         "air source heat pump, underfloor heating, radiators, pipes in insulated timber floor, electric",
                         "air source heat pump, underfloor heating, radiators, pipes in screed above insulation, electric",
                         "air source heat pump, underfloor heating, dual fuel (mineral and wood)" ,
                         "air source heat pump, underfloor heating, electric",
                         "air source heat pump, underfloor heating, lpg",
                         "air source heat pump, underfloor heating, mains gas",
                         "air source heat pump, underfloor heating, oil",
                         "air source heat pump, underfloor heating, pipes in concrete slab, electric",
                         "air source heat pump, underfloor heating, pipes in insulated timber floor, electric",
                         "air source heat pump, underfloor heating, pipes in screed above insulation, electric",
                         "air source heat pump, underfloor heating, wood pellets",
                         "air source heat pump, warm air, electric",
                         "air source heat pump, warm air, mains gas",
                         "back boiler to radiators source heat pump, radiators, electric",
                         "boiler and, coal",
                         "boiler and, dual fuel (mineral and wood)",
                         "boiler and, mains gas",
                         "boiler and, electric",
                         "boiler and, mains gas",
                         "boiler and, smokeless fuel",
                         "boiler and fan coil units, mains gas",
                         "boiler and fan coil units, wood logs",
                         "boiler, wood chips",
                         "boiler, wood pellets",
                         "boiler, radiators,",
                         "boiler, radiators, anthracite",
                         "boiler, radiators, b30k",
                         "boiler, radiators, bioethanol",
                         "boiler, radiators, biomass",
                         "boiler, radiators, appliances able to use mineral oil or liquid biofuel",                                    
                         "boiler, radiators, biogas",
                         "boiler, radiators, bottled gas",
                         "boiler, radiators, bottled lpg",
                         "boiler, radiators, coal",
                         "boiler, radiators, dual fuel (mineral and wood)",
                         "boiler, radiators, electric",
                         "boiler, radiators, electric (24-hr heating tariff)",
                         "boiler, radiators, liquid biofuel",
                         "boiler, radiators, lpg",
                         "boiler, radiators, lpg subject to special condition 18",
                         "boiler, radiators, mains gas",
                         "boiler, radiators, oil",
                         "boiler, radiators, smokeless fuel",
                         "boiler, radiators, wood chips",
                         "boiler, radiators, wood logs",
                         "boiler, radiators, wood pellets",
                         "boiler, underfloor heating,",
                         "boiler, underfloor heating, anthracite",
                         "boiler, underfloor heating, b30k",
                         "boiler, underfloor heating, bottled gas",
                         "boiler, underfloor heating, bottled lpg",
                         "boiler, underfloor heating, coal",
                         "boiler, underfloor heating, dual fuel (mineral and wood)",
                         "boiler, underfloor heating, electric",
                         "boiler, underfloor heating, electric (24-hr heating tariff)",
                         "boiler, underfloor heating, lpg",
                         "boiler, underfloor heating, lpg subject to special condition 18",
                         "boiler, underfloor heating, liquid biofuel",
                         "boiler, underfloor heating, mains gas",
                         "boiler, underfloor heating, oil",
                         "boiler, underfloor heating, smokeless fuel",
                         "boiler, underfloor heating, wood chips",
                         "boiler, underfloor heating, wood logs",
                         "boiler, underfloor heating, wood pellets",
                         "boiler, anthracite",
                         "boiler, electric",
                         "boiler, mains gas",
                         "boiler, wood logs",
                         "community heat pump,",
                         "community heat pump, electric",
                         "community heat pump, electric and mains gas",
                         "community heat pump, mains gas",
                         "community scheme",
                         "community scheme utilising geothermal heat and biomass",
                         "community scheme with chp",
                         
                         "community scheme with chp and electric",
                         "community scheme with chp and geothermal",
                         "community scheme with chp and mains gas",
                         "community scheme with chp and waste combustion",
                         "community scheme with chp and wood logs",
                         "community scheme, biomass",
                         "community scheme, mains gas",
                         "community scheme, mains gas and biomass",
                         "community scheme, mains gas and mains gas",
                         "community scheme, mains gas and wood pellets",
                         "community scheme, oil",
                         "community scheme, radiators, coal",
                         "community scheme, radiators, mains gas",
                         "community scheme, radiators, oil",
                         "community scheme, radiators, waste combustion",
                         "community scheme, biomass and oil",
                         "community scheme, coal",
                         "community scheme, lpg",
                         "community scheme, radiators, lpg",
                         "community scheme, smokeless fuel",
                         "community scheme, underfloor heating, mains gas",
                         "community scheme, waste combustion",
                         "community scheme, wood chips",
                         "community scheme, wood pellets",
                         "electric ceiling heating",
                         "electric ceiling, mains gas",
                         "electric ceiling heating, radiators, electric",
                         "electric ceiling heating, underfloor heating, electric",
                         "electric heat pumps, electric",
                         "electric storage heaters",
                         "electric storage heaters, radiators",
                         "electric underfloor heating",
                         "electric underfloor heating (standard tariff), electric",
                         "radiator heating, electric",
                         "radiator heating, electric (24-hr heating tariff)",
                         "exhaust air mev source heat pump, systems with radiators, electric",
                         "exhaust air mev source heat pump, electric",
                         "exhaust air mev source heat pump, underfloor heating, pipes in screed above insulation, electric",
                         "exhaust air mev source heat pump, underfloor heating, radiators, pipes in screed above insulation, electric",
                         "exhaust source heat pump, fan coil units, electric",
                         "exhaust source heat pump, underfloor heating, pipes in screed above insulation, electric",
                         "gas-fired heat pumps, electric",
                         "gas/lpg boiler 1998 or later, gas",
                         "gas/lpg boiler pre-1998 with balanced or open-flue, mains gas",
                         "gas/lpg boiler pre-1998, with fan-assisted flue, gas",
                         "ground source heat pump, electric",
                         "ground source heat pump, fan coil units, electric",
                         "ground source heat pump, radiators, electric",
                         "ground source heat pump, radiators, mains gas",
                         "ground source heat pump, underfloor heating, radiators, pipes in concrete slab, electric",
                         "ground source heat pump, underfloor heating, radiators, pipes in insulated timber floor, electric" ,
                         "ground source heat pump, underfloor heating, radiators, pipes in screed above insulation, electric",
                         "ground source heat pump, underfloor heating, pipes in insulated timber floor, electric",
                         "ground source heat pump, underfloor heating, pipes in screed above insulation, electric",
                         "ground source heat pump, underfloor heating, electric",
                         "ground source heat pump, underfloor heating, lpg",
                         "ground source heat pump, underfloor heating, mains gas",
                         "ground source heat pump, warm air, electric",
                         "ground source heat pump, radiators, oil",
                         "ground source heat pump, underfloor heating, pipes in concrete slab, electric",
                         "ground source heat pump, warm air, mains gas",
                         "heat pumpelectric",
                         "hot-water-only systems, electric",
                         "hot-water-only systems, gas",
                         "micro-cogeneration, lpg",
                         "micro-cogeneration, oil",
                         "micro-cogeneration, mains gas",
                         "no system present: electric heaters assumed",
                         "portable electric heaters",
                         "portable electric heaters assumed for most rooms",
                         "room heaters,",
                         "room heaters, anthracite",
                         "room heaters, bottled gas",
                         "room heaters, bottled lpg",
                         "room heaters, coal",
                         "room heaters, dual fuel",
                         "room heaters, dual fuel (mineral and wood)",
                         "room heaters, electric",
                         "room heaters, lpg",
                         "room heaters, mains gas",
                         "room heaters, oil",
                         "room heaters, radiators, electric",
                         "room heaters, radiators, oil",
                         "room heaters, smokeless fuel",
                         "room heaters, wood chips",
                         "room heaters, wood logs",
                         "room heaters, wood pellets",
                         "room heaters, b30k",
                         "room heaters, biogas landfill",
                         "room heaters, radiators, mains gas",
                         "room heaters, radiators, wood logs",
                         "room heaters, underfloor heating, dual fuel (mineral and wood)",
                         "room heaters, underfloor heating, wood logs",
                         "solid-fuel boiler, solid fuel",
                         "solar assisted heat pump, underfloor heating, electric",
                         "solar assisted heat pump, underfloor heating, pipes in insulated timber floor, electric",
                         "solar assisted heat pump, underfloor heating, radiators, pipes in screed above insulation, electric",
                         "warm air,",
                         "warm air, electric",
                         "warm air, heat pump, electric",
                         "warm air, lpg",
                         "warm air, mains gas",
                         "warm air, oil",
                         "warm air, b30k",
                         "warm air, bottled gas",
                         "warm air, bottled lpg",
                         "warm air, dual fuel (mineral and wood)",
                         "warm air, lpg (bottled)",
                         "warm air, radiators, mains gas",
                         "water source heat pump, electric",
                         "water source heat pump, fan coil units, electric",
                         "water source heat pump, radiators, electric",
                         "water source heat pump, radiators, mains gas",
                         "water source heat pump, radiators, oil",
                         "water source heat pump, underfloor heating, electric",
                         "water source heat pump, warm air, electric",
                         "water source heat pump, underfloor heating, mains gas",
                         "water source heat pump, warm air, mains gas",
                         "water source heat pump, warm air, wood pellets",
                         NA  )


validate(MAINHEAT_DESCRIPTION, "MAINHEAT_DESCRIPTION")

# WINDOWS_DESCRIPTION -----------------------------------------------------

sub_WINDOWS_DESCRIPTION("glazed","glazing")
sub_WINDOWS_DESCRIPTION("fully","full")

td_WINDOWS_DESCRIPTION("single glazingsingle glazing", "single glazing")
td_WINDOWS_DESCRIPTION(c("full double glazingdouble glazing",
                         "multiple glazing throught",
                         "multiple glazing throughout",
                         "multiple glazing throughout double glazing"
), "full double glazing")
td_WINDOWS_DESCRIPTION("partial double glazingdouble glazing", "partial double glazing")
td_WINDOWS_DESCRIPTION(c("sap05:windows",
                         "solid, no insulation (assumed)",
                         "suspended, no insulation (assumed)",
                         "(other premises below)",
                         "full",
                         "mostly",
                         "some"
),NA)


td_WINDOWS_DESCRIPTION("multiple glazing throughout triple glazing", "full triple glazing")


td_WINDOWS_DESCRIPTION("partial multiple glazing", "partial double glazing")
td_WINDOWS_DESCRIPTION("some multiple glazing", "some double glazing")
td_WINDOWS_DESCRIPTION("mostly multiple glazing", "mostly double glazing")

td_WINDOWS_DESCRIPTION("some multiple glazingsecondary glazing", "some double glazing")

td_WINDOWS_DESCRIPTION("single glazingdouble glazing","single glazing")
td_WINDOWS_DESCRIPTION("single glazingsecondary glazing","single glazing")
td_WINDOWS_DESCRIPTION("single glazingtriple glazing","single glazing")
td_WINDOWS_DESCRIPTION("partial multiple glazingdouble glazing","partial double glazing")
td_WINDOWS_DESCRIPTION("some multiple glazingdouble glazing","some double glazing")


WINDOWS_DESCRIPTION = c("high performance glazing",
                        
                        "full double glazing",
                        "mostly double glazing",
                        "partial double glazing",
                        "some double glazing",
                        "double glazing",
                        
                        "single glazing",
                        "some single glazing",
                        "partial single glazing",
                        "single and multiple glazing",
                        
                        "full triple glazing",
                        "partial triple glazing",
                        "mostly triple glazing",
                        "some triple glazing",
                        
                        "some secondary glazing",
                        "partial secondary glazing",
                        "secondary glazing",
                        "full secondary glazing",
                        "mostly secondary glazing",
                        
                        "unknown complex glazing regime",
                        NA
)

validate(WINDOWS_DESCRIPTION, "WINDOWS_DESCRIPTION")


# HOTWATER_DESCRIPTION ----------------------------------------------------

sub_HOTWATER_DESCRIPTION("cylinderstat","cylinder thermostat")
sub_HOTWATER_DESCRIPTION("no cylinder thermostat, no cylinder thermostat","no cylinder thermostat")
sub_HOTWATER_DESCRIPTION("from secondary heater","from secondary system")
sub_HOTWATER_DESCRIPTION("from community","community")
sub_HOTWATER_DESCRIPTION("from main heating system","from main system")

td_HOTWATER_DESCRIPTION( c("no system present?electric immersion assumed",
                           "no hot water system present - electric immersion assumed",
                           "no system present : electric immersion assumed"),
                         "no system present: electric immersion assumed")
td_HOTWATER_DESCRIPTION("electric immersion, standard tariff ","electric immersion, standard tariff")
td_HOTWATER_DESCRIPTION("from main system , no cylinder thermostat","from main system, no cylinder thermostat")
td_HOTWATER_DESCRIPTION(c("from main system ","from main system"),"from main system,")
td_HOTWATER_DESCRIPTION("from main system, flue gas heat recovery, plus solar","from main system, plus solar, flue gas heat recovery")
td_HOTWATER_DESCRIPTION(c("sap:hot-water","sap05:hot-water","***sample***"),NA)


HOTWATER_DESCRIPTION = c(", plus solar, no cylinder thermostat",
                         "back boiler (hot water only), gas",
                         "community scheme",
                         "community scheme with chp",
                         "community scheme, no cylinder thermostat",
                         "community scheme, no cylinder thermostat, plus solar",
                         "community scheme, plus solar",
                         "community scheme, waste water heat recovery",
                         "community heat pump",
                         "electric immersion, off-peak",
                         "electric immersion, off-peak, plus solar",
                         "electric immersion, off-peak, no cylinder thermostat",
                         "electric immersion, off-peak, flue gas heat recovery, waste water heat recovery",
                         "electric immersion, dual tariff",
                         "electric immersion (on-peak or off-peak)",
                         "electric immersion, standard tariff",
                         "electric immersion, standard tariff, no cylinder thermostat",
                         "electric immersion, standard tariff, plus solar",
                         "electric immersion, standard tariff, flue gas heat recovery, waste water heat recovery",
                         "electric immersion, standard tariff, waste water heat recovery",
                         "electric immersion, off-peak, no cylinder thermostat, plus solar",
                         "electric immersion, off-peak, plus solar, waste water heat recovery",
                         "electric immersion, off-peak, waste water heat recovery",
                         "electric immersion, standard tariff, flue gas heat recovery",
                         "electric immersion, standard tariff, plus solar, flue gas heat recovery",
                         "electric immersion, standard tariff, plus solar, no cylinder thermostat",
                         "electric immersion, standard tariff, waste water heat recovery, no cylinder thermostat",
                         "electric instantaneous at point of use ",
                         "electric instantaneous at point of use, 7-hour tariff (on-peak)",
                         "electric instantaneous at point of use, 7-hour tariff (on-peak), 7-hour tariff (on-peak)",
                         "electric instantaneous at point of use, flue gas heat recovery",
                         "electric instantaneous at point of use, standard tariff",
                         "electric instantaneous at point of use, waste water heat recovery",
                         "electric instantaneous at point of use",
                         "electric instantaneous at point of use, no cylinder thermostat",
                         "electric instantaneous at point of use, plus solar",
                         "electric heat pump",
                         "electric heat pump, plus solar, no cylinder thermostat",
                         "electric heat pump, plus solar",
                         "electric heat pump for water heating only",
                         "electric heat pump for water heating only, no cylinder thermostat",
                         "electric heat pump for water heating only, plus solar",
                         "electric heat pump for water heating only, plus solar, no cylinder thermostat",
                         
                         "electric multipoint",
                         "from main system,",
                         "from main system, standard tariff",
                         "from main system, flue gas heat recovery",
                         "from main system, flue gas heat recovery, waste water heat recovery",
                         
                         
                         "from main system, 7-hour tariff (on-peak)",
                         "from main system, flue gas heat recovery, no cylinder thermostat",
                         "from main system, no cylinder thermostat, flue gas heat recovery",
                         "from main system, no cylinder thermostat, waste water heat recovery",
                         "from main system, plus solar, no cylinder thermostat, flue gas heat recovery",
                         "from main system, waste water heat recovery, flue gas heat recovery",
                         "from main system, waste water heat recovery, no cylinder thermostat",
                         
                         "from main system, no cylinder thermostat",
                         "from main system, no cylinder thermostat, plus solar",
                         "from main system, plus solar",
                         "from main system, plus solar, flue gas heat recovery",
                         "from main system, plus solar, no cylinder thermostat",
                         "from main system, plus solar, waste water heat recovery",
                         "from main system, waste water heat recovery",
                         "from second main heating system",
                         "from secondary system",
                         "from secondary system, no cylinder thermostat",
                         "from secondary system, plus solar, no cylinder thermostat",
                         "from secondary system, plus solar",
                         "heat pump",
                         "single-point gas water heater, standard tariff",
                         "single-point gas water heater",
                         "single-point gas water heater, off-peak",
                         "point gas water heater, no cylinder thermostat",
                         
                         "gas boiler/circulator",
                         "gas boiler/circulator for water heating only",
                         "gas boiler/circulator for water heating only, plus solar",
                         "gas boiler/circulator, flue gas heat recovery",
                         "gas boiler/circulator, no cylinder thermostat, flue gas heat recovery",
                         "gas boiler/circulator, plus solar, no cylinder thermostat",
                         "gas boiler/circulator, no cylinder thermostat",
                         "gas boiler/circulator, no cylinder thermostat, plus solar",
                         "gas boiler/circulator, plus solar",
                         "gas multipoint",
                         "gas multipoint, plus solar",
                         "gas multipoint, no cylinder thermostat",
                         "gas range cooker, no cylinder thermostat",  
                         "gas range cooker",
                         "gas range cooker, plus solar",
                         "gas instantaneous at point of use",
                         "gas instantaneous at point of use, plus solar",
                         "gas instantaneous at point of use, flue gas heat recovery",
                         "gas instantaneous at point of use, no cylinder thermostat",
                         "no system present: electric immersion assumed",
                         "no system present: electric immersion assumed, plus solar",
                         "no system present: electric immersion assumed, no cylinder thermostat",
                         ", no cylinder thermostat",
                         "oil boiler/circulator",
                         "oil boiler/circulator, no cylinder thermostat",
                         "oil boiler/circulator, plus solar",
                         "oil range cooker",
                         "oil range cooker, no cylinder thermostat",
                         "oil range cooker, plus solar, no cylinder thermostat",
                         "oil range cooker, plus solar",
                         "solid fuel range cooker",
                         "solid fuel range cooker, plus solar",
                         "solid fuel range cooker, no cylinder thermostat",
                         "solid fuel range cooker, no cylinder thermostat, plus solar",
                         "solid fuel boiler/circulator",
                         "solid fuel boiler/circulator, no cylinder thermostat",
                         "solid fuel boiler/circulator, plus solar",
                         "solid fuel boiler/circulator, plus solar, no cylinder thermostat",
                         NA)

validate(HOTWATER_DESCRIPTION, "HOTWATER_DESCRIPTION")


# FLOOR_LEVEL -------------------------------------------------------------

certs$FLOOR_LEVEL[certs$FLOOR_LEVEL == "ground floor"] <- "ground"

FLOOR_LEVEL = c("basement","ground","1st","mid floor",
                "2nd","3rd",
                paste0(c(4:20,24:30),"th"),
                "21st","22nd","23rd",
                "21st or above","top floor", NA)

validate(FLOOR_LEVEL, "FLOOR_LEVEL")


# SECONDHEAT_DESCRIPTION --------------------------------------------------

td_SECONDHEAT_DESCRIPTION("room heaters, lpg","room heaters, lpg")
td_SECONDHEAT_DESCRIPTION("dim","none")
td_SECONDHEAT_DESCRIPTION("portable electric heaters(assumed)","portable electric heaters (assumed)")

td_SECONDHEAT_DESCRIPTION("room heaters, (null)","room heaters,")
td_SECONDHEAT_DESCRIPTION("room heaters, bulk lpg","room heaters, lpg")
td_SECONDHEAT_DESCRIPTION("room heaters, bulk wood pellets","room heaters, wood pellets")
td_SECONDHEAT_DESCRIPTION(c(",","sap05:secondary-heating"),NA)
td_SECONDHEAT_DESCRIPTION("room heaters, heating oil","room heaters, oil")
td_SECONDHEAT_DESCRIPTION(c("room heaters, main wood pellets", "room heaters, secondary wood pellets", "room heaters, wood pellets (bags)"),"room heaters, wood pellets")
td_SECONDHEAT_DESCRIPTION("room heaters","room heaters,")
td_SECONDHEAT_DESCRIPTION("community, community","community scheme")
td_SECONDHEAT_DESCRIPTION(c("gas (including lpg) room heaters, gas","mains gas room heaters, gas"),"room heaters, mains gas")
td_SECONDHEAT_DESCRIPTION("room heaters","room heaters,")
td_SECONDHEAT_DESCRIPTION("lpg room heaters, gas","room heaters, lpg")




SECONDHEAT_DESCRIPTION = c("none",
                           "community scheme",
                           "community scheme, heat from boilers mains gas",
                           ", gas",
                           "portable electric heaters",
                           "portable electric heaters (assumed)",
                           "gas/lpg boiler pre-1998, with fan-assisted flue, gas",
                           "gas/lpg boiler pre-1998 with balanced or open-flue, gas",
                           "gas/lpg boiler 1998 or later, gas",
                           "gas/lpg cpsu, gas",
                           "electric underfloor heating (standard tariff), electric",
                           "electric underfloor heating",
                           "electric ceiling heating",
                           "hot-water-only systems, electric",
                           "hot-water-only systems, gas",
                           "other space heating systems, electric",
                           "room heaters,",
                           "room heaters, b30k",
                           "room heaters, bioethanol",
                           "room heaters, oil",
                           "room heaters, electric",
                           "room heaters, coal",
                           "room heaters, dual fuel (mineral and wood)",
                           "room heaters, dual fuel",
                           "room heaters, wood pellets",
                           "room heaters, wood logs",
                           "room heaters, wood chips",
                           "room heaters, smokeless fuel",
                           "room heaters, lpg",
                           "room heaters, lng",
                           "room heaters, bottled lpg",
                           "room heaters, mains gas",
                           "room heaters, bottled gas",
                           "room heaters, anthracite",
                           "room heaters, appliances able to use mineral oil or liquid biofuel",
                           "room heaters, biodiesel from any biomass source",
                           "room heaters, biomass",
                           "room heaters, heat from eletric heat pump",
                           "room heaters, liquid biofuel",
                           "room heaters, rapeseed oil",
                           "room heaters, waste combustion",
                           NA)

validate(SECONDHEAT_DESCRIPTION, "SECONDHEAT_DESCRIPTION")

# TRANSACTION_TYPE --------------------------------------------------------

certs$TRANSACTION_TYPE[certs$TRANSACTION_TYPE == "not recorded"] <- NA
certs$TRANSACTION_TYPE[certs$TRANSACTION_TYPE == "none of the above"] <- NA
certs$TRANSACTION_TYPE[certs$TRANSACTION_TYPE == "no data!"] <- NA
certs$TRANSACTION_TYPE[certs$TRANSACTION_TYPE == "unknown"] <- NA
certs$TRANSACTION_TYPE <- gsub(" this is for backwards compatibility only and should not be used","",certs$TRANSACTION_TYPE, fixed = TRUE)

TRANSACTION_TYPE = c("new dwelling",
                     "rental (social)",
                     "rental (private)",
                     "eco assessment",
                     "marketed sale",
                     "non marketed sale",
                     "assessment for green deal",
                     "rhi application",
                     "rental",
                     "fit application",
                     "following green deal",
                     "stock condition survey",
                     NA)

validate(TRANSACTION_TYPE, "TRANSACTION_TYPE")

# LIGHTING_DESCRIPTION ---------------------------------------------------------------

certs$LIGHTING_DESCRIPTION <- gsub("low energy lighting in ","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("% fixed outlets","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("% of fixed outlets","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("all fixed outlets","100",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("no low energy lighting","0",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("sap05:lighting","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("goleuadau ynni-isel mewn ","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("% o'r mannau gosod","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("% o?r mannau gosod","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("% o???r mannau gosod","",certs$LIGHTING_DESCRIPTION, fixed = TRUE)
certs$LIGHTING_DESCRIPTION <- gsub("goleuadau ynni-isel ym mhob un o?r mannau gosod","100",certs$LIGHTING_DESCRIPTION, fixed = TRUE)

certs$LIGHTING_DESCRIPTION[certs$LIGHTING_DESCRIPTION == "dim goleuadau ynni-isel"] <- "0"
certs$LIGHTING_DESCRIPTION[certs$LIGHTING_DESCRIPTION == "low energy lighting 100% 100"] <- "100"
certs$LIGHTING_DESCRIPTION[certs$LIGHTING_DESCRIPTION == "goleuadau ynni-isel ym mhob un o'r mannau gosod"] <- "100"

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

saveRDS(certs,"epc_all_clean.Rds")



