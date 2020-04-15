translatewelsh <- function(x){
  x <- gsub("Bwyler a rheiddiaduron","Boiler and radiators",x, fixed = TRUE)
  x <- gsub("nwy prif gyflenwad","mains gas",x, fixed = TRUE)
  
  x <- gsub("Solet","Solid",x, fixed = TRUE)
  x <- gsub("rhagdybiaeth","assumed",x, fixed = TRUE)
  x <- gsub("wedi?i inswleiddio","insulated",x, fixed = TRUE)
  x <- gsub("Crog","Suspended",x, fixed = TRUE)
  x <- gsub("Waliau ceudod","Cavity wall",x, fixed = TRUE)
  x <- gsub("fel y'u hadeiladwyd","as built",x, fixed = TRUE)
  x <- gsub("fel y?u hadeiladwyd","as built",x, fixed = TRUE)
  x <- gsub("dim inswleiddio","no insulation",x, fixed = TRUE)
  
  x <- gsub("Briciau solet","Solid brick",x, fixed = TRUE)
  x <- gsub("ceudod wedi?i lenwi","filled cavity",x, fixed = TRUE)
  x <- gsub("Ar oleddf","Pitched",x, fixed = TRUE)
  x <- gsub("o inswleiddio yn y llofft","loft insulation",x, fixed = TRUE)
  x <- gsub("To gwellt","Thatched",x, fixed = TRUE)
  x <- gsub("gydag inswleiddio ychwanegol","with additional insulation",x, fixed = TRUE)
  
  x <- gsub("Rhaglennydd","Programmer",x, fixed = TRUE)
  x <- gsub("dim thermostat ystafell","no room thermostat",x, fixed = TRUE)
  x <- gsub("thermostat ystafell a TRVs","room thermostat and TRVs",x, fixed = TRUE)
  
  x <- gsub("a falf osgoi","and bypass",x, fixed = TRUE)
  
  x <- gsub("Thermostat ystafell yn unig","Room thermostat only",x, fixed = TRUE)
  
  x <- gsub("Gwydrau dwbl llawn","Fully double glazed",x, fixed = TRUE)
  x <- gsub("Gwydrau dwbl rhannol","Partial double glazing",x, fixed = TRUE)
  
  x <- gsub("O'r brif system","From main system",x, fixed = TRUE)
  x <- gsub("O?r brif system","From main system",x, fixed = TRUE)
  x <- gsub("adfer gwres nwyon ffliw","flue gas heat recovery",x, fixed = TRUE)
  
  x <- gsub("Gwresogyddion ystafell","Room heaters",x, fixed = TRUE)
  
  
  
  #x <- gsub("","",x, fixed = TRUE)
  return(x)
}
