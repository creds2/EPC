translatewelsh <- function(x){
  x <- gsub("Bwyler a rheiddiaduron","Boiler and radiators",x)
  x <- gsub("nwy prif gyflenwad","mains gas",x)
  
  x <- gsub("Solet","Solid",x)
  x <- gsub("rhagdybiaeth","assumed",x)
  x <- gsub("wedi?i inswleiddio","insulated",x)
  x <- gsub("Crog","Suspended",x)
  x <- gsub("Waliau ceudod","Cavity wall",x)
  x <- gsub("fel y'u hadeiladwyd","as built",x)
  x <- gsub("fel y?u hadeiladwyd","as built",x)
  x <- gsub("dim inswleiddio","no insulation",x)
  
  x <- gsub("Briciau solet","Solid brick",x)
  x <- gsub("ceudod wedi?i lenwi","filled cavity",x)
  x <- gsub("Ar oleddf","Pitched",x)
  x <- gsub("o inswleiddio yn y llofft","loft insulation",x)
  x <- gsub("To gwellt","Thatched",x)
  x <- gsub("gydag inswleiddio ychwanegol","with additional insulation",x)
  
  x <- gsub("Rhaglennydd","Programmer",x)
  x <- gsub("dim thermostat ystafell","no room thermostat",x)
  x <- gsub("thermostat ystafell a TRVs","room thermostat and TRVs",x)
  
  x <- gsub("a falf osgoi","and bypass",x)
  
  x <- gsub("Thermostat ystafell yn unig","Room thermostat only",x)
  
  
  
  
  
  
  
  
  x <- gsub("","",x)
  return(x)
}
