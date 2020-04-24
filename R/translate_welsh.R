translatewelsh <- function(x){
  x <- gsub("bwyler a rheiddiaduron","boiler and radiators",x, fixed = TRUE)
  x <- gsub("nwy prif gyflenwad","mains gas",x, fixed = TRUE)
  
  x <- gsub("rhagdybiaeth","assumed",x, fixed = TRUE)
  x <- gsub("wedi?i inswleiddio","insulated",x, fixed = TRUE)
  x <- gsub("wedi'i inswleiddio","insulated",x, fixed = TRUE)
  x <- gsub("wedi???i inswleiddio","insulated",x, fixed = TRUE)
  x <- gsub("wedi'u hinswleiddio","insulated",x, fixed = TRUE)
  x <- gsub("wedi???u hinswleiddio","insulated",x, fixed = TRUE)
  x <- gsub("wedi'i hinswleiddio","insulated",x, fixed = TRUE)
  x <- gsub("wedi?i hinswleiddio","insulated",x, fixed = TRUE)
  
  x <- gsub("waliau ceudod","cavity wall",x, fixed = TRUE)
  x <- gsub("fel y'u hadeiladwyd","as built",x, fixed = TRUE)
  x <- gsub("fel y?u hadeiladwyd","as built",x, fixed = TRUE)
  x <- gsub("fel y???u hadeiladwyd","as built",x, fixed = TRUE)
  x <- gsub("dim inswleiddio","no insulation",x, fixed = TRUE)
  x <- gsub("heb ei inswleiddio","no insulation",x, fixed = TRUE)
  
  x <- gsub("briciau solet","solid brick",x, fixed = TRUE)
  x <- gsub("ceudod wedi?i lenwi","filled cavity",x, fixed = TRUE)
  x <- gsub("ar oleddf","pitched",x, fixed = TRUE)
  x <- gsub("o inswleiddio yn y llofft","loft insulation",x, fixed = TRUE)
  x <- gsub("lo inswleiddio yn y llof","loft insulation",x, fixed = TRUE)
  
  x <- gsub("to gwellt","thatched",x, fixed = TRUE)
  x <- gsub("gydag inswleiddio ychwanegol","with additional insulation",x, fixed = TRUE)
  x <- gsub("gydag inswleiddio ychwanegol","with additional insulation",x, fixed = TRUE)
  
  x <- gsub("rhaglennydd","programmer",x, fixed = TRUE)
  x <- gsub("a thermostat ystafell","and room thermostat",x, fixed = TRUE)
  x <- gsub("dim thermostat ystafell","no room thermostat",x, fixed = TRUE)
  x <- gsub("thermostat ystafell a trvs","room thermostat and trvs",x, fixed = TRUE)
  
  x <- gsub("a falf osgoi","and bypass",x, fixed = TRUE)
  
  x <- gsub("thermostat ystafell yn unig","room thermostat only",x, fixed = TRUE)
  
  x <- gsub("gwydrau dwbl llawn","fully double glazed",x, fixed = TRUE)
  x <- gsub("gwydrau dwbl rhannol","partial double glazing",x, fixed = TRUE)
  
  x <- gsub("o'r brif system","from main system",x, fixed = TRUE)
  x <- gsub("o?r brif system","from main system",x, fixed = TRUE)
  x <- gsub("adfer gwres nwyon ffliw","flue gas heat recovery",x, fixed = TRUE)
  
  x <- gsub("gwresogyddion ystafell","room heaters",x, fixed = TRUE)
  
  x <- gsub("gwydrau dwbl gan mwyaf","mostly double glazing",x, fixed = TRUE)
  x <- gsub("gwydrau sengl","single glazed",x, fixed = TRUE)
  
  x <- gsub("eiddo arall islaw","other premises below",x, fixed = TRUE)
  x <- gsub("eiddo arall uwchben","other premises above",x, fixed = TRUE)
  
  x <- gsub("annedd arall uwchben","another dwelling above",x, fixed = TRUE)
  x <- gsub("awyr gynnes","warm air",x, fixed = TRUE)
  
  x <- gsub("twymwr tanddwr","electric immersion",x, fixed = TRUE)
  x <- gsub("tarriff safonol","standard tariff",x, fixed = TRUE)
  x <- gsub("ceudod wedi'i lenwi","filled cavity",x, fixed = TRUE)
  x <- gsub("inswleiddio rhannol","partial insulation",x, fixed = TRUE)
  x <- gsub("wedi?u hinswleiddio","insulated",x, fixed = TRUE)
  
  x <- gsub("dim thermostat ar y silindr","no cylinder thermostat",x, fixed = TRUE)
  x <- gsub("anheddiad arall islaw","another dwelling below",x, fixed = TRUE)
  
  x <- gsub("wedi?u hadeiladu yn ol system","system built",x, fixed = TRUE)
  x <- gsub("wedi'u hadeiladu yn ôl system","system built",x, fixed = TRUE)
  x <- gsub("wedi?u hadeiladu yn ?l system","system built",x, fixed = TRUE)
  
  x <- gsub("gydag inswleiddio allanol","with external insulation",x, fixed = TRUE)
  
  x <- gsub("gydag ynni'r haul","plus solar",x, fixed = TRUE)
  
  x <- gsub("ffrâm bren","timber frame",x, fixed = TRUE)
  x <- gsub("ffr?m bren","timber frame",x, fixed = TRUE)
  x <- gsub("ffram bren","timber frame",x, fixed = TRUE)
  
  x <- gsub("gwenithfaen neu risgraig","granite or whinstone",x, fixed = TRUE)
  
  x <- gsub("rheolaeth amser a rheolaeth parthau tymheredd","time and temperature zone control",x, fixed = TRUE)
  
  x <- gsub("tywodfaen","sandstone",x, fixed = TRUE)
  
  x <- gsub("a thermostatau ar y cyfarpar","and appliance thermostats",x, fixed = TRUE)
  x <- gsub("ac o leiaf ddau thermostat ystafell","and appliance thermostats",x, fixed = TRUE)
  
  x <- gsub("wrth y trawstiau","at rafters",x, fixed = TRUE)
  x <- gsub("wrth y trawstia","at rafters",x, fixed = TRUE)
  
  x <- gsub("ystafell(oedd) to","roof room(s)",x, fixed = TRUE)
  
  x <- gsub("inswleiddio cyfyngedig","limited insulation",x, fixed = TRUE)
  
  x <- gsub("dim rheolaeth amser na rheolaeth thermostatig ar dymheredd yr ystafell","no time or thermostatic control of room temperature",x, fixed = TRUE)
  
  x <- gsub("dim rheolaeth thermostatig ar dymheredd yr ystafell","no thermostatic control of room temperature",x, fixed = TRUE)
  
  x <- gsub("dim rheolaeth thermostatig ar dymheredd yr ystafell","no thermostatic control of room temperature",x, fixed = TRUE)
  
  x <- gsub("rheoli'r t?l ? llaw","manual charge control",x, fixed = TRUE)
  x <- gsub("rheoli'r tâl â llaw","manual charge control",x, fixed = TRUE)
  
  x <- gsub("logiau coed","wood logs",x, fixed = TRUE)
  
  x <- gsub("st?r wresogyddion trydan","electric storage heaters",x, fixed = TRUE)
  x <- gsub("stor wresogyddion trydan","electric storage heaters",x, fixed = TRUE)
  x <- gsub("stôr wresogyddion trydan","electric storage heaters",x, fixed = TRUE)
  
  x <- gsub("ffenestri perfformiad uchel","high performance glazing",x, fixed = TRUE)
  
  x <- gsub("gwydrau triphlyg gan mwyaf","mostly triple glazing",x, fixed = TRUE)
  
  x <- gsub("rhai gwydrau dwbl","some double glazing",x, fixed = TRUE)
  
  x <- gsub("i ofod heb ei wresogi","to unheated space",x, fixed = TRUE)
  x <- gsub("ynysydd allanol a llenwi ceudod","filled cavity and external insulation",x, fixed = TRUE)
  
  x <- gsub("gydag inswleiddio mewnol","with internal insulation",x, fixed = TRUE)
  
  x <- gsub("rheoli gwefr drydanol yn awtomatig","automatic charge control",x, fixed = TRUE)
  
  x <- gsub("rheoli?r tal a llaw","manual charge control",x, fixed = TRUE)
  
  x <- gsub("system dalu wedi'i chysylltu â defnyddio gwres cymunedol","charging system linked to use of community heating",x, fixed = TRUE)
  
  x <- gsub("t?l un gyfradd","flat rate charging",x, fixed = TRUE)
  x <- gsub("tã¢l un gyfradd","flat rate charging",x, fixed = TRUE)
  x <- gsub("tal un gyfradd","flat rate charging",x, fixed = TRUE)
  x <- gsub("tâl un gyfradd","flat rate charging",x, fixed = TRUE)
  
  x <- gsub("thermostatau ar y cyfarpar","appliance thermostats",x, fixed = TRUE)
  
  x <- gsub("dau danwydd","dual fuel",x, fixed = TRUE)
  x <- gsub("mwynau a choed","mineral and wood",x, fixed = TRUE)
  
  x <- gsub("nwy potel","bottled gas",x, fixed = TRUE)
  
  x <- gsub("tanwydd di-fwg","smokeless fuel",x, fixed = TRUE)
  
  x <- gsub("bwyler a gwres dan y llawr","boiler, underfloor heating",x, fixed = TRUE)
  
  x <- gsub("cynllun cymunedol","community scheme",x, fixed = TRUE)
  
  x <- gsub("dim system ar gael","no system present",x, fixed = TRUE)
  
  x <- gsub("rhagdybir bod gwresogyddion trydan","electric heaters assumed",x, fixed = TRUE)
  
  x <- gsub("pwmp gwres sy'n tarddu yn y ddaear","ground source heat pump",x, fixed = TRUE)
  x <- gsub("pwmp gwres sy'n tarddu yn yr awyr","air source heat pump",x, fixed = TRUE)
  
  x <- gsub("dan y llawr","underfloor heating",x, fixed = TRUE)
  
  x <- gsub("gwydrau eilaidd llawn","full secondary glazing",x, fixed = TRUE)
  
  x <- gsub("gwydrau lluosog ym mhobman","full double glazing",x, fixed = TRUE)
  
  x <- gsub("gwydrau triphlyg llawn","full triple glazing",x, fixed = TRUE)
  
  x <- gsub("an-frig","on-peak",x, fixed = TRUE)
  
  x <- gsub("gwresogyddion trydan cludadwy","portable electric heaters",x, fixed = TRUE)
  
  
  
  # Single word translations: Risky
  x <- gsub("nenfwd","ceiling",x, fixed = TRUE)
  x <- gsub("yn wastad","flat",x, fixed = TRUE)
  x <- gsub("trydan","electric",x, fixed = TRUE)
  x <- gsub("solet","solid",x, fixed = TRUE)
  x <- gsub("olew","oil",x, fixed = TRUE)
  x <- gsub("glo caled","anthracite",x, fixed = TRUE)
  x <- gsub("glo","coal",x, fixed = TRUE)
  x <- gsub("crog","suspended",x, fixed = TRUE)
  return(x)
}