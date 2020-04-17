

func <- function(x){
  x <- gsub("²","2",x)
  return(x)
}



fix_wm2k <- function(x){
  
  #Encoding(x) <- "UTF-8"
  #x <- iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  #x <- stringr::str_replace_all(x, "²","")
  x <- gsub("/\p{No}/gu/","?", x)
  x <- gsub("W/m?K","W/m2K", x, fixed = TRUE)
  x <- gsub("W/mA?K","W/m2K", x, fixed = TRUE)
  x <- gsub("W/mÂ2K","W/m2K", x, fixed = TRUE)
  x <- gsub("W/m&#0178;K","W/m2K", x, fixed = TRUE)
  x <- gsub("W/m??K","W/m2K", x, fixed = TRUE)
  
  
  if(grepl("W/m2K", x)){
    y <- strsplit(x," ")[[1]]
    if(length(y) != 2){
      stop(paste0("Don't know how to process",x))
    }
    if(nchar(y[1]) != 4){
      y[1] <- format(as.numeric(y[1]), digits = 2, nsmall = 2)
    }
    x <- paste0(y[1]," ",y[2])
  }
  
  return(x)
}



