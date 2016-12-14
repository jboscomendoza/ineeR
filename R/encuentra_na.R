limpiar_na <- function(tabla) {
  df_na <- data.frame(
    cbind(c(7:9), c(97:99), c(997:999), c(9997:9999)
    )
  )
  
  define_na <- function(valor){
    columna_na <- sapply(df_na, function(valores_posibles){
      valor %in% valores_posibles
    })
    
    if(sum(columna_na) == 0) {
      valores_na <- NA
    } else {
      valores_na <- ( df_na[3, columna_na]-2 ):df_na[3, columna_na]
    }
  
    valores_na
  }
  
  encuentra_na <- function(columna){
    if(is.factor(columna) | is.character(columna)){
      columna_extraer <- as.numeric(as.character(columna))
    } else {
      columna_extraer <- columna
    }
    
    valor_buscado <- max(columna_extraer, na.rm = T)
   
    if(is.infinite(valor_buscado)) valor_buscado <- NA
     
    valor_na <- define_na(valor_buscado)

    valor_na    
     columna[columna %in% valor_na] <- NA
     
     columna
  }
   
   data.frame(
     lapply(tabla, encuentra_na)
   )
}
