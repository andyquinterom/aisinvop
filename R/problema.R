#' Problema de investigación de operaciones con estructura permitida
#' 
#' @description Modelo de optimización con estructura variables, indices, 
#' función objetivo y restricciones

problema <- function(){
  modelo <- structure(
    list(
      variables = list(),
      funcion_objetivo = NULL,
      restricciones = list()
    )
  )

  class(modelo) <- "modelo_aisinvop"
return(modelo)
}
