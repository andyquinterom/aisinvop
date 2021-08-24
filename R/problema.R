#' Problema de investigación de operaciones con estructura permitida
#' 
#' @description Modelo de optimización con estructura variables, indices, 
#' función objetivo y restricciones

problema <- function(){
  modelo <- structure(list(variables = list(),
                           variables_tipo = list(),
                 indices_variables = list(),
                 funcion_objetivo = NULL,
                 restricciones = list()))
return(modelo)
}