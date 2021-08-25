#' Adiciona variables y atributos de la variable al modelo 
#' 
#' @description Adiciona variables a la estructura modelo
#' @param modelo Modelo con estructura 
#' @param variable Nombre de la variable a resolver
#' @param indices Lista de indices asociadas a la variable 
#' @param tipo caracter con tipo de variable: continua, binaria o entera
#' @example 
#' problema() %>%
#' adicion_variables(variables = b, indices = list(1:2,1:3),"bin")
#' @return modelo estructura modelo actualizada con la variable adicionada

adicion_variables <- function(modelo, variable, indices = list(), tipo = "") {

  variable <- as_label(enquo(variable))

  if (variable %in% names(modelo$variables)) stop("Variable ya existe")

  modelo$variables <- append(
    modelo$variables,
    list(
      list(
        indices = indices,
        tipo = tipo
      )
    ) %>%
      set_names(variable)
  )

 return(modelo) 
}
