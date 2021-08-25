#' Almacena los nombres de  variables del modelo a resolver. 
#' 
#' @description  Los nombres se usan en las operaciones matriciales en
#' las restricciones y función objetivo del modelo de optimización
#' @param modelo Modelo con structura almacenada
#' @example 
#' problema() %>% 
#' adicion_variables(variable = a,indices = list("i" = 1:2),"bin") %>% 
#' nombres_variables()
#' @return nombre_variables lista con nombre de las variables asociadas a sus
#' indices

nombres_variables <- function(modelo) {
  nombre_variables <- purrr::map2(
    .x = modelo$variables,
    .y = names(modelo$variables),
    function(variable, nom_var) {
      combinaciones <- do.call(expand.grid, variable$indices) %>%
        unite(visual, everything(), sep = ", ") %>%
        pull(visual)
      combinaciones <- paste0("[", combinaciones, "]") %>%
        paste(collapse = " ")
      message(paste(nom_var, combinaciones))
    }
  )

}

