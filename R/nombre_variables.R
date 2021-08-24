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

nombres_variables <- function(
  modelo
){
  var <- modelo$variables
  nombre_variables <- 
    var %>% purrr::map(function(x){
      modelo$indices_variables %>% 
        purrr::map(function(y) cross(y) %>% map(lift(paste)))  %>% 
        pluck(x) %>% unlist() %>% 
        stringr::str_replace(" ",",") %>%
        paste0(x,"[",.,"]")
})

return(nombre_variables)    
}

