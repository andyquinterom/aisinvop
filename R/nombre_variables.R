

# Función nombre_variables sirve para almacenar los nombres de  variables del
# modelo a resolver. Estos nombres se usaran en las operaciones matriciales en
# las restricciones y función objetivo

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

