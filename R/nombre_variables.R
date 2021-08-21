
 # Función nombre_variables sirve para almacenar los nombres de  variables del
# modelo a resolver. Estos nombres se usaran en las operaciones matriciales en
# las restricciones y función objetivo

nombres_variables <- function(
  # modelo = "",
  variables,
  indices = list(),
  type = ""
  ){
    var <- enquo(variables)
    nombre_variables <- indices %>% 
      cross %>% map(lift(paste)) %>% 
      unlist() %>% 
      stringr::str_replace(" ",",") %>% 
      paste0(as_label(var),"[",.,"]")
    
}

