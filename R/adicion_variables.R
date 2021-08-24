# Función que adiciona variables y atributos de la variable al modelo 

adicion_variables <- function(modelo,
                              variables,
                              indices = list(),
                              tipo = ""){
  variable <- as_label(enquo(variables))
  
  modelo$variables <- c(modelo$variables, 
                        variable %>% set_names(variable)) 
  
  modelo$indices_variables <- c(modelo$indices_variables, 
                                list(indices %>% 
                                  set_names(
                                    paste(variable,names(.),sep = "_")
                                    )) %>% 
                                  set_names(variable))
  
  modelo$variables_tipo <- c(modelo$variables_tipo,
                            tipo %>% set_names(variable))
  
 return(modelo) 
}
