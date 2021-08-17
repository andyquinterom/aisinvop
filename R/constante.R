

constante <- function (tabla,
                       col_filtros = list(),
                       indices = list(),
                       col_valor = ""){
  base <- 
    purrr::map2(
      .x = col_filtros,
      .y = indices,
      function(.x,.y){
        tabla %>% 
          filter(!!rlang::sym(.x) %in% .y) 
      }) %>% 
    purrr::reduce(inner_join) %>% 
    pull(!!rlang::sym(col_valor))
  return(slam::as.simple_triplet_matrix(base))
}