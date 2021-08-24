#' Retorna matriz simple triplet con la constante a multiplicar a variable del 
#' problema
#' @description Devuelve la columna constante teniendo cuenta la lista de 
#' columnas a filtras y los valores de estos mismos 
#'
#' @param tabla dataframe con información a realizar filtros
#' @param col_filtros lista de columnas a filtrar de la tabla 
#' @param indices lista con los indices o rango de valores a filtrar en columnas
#' @param col_valor columna que se requiere devolver
#' @example 
#' constante(tabla = trabajadores$disponibilidad,
#'           col_filtros = list("id","turnos_id"),
#'           indices = list(1:5,1:14),
#'           col_valor = "disponibilidad")
#' @return matriz simple triplet con información de indices y col_valor

constante <- function (tabla,
                       col_filtros = list(),
                       indices = list(),
                       col_valor = ""){
  tabla = 
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