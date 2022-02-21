#' Descarga los datos pa_div_lineas_pl
#'
#' Esta función descarga los datos de la tabla pa_div_lineas_pl
#' @param conexion clase formal. Conexión base de datos
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_pa_div_lineas_pl<- function(conexion,seudonimo="REAL"){

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                     MONEDA, MONTO
                                     FROM PA_DIV_LINEAS_PL"))

  return(datos)
}



