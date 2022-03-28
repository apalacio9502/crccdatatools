#' Descargar los datos app_gen_estructura_llave
#'
#' Esta función descarga los datos de la tabla app_gen_estructura_llave
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param llave_analisis clase character.
#' @param segmento_analisis clase character. Segmento analisis ("GE","CD","CV","C2","C7","C8","C9")
#' @export

dt_app_gen_estructura_llave<- function(conexion,llave_analisis,segmento_analisis){

  # Descarga datos
  datos <- dbGetQuery(conexion,glue("SELECT * FROM APP_GEN_ESTRUCTURA_LLAVE WHERE LLAVE_ID ='{llave_analisis}' AND SEGMENTO_ID='{segmento_analisis}'"))

  return(datos)
}

