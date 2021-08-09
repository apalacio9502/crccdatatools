#' Descarga los datos rf_posicion_abierta_por_tramo
#'
#' Esta función descarga los datos de la tabla rf_posicion_abierta_por_tramo para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @export

dt_rf_pa_por_tramo <- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                          TRAMO, POSICION_COMPRADORA_VALORADA,
                                          POSICION_VENDEDORA_VALORADA, POSICION_NETA_VALORADA,
                                          DUR_POSICION_COMPRADORA_VALORADA, DUR_POSICION_VENDEDORA_VALORADA,
                                          DUR_POSICION_NETA_VALORADA, DUR_MOD_POSICION_COMPRADORA_VALORADA,
                                          DUR_MOD_POSICION_VENDEDORA_VALORADA, DUR_MOD_POSICION_NETA_VALORADA,
                                          PV01, GARANTIA_EXIGIDA FROM RF_POSICION_ABIERTA_POR_TRAMO
                                          WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND
                                          {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}

#' Descarga los datos rf_curva_tes
#'
#' Esta función descarga los datos de la tabla rf_curva_tes para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @export

dt_rf_curva_tes <- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Descarga datos
  datos <-  dbGetQuery(conexion, glue("SELECT FECHA, NEMOTECNICO, FECHA_VENCIMIENTO, DURACION_ANOS, TASA
                                      FROM RF_CURVA_TES
                                      WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND
                                      {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}
