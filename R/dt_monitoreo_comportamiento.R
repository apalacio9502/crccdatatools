#' Descarga los datos div_monitoreo_comportamiento
#'
#' Esta función descarga los datos de la tabla div_monitoreo_comportamiento para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_div_monitoreo_comportamiento<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                     MIEMBRO_TIPO, VOLUMEN_COMPRA, VOLUMEN_VENTA, LIQUIDACION_COMPRA,
                                     LIQUIDACION_VENTA, NUMERO_OPERACIONES_COMPRA, NUMERO_OPERACIONES_VENTA,
                                     PARTICIPACION_VOLUMEN_COMPRA, PARTICIPACION_VOLUMEN_VENTA, PREDICCION,
                                     QIF_9990, QIF_9975, QIF_9950, ATIPICO_QIF_9990, ATIPICO_QIF_9975,
                                     ATIPICO_QIF_9950
                                     FROM DIV_MONITOREO_COMPORTAMIENTO
                                     WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))


  return(datos)
}


