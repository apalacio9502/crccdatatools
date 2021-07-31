#' Descarga los datos gen_rss_promedio
#'
#' Esta función descarga los datos de la tabla gen_rss_promedio para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @param segmentos clase array character. Lista de segmentos de los cuales se desea descargar la información.
#' Por defecto descarga la información de todos los segmentos.
#' @param oracle clase boolean. TRUE si la base de datos utilizada es oracle. Por defecto FALSE.
#' @export

dt_gen_rss_promedio<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE,segmentos=NULL,oracle=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,oracle)

  # Descarga datos
  datos <- dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,SEGMENTO_NOMBRE,
                                        MIEMBRO_LIQ_{dt_ficticio_sql(ficticio)} AS MIEMBRO_LIQ_ID_SEUDONIMO,
                                        MIEMBRO_LIQ_TIPO, MIEMBRO_LIQ_COLETIVIZADOR , POSICION, RIESGO_ST,
                                        RIESGO_ST_PROMEDIO, GARANTIA_EXIGIDA, GARANTIA_EXIGIDA_FGC
                                        FROM MOD_GE_SUB_IRSS_PROMEDIO
                                        WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                        AND {periodo_analisis_sql[2]}"))

  return(datos)
}

#' Descarga los datos gen_rss_test_fgc
#'
#' Esta función descarga los datos de la tabla gen_rss_test_fgc para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @param segmentos clase array character. Lista de segmentos de los cuales se desea descargar la información.
#' Por defecto descarga la información de todos los segmentos.
#' @param oracle clase boolean. TRUE si la base de datos utilizada es oracle. Por defecto FALSE.
#' @export

dt_gen_rss_test_fgc<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE,segmentos=NULL,oracle=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,oracle)

  # Descarga datos
  datos <- dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID, SEGMENTO_NOMBRE,
                                        MIEMBRO_LIQ_{dt_ficticio_sql(ficticio)}_1 AS MIEMBRO_LIQ_ID_SEUDONIMO_1,
                                        MIEMBRO_LIQ_{dt_ficticio_sql(ficticio)}_2 AS MIEMBRO_LIQ_ID_SEUDONIMO_2,
                                        RIESGO_ST_1, RIESGO_ST_2, GARANTIA_GIST,
                                        GARANTIA_GGL, GARANTIA_GPT, GARANTIA_FGC, GARANTIA_FGG_CRCC
                                        FROM MOD_GE_SUB_IRSS_TEST_FGC
                                        WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                        AND {periodo_analisis_sql[2]}"))

  return(datos)
}


