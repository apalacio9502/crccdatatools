#' Descarga los datos dv_posicion_abierta_por_rango
#'
#' Esta función descarga los datos de la tabla dv_posicion_abierta_por_rango para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @export

dt_dv_pa_por_rango <- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Descarga datos
  datos <-   dbGetQuery(conexion, glue("SELECT FECHA, MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                            PRODUCTO_TIPO, RANGO, POSICION_VENDEDORA_VALORADA, POSICION_COMPRADORA_VALORADA,
                                            POSICION_NETA_VALORADA FROM DV_POSICION_ABIERTA_POR_RANGO
                                            WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND
                                            {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}

#' Descarga los datos dv_curva_fwd
#'
#' Esta función descarga los datos de la tabla dv_curva_fwd para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @export

dt_dv_curva_fwd <- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Descarga datos
  datos <-  dbGetQuery(conexion, glue("SELECT FECHA, NODO, NODO_DIAS,PRECIO, PF, DEVALUACION
                                           FROM DV_CURVA_FWD
                                           WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND
                                           {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}

#' Descarga los datos dv_liquidaciones_resumen
#'
#' Esta función descarga los datos de la tabla dv_liquidaciones_resumen para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @export

dt_dv_liq_resumen <- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Descarga datos
  datos <- dbGetQuery(conexion , glue("SELECT FECHA,
                                        MIEMBRO_LIQ_{dt_ficticio_sql(ficticio)} AS MIEMBRO_LIQ_ID_SEUDONIMO,
                                        MIEMBRO_LIQ_TIPO, CUENTA_GARANTIA_TIPO, PRODUCTO_NOMBRE,
                                        PRODUCTO_TIPO, PRODUCTO_SUBTIPO, PRODUCTO_ORIGEN, LIQUIDACION
                                        FROM DV_LIQUIDACIONES_RESUMEN
                                        WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                        AND {periodo_analisis_sql[2]}"))

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>% mutate(FECHA=ymd(FECHA)) %>%
    complete(FECHA,nesting(MIEMBRO_LIQ_ID_SEUDONIMO,MIEMBRO_LIQ_TIPO, CUENTA_GARANTIA_TIPO,
                           PRODUCTO_NOMBRE, PRODUCTO_TIPO,PRODUCTO_SUBTIPO,PRODUCTO_ORIGEN),
             fill = list(LIQUIDACION=0)) %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA")

  return(datos)
}

#' Descarga los datos dv_indicador_liquidez
#'
#' Esta función descarga los datos de la tabla dv_indicador_liquidez para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @export

dt_dv_indicador_liquidez <- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Descarga datos
  datos <-  dbGetQuery(conexion, glue("SELECT FECHA,
                                      MIEMBRO_LIQ_{dt_ficticio_sql(ficticio)}_1 AS MIEMBRO_LIQ_ID_SEUDONIMO_1,
                                      MIEMBRO_LIQ_{dt_ficticio_sql(ficticio)}_2 AS MIEMBRO_LIQ_ID_SEUDONIMO_2,
                                      LIQUIDACION_1, LIQUIDACION_2, LIQUIDACION_ESTRESADA_1, LIQUIDACION_ESTRESADA_2,
                                      GARANTIAS_CASH_1, GARANTIAS_CASH_2, GARANTIAS_TES_1, GARANTIAS_TES_2,
                                      GARANTIAS_CASH_DISPONIBLES, TITULOS_APT, LNC_BCO_BOGOTA, LNC_BANCOLOMBIA, LNC_DAVIVIENDA, CTA_APT
                                      FROM DV_INDICADOR_LIQUIDEZ
                                      WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                      AND {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}
