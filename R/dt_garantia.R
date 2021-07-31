#' Descarga los datos gen_garantia_depositada_exigida
#'
#' Esta función descarga los datos de la tabla gen_garantia_depositada_exigida para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis. Por defecto NULL
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @param segmentos clase array character. Lista de segmentos de los cuales se desea descargar la información.
#' Por defecto descarga la información de todos los segmentos.
#' @param oracle clase boolean. TRUE si la base de datos utilizada es oracle. Por defecto FALSE.
#' @export

dt_gen_gar_dep_exi_resumen<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE,segmentos=NULL,oracle=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,oracle)

  # Descarga datos
  datos <-  dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                    SEGMENTO_NOMBRE, MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                    MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO,GARANTIA_DEPOSITADA,
                                    GARANTIA_EXIGIDA,POSICION_COMPRADORA_VALORADA, POSICION_VENDEDORA_VALORADA,
                                    POSICION_BRUTA_VALORADA FROM MOD_GE_SUB_IO_GARANTIA_DEPOSITADA_EXIGIDA
                                    WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                    AND {periodo_analisis_sql[2]}"))

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>% mutate(FECHA=ymd(FECHA)) %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO, CUENTA_GARANTIA_TIPO),
             fill = list(GARANTIA_DEPOSITADA=0,GARANTIA_EXIGIDA=0,POSICION_COMPRADORA_VALORADA=0,POSICION_VENDEDORA_VALORADA=0,POSICION_BRUTA_VALORADA=0)) %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA")

  return(datos)
}

#' Descarga los datos gen_garantia_depositada_exigida_liq
#'
#' Esta función descarga los datos de la tabla gen_garantia_depositada_exigida_liq para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis. Por defecto NULL
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @param segmentos clase array character. Lista de segmentos de los cuales se desea descargar la información.
#' Por defecto descarga la información de todos los segmentos.
#' @param oracle clase boolean. TRUE si la base de datos utilizada es oracle. Por defecto FALSE.
#' @export

dt_gen_gar_dep_exi_liq<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE,segmentos=NULL,oracle=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,oracle)

  # Descarga datos
  datos <-  dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                    SEGMENTO_NOMBRE, MIEMBRO_LIQ_{dt_ficticio_sql(ficticio)} AS MIEMBRO_LIQ_ID_SEUDONIMO,
                                    MIEMBRO_LIQ_TIPO,GARANTIA_DEPOSITADA,GARANTIA_EXIGIDA,
                                    POSICION_COMPRADORA_VALORADA, POSICION_VENDEDORA_VALORADA,
                                    POSICION_BRUTA_VALORADA, RIESGO_ST, PATRIMONIO
                                    FROM MOD_GE_SUB_IO_GARANTIA_DEPOSITADA_EXIGIDA_LIQ
                                    WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                    AND {periodo_analisis_sql[2]}"))

  return(datos)
}

#' Descarga los datos gen_garantia_exigida_resumen
#'
#' Esta función descarga los datos de la tabla gen_garantia_exigida_resumen para un periodo de análisis y
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

dt_gen_gar_exi_resumen<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE,segmentos=NULL,oracle=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,oracle)

  # Descarga datos
  datos <-  dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                       SEGMENTO_NOMBRE, MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                       MIEMBRO_TIPO,PRODUCTO_DETALLE,GARANTIA_EXIGIDA,POSICION_BRUTA_VALORADA
                                       FROM MOD_GE_SUB_IO_GARANTIA_EXIGIDA_RESUMEN
                                       WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                       AND {periodo_analisis_sql[2]}"))

  return(datos)
}


#' Descarga los datos gen_garantia_ggl_ind_fgc_liq
#'
#' Esta función descarga los datos de la tabla gen_garantia_ggl_ind_fgc_liq para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis. Por defecto NULL
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @param segmentos clase array character. Lista de segmentos de los cuales se desea descargar la información.
#' Por defecto descarga la información de todos los segmentos.
#' @param oracle clase boolean. TRUE si la base de datos utilizada es oracle. Por defecto FALSE.
#' @export

dt_gen_gar_ggl_ind_fgc_liq<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE,segmentos=NULL,oracle=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,oracle)

  # Descarga datos
  datos <-  dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                    SEGMENTO_NOMBRE, MIEMBRO_LIQ_{dt_ficticio_sql(ficticio)} AS MIEMBRO_LIQ_ID_SEUDONIMO,
                                    MIEMBRO_LIQ_TIPO, GARANTIA_TIPO, IMPORTE FROM MOD_GE_SUB_IO_GARANTIA_IND_FGC_DEPOSITADA_EXIGIDA_LIQ
                                    WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                    AND {periodo_analisis_sql[2]}"))

  return(datos)
}

#' Descarga los datos gen_lmc_consumo
#'
#' Esta función descarga los datos de la tabla gen_lmc_consumo para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis. Por defecto NULL
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @param oracle clase boolean. TRUE si la base de datos utilizada es oracle. Por defecto FALSE.
#' @export

dt_gen_lmc_consumo<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE,oracle=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,oracle)

  # Descarga datos
  datos <-  dbGetQuery(conexion , glue("SELECT FECHA,
                                    MIEMBRO_LIQ_{dt_ficticio_sql(ficticio)} AS MIEMBRO_LIQ_ID_SEUDONIMO,
                                    MIEMBRO_LIQ_TIPO,LIMITE, RIESGO, GARANTIA_DEPOSITADA,
                                    GARANTIA_EXIGIDA, LIMITE_AJUSTADO, CONSUMO_LIMITE
                                    FROM MOD_GE_SUB_IO_COSUMO_LMC
                                    WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                    AND {periodo_analisis_sql[2]}"))


  return(datos)
}

