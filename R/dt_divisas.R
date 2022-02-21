#' Descarga los datos div_vol_liq_op
#'
#' Esta función descarga los datos de la tabla div_vol_liq_op para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_div_vol_liq_op<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID, SEGMENTO_NOMBRE,
                                     MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO, MIEMBRO_TIPO,
                                     VOLUMEN_COMPRA, VOLUMEN_VENTA, LIQUIDACION_COMPRA,
                                     LIQUIDACION_VENTA, NUMERO_OPERACIONES_COMPRA, NUMERO_OPERACIONES_VENTA
                                     FROM DIV_VOL_LIQ_OP
                                     WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Se agregan todas las posibles fechas del periodo de análisis
  datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO),
             fill = list(VOLUMEN_COMPRA=0,VOLUMEN_VENTA=0,LIQUIDACION_COMPRA=0,LIQUIDACION_VENTA=0,
                         NUMERO_OPERACIONES_COMPRA=0,NUMERO_OPERACIONES_VENTA=0))

  return(datos)
}

#' Descarga los datos div_analisis_lole
#'
#' Esta función descarga los datos de la tabla div_analisis_lole para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_div_analisis_lole<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID, SEGMENTO_NOMBRE,
                                     MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO, MIEMBRO_TIPO,
                                     MAXIMA_OBLIGACION, MAXIMA_OBLIGACION_VALORADA
                                     FROM DIV_ANALISIS_LOLE
                                     WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Se agregan todas las posibles fechas del periodo de análisis
  # datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  # datos <- datos %>%
  #   complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO),
  #           fill = list(VOLUMEN_COMPRA=0,VOLUMEN_VENTA=0,LIQUIDACION_COMPRA=0,LIQUIDACION_VENTA=0,
  #                       NUMERO_OPERACIONES_COMPRA=0,NUMERO_OPERACIONES_VENTA=0))

  return(datos)
}


