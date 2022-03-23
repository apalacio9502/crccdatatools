#' Descarga los datos gen_vol_resumen
#'
#' Esta función descarga los datos de la tabla gen_vol_resumen para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param miembros_analisis clase array character. Lista de miembros
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros
#' @param miembros_liq_analisis clase array character. Lista de miembros liquidadores
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros liquidadores
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CD","CN","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_vol_resumen<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_analisis=NULL,miembros_liq_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte periodo_analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte miembros_analisis a SQL
  miembros_analisis_sql <- dt_miembros_analisis_sql(miembros_analisis)

  # Se covierte miembros_liq_analisis a SQL
  miembros_liq_analisis_sql <- dt_miembros_analisis_sql(miembros_liq_analisis,liquidadores =TRUE)

  # Se covierte segmentos_analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                      SEGMENTO_NOMBRE, MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                      MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO,PRODUCTO_NOMBRE,PRODUCTO_TIPO,PRODUCTO_SUBTIPO,
                                      PRODUCTO_ORIGEN,EFECTIVO_COMPRA,
                                      EFECTIVO_VENTA FROM GEN_VOL_RESUMEN
                                      WHERE {miembros_analisis_sql} AND {miembros_liq_analisis_sql} AND {segmentos_analisis_sql} AND
                                      FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Se verifica si segmentos_analisis o miembros_analisis o miembros_liq_analisis es diferente de nulo
  if (!is.null(segmentos_analisis) | !is.null(miembros_analisis) | !is.null(miembros_liq_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos  %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO, CUENTA_GARANTIA_TIPO,PRODUCTO_NOMBRE,
                           PRODUCTO_TIPO,PRODUCTO_SUBTIPO,PRODUCTO_ORIGEN),fill = list(EFECTIVO_COMPRA=0,EFECTIVO_VENTA=0)) %>%
    filter(!is.na(SEGMENTO_ID))

  return(datos)
}
