#' Descarga los datos gen_ingresos_resumen
#'
#' Esta función descarga los datos de la tabla gen_ingresos_resumen para un periodo de análisis y
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

dt_gen_ing_resumen<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE,segmentos=NULL,oracle=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,oracle)

  # Descarga datos
  datos <- dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                      SEGMENTO_NOMBRE, MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                      MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO,PRODUCTO_NOMBRE,PRODUCTO_TIPO,PRODUCTO_SUBTIPO,
                                      PRODUCTO_ORIGEN,TARIFA_CONCEPTO, TARIFA_SANCION, TARIFA
                                      FROM MOD_GE_SUB_IC_INGRESOS
                                      WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                      AND {periodo_analisis_sql[2]}"))

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>% mutate(FECHA=ymd(FECHA)) %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO, CUENTA_GARANTIA_TIPO,
                           PRODUCTO_NOMBRE, PRODUCTO_TIPO,PRODUCTO_SUBTIPO,
                           PRODUCTO_ORIGEN,TARIFA_CONCEPTO, TARIFA_SANCION),
             fill = list(TARIFA=0)) %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA")

  return(datos)
}

#' Crea la tabla gen_ing_cumplimiento_presupuesto
#'
#' Esta función descarga los datos params_proyeccion_ingresos y los une a gen_ingresos_resumen (descargados con anterioridad)
#' creando la tabla gen_ing_cumplimiento_presupuesto (localmente).
#' @param conexion clase formal. Conexión base de datos
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @export

dt_gen_ing_cumplimiento_presupuesto<- function(conexion,datos,periodo_analisis=NULL,fecha_analisis=NULL){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Descarga datos_provisional
  datos_provisional <- dbGetQuery(conexion , glue("SELECT  FECHA_ANO_MES, SEGMENTO_ID,
                                                  SEGMENTO_NOMBRE, PRODUCTO_TIPO, PRODUCTO_NOMBRE,
                                                  PRODUCTO_ORIGEN, PROYECCION_DIARIA
                                                  FROM MOD_GE_SUB_PARAMS_PROYECCION_INGRESOS
                                                  WHERE FECHA_ANO_MES BETWEEN '{format(periodo_analisis[1], '%Y-%m')}'
                                                  AND '{format(periodo_analisis[2], '%Y-%m')}'"))

  # Se modifica el dataframe datos
  datos <- datos %>% distinct(FECHA,FECHA_ANO_MES) %>% left_join(datos_provisional) %>%
    left_join(datos %>% filter(TARIFA_CONCEPTO %in% c("Tarifa compensacion y liquidacion","Tarifa ajuste compensacion y liquidacion divisas"),TARIFA_SANCION==0 )%>%
        group_by(FECHA,SEGMENTO_ID,SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO,PRODUCTO_ORIGEN) %>%
        summarise(TARIFA=sum(TARIFA),.groups="drop")) %>%
    replace_na(list(TARIFA=0))

  return(datos)
}
