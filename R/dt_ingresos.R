#' Descarga los datos gen_ingresos_resumen
#'
#' Esta función descarga los datos de la tabla gen_ingresos_resumen para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CD","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @export

dt_gen_ing_resumen<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,segmentos_analisis=NULL,ficticio=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte el segmentos analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID,
                                           SEGMENTO_NOMBRE, MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                           MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO,PRODUCTO_NOMBRE,PRODUCTO_TIPO,PRODUCTO_SUBTIPO,
                                           PRODUCTO_ORIGEN,TARIFA_CONCEPTO, TARIFA
                                           FROM GEN_INGRESOS_RESUMEN
                                           WHERE {segmentos_analisis_sql} AND FECHA BETWEEN {periodo_analisis_sql[1]}
                                           AND {periodo_analisis_sql[2]} AND TARIFA_SANCION=0"))

  # Se verifica si segmentos_analisis es diferente de nulo
  if (!is.null(segmentos_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_fechas(conexion=conexion,proveedor=proveedor,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO, CUENTA_GARANTIA_TIPO,
                           PRODUCTO_NOMBRE, PRODUCTO_TIPO,PRODUCTO_SUBTIPO,
                           PRODUCTO_ORIGEN,TARIFA_CONCEPTO),
             fill = list(TARIFA=0))

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
                                                  FROM PA_GEN_PROYECCION_INGRESOS
                                                  WHERE FECHA_ANO_MES BETWEEN '{format(periodo_analisis[1], '%Y-%m')}'
                                                  AND '{format(periodo_analisis[2], '%Y-%m')}'"))

  # Se modifica el dataframe datos
  datos <- datos %>% distinct(FECHA) %>% mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>% left_join(datos_provisional,by="FECHA_ANO_MES") %>%
    left_join(datos %>% filter(TARIFA_CONCEPTO %in% c("Tarifa compensacion y liquidacion","Tarifa ajuste compensacion y liquidacion divisas") )%>%
        group_by(FECHA,SEGMENTO_ID,SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO,PRODUCTO_ORIGEN) %>%
        summarise(TARIFA=sum(TARIFA),.groups="drop")) %>%
    replace_na(list(TARIFA=0))

  return(datos)
}
