#' Descarga los datos gen_pa_resumen
#'
#' Esta función descarga los datos de la tabla gen_pa_resumen para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CN","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_pa_resumen <- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte el segmentos analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <-  dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID,
                                    SEGMENTO_NOMBRE, MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                    MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO,PRODUCTO_NOMBRE,PRODUCTO_TIPO,PRODUCTO_SUBTIPO,PRODUCTO_ORIGEN,
                                    POSICION_COMPRADORA_VALORADA,POSICION_VENDEDORA_VALORADA FROM GEN_PA_RESUMEN
                                    WHERE {segmentos_analisis_sql} AND FECHA BETWEEN {periodo_analisis_sql[1]}
                                    AND {periodo_analisis_sql[2]}"))

  # Se verifica si segmentos_analisis es diferente de nulo
  if (!is.null(segmentos_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos  %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO, CUENTA_GARANTIA_TIPO,PRODUCTO_NOMBRE,
                           PRODUCTO_TIPO,PRODUCTO_SUBTIPO,PRODUCTO_ORIGEN),fill = list(POSICION_COMPRADORA_VALORADA=0,POSICION_VENDEDORA_VALORADA=0)) %>%
    filter(!is.na(SEGMENTO_ID))

  return(datos)
}

