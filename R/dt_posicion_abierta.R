#' Descarga los datos gen_posicion_abierta_resumen
#'
#' Esta función descarga los datos de la tabla gen_posicion_abierta_resumen para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CN","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @param valorada clase boolean. TRUE si se desea descargar la posición abierta valorada. Por defecto TRUE
#' @param importe clase boolean. TRUE si se desea descargar la posición abierta importe. Por defecto FALSE
#' @export

dt_gen_pa_resumen <- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL,
                              segmentos_analisis=NULL,ficticio=FALSE,valorada=TRUE,importe=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Se covierte el segmentos analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Se crea la campos_sql
  campos_sql <- ''
  # Se crea la lista fill_campos
  fill_campos <- list()

  # Se verifica si valorada es igual a true
  if (valorada==TRUE) {
    # Se agrega los nombres a la variable campos_sql
    campos_sql <- paste0(campos_sql,",POSICION_COMPRADORA_VALORADA,POSICION_VENDEDORA_VALORADA")
    # Se agrega los nombres a la variable fill_campos
    fill_campos <- append(fill_campos,list(POSICION_COMPRADORA_VALORADA=0,POSICION_VENDEDORA_VALORADA=0))
  }

  # Se verifica si valorada es igual a importe
  if (importe==TRUE) {
    # Se agrega los nombres a la variable campos_sql
    campos_sql <- paste0(campos_sql,",POSICION_COMPRADORA_IMPORTE,POSICION_VENDEDORA_IMPORTE")
    # Se agrega los nombres a la variable fill_campos
    fill_campos <- append(fill_campos,list(POSICION_COMPRADORA_IMPORTE=0,POSICION_VENDEDORA_IMPORTE=0))
  }

  # Descarga datos
  datos <-  dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID,
                                    SEGMENTO_NOMBRE, MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                    MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO,PRODUCTO_NOMBRE,PRODUCTO_TIPO,PRODUCTO_SUBTIPO,PRODUCTO_ORIGEN
                                    {campos_sql} FROM GEN_POSICION_ABIERTA_RESUMEN
                                    WHERE {segmentos_analisis_sql} AND FECHA BETWEEN {periodo_analisis_sql[1]}
                                    AND {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  # Se verifica si segmentos_analisis es diferente de nulo
  if (!is.null(segmentos_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_fechas(conexion=conexion,proveedor=proveedor,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos  %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO, CUENTA_GARANTIA_TIPO,PRODUCTO_NOMBRE,
                           PRODUCTO_TIPO,PRODUCTO_SUBTIPO,PRODUCTO_ORIGEN),fill = fill_campos) %>%
    filter(!is.na(SEGMENTO_ID))

  return(datos)
}

