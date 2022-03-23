#' Descarga los datos gen_gar_dep_exi
#'
#' Esta función descarga los datos de la tabla gen_gar_dep_exi para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis. Por defecto NULL
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param miembros_analisis clase array character. Lista de miembros
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros
#' @param miembros_liq_analisis clase array character. Lista de miembros liquidadores
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros liquidadores
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CD","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_gar_dep_exi<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_analisis=NULL,miembros_liq_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

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
  datos <-  dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                    SEGMENTO_NOMBRE, MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                    MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO,GARANTIA_DEPOSITADA,
                                    GARANTIA_EXIGIDA,POSICION_COMPRADORA_VALORADA, POSICION_VENDEDORA_VALORADA,
                                    POSICION_BRUTA_VALORADA FROM GEN_GAR_DEP_EXI
                                    WHERE {miembros_analisis_sql} AND {miembros_liq_analisis_sql} AND {segmentos_analisis_sql} AND
                                    FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Se verifica si segmentos_analisis o miembros_analisis o miembros_liq_analisis es diferente de nulo
  if (!is.null(segmentos_analisis) | !is.null(miembros_analisis) | !is.null(miembros_liq_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO, CUENTA_GARANTIA_TIPO),
             fill = list(GARANTIA_DEPOSITADA=0,GARANTIA_EXIGIDA=0,POSICION_COMPRADORA_VALORADA=0,
                         POSICION_VENDEDORA_VALORADA=0,POSICION_BRUTA_VALORADA=0)) %>%
    filter(!is.na(SEGMENTO_ID))


  return(datos)
}

#' Descarga los datos gen_gar_dep_exi_liq
#'
#' Esta función descarga los datos de la tabla gen_gar_dep_exi_liq para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis. Por defecto NULL
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param miembros_liq_analisis clase array character. Lista de miembros liquidadores
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros liquidadores
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CD","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_gar_dep_exi_liq<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_liq_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte periodo_analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte miembros_liq_analisis a SQL
  miembros_liq_analisis_sql <- dt_miembros_analisis_sql(miembros_liq_analisis,liquidadores =TRUE)

  # Se covierte segmentos_analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <-  dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                    SEGMENTO_NOMBRE, MIEMBRO_LIQ_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_LIQ_ID_SEUDONIMO,
                                    MIEMBRO_LIQ_TIPO,GARANTIA_DEPOSITADA,GARANTIA_EXIGIDA,
                                    POSICION_COMPRADORA_VALORADA, POSICION_VENDEDORA_VALORADA,
                                    POSICION_BRUTA_VALORADA, RIESGO_ST, PATRIMONIO
                                    FROM GEN_GAR_DEP_EXI_LIQ
                                    WHERE {miembros_liq_analisis_sql} AND {segmentos_analisis_sql} AND
                                    FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Se verifica si segmentos_analisis o miembros_liq_analisis es diferente de nulo
  if (!is.null(segmentos_analisis) | !is.null(miembros_liq_analisis_sql)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_LIQ_ID_SEUDONIMO,MIEMBRO_LIQ_TIPO),
             fill = list(GARANTIA_DEPOSITADA=0,GARANTIA_EXIGIDA=0,POSICION_COMPRADORA_VALORADA=0,
                         POSICION_VENDEDORA_VALORADA=0,POSICION_BRUTA_VALORADA=0,RIESGO_ST=0,PATRIMONIO=0)) %>%
    filter(!is.na(SEGMENTO_ID))

  return(datos)
}

#' Descarga los datos gen_gar_exi_resumen
#'
#' Esta función descarga los datos de la tabla gen_gar_exi_resumen para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param miembros_analisis clase array character. Lista de miembros
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros
#' @param miembros_liq_analisis clase array character. Lista de miembros liquidadores
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros liquidadores
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CD","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_gar_exi_resumen<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_analisis=NULL,miembros_liq_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

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
  datos <-  dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                       SEGMENTO_NOMBRE, MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                       MIEMBRO_TIPO,PRODUCTO_DETALLE,GARANTIA_EXIGIDA,
                                       POSICION_COMPRADORA_VALORADA, POSICION_VENDEDORA_VALORADA,
                                       POSICION_BRUTA_VALORADA
                                       FROM GEN_GAR_EXI_RESUMEN
                                       WHERE {miembros_analisis_sql} AND {miembros_liq_analisis_sql} AND {segmentos_analisis_sql} AND
                                       FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Se verifica si segmentos_analisis o miembros_analisis o miembros_liq_analisis es diferente de nulo
  if (!is.null(segmentos_analisis) | !is.null(miembros_analisis) | !is.null(miembros_liq_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO,PRODUCTO_DETALLE),
             fill = list(GARANTIA_EXIGIDA=0,POSICION_COMPRADORA_VALORADA=0,
                         POSICION_VENDEDORA_VALORADA=0,POSICION_BRUTA_VALORADA=0)) %>%
    filter(!is.na(SEGMENTO_ID))

  return(datos)
}


#' Descarga los datos gen_gar_ggl_ind_fgc_liq
#'
#' Esta función descarga los datos de la tabla gen_gar_ggl_ind_fgc_liq para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis. Por defecto NULL
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param miembros_liq_analisis clase array character. Lista de miembros liquidadores
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros liquidadores
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CD","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_gar_ggl_ind_fgc_liq<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_liq_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte periodo_analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte miembros_liq_analisis a SQL
  miembros_liq_analisis_sql <- dt_miembros_analisis_sql(miembros_liq_analisis,liquidadores =TRUE)

  # Se covierte segmentos_analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <-  dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID,
                                    SEGMENTO_NOMBRE, MIEMBRO_LIQ_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_LIQ_ID_SEUDONIMO,
                                    MIEMBRO_LIQ_TIPO, GARANTIA_TIPO, IMPORTE FROM GEN_GAR_GGL_IND_FGC_LIQ
                                    WHERE {miembros_liq_analisis_sql} AND {segmentos_analisis_sql} AND
                                    FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

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
#' @param miembros_liq_analisis clase array character. Lista de miembros liquidadores
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros liquidadores
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_lmc_consumo<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_liq_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte periodo_analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte miembros_liq_analisis a SQL
  miembros_liq_analisis_sql <- dt_miembros_analisis_sql(miembros_liq_analisis,liquidadores =TRUE)

  # Descarga datos
  datos <-  dbGetQuery(conexion , glue("SELECT FECHA,
                                    MIEMBRO_LIQ_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_LIQ_ID_SEUDONIMO,
                                    MIEMBRO_LIQ_TIPO,LIMITE, RIESGO, GARANTIA_DEPOSITADA,
                                    GARANTIA_EXIGIDA, LIMITE_AJUSTADO, CONSUMO_LIMITE
                                    FROM GEN_LMC_CONSUMO
                                    WHERE {miembros_liq_analisis_sql} AND
                                    FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  return(datos)
}

