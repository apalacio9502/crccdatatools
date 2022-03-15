#' Descarga los datos gen_gar_depo_resumen
#'
#' Esta función descarga los datos de la tabla gen_gar_depo_resumen para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param miembros_analisis clase array character. Lista de miembros
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CN","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_gar_dep_resumen<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte el miembros analisis a SQL
  miembros_analisis_sql <- dt_miembros_analisis_sql(miembros_analisis)

  # Se covierte el segmentos analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                           SEGMENTO_NOMBRE, MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                           MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO,ACTIVO_TIPO,
                                           VOLUMEN, IMPORTE_ANTES_HAIRCUT, IMPORTE
                                           FROM GEN_GAR_DEP_RESUMEN
                                           WHERE {miembros_analisis_sql} AND {segmentos_analisis_sql} AND
                                           FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Se verifica si segmentos_analisis o miembros_analisis es diferente de nulo
  if (!is.null(segmentos_analisis) | !is.null(miembros_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <-  datos %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO, CUENTA_GARANTIA_TIPO,ACTIVO_TIPO),
             fill = list(VOLUMEN=0,IMPORTE_ANTES_HAIRCUT=0,IMPORTE=0))

  return(datos)
}

#' Descarga los datos gen_cm_titulos
#'
#' Esta función descarga los datos de la tabla gen_cm_titulos para un periodo de análisis y
#' con base en los parametros ingresados. Adicionalmente tranforma la tabla (Cambia su estructura a pivot_longer)
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param miembros_analisis clase array character. Lista de miembros
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CN","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_cm_titulos<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte el miembros analisis a SQL
  miembros_analisis_sql <- dt_miembros_analisis_sql(miembros_analisis)

  # Se covierte el segmentos analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID, SEGMENTO_NOMBRE,
                                     MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                     REPLACE(CUENTA_GARANTIA_ID,MIEMBRO_ID,MIEMBRO_{dt_id_seudonimo(seudonimo)}) AS CUENTA_GARANTIA_ID_SEUDONIMO,
                                     CUENTA_GARANTIA_TIPO, ACTIVO_DESCRIPCION,
                                     VOLUMEN_GARANTIA,IMPORTE_GARANTIA_DESPUES_HAIRCUT,
                                     IMPORTE_GARANTIA_HAIRCUT
                                     FROM GEN_CM_TITULOS
                                     WHERE {miembros_analisis_sql} AND {segmentos_analisis_sql} AND
                                     FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Descarga datos_complemento
  datos_complemento <- dbGetQuery(conexion, glue("SELECT FECHA, ACTIVO_DESCRIPCION,
                                     MAX(VOLUMEN_MEC_SEN) AS VOLUMEN_MEC_SEN,
                                     MAX(VOLUMEN_SIMULTANEAS) AS VOLUMEN_SIMULTANEAS,
                                     MAX(IMPORTE_MEC_SEN) AS IMPORTE_MEC_SEN,
                                     MAX(IMPORTE_SIMULTANEAS) AS IMPORTE_SIMULTANEAS
                                     FROM GEN_CM_TITULOS
                                     WHERE {miembros_analisis_sql} AND {segmentos_analisis_sql} AND
                                     FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}
                                     GROUP BY FECHA, ACTIVO_DESCRIPCION"))

  # Se verifica si segmentos_analisis o miembros_analisis es diferente de nulo
  if (!is.null(segmentos_analisis) | !is.null(miembros_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <-  datos %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO, CUENTA_GARANTIA_ID_SEUDONIMO,
                           CUENTA_GARANTIA_TIPO, ACTIVO_DESCRIPCION),
             fill = list(VOLUMEN_GARANTIA=0,IMPORTE_GARANTIA_DESPUES_HAIRCUT=0,IMPORTE_GARANTIA_HAIRCUT=0)) %>%
    mutate(IMPORTE_GARANTIA=IMPORTE_GARANTIA_DESPUES_HAIRCUT+IMPORTE_GARANTIA_HAIRCUT) %>%
    left_join(datos_complemento,by = c("FECHA", "ACTIVO_DESCRIPCION")) %>%
    replace_na(list(VOLUMEN_MEC_SEN=0,VOLUMEN_SIMULTANEAS=0,IMPORTE_MEC_SEN=0,IMPORTE_SIMULTANEAS=0))

  return(datos)
}

#' Descarga los datos gen_cm_acciones
#'
#' Esta función descarga los datos de la tabla gen_cm_acciones para un periodo de análisis y
#' con base en los parametros ingresados. Adicionalmente tranforma la tabla (Cambia su estructura a pivot_longer)
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param miembros_analisis clase array character. Lista de miembros
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CN","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_cm_acciones<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte el miembros analisis a SQL
  miembros_analisis_sql <- dt_miembros_analisis_sql(miembros_analisis)

  # Se covierte el segmentos analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID, SEGMENTO_NOMBRE,
                                     MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                     REPLACE(CUENTA_GARANTIA_ID,MIEMBRO_ID,MIEMBRO_{dt_id_seudonimo(seudonimo)}) AS CUENTA_GARANTIA_ID_SEUDONIMO,
                                     CUENTA_GARANTIA_TIPO, ACTIVO_DESCRIPCION,
                                     VOLUMEN_GARANTIA,IMPORTE_GARANTIA_DESPUES_HAIRCUT,
                                     IMPORTE_GARANTIA_HAIRCUT
                                     FROM GEN_CM_ACCIONES
                                     WHERE {miembros_analisis_sql} AND {segmentos_analisis_sql} AND
                                     FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Descarga datos_complemento
  datos_complemento <- dbGetQuery(conexion, glue("SELECT FECHA, ACTIVO_DESCRIPCION,
                                     MAX(VOLUMEN_CONTADO) AS VOLUMEN_CONTADO,
                                     MAX(VOLUMEN_ADR) AS VOLUMEN_ADR,
                                     MAX(IMPORTE_CONTADO) AS IMPORTE_CONTADO,
                                     MAX(IMPORTE_ADR) AS IMPORTE_ADR
                                     FROM GEN_CM_ACCIONES
                                     WHERE {miembros_analisis_sql} AND {segmentos_analisis_sql} AND
                                     FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}
                                     GROUP BY FECHA, ACTIVO_DESCRIPCION"))

  # Se verifica si segmentos_analisis o miembros_analisis es diferente de nulo
  if (!is.null(segmentos_analisis) | !is.null(miembros_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <-  datos %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO, CUENTA_GARANTIA_ID_SEUDONIMO,
                           CUENTA_GARANTIA_TIPO, ACTIVO_DESCRIPCION),
             fill = list(VOLUMEN_GARANTIA=0,IMPORTE_GARANTIA_DESPUES_HAIRCUT=0,IMPORTE_GARANTIA_HAIRCUT=0)) %>%
    mutate(IMPORTE_GARANTIA=IMPORTE_GARANTIA_DESPUES_HAIRCUT+IMPORTE_GARANTIA_HAIRCUT) %>%
    left_join(datos_complemento,by = c("FECHA", "ACTIVO_DESCRIPCION")) %>%
    replace_na(list(VOLUMEN_CONTADO=0,VOLUMEN_ADR=0,IMPORTE_CONTADO=0,IMPORTE_ADR=0))


  return(datos)
}

#' Promedio diario de los datos gen_cm_titulos o gen_cm_acciones
#'
#' Esta función calcula el promedio diario de los datos gen_cm_titulos o gen_cm_acciones.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' @param activo clase string. Nombre del activo ("Titulos" o "Acciones")
#' \code{\link{dt_gen_cm_titulos}} / \code{\link{dt_gen_cm_acciones}} o tener una estructura igual a dichos datos
#' @export

dt_gen_cm_promedio_diario<- function(datos,activo){

  # Se verifica si el activo es Acciones
  if (activo=="Titulos") {
    # Se modifica el dataframe datos
    datos <- datos %>%
      group_by(SEGMENTO_ID,MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_ID_SEUDONIMO,
               CUENTA_GARANTIA_TIPO,ACTIVO_DESCRIPCION) %>%
      summarise(across(c(VOLUMEN_GARANTIA:IMPORTE_SIMULTANEAS), ~mean(.x)),.groups="drop")

  }else{
    # Se modifica el dataframe datos
    datos <- datos %>%
      group_by(SEGMENTO_ID,MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_ID_SEUDONIMO,
               CUENTA_GARANTIA_TIPO,ACTIVO_DESCRIPCION) %>%
      summarise(across(c(VOLUMEN_GARANTIA:IMPORTE_ADR), ~mean(.x)),.groups="drop")
  }



  return(datos)
}


#' Descarga los datos gen_cm_remuneracion_resumen
#'
#' Esta función descarga los datos de la tabla gen_cm_remuneracion_resumen para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param miembros_analisis clase array character. Lista de miembros
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los miembros
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CD","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_gen_cm_remuneracion_resumen<- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,miembros_analisis=NULL,segmentos_analisis=NULL,seudonimo="REAL"){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se covierte el miembros analisis a SQL
  miembros_analisis_sql <- dt_miembros_analisis_sql(miembros_analisis)

  # Se covierte el segmentos analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID,
                                           SEGMENTO_NOMBRE, MIEMBRO_{dt_id_seudonimo(seudonimo)} AS MIEMBRO_ID_SEUDONIMO,
                                           MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO, ESTADO,
                                           GARANTIA, UTILIDAD_REMUNERACION, TARIFA_REMUNERACION
                                           FROM GEN_CM_REMUNERACION_RESUMEN
                                           WHERE {miembros_analisis_sql} AND {segmentos_analisis_sql} AND
                                           FECHA BETWEEN {periodo_analisis_sql[1]}
                                           AND {periodo_analisis_sql[2]}"))

  # Se verifica si segmentos_analisis es diferente de nulo
  if (!is.null(segmentos_analisis) | !is.null(miembros_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_adm_gen_fechas(conexion=conexion,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
  }

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>%
    complete(FECHA,nesting(SEGMENTO_ID, SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO,
                           CUENTA_GARANTIA_TIPO, ESTADO),
             fill = list(GARANTIA=0,UTILIDAD_REMUNERACION=0,TARIFA_REMUNERACION=0))

  return(datos)
}



