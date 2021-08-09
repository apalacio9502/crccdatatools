#' Descarga los datos gen_garantia_depositada_resumen
#'
#' Esta función descarga los datos de la tabla gen_garantia_depositada_resumen para un periodo de análisis y
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
#' @export

dt_gen_gar_dep_resumen<- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL,segmentos_analisis=NULL,ficticio=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Se covierte el segmentos analisis a SQL
  segmentos_analisis_sql <- dt_segmentos_analisis_sql(segmentos_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion , glue("SELECT FECHA, SEGMENTO_ID,
                                           SEGMENTO_NOMBRE, MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                           MIEMBRO_TIPO,CUENTA_GARANTIA_TIPO,ACTIVO_TIPO,
                                           VOLUMEN, IMPORTE_ANTES_HAIRCUT, IMPORTE
                                           FROM GEN_GARANTIA_DEPOSITADA_RESUMEN
                                           WHERE {segmentos_analisis_sql} AND
                                           FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  # Se verifica si segmentos_analisis es diferente de nulo
  if (!is.null(segmentos_analisis)) {
    # Se agregan todas las posibles fechas del periodo de análisis
    datos <- dt_fechas(conexion=conexion,proveedor=proveedor,periodo_analisis=periodo_analisis) %>% left_join(datos,by="FECHA")
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
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @export

dt_gen_cm_titulos<- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID, SEGMENTO_NOMBRE,
                                     MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                     CUENTA_GARANTIA_{dt_ficticio_sql(ficticio)} AS CUENTA_GARANTIA_ID_SEUDONIMO,
                                     CUENTA_GARANTIA_TIPO, ACTIVO_DESCRIPCION,
                                     VOLUMEN_GARANTIA, VOLUMEN_MEC_SEN, VOLUMEN_SIMULTANEAS,
                                     IMPORTE_GARANTIA_DESPUES_HAIRCUT,
                                     IMPORTE_GARANTIA_HAIRCUT, IMPORTE_MEC_SEN, IMPORTE_SIMULTANEAS
                                     FROM GEN_CM_TITULOS
                                     WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND {periodo_analisis_sql[2]}"))


  # Se modifica el dataframe datos (Se cambia la estructura a pivot_longer)
  datos <- datos %>%
    pivot_longer(c(VOLUMEN_GARANTIA, VOLUMEN_MEC_SEN, VOLUMEN_SIMULTANEAS,
                   IMPORTE_GARANTIA_DESPUES_HAIRCUT,
                   IMPORTE_GARANTIA_HAIRCUT, IMPORTE_MEC_SEN, IMPORTE_SIMULTANEAS),names_to ="VARIABLE",values_to = "VALOR") %>%
    mutate(UNIDAD=case_when(str_detect(VARIABLE,"VOLUMEN")==TRUE ~"NOMINAL",
                            TRUE~"EFECTIVO"),
           VARIABLE=case_when(str_detect(VARIABLE,"GARANTIA_DESPUES_HAIRCUT")==TRUE~ "Garantia Con HC",
                              str_detect(VARIABLE,"GARANTIA_HAIRCUT")==TRUE~ "HC Garantia",
                              str_detect(VARIABLE,"GARANTIA")==TRUE~ "Garantia",
                              str_detect(VARIABLE,"SIMULTANEAS")==TRUE~ "Simultaneas",
                              str_detect(VARIABLE,"MEC_SEN")==TRUE~ "MEC+SEN"),.before="VARIABLE")

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>%
    filter(VARIABLE %in% c("Garantia Con HC","HC Garantia","Garantia")) %>%
    bind_rows(datos %>% filter(!VARIABLE %in% c("Garantia Con HC","HC Garantia","Garantia")) %>%
                distinct(FECHA,SEGMENTO_ID="NA",SEGMENTO_NOMBRE="NA",MIEMBRO_ID_SEUDONIMO="NA",
                         CUENTA_GARANTIA_ID_SEUDONIMO="NA",CUENTA_GARANTIA_TIPO="NA",
                         ACTIVO_DESCRIPCION,UNIDAD,VARIABLE,VALOR)) %>%
    complete(FECHA,nesting(SEGMENTO_ID,SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,
                           CUENTA_GARANTIA_ID_SEUDONIMO,CUENTA_GARANTIA_TIPO,
                           ACTIVO_DESCRIPCION,UNIDAD,VARIABLE), fill = list(VALOR=0))

  return(datos)
}

#' Descarga los datos gen_cm_acciones
#'
#' Esta función descarga los datos de la tabla gen_cm_acciones para un periodo de análisis y
#' con base en los parametros ingresados. Adicionalmente tranforma la tabla (Cambia su estructura a pivot_longer)
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @export

dt_gen_cm_acciones<- function(conexion,proveedor="MySQL",periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA, SEGMENTO_ID,
                                    MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                    CUENTA_GARANTIA_{dt_ficticio_sql(ficticio)} AS CUENTA_GARANTIA_ID_SEUDONIMO,
                                    CUENTA_GARANTIA_TIPO, ACTIVO_DESCRIPCION,
                                    VOLUMEN_GARANTIA, VOLUMEN_CONTADO, VOLUMEN_ADR,
                                    IMPORTE_GARANTIA_DESPUES_HAIRCUT,
                                    IMPORTE_GARANTIA_HAIRCUT, IMPORTE_CONTADO, IMPORTE_ADR
                                    FROM GEN_CM_ACCIONES
                                    WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                    AND {periodo_analisis_sql[2]}"))


  # Se modifica el dataframe datos (Se cambia la estructura a pivot_longer)
  datos <- datos %>%
    pivot_longer(c(VOLUMEN_GARANTIA, VOLUMEN_CONTADO, VOLUMEN_ADR,
                   IMPORTE_GARANTIA_DESPUES_HAIRCUT,
                   IMPORTE_GARANTIA_HAIRCUT, IMPORTE_CONTADO, IMPORTE_ADR),names_to ="VARIABLE",values_to = "VALOR") %>%
    mutate(UNIDAD=case_when(str_detect(VARIABLE,"VOLUMEN")==TRUE ~"ACCIONES",
                            TRUE~"EFECTIVO"),
           VARIABLE=case_when(VARIABLE =="IMPORTE_GARANTIA_DESPUES_HAIRCUT"~ "Garantia Con HC",
                              VARIABLE =="IMPORTE_GARANTIA_HAIRCUT"~ "HC Garantia",
                              VARIABLE =="VOLUMEN_GARANTIA" ~ "Garantia",
                              VARIABLE %in% c("VOLUMEN_CONTADO","IMPORTE_CONTADO")~ "Contado",
                              VARIABLE %in% c("VOLUMEN_ADR","IMPORTE_ADR")==TRUE~ "ADR"),.before="VARIABLE")

  # Se modifica el dataframe datos (Se completan los datos con la función complete)
  datos <- datos %>%
    filter(VARIABLE %in% c("Garantia Con HC","HC Garantia","Garantia")) %>%
    bind_rows(datos %>% filter(!VARIABLE %in% c("Garantia Con HC","HC Garantia","Garantia")) %>%
                distinct(FECHA,SEGMENTO_ID="NA",SEGMENTO_NOMBRE="NA",MIEMBRO_ID_SEUDONIMO="NA",
                         CUENTA_GARANTIA_ID_SEUDONIMO="NA",CUENTA_GARANTIA_TIPO="NA",
                         ACTIVO_DESCRIPCION,UNIDAD,VARIABLE,VALOR)) %>%
    complete(FECHA,nesting(SEGMENTO_ID,SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO,
                           CUENTA_GARANTIA_ID_SEUDONIMO,CUENTA_GARANTIA_TIPO,
                           ACTIVO_DESCRIPCION,UNIDAD,VARIABLE), fill = list(VALOR=0))

  return(datos)
}

#' Promedio diario de los datos gen_cm_titulos o gen_cm_acciones
#'
#' Esta función calcula el promedio diario de los datos gen_cm_titulos o gen_cm_acciones.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_titulos_periodo}} / \code{\link{dt_gen_cm_acciones_periodo}} o tener una estructura igual a dichos datos
#' @export

dt_gen_cm_promedio_diario<- function(datos){

  # Se modifica el dataframe datos
  datos <- datos %>%
    group_by(MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_ID_SEUDONIMO,
             CUENTA_GARANTIA_TIPO,ACTIVO_DESCRIPCION,UNIDAD,VARIABLE) %>%
    summarise(VALOR=mean(VALOR),.groups="drop")

  return(datos)

  return(datos)
}



