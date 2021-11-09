#' Descargar los datos adm_gen_fechas
#'
#' Esta función descarga los datos de la tabla adm_gen_fechas para un periodo de análisis
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @export

dt_adm_gen_fechas<- function(conexion,periodo_analisis){

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA FROM ADM_GEN_FECHAS WHERE
                                          DIA_SEMANA NOT IN (6,7) AND FESTIVO<>1 AND
                                          FECHA BETWEEN {periodo_analisis_sql[1]}
                                          AND {periodo_analisis_sql[2]}"))


  return(datos)
}

#' Descargar los datos adm_gen_miembros
#'
#' Esta función descarga los datos de la tabla adm_gen_miembros
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param segmentos_analisis clase array character. Lista de segmentos ("GE","CD","CV","C2","C7","C8","C9")
#' de los cuales se desea descargar la información. Por defecto descarga la información de todos los segmentos.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".Por defecto "REAL"
#' @export

dt_adm_gen_miembros<- function(conexion,segmentos_analisis=NULL,seudonimo="REAL"){

  # Se inicializa la variable
  segmentos_analisis_sql <- ""

  # Se verifica si segmentos_analisis es nulo
  if (!is.null(segmentos_analisis)) {
    # Se crea la variable segmentos_analisis_sql
    segmentos_analisis_sql <- glue("AND ({paste0(paste0(\"SEGMENTOS LIKE '%\",segmentos_analisis,\"%'\"),collapse = \" OR \")})")
  }

  # Descarga datos
  datos <- dbGetQuery(conexion,glue("SELECT {dt_id_seudonimo(seudonimo)} AS ID_SEUDONIMO,
                                    {dt_nombre_abreviacion_seudonimo(seudonimo)} AS NOMBRE_ABREVIACION_SEUDONIMO
                                    FROM ADM_GEN_MIEMBROS WHERE ID<>'CRCC' {segmentos_analisis_sql}"))


  return(datos)
}

#' Descarga los datos adm_gen_segmentos
#'
#' Esta función descarga los datos de la tabla adm_gen_segmentos
#' @export

dt_adm_gen_segmentos<- function(conexion){

  # Descarga datos
  datos <- dbReadTable(conexion,"ADM_GEN_SEGMENTOS")

  return(datos)
}

#' Descarga los datos adm_gen_cuentas
#'
#' Esta función descarga los datos de la tabla adm_gen_cuentas
#' @export

dt_adm_gen_cuentas<- function(conexion){

  # Descarga datos
  datos <- dbReadTable(conexion,"ADM_GEN_CUENTAS")

  return(datos)
}

#' Descarga los datos adm_gen_colores
#'
#' Esta función descarga los datos de la tabla adm_colores
#' @export

dt_adm_gen_colores<- function(conexion){

  # Descarga datos
  datos <- dbReadTable(conexion,"ADM_GEN_COLORES")

  return(datos)
}


