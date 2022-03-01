#' Convertir periodo análisis a formato SQL (system)
#'
#' Esta función convierte un periodo de análisis a formato SQL dependiendo del tipo de base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis

dt_periodo_analisis_sql<- function(periodo_analisis){

  # Se crea la lista periodo_analisis_sql
  periodo_analisis_sql <- c(glue("TO_DATE('{periodo_analisis[1]}','YYYY-MM-DD')"), glue("TO_DATE('{periodo_analisis[2]}','YYYY-MM-DD')"))

  return(periodo_analisis_sql)
}

#' Convertir fecha análisis a formato SQL (system)
#'
#' Esta función convierte una fecha de análisis a formato SQL dependiendo del tipo de base de datos
#' @param fecha_analisis clase date. Debe contener la fecha del análisis

dt_fecha_analisis_sql<- function(fecha_analisis){

  # Se crea la variable fecha_analisis_sql
  fecha_analisis_sql <- glue("TO_DATE('{fecha_analisis}','YYYY-MM-DD')")

  return(fecha_analisis_sql)
}

#' Convertir segmentos_analisis a formato SQL (system)
#'
#' Esta función convierte una lista de segmentos a formato SQL
#' @param segmentos_analisis clase array character. Debe contener la lista de segmentos análisis

dt_segmentos_analisis_sql<- function(segmentos_analisis){
  # Se verifica si segmentos_analisis es nulo
  if (is.null(segmentos_analisis)) {
    # Se crea la variable segmentos_analisis_sql
    segmentos_analisis_sql <- glue("SEGMENTO_ID NOT IN (' ')")
  }else{
    # Se crea la variable segmentos_analisis_sql
    segmentos_analisis_sql <- glue("SEGMENTO_ID IN ('{paste0(segmentos_analisis,collapse = \"','\")}')")
  }
  return(segmentos_analisis_sql)
}

#' Convertir miembros_analisis a formato SQL (system)
#'
#' Esta función convierte una lista de miembros a formato SQL
#' @param miembros_analisis clase array character. Debe contener la lista de miembros análisis

dt_miembros_analisis_sql<- function(miembros_analisis){
  # Se verifica si miembros_analisis es nulo
  if (is.null(miembros_analisis)) {
    # Se crea la variable miembros_analisis_sql
    miembros_analisis_sql <- glue("MIEMBRO_ID NOT IN (' ')")
  }else{
    # Se crea la variable miembros_analisis_sql
    miembros_analisis_sql <- glue("MIEMBRO_ID IN ('{paste0(miembros_analisis,collapse = \"','\")}')")
  }
  return(miembros_analisis_sql)
}

#' ID_SEUDONIMO Miembros
#'
#' Esta función devuelve el ID_SEUDONIMO correspondiente acorde a la condición.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".

dt_id_seudonimo<- function(seudonimo){
  if_else(seudonimo=="REAL","ID","ID_FICTICIO")
}

#' NOMBRE_SEUDONIMO Miembros
#'
#' Esta función devuelve el NOMBRE_ABREVIACION_SEUDONIMO correspondiente acorde a la condición.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".

dt_nombre_abreviacion_seudonimo<- function(seudonimo){
  if_else(seudonimo=="REAL","NOMBRE_ABREVIACION","NOMBRE_ABREVIACION_FICTICIO")
}

#' Devuelve el sql para extraer CUENTA_GARANTIA_TITULAR_SEUDONIMO
#'
#' Esta función devuelve el sql para extraer la CUENTA_GARANTIA_TITULAR_SEUDONIMO acorde a la condición.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".

dt_cuenta_garantia_titular_seudonimo_sql<- function(seudonimo){
  if_else(seudonimo=="REAL","CUENTA_GARANTIA_TITULAR","'No Aplica'")
}

#' Devuelve el sql para extraer CUENTA_GARANTIA_IDENTIFICACION_SEUDONIMO
#'
#' Esta función devuelve el sql para extraer la CUENTA_GARANTIA_IDENTIFICACION_SEUDONIMO acorde a la condición.
#' @param seudonimo clase character. Debe ser igual a "REAL" o "FICTICIO".

dt_cuenta_garantia_identificacion_seudonimo_sql<- function(seudonimo){
  if_else(seudonimo=="REAL","CUENTA_GARANTIA_IDENTIFICACION","TRANSLATE(CUENTA_GARANTIA_IDENTIFICACION, '0123456789','ZXYABCLMNK')")
}


#' Convertir un numero a porcentaje (system)
#'
#' Esta función convierte un numero a porcentaje retornando el valor como un caracter
#' @param x clase numeric. Valor a convertir
#' @param digitos clase integer. Número de digitos a contemplar en el porcentaje

dt_porcentaje_caracter <- function(x,digitos=1){
  paste0(round(if_else(is.nan(x),0,x)*100,digitos),"%")
}

#' Calcular porcentaje de variación (system)
#'
#' Esta función calcula el porcentaje de variación de una variable entre dos filas
#' consecutivas. Unicamente se puede aplicar a un data.frame
#' @param x clase column. Columna sobre la cual se va a calcular la variación

dt_porcentaje_variacion<- function(x){
  paste0(round(if_else(lag(x)!=0,x /lag(x )-1,if_else(x ==0,0,1))*100,2),"%")
}

#' Convertir numero a caracter (system)
#'
#' Esta función convierte un numero a caracter. Unicamente se puede aplicar a un data.frame
#' @param x clase column. Columna sobre la cual se va a realizar la converción

dt_num_char <- function(x){
  x <- as.numeric(x)
  paste0(if_else(str_length(x)==1,"0",""),x)
}

#' Filtrar el data.frame con base en los inputs
#'
#' Esta función filtra el data.frame con base en los inputs segmentos, miembros y cuentas
#' @param datos clase data.frame. Los datos a filtrar
#' @param fecha_analisis clase date. Debe contener la fecha del análisis
#' @param segmentos clase array character. Debe contener los segmentos que se desean filtrar. Por defecto NULL
#' @param miembros clase array character. Debe contener los miembros que se desean filtrar. Por defecto NULL
#' @param cuentas clase array character. Debe contener los cuentas que se desean filtrar. Por defecto NULL
#' @export

dt_filtro_datos<- function(datos,fecha_analisis=NULL,segmentos=NULL,miembros=NULL,cuentas=NULL,activos=NULL){

  # Se crea la lista de las columnas de los datos
  columnas <- colnames(datos)

  # Se verifica si se debe filtrar por FECHA
  if (!is.null(fecha_analisis) & "FECHA" %in% columnas) {
    # Se modifica el dataframe datos
    datos <- datos  %>% filter(FECHA==fecha_analisis)
  }

  # Se verifica si se debe filtrar por SEGMENTO_ID
  if (!is.null(segmentos) & "SEGMENTO_ID" %in% columnas) {
    # Se modifica el dataframe datos
    datos <- datos  %>% filter(SEGMENTO_ID %in% segmentos)
  }

  # Se verifica si se debe filtrar por MIEMBRO_ID_SEUDONIMO
  if (!is.null(miembros) & "MIEMBRO_ID_SEUDONIMO" %in% columnas) {
    # Se modifica el dataframe datos
    datos <- datos  %>% filter(MIEMBRO_ID_SEUDONIMO %in% miembros)
  }

  # Se verifica si se debe filtrar por MIEMBRO_LIQ_ID_SEUDONIMO
  if (!is.null(miembros) & "MIEMBRO_LIQ_ID_SEUDONIMO" %in% columnas) {
    # Se modifica el dataframe datos
    datos <- datos  %>% filter(MIEMBRO_LIQ_ID_SEUDONIMO %in% miembros)
  }

  # Se verifica si se debe filtrar por CUENTA_GARANTIA_TIPO
  if (!is.null(cuentas) & "CUENTA_GARANTIA_TIPO" %in% columnas) {
    # Se modifica el dataframe datos
    datos <- datos  %>% filter(CUENTA_GARANTIA_TIPO %in% cuentas)
  }

  # Se verifica si se debe filtrar por ACTIVO_DESCRIPCION
  if (!is.null(activos) & "ACTIVO_DESCRIPCION" %in% columnas) {
    # Se modifica el dataframe datos
    datos <- datos  %>% filter(ACTIVO_DESCRIPCION %in% activos)
  }

  return(datos)
}

#' Abrir conexion de la base de datos
#'
#' Esta función abre la conexion de la base de datos
#' @param config clase data.frame. Configuración de la conexión
#' @export

dt_abrir_conexion <- function(config){

  # Se crea la conexión con la bodega de datos
  conexion <-  dbConnect(drv = odbc::odbc(),timezone = "America/Bogota",timezone_out="America/Bogota",
                         Driver="Oracle",Host=config$host,SVC=config$dbname,
                         UID=config$username,  PWD=config$password,Port = config$port)

}



