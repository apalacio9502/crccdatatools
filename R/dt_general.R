#' Convertir periodo análisis a formato SQL (system)
#'
#' Esta función convierte un periodo de análisis a formato SQL dependiendo del tipo de base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param oracle clase boolean. TRUE si la base de datos utilizada es oracle. Por defecto FALSE

dt_periodo_analisis_sql<- function(periodo_analisis,oracle=FALSE){
  # Se verifica si oracle es igual a true
  if (oracle==TRUE) {
    # Se crea la lista periodo_analisis_sql
    periodo_analisis_sql <- c(glue("TO_DATE('{periodo_analisis[1]}','YYYY-MM-DD')"), glue("TO_DATE('{periodo_analisis[2]}','YYYY-MM-DD')"))
  }else{
    # Se crea la lista periodo_analisis_sql
    periodo_analisis_sql <- c(glue("'{periodo_analisis[1]}'"), glue("'{periodo_analisis[2]}'"))
  }
  return(periodo_analisis_sql)
}

#' Convertir fecha análisis a formato SQL (system)
#'
#' Esta función convierte una fecha de análisis a formato SQL dependiendo del tipo de base de datos
#' @param fecha_analisis clase date. Debe contener la fecha del análisis
#' @param oracle clase boolean. TRUE si la base de datos utilizada es oracle. Por defecto FALSE

dt_fecha_analisis_sql<- function(fecha_analisis,oracle=FALSE){
  # Se verifica si oracle es igual a true
  if (oracle==TRUE) {
    # Se crea la variable fecha_analisis_sql
    fecha_analisis_sql <- glue("TO_DATE('{fecha_analisis}','YYYY-MM-DD')")
  }else{
    # Se crea la variable fecha_analisis_sql
    fecha_analisis_sql <-glue("'{fecha_analisis}'")
  }
  return(fecha_analisis_sql)
}

#' ID Miembros SQL (system)
#'
#' Esta función devuelve el ID correspondiente acorde a la condición.
#' @param ficticio clase boolean. TRUE si se desea descargar el 'ID_FICTICIO' de los miembros en
#' caso contrario se descargara el 'ID'. Por defecto FALSE

dt_ficticio_sql<- function(ficticio){
  if_else(ficticio==TRUE,"ID_FICTICIO","ID")
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


#' Filtrar el data.frame con base en los inputs
#'
#' Esta función filtra el data.frame con base en los inputs segmentos, miembros y cuentas
#' @param datos clase data.frame. Los datos a filtrar
#' @param fecha_analisis clase date. Debe contener la fecha del análisis
#' @param segmentos clase array character. Debe contener los segmentos que se desean filtrar. Por defecto NULL
#' @param miembros clase array character. Debe contener los miembros que se desean filtrar. Por defecto NULL
#' @param cuentas clase array character. Debe contener los cuentas que se desean filtrar. Por defecto NULL
#' @export

dt_filtro_datos<- function(datos,fecha_analisis=NULL,segmentos=NULL,miembros=NULL,cuentas=NULL){

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
  return(datos)
}


#' Descarga los datos adm_colores
#'
#' Esta función descarga los datos de la tabla adm_colores
#' @export

dt_colores<- function(conexion){

  # Descarga datos
  datos <- dbReadTable(conexion,"MOD_ADM_COLORES")

  return(datos)
}
