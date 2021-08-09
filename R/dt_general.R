#' Convertir periodo análisis a formato SQL (system)
#'
#' Esta función convierte un periodo de análisis a formato SQL dependiendo del tipo de base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"

dt_periodo_analisis_sql<- function(periodo_analisis,proveedor="MySQL"){
  # Se verifica si el proveedor es Oracle
  if (proveedor=="Oracle") {
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
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"

dt_fecha_analisis_sql<- function(fecha_analisis,proveedor="MySQL"){
  # Se verifica si el proveedor es Oracle
  if (proveedor=="Oracle") {
    # Se crea la variable fecha_analisis_sql
    fecha_analisis_sql <- glue("TO_DATE('{fecha_analisis}','YYYY-MM-DD')")
  }else{
    # Se crea la variable fecha_analisis_sql
    fecha_analisis_sql <-glue("'{fecha_analisis}'")
  }
  return(fecha_analisis_sql)
}

#' Convertir segmentos_analisis a formato SQL (system)
#'
#' Esta función convierte una lista de segmentos a formato SQL
#' @param segmentos_analisis clase array character. Debe contener la lista de segmentos análisis
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"

dt_segmentos_analisis_sql<- function(segmentos_analisis,proveedor="MySQL"){
  # Se verifica si segmentos_analisis es nulo
  if (is.null(segmentos_analisis)) {
    # Se crea la variable segmentos_analisis_sql
    segmentos_analisis_sql <- glue("SEGMENTO_ID NOT IN ('')")
  }else{
    # Se crea la variable segmentos_analisis_sql
    segmentos_analisis_sql <- glue("SEGMENTO_ID IN ('{paste0(segmentos_analisis,collapse = \"','\")}')")
  }
  return(segmentos_analisis_sql)
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

#' Convertir numero a caracter (system)
#'
#' Esta función convierte un numero a caracter. Unicamente se puede aplicar a un data.frame
#' @param x clase column. Columna sobre la cual se va a realizar la converción

dt_num_char <- function(x){
  x <- as.numeric(x)
  paste0(if_else(str_length(x)==1,"0",""),x)
}

#' Descargar los datos adm_fechas
#'
#' Esta función descarga los datos de la tabla adm_fechas para un periodo de análisis
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis

dt_fechas<- function(conexion,proveedor="MySQL",periodo_analisis){

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis,proveedor)

  # Descarga datos
  datos <- dbGetQuery(conexion, glue("SELECT FECHA FROM ADM_FECHAS WHERE
                                          DIA_SEMANA NOT IN (6,7) AND FESTIVO<>1 AND
                                          FECHA BETWEEN {periodo_analisis_sql[1]}
                                          AND {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))


  return(datos)
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
  datos <- dbReadTable(conexion,"ADM_COLORES")

  return(datos)
}

#' Abrir conexion de la base de datos
#'
#' Esta función abre la conexion de la base de datos
#' @param config clase data.frame. Configuración de la conexión
#' @param proveedor clase character. Se debe especificar le proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @export

dt_abrir_conexion <- function(config,proveedor="MySQL"){
  if (proveedor=="Oracle") {
    # Se crea la conexión con la bodega de datos
    conexion <- dbConnect(ROracle::Oracle(),username=config$username,password=config$password,dbname=config$dbname)
  }else{
    # Se crea la conexión con la bodega de datos
    conexion <- dbConnect(drv= RMySQL::MySQL() , port=config$port, host=config$host, username=config$username,
                                    dbname=config$dbname, password=config$password)
  }
}




