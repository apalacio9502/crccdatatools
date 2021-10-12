#' Descarga los datos rv_vol
#'
#' Esta función descarga los datos de la tabla rv_vol para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @export

dt_rv_vol <- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Descarga datos
  datos <-   dbGetQuery(conexion, glue("SELECT * FROM RV_VOL WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND
                                       {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}

#' Descarga los datos rv_activos_elegibles_repo
#'
#' Esta función descarga los datos de la tabla rv_activos_elegibles_repo para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param proveedor clase character. Proveedor de la base de datos ("Oracle", "MySQL"). Por defecto "MySQL"
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @export

dt_rv_activos_elegibles_repo <- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Descarga datos
  datos <-   dbGetQuery(conexion, glue("SELECT * FROM RV_ACTIVOS_ELEGIBLES_REPOS WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                    AND {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}


#' Descarga los datos gen_pa para el producto repos
#'
#' Esta función descarga los datos de la tabla gen_pa para el producto repos, un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @param ficticio clase boolean. TRUE si se desea que el "ID_SEUDONIMO" de los miembros se igua al "ID_FICTICIO"  en
#' caso contrario sera igual al "ID". Por defecto FALSE
#' @param nominal clase boolean. TRUE si se desea descargar la posición abierta nominal Por defecto TRUE
#' @param valorada clase boolean. TRUE si se desea descargar la posición abierta valorada. Por defecto TRUE
#' @param importe clase boolean. TRUE si se desea descargar la posición abierta importe. Por defecto FALSE
#' @export

dt_rv_pa_repos <- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,
                           ficticio=FALSE,nominal=FALSE,valorada=TRUE,importe=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Se crea la campos_sql
  campos_sql <- ''

  # Se crea la lista fill_campos
  fill_campos <- list()

  # Se verifica si nominal es igual a true
  if (nominal==TRUE) {
    # Se agrega los nombres a la variable campos_sql
    campos_sql <- paste0(campos_sql,",POSICION_COMPRADORA,POSICION_VENDEDORA")
    # Se agrega los nombres a la variable fill_campos
    fill_campos <- append(fill_campos,list(POSICION_COMPRADORA=0,POSICION_VENDEDORA=0))
  }

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
  datos <-  dbGetQuery(conexion, glue("SELECT FECHA, MIEMBRO_{dt_ficticio_sql(ficticio)} AS MIEMBRO_ID_SEUDONIMO,
                                    MIEMBRO_TIPO, CUENTA_GARANTIA_TITULAR, CONTRATO_DESCRIPCION
                                    {campos_sql} FROM GEN_PA
                                    WHERE FECHA BETWEEN {periodo_analisis_sql[1]}
                                    AND {periodo_analisis_sql[2]} AND PRODUCTO_NOMBRE='Repos'"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA)) %>%
    filter(!is.na(MIEMBRO_ID_SEUDONIMO)) %>%
    group_by(FECHA,MIEMBRO_ID_SEUDONIMO,MIEMBRO_TIPO,CUENTA_GARANTIA_TITULAR,CONTRATO_DESCRIPCION) %>%
    summarise(across(fill_campos %>% names(),.fns = sum),.groups="drop")

  return(datos)
}


#' Descarga los datos rv_conc_repos_por_activo
#'
#' Esta función descarga los datos de la tabla rv_conc_repos_por_activo para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @export

dt_rv_conc_repos_por_activo <- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Descarga datos
  datos <-   dbGetQuery(conexion, glue("SELECT * FROM RV_CONC_REPOS_POR_ACTIVO
                                       WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND
                                       {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}


#' Descarga los datos rv_conc_repos_por_tercero
#'
#' Esta función descarga los datos de la tabla rv_conc_repos_por_tercero para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @export

dt_rv_conc_repos_por_tercero <- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Descarga datos
  datos <-   dbGetQuery(conexion, glue("SELECT * FROM RV_CONC_REPOS_POR_TERCERO
                                       WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND
                                       {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}


#' Descarga los datos rv_haircuts
#'
#' Esta función descarga los datos de la tabla rv_haircuts para un periodo de análisis y
#' con base en los parametros ingresados
#' @param conexion clase formal. Conexión base de datos
#' @param periodo_analisis clase array date. Debe contener la fecha inicio y fin del análisis
#' @param fecha_analisis clase date. Debe contener la fecha del análisis, si el parametro periodo_analisis es
#' diferente de NULL este parametro no se tendra en cuenta. Por defecto NULL
#' @export

dt_rv_haircuts <- function(conexion,periodo_analisis=NULL,fecha_analisis=NULL,ficticio=FALSE){

  # Se verifica si la descarga va hacer para una fecha de análisis
  if(is.null(periodo_analisis) & !is.null(fecha_analisis)) periodo_analisis <- rep(fecha_analisis,2)

  # Se covierte el periodo de analisis a SQL
  periodo_analisis_sql <-  dt_periodo_analisis_sql(periodo_analisis)

  # Descarga datos
  datos <-   dbGetQuery(conexion, glue("SELECT * FROM RV_HAIRCUTS
                                       WHERE FECHA BETWEEN {periodo_analisis_sql[1]} AND
                                       {periodo_analisis_sql[2]}"))

  # Se convierte la fecha de los datos en un date
  datos <- datos %>% mutate(FECHA=ymd(FECHA))

  return(datos)
}


