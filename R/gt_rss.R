#' Grafica el riesgo en situación de estres promedio 90 dias vs FGC (lineas)
#'
#' Esta función crea la gráfica del riesgo en situación de estres promedio 90 dias vs FGC en formato lineas.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_rss_promedio}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica (....). Por defecto c()
#' @export

gt_rss_promedio <- function(datos,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se convierte el SEGMENTO_NOMBRE en un factor
    datos <- datos %>% filter(MIEMBRO_LIQ_COLETIVIZADOR==1, !SEGMENTO_NOMBRE %in% botones_inactivos) %>%
      mutate(SEGMENTO_NOMBRE=factor(SEGMENTO_NOMBRE)) %>%
      arrange(SEGMENTO_NOMBRE)

    # Se crea la lista de segmetos
    segmentos <- levels(datos$SEGMENTO_NOMBRE)

    # Verificación inputs
    if (is.null(boton_activo) || !boton_activo %in% segmentos) boton_activo <- segmentos[1]

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% filter(POSICION<=2) %>%
      select(-c(MIEMBRO_LIQ_TIPO,MIEMBRO_LIQ_COLETIVIZADOR,RIESGO_ST,GARANTIA_EXIGIDA,GARANTIA_EXIGIDA_FGC)) %>%
      pivot_wider(names_from = POSICION, values_from = c(MIEMBRO_LIQ_ID_SEUDONIMO,RIESGO_ST_PROMEDIO)) %>%
      left_join(datos %>% group_by(FECHA,SEGMENTO_ID) %>%
                  summarise(GARANTIA_FGC=sum(GARANTIA_EXIGIDA_FGC),.groups = "drop"),by=c("FECHA","SEGMENTO_ID"))%>%
      mutate(RIESGO_ST_PROMEDIO_1=round(RIESGO_ST_PROMEDIO_1/1e+9,6),
             RIESGO_ST_PROMEDIO_2=round(RIESGO_ST_PROMEDIO_2/1e+9,6),
             GARANTIA_FGC=round(GARANTIA_FGC/1e+9,6),
             TEXT_RIESGO_ST_1=paste("Miles M",RIESGO_ST_PROMEDIO_1,"/",MIEMBRO_LIQ_ID_SEUDONIMO_1),
             TEXT_RIESGO_ST_2=paste("Miles M",RIESGO_ST_PROMEDIO_2,"/",MIEMBRO_LIQ_ID_SEUDONIMO_2),
             TEXT_GARANTIA_FGC=paste("Miles M",GARANTIA_FGC),
             VISIBLE=SEGMENTO_NOMBRE==boton_activo)


    # Se verifica si se debe crear el updatemenus
    if (length(segmentos)>1) {
      # Se crean los botones
      botones <- foreach(i=1:length(segmentos),.combine = append) %do% {
        visible <- segmentos[i]==segmentos
        list(list(label = segmentos[i],method = "restyle",
                  args = list(list(boton_activo=segmentos[i],
                                   visible = as.logical(rep(visible,3))))))
      }

      # Se crea el updatemenus
      updatemenus <- list(
        list(active = which(segmentos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
             yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones))
    }else{
      # Se crea el updatemenus
      updatemenus <- NULL
    }

    # Se crea la gráfica
    plot <- plot_ly(data=datos_completos,split=~SEGMENTO_NOMBRE,x=~FECHA,colors=c("#66c2a5","#8da0cb"),
                    hoverinfo="text+x+name") %>%
      add_lines(y=~RIESGO_ST_PROMEDIO_1,text=~TEXT_RIESGO_ST_1,visible=~VISIBLE,stackgroup="1",name="1st. RSS",color="1") %>%
      add_lines(y=~RIESGO_ST_PROMEDIO_2,text=~TEXT_RIESGO_ST_2,visible=~VISIBLE,stackgroup="1",name="2st. RSS",color="2") %>%
      add_lines(y=~GARANTIA_FGC,text=~TEXT_GARANTIA_FGC,visible=~VISIBLE,fillcolor = 'transparent',line = list(color="blue",dash = "dash"),stackgroup="2",name="FGC") %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2),
             updatemenus=updatemenus,
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


#' Tabla fgc resumen
#'
#' Esta función crea la tabla fgc en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_rss_fgc}} o tener una estructura igual a dichos datos
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_rss_fgc_resumen<- function(datos,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <- datos  %>%
    group_by(across(MIEMBRO_LIQ_ID_SEUDONIMO:MIEMBRO_LIQ_COLETIVIZADOR)) %>%
    summarise(across(RIESGO_ST_PROMEDIO:APORTACION_NUEVA, ~sum(.x)),.groups="drop") %>%
    arrange(desc(RIESGO_ST_PROMEDIO)) %>%
    group_by(MIEMBRO_LIQ_COLETIVIZADOR) %>%
    mutate(POSICION=row_number()) %>% ungroup() %>%
    transmute("ID Miembro"=MIEMBRO_LIQ_ID_SEUDONIMO,
              "Nombre Miembro"=MIEMBRO_LIQ_NOMBRE,
              "Colectiviza"=if_else(MIEMBRO_LIQ_COLETIVIZADOR==1,"Si","No"),
              "Posición Ranking"=row_number(),
              "Riesgo en Situación de Estrés Promedio"=RIESGO_ST_PROMEDIO,
              "Aportación Mínima"=APORTACION_MINIMA,
              "Aportación Actual"=APORTACION_ACTUAL,
              "Aportación Nueva"=APORTACION_NUEVA,
              "Variación Aportación"=if_else(APORTACION_ACTUAL!=0,APORTACION_NUEVA/APORTACION_ACTUAL-1,if_else(APORTACION_NUEVA ==0,0,1)))

  # Se crea la tabla
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatCurrency(c(5,6,7,8), '$',digits = 0) %>% formatPercentage(9,digits = 2)

  return(table)
}

#' Grafica el fgc diario (lineas)
#'
#' Esta función crea la gráfica del fgc diario en formato de lineas
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_rss_fgc}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' #' por la función colores
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_rss_fgc_diario<- function(datos,colores,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(VALOR=GARANTIA_EXIGIDA_FGC) %>%
      group_by(TIPO="SEGMENTO_NOMBRE",ID=SEGMENTO_NOMBRE,FECHA) %>%
      summarise(across(VALOR, ~round(sum(.x)/1e+9,6)),.groups="drop_last")%>%
      mutate(across(VALOR,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA,TIPO)  %>% group_by(FECHA,TIPO) %>%
      group_by(FECHA) %>%
      mutate(TEXTO=paste(VALOR,"Miles M /",dt_porcentaje_caracter(VALOR/sum(VALOR)), "P /",CAMBIO_VALOR,"C")) %>% ungroup() %>%
      ungroup() %>% mutate(COLOR_ID=as.character(as.numeric(fct_reorder(factor(ID),VALOR,.fun=mean,.desc=T))))

    # Se crea el vector_colores
    vector_colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA,colors = vector_colores,color=~COLOR_ID,alpha=1,
                    textposition = 'none') %>%
      add_lines(y=~VALOR,text=~TEXTO,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID,hoverinfo="text+x+name") %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Miles M-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el test del FGC (lineas)
#'
#' Esta función crea la gráfica del test del FGC en formato lineas.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_rss_test_fgc}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica (....). Por defecto c()
#' @export

gt_rss_test_fgc <- function(datos,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se convierte el SEGMENTO_NOMBRE en un factor
    datos <- datos %>% filter(!SEGMENTO_NOMBRE %in% botones_inactivos) %>%
      mutate(SEGMENTO_NOMBRE=factor(SEGMENTO_NOMBRE)) %>%
      arrange(SEGMENTO_NOMBRE)

    # Se crea la lista de segmetos
    segmentos <- levels(datos$SEGMENTO_NOMBRE)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% segmentos) boton_activo <- segmentos[1]

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(
      RIESGO_ST_1=round(RIESGO_ST_1/1e+9,6),
      RIESGO_ST_2=round(RIESGO_ST_2/1e+9,6),
      GARANTIA_GIST=round(GARANTIA_GIST/1e+9,6),
      GARANTIA_GGL=round(GARANTIA_GGL/1e+9,6),
      GARANTIA_GPT=round(GARANTIA_GPT/1e+9,6),
      GARANTIA_FGG=round((GARANTIA_FGC+GARANTIA_FGG_CRCC)/1e+9,6),
      TEXT_RIESGO_ST_1=paste("Miles M",RIESGO_ST_1,"/",MIEMBRO_LIQ_ID_SEUDONIMO_1),
      TEXT_RIESGO_ST_2=paste("Miles M",RIESGO_ST_2,"/",MIEMBRO_LIQ_ID_SEUDONIMO_2),
      TEXT_GARANTIA_GIST=paste("Miles M",GARANTIA_GIST),
      TEXT_GARANTIA_GGL=paste("Miles M",GARANTIA_GGL),
      TEXT_GARANTIA_GPT=paste("Miles M",GARANTIA_GPT),
      TEXT_GARANTIA_FGC=paste("Miles M",GARANTIA_FGC),
      VISIBLE=if_else(SEGMENTO_NOMBRE==boton_activo,TRUE,FALSE))

    # Se verifica si se debe crear el updatemenus
    if (length(segmentos)>1) {
      # Se crean los botones
      botones <- foreach(i=1:length(segmentos),.combine = append) %do% {
        visible <- segmentos[i]==segmentos
        list(list(label = segmentos[i],method = "restyle",
                  args = list(list(boton_activo=segmentos[i],
                                   visible = as.logical(rep(visible,5))))))
      }

      # Se crea el updatemenus
      updatemenus <- list(
        list(active = which(segmentos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
             yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones))
    }else{
      # Se crea el updatemenus
      updatemenus <- NULL
    }

    # Se crea la gráfica
    plot <- plot_ly(data=datos_completos,split=~SEGMENTO_NOMBRE,x=~FECHA,colors=c("#66c2a5","#8da0cb"),hoverinfo="text+x+name") %>%
      add_lines(y=~RIESGO_ST_1,text=~TEXT_RIESGO_ST_1,visible=~VISIBLE,stackgroup="1",name="1st. RSS",color="1") %>%
      add_lines(y=~RIESGO_ST_2,text=~TEXT_RIESGO_ST_2,visible=~VISIBLE,stackgroup="1",name="2st. RSS",color="2") %>%
      add_lines(y=~GARANTIA_FGG,text=~TEXT_GARANTIA_FGC,visible=~VISIBLE,fillcolor = 'transparent',line = list(color="blue",dash = "dash"),stackgroup="2",name="FGC+FGG") %>%
      add_lines(y=~GARANTIA_GIST,text=~TEXT_GARANTIA_GIST,visible=~VISIBLE,fillcolor = 'transparent',line = list(color="red",dash = "dot"),stackgroup="2",name="GI ST") %>%
      add_lines(y=~GARANTIA_GGL,text=~TEXT_GARANTIA_GGL,visible=~VISIBLE,fillcolor = 'transparent',line = list(color="orange",dash = "dash"),stackgroup="2",name="GGL") %>%
      add_lines(y=~GARANTIA_GPT,text=~TEXT_GARANTIA_GPT,visible=~VISIBLE,fillcolor = 'transparent',line = list(color="black",dash = "dot"),stackgroup="2",name="SOB GPT") %>%
      layout(hovermode = 'compare',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2),
             updatemenus=updatemenus,
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange))%>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

