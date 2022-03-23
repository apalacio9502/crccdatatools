#' Tabla garantía depositada
#'
#' Esta función crea la tabla garantía depositada en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_resumen}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_gar_dep_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <- datos %>% bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado",ACTIVO_TIPO="Consolidado")) %>%
    group_by(FECHA,SEGMENTO_NOMBRE,ACTIVO_TIPO) %>%
    summarise(IMPORTE=sum(IMPORTE),.groups="drop") %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
    group_by(SEGMENTO_NOMBRE,ACTIVO_TIPO) %>%
    summarise(IMPORTE_DIARIO=sum(IMPORTE[FECHA==fecha_analisis]),
              IMPORTE_DIARIO_PROMEDIO_MENSUAL=mean(IMPORTE[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              IMPORTE_DIARIO_PROMEDIO_PERIODO=mean(IMPORTE),.groups="drop") %>%
    arrange(desc(IMPORTE_DIARIO)) %>%
    transmute(Segmento=SEGMENTO_NOMBRE,"Tipo Activo"=ACTIVO_TIPO,
              "%"=if_else(SEGMENTO_NOMBRE=="Consolidado",1,IMPORTE_DIARIO/sum(IMPORTE_DIARIO[SEGMENTO_NOMBRE!="Consolidado"])),
              "Garantía Depositada Último Día M-COP"=IMPORTE_DIARIO/1e+6,
              "Garantía Depositada Promedio Diario Último Mes M-COP"=IMPORTE_DIARIO_PROMEDIO_MENSUAL/1e+6,
              "Garantía Depositada Promedio Diario Periodo M-COP"=IMPORTE_DIARIO_PROMEDIO_PERIODO/1e+6)

  # Se crea la tabla
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatPercentage(3,digits = 2) %>% formatCurrency(c(4,5,6), '$',digits = 0)

  return(table)
}

#' Gráfica la garantía depositada (pie)
#'
#' Esta función crea la gráfica de la garantía depositada tipo activo en formato de pie
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @export

gt_gar_dep <- function(datos,colores){

  # Se filtran los datos
  datos <-  datos %>% filter(VOLUMEN>0 | IMPORTE_ANTES_HAIRCUT>0 | IMPORTE>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(TIPO="ACTIVO_TIPO",ID=ACTIVO_TIPO) %>%
      summarise(VALOR=round(sum(IMPORTE,na.rm = TRUE)/1e+12,6),.groups="drop_last") %>%
      mutate(TEXTO=paste(VALOR,"Billones /",dt_porcentaje_caracter(VALOR/sum(VALOR)),"P")) %>% ungroup() %>%
      mutate(COLOR_ID=dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T))) %>%
      arrange(COLOR_ID)

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
                        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la garantia depositada (Fecha Especifica)
    plot <- plot_ly(data= datos_completos ,labels=~ID) %>%
      add_pie(values=~VALOR,text=~TEXTO,textinfo='percent',hoverinfo="text",
              marker = list(colors =colores),domain = list(x = c(0, 1), y = c(0.1, 0.95))) %>%
      layout(legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             margin=list("l"=50,"r"=50)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la garantía depositada por miembro de una fecha de análisis (barras)
#'
#' Esta función crea la gráfica de la garantía depositada por tipo activo y miembro para una
#' fecha de análisis en formato de barras
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_gar_dep_por_miembro_fecha <- function(datos,colores,fixedrange=FALSE){

  # Se filtran los datos
  datos <-  datos %>% filter(VOLUMEN>0 | IMPORTE_ANTES_HAIRCUT>0 | IMPORTE>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO), IMPORTE,.fun=sum,.desc=T),VALOR=IMPORTE)  %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO="ACTIVO_TIPO",ID=ACTIVO_TIPO) %>% summarise(across(VALOR, ~round(sum(.x)/1e+12,6)),.groups="drop_last")%>%
      mutate(TEXTO=paste(VALOR,"Billones","/",dt_porcentaje_caracter(VALOR/sum(VALOR)),"P")) %>% ungroup() %>%
      mutate(COLOR_ID=dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T))) %>%
      arrange(COLOR_ID)

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la garantia depositada por miembro (Fecha Especifica)
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR,text=~TEXTO,name=~ID,
               legendgroup=~ID,hoverinfo="text+x+name") %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la garantía depositada diaria (lineas)
#'
#' Esta función crea la gráfica de la garantía depositada diaria en formato de lineas
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' #' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_gar_dep_diaria<- function(datos,colores,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(VALOR=IMPORTE) %>%
      group_by(TIPO="ACTIVO_TIPO",ID=ACTIVO_TIPO,FECHA) %>%
      summarise(across(VALOR, ~round(sum(.x)/1e+12,6)),.groups="drop_last")%>%
      mutate(across(VALOR,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA,TIPO)  %>% group_by(FECHA,TIPO) %>%
      mutate(TEXTO=paste(VALOR,"Billones /",dt_porcentaje_caracter(VALOR/sum(VALOR)), "P /",CAMBIO_VALOR,"C")) %>% ungroup() %>%
      mutate(COLOR_ID=dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T))) %>%
      arrange(COLOR_ID)

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la garantia depositada diaria
    plot <- plot_ly(data= datos_completos ,x=~FECHA,colors = colores,color=~COLOR_ID,alpha=1,
                    textposition = 'none') %>%
      add_lines(y=~VALOR,text=~TEXTO,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID,hoverinfo="text+x+name") %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la garantía depositada promedio diario por (Mes o Año) (barras)
#'
#' Esta función crea la gráfica de la garantía depositada promedio diario en formato de barras.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' #' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param promedio clase character. "m" si se desea promediar por mes y "y" si se desea promediar por año. Por defecto "m"
#' @export

gt_gar_dep_promedio_diario<- function(datos,colores,fixedrange=FALSE,promedio="m"){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se define la granularidad del promedio
    if (promedio=="m"){
      fecha_formato <- list(FORMATO_DATOS="%Y-%m",FORMATO_TIPO_GRAFICA="date",FORMATO_GRAFICA="%b-%Y")
    }else{
      fecha_formato <- list(FORMATO_DATOS="%Y",FORMATO_TIPO_GRAFICA=NULL,FORMATO_GRAFICA=NULL)
    }

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),VALOR=IMPORTE)  %>%
      group_by(TIPO="ACTIVO_TIPO",ID=ACTIVO_TIPO,FECHA_FORMATO,FECHA) %>%
      summarise(across(VALOR, ~sum(.x)/1e+12),.groups="drop_last")%>%
      summarise(across(VALOR, ~round(mean(.x),6)),.groups="drop_last")%>%
      mutate(across(VALOR,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO=paste(VALOR,"Billones /",dt_porcentaje_caracter(proportions(VALOR)), "P /",CAMBIO_VALOR,"C")) %>% ungroup() %>%
      mutate(COLOR_ID=dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T))) %>%
      arrange(COLOR_ID)

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la garantia depositada promedio diario por (Mes o Año)
    plot <- plot_ly(data= datos_completos ,x=~FECHA_FORMATO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR,text=~TEXTO,
               name=~ID,legendgroup=~ID,hoverinfo="text+x+name") %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type=fecha_formato$FORMATO_TIPO_GRAFICA,tickformat = fecha_formato$FORMATO_GRAFICA,title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}


#' Gráfica la garantía depositada vs volumen negociado por título promedio diario (barras+puntos)
#'
#' Esta función crea la gráfica de la garantía depositada (barras) vs volumen negociado (puntos)
#' por título promedio diario
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_promedio_diario}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' #' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("Efectivo", "Nominal", "Efectivo Cover 2", "Nominal Cover 2"). Por defecto NULL
#' @param completa clase boolean. TRUE si se desea mostar la gráfica completa.
#' FALSE si desea mostrar solo los valores de las garantias depositadas por título. Por defecto TRUE
#' @export

gt_gar_dep_vol_negociado_promedio_diario_por_titulo<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,completa=TRUE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {
    # Se verifica si la grafica se debe mostrar completa o parcial
    if (completa==TRUE) {
      # Verificación inputs
      if (is.null(boton_activo)) boton_activo <- "Efectivo"

      # Se crea el data.frame tipos
      tipos <- data.frame(TIPO=c("EFECTIVO","NOMINAL","EFECTIVO COVER 2","NOMINAL COVER 2"),
                          BOTON=c("Efectivo","Nominal","Efectivo Cover 2","Nominal Cover 2"),
                          POSICION=c(1,2,3,4)) %>%
        mutate(VISIBLE=BOTON==boton_activo)

      # Se crea el data.frame datos_completos
      datos_completos <- datos %>%
        group_by(GRUPO="GENERAL",ACTIVO_DESCRIPCION) %>%
        summarise(across(c(VOLUMEN_GARANTIA:IMPORTE_GARANTIA_HAIRCUT), ~round(sum(.x)/1e+9,6)),
                  across(c(VOLUMEN_MEC_SEN:IMPORTE_SIMULTANEAS), ~round(max(.x)/1e+9,6)),.groups="drop") %>%
        bind_rows(datos %>%
                    group_by(GRUPO="COVER 2",ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO) %>%
                    summarise(across(c(VOLUMEN_GARANTIA:IMPORTE_GARANTIA_HAIRCUT), ~round(sum(.x)/1e+9,6)),
                              across(c(VOLUMEN_MEC_SEN:IMPORTE_SIMULTANEAS), ~round(max(.x)/1e+9,6)),.groups="drop_last") %>%
                    arrange(desc(IMPORTE_GARANTIA_HAIRCUT)) %>% slice_head(n = 2) %>% ungroup()) %>%
        pivot_longer(c(VOLUMEN_GARANTIA, VOLUMEN_MEC_SEN, VOLUMEN_SIMULTANEAS,
                       IMPORTE_GARANTIA_DESPUES_HAIRCUT,
                       IMPORTE_GARANTIA_HAIRCUT, IMPORTE_MEC_SEN, IMPORTE_SIMULTANEAS),names_to ="ID",values_to = "VALOR") %>%
        mutate(TIPO=case_when(str_detect(ID,"VOLUMEN")==TRUE ~"NOMINAL",TRUE~"EFECTIVO"),
               TIPO=case_when(GRUPO=="COVER 2" ~paste(TIPO,GRUPO),TRUE~ TIPO),
               ID=case_when(ID =="IMPORTE_GARANTIA_DESPUES_HAIRCUT"~ "Garantia Con HC",
                            ID =="IMPORTE_GARANTIA_HAIRCUT"~ "HC Garantia",
                            ID =="VOLUMEN_GARANTIA" ~ "Garantia",
                            ID %in% c("VOLUMEN_MEC_SEN","IMPORTE_MEC_SEN")~ "MEC+SEN",
                            ID %in% c("VOLUMEN_SIMULTANEAS","IMPORTE_SIMULTANEAS")==TRUE~ "Simultaneas"),.before="ID") %>%
        group_by(GRUPO,ACTIVO_DESCRIPCION,TIPO,ID) %>%
        summarise(TEXTO_COMPLEMENTO=paste(paste(MIEMBRO_ID_SEUDONIMO,VALOR,"Miles M"),collapse = "\n"),
                  VALOR=case_when(first(ID) %in% c("Simultaneas","MEC+SEN") ~max(VALOR),TRUE~sum(VALOR)),
                  TEXTO=case_when(!(first(GRUPO)=="COVER 2" & first(ID) %in% c("Garantia Con HC","HC Garantia","Garantia"))~paste(VALOR,"Miles M"),
                                  TRUE~paste(paste(VALOR,"Miles M"),TEXTO_COMPLEMENTO,sep="\n")),.groups = "drop") %>%
        left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
        mutate(ID=fct_reorder(factor(ID),VALOR,.fun=mean,.desc=T),
               ACTIVO_DESCRIPCION=fct_reorder(factor(ACTIVO_DESCRIPCION),if_else(ID %in% c("Garantia Con HC","HC Garantia"),VALOR,0),.fun=sum,.desc=T),
               COLOR_ID=paste(POSICION,dt_num_char(ID),sep="-")) %>% arrange(COLOR_ID)

      # Se crean los botones
      botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
        visible <- tipos$BOTON[i]==tipos$BOTON
        list(list(label = tipos$BOTON[i],method = "restyle",
                  args = list(list(boton_activo=tipos$BOTON[i],
                                   visible = as.logical(c(rep(visible[1],2),
                                                          rep(visible[2],1),
                                                          rep(visible[3],2),
                                                          rep(visible[4],1),
                                                          rep(visible[1],2),
                                                          rep(visible[2],2),
                                                          rep(visible[3],2),
                                                          rep(visible[4],2)))))))
      }

      # Se crea el vector colores
      colores <- datos_completos %>% distinct(TIPO="UNIDAD",ID,COLOR_ID) %>%
        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

      # Se grafica la garantia depositada vs volumen negociado por titulo promedio diario
      plot <- plot_ly(data= datos_completos %>% filter(str_detect(ID,"Garantia")==TRUE) ,x=~ACTIVO_DESCRIPCION,
                      color=~COLOR_ID,colors=colores,textposition = 'none') %>%
        add_bars(y=~VALOR,text=~TEXTO,visible=~VISIBLE,
                 name=~ID,hoverinfo="text+x+name") %>%
        add_data(data= datos_completos %>%  filter(str_detect(ID,"Garantia")!=TRUE)) %>%
        add_markers(y=~VALOR,text=~TEXTO,visible=~VISIBLE,
                    name=~ID,hoverinfo="text+x+name",stackgroup="1",fillcolor="transparent") %>%
        layout(barmode="stack",hovermode = 'x',
               legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=1.05,tracegroupgap=0),
               updatemenus=list(
                 list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                      yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
               xaxis = list(title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
        config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

      return(plot)

    }else{

      # Verificación inputs
      if (is.null(boton_activo)) boton_activo <- "Efectivo"

      # Se crea el data.frame tipos
      tipos <- data.frame(TIPO=c("EFECTIVO","NOMINAL"),
                          BOTON=c("Efectivo","Nominal"),
                          POSICION=c(1,2)) %>%
        mutate(VISIBLE=BOTON==boton_activo)

      # Se crea el data.frame datos_completos
      datos_completos <- datos %>%
        group_by(ACTIVO_DESCRIPCION) %>%
        summarise(across(c(VOLUMEN_GARANTIA,IMPORTE_GARANTIA_DESPUES_HAIRCUT,IMPORTE_GARANTIA_HAIRCUT), ~round(sum(.x)/1e+9,6)),.groups="drop") %>%
        pivot_longer(c(VOLUMEN_GARANTIA,IMPORTE_GARANTIA_DESPUES_HAIRCUT,IMPORTE_GARANTIA_HAIRCUT),names_to ="ID",values_to = "VALOR") %>%
        mutate(TIPO=case_when(str_detect(ID,"VOLUMEN")==TRUE ~"NOMINAL",TRUE~"EFECTIVO"),
               ID=case_when(ID =="IMPORTE_GARANTIA_DESPUES_HAIRCUT"~ "Garantia Con HC",
                            ID =="IMPORTE_GARANTIA_HAIRCUT"~ "HC Garantia",
                            ID =="VOLUMEN_GARANTIA" ~ "Garantia"),.before="ID") %>%
        left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
        mutate(TEXTO=paste(VALOR,"Miles M"),ID=fct_reorder(factor(ID),VALOR,.fun=mean,.desc=T),
               ACTIVO_DESCRIPCION=fct_reorder(factor(ACTIVO_DESCRIPCION),if_else(ID %in% c("Garantia Con HC","HC Garantia"),VALOR,0),.fun=sum,.desc=T),
               COLOR_ID=paste(POSICION,dt_num_char(ID),sep="-")) %>% arrange(COLOR_ID)

      # Se crean los botones
      botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
        visible <- tipos$BOTON[i]==tipos$BOTON
        list(list(label = tipos$BOTON[i],method = "restyle",
                  args = list(list(boton_activo=tipos$BOTON[i],
                                   visible = as.logical(c(rep(visible[1],2),
                                                          rep(visible[2],1)))))))
      }

      # Se crea el vector colores
      colores <- datos_completos %>% distinct(TIPO="UNIDAD",ID,COLOR_ID) %>%
        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

      # Se grafica la garantia depositada vs volumen negociado por titulo promedio diario
      plot <- plot_ly(data= datos_completos %>% filter(str_detect(ID,"Garantia")==TRUE) ,x=~ACTIVO_DESCRIPCION,
                      color=~COLOR_ID,colors=colores,textposition = 'none') %>%
        add_bars(y=~VALOR,text=~TEXTO,visible=~VISIBLE,
                 name=~ID,hoverinfo="text+x+name") %>%
        layout(barmode="stack",hovermode = 'x',
               legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=1.05,tracegroupgap=0),
               updatemenus=list(
                 list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                      yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
               xaxis = list(title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
        config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

      return(plot)
    }
  }else{
    return(gt_mensaje_error)
  }
}


#' Gráfica la garantía depositada por título, miembro y tipo cuenta promedio diario (treemap)
#'
#' Esta función crea la gráfica de la  garantía depositada promedio diario por título, miembro y tipo cuenta en
#' formato treemap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_promedio_diario}} o tener una estructura igual a dichos datos
#' @export

gt_gar_dep_promedio_diario_por_titulo_miembro_tipocuenta<- function(datos){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se filtra y modifica la granularidad de los datos
    datos <- datos %>% filter(IMPORTE_GARANTIA>0) %>%
      group_by(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_ID_SEUDONIMO,CUENTA_GARANTIA_TIPO) %>%
      summarise(VALOR=sum(IMPORTE_GARANTIA,na.rm = TRUE)/1e+9,.groups="drop")

    # Se crea el data.frame datos_completos
    datos_completos <- datos  %>% group_by(LABEL="Títulos",PARENT="") %>%
      summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M"),.groups="drop")  %>%
      bind_rows(datos %>% group_by(LABEL=ACTIVO_DESCRIPCION,PARENT="Títulos") %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M"),.groups="drop")) %>%
      bind_rows(datos %>% group_by(LABEL=paste(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO),PARENT=ACTIVO_DESCRIPCION) %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M"),.groups="drop")) %>%
      bind_rows(datos %>% group_by(LABEL=paste(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_TIPO),PARENT=paste(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO)) %>%
                  summarise(VALOR_COMPLEMETARIO=VALOR,VALOR=sum(VALOR,na.rm = TRUE),
                            TEXTO_COMPLEMENTARIO=paste0(CUENTA_GARANTIA_ID_SEUDONIMO,": ",round(VALOR_COMPLEMETARIO,6)," Miles M / ",dt_porcentaje_caracter(VALOR_COMPLEMETARIO/VALOR)), .groups="keep") %>%
                  arrange(desc(VALOR_COMPLEMETARIO)) %>% slice_head(n = 10) %>%
                  summarise(VALOR=max(VALOR),TEXTO=paste0(round(VALOR,6)," Miles M","\n","\n","Cuentas Garantías Top 10:","\n",paste0(TEXTO_COMPLEMENTARIO,collapse="\n")),.groups="drop"))

    # Se grafica la garantia depositada por titulo, miembro y tipo de cuenta promedio diario
    plot <- plot_ly(data = datos_completos,type="treemap",labels=~LABEL,parents=~PARENT,values=~VALOR,text=~TEXTO,
                    textinfo="text+label+percent parent+name",branchvalues="total",hoverinfo="label+percent parent",
                    maxdepth=2,domain=list(column=0)) %>% layout(margin=list(l=0, r=0, b=0, t=0)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


#' Gráfica la garantía depositada por miembro, título y tipo cuenta promedio diario (treemap)
#'
#' Esta función crea la gráfica de la  garantía depositada promedio diario por miembro, título y tipo cuenta en
#' formato treemap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_promedio_diario}} o tener una estructura igual a dichos datos
#' @export

gt_gar_dep_promedio_diario_por_miembro_titulo_tipocuenta<- function(datos){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se filtra y modifica la granularidad de los datos
    datos <- datos %>% filter(IMPORTE_GARANTIA>0) %>%
      group_by(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_ID_SEUDONIMO,CUENTA_GARANTIA_TIPO) %>%
      summarise(VALOR=sum(IMPORTE_GARANTIA,na.rm = TRUE)/1e+9,.groups="drop")

    # Se crea el data.frame datos_completos
    datos_completos <- datos  %>% group_by(LABEL="Miembros",PARENT="") %>%
      summarise(VALOR=round(sum(VALOR,na.rm = TRUE),6),TEXTO=paste(VALOR,"Miles M") ,.groups="drop")  %>%
      bind_rows(datos %>% group_by(LABEL=MIEMBRO_ID_SEUDONIMO,PARENT="Miembros") %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M"),.groups="drop"))  %>%
      bind_rows(datos %>% group_by(LABEL=paste(MIEMBRO_ID_SEUDONIMO,ACTIVO_DESCRIPCION),PARENT=MIEMBRO_ID_SEUDONIMO) %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M"),.groups="drop")) %>%
      bind_rows(datos %>% group_by(LABEL=paste(MIEMBRO_ID_SEUDONIMO,ACTIVO_DESCRIPCION,CUENTA_GARANTIA_TIPO),PARENT=paste(MIEMBRO_ID_SEUDONIMO,ACTIVO_DESCRIPCION)) %>%
                  summarise(VALOR_COMPLEMETARIO=VALOR,VALOR=sum(VALOR,na.rm = TRUE),
                            TEXTO_COMPLEMENTARIO=paste0(CUENTA_GARANTIA_ID_SEUDONIMO,": ",round(VALOR_COMPLEMETARIO,6)," Miles M / ",dt_porcentaje_caracter(VALOR_COMPLEMETARIO/VALOR)), .groups="keep") %>%
                  arrange(desc(VALOR_COMPLEMETARIO)) %>% slice_head(n = 10) %>%
                  summarise(VALOR=max(VALOR),TEXTO=paste0(round(VALOR,6)," Miles M","\n","\n","Cuentas Garantías Top 10:","\n",paste0(TEXTO_COMPLEMENTARIO,collapse="\n")),.groups="drop"))

    # Se grafica la garantia depositada por miembro, titulo y tipo de cuenta promedio diario
    plot <- plot_ly(data = datos_completos,type="treemap",labels=~LABEL,parents=~PARENT,values=~VALOR,text=~TEXTO,
                    textinfo="text+label+percent parent+name",branchvalues="total",hoverinfo="label+percent parent",
                    maxdepth=2,domain=list(column=0)) %>% layout(margin=list(l=0, r=0, b=0, t=0)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


#' Gráfica el ratio periodo de liquidación por título (heatmap)
#'
#' Esta función crea la gráfica del ratio periodo de liquidación por título en formato heatmap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_titulos}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("General", "MEC+SEN", "Simultaneas", "General Cover 2",
#' "MEC+SEN Cover 2", "Simultaneas Cover 2"). Por defecto NULL
#' @export

gt_gar_dep_ratio_liquidacion_por_titulo<- function(datos,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Verificación inputs
    if (is.null(boton_activo)) boton_activo <- "General"

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","MEC+SEN","SIMULTANEAS","GENERAL COVER 2","MEC+SEN COVER 2","SIMULTANEAS COVER 2"),
                        BOTON=c("General","MEC+SEN","Simultaneas","General Cover 2","MEC+SEN Cover 2","Simultaneas Cover 2")) %>%
      mutate(VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,GRUPO="GENERAL",ACTIVO_DESCRIPCION) %>%
      summarise(across(c(IMPORTE_GARANTIA), ~round(sum(.x)/1e+9,6)),
                across(c(IMPORTE_MEC_SEN:IMPORTE_SIMULTANEAS), ~round(max(.x)/1e+9,6)),.groups="drop") %>%
      bind_rows(datos %>%
                  group_by(FECHA,GRUPO="COVER 2",ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO) %>%
                  summarise(across(c(IMPORTE_GARANTIA), ~round(sum(.x)/1e+9,6)),
                            across(c(IMPORTE_MEC_SEN:IMPORTE_SIMULTANEAS), ~round(max(.x)/1e+9,6)),.groups="drop_last") %>%
                  arrange(desc(IMPORTE_GARANTIA)) %>% slice_head(n = 2) %>% ungroup() %>%
                  group_by(FECHA,GRUPO,ACTIVO_DESCRIPCION) %>%
                  summarise(TEXTO_COMPLEMENTO=paste(paste(MIEMBRO_ID_SEUDONIMO,IMPORTE_GARANTIA,"Miles M"),collapse = "\n"),
                            across(c(IMPORTE_GARANTIA), ~sum(.x)),
                            across(c(IMPORTE_MEC_SEN:IMPORTE_SIMULTANEAS), ~max(.x)),.groups="drop")) %>%
      group_by(FECHA,ACTIVO_DESCRIPCION) %>%
      summarise(GARANTIAS=sum(IMPORTE_GARANTIA[GRUPO=="GENERAL"]),
                GARANTIAS_COVER_2=sum(IMPORTE_GARANTIA[GRUPO=="COVER 2"]),
                MEC_SEN=max(IMPORTE_MEC_SEN),
                SIMULTANEAS=max(IMPORTE_SIMULTANEAS),
                VALOR_1=ifelse(GARANTIAS==0,0,ifelse(MEC_SEN+SIMULTANEAS>0,round(GARANTIAS/(MEC_SEN+SIMULTANEAS),2),NaN)),
                TEXTO_1=paste(paste("Periodo Liquidación:",VALOR_1,"días"),paste("Garantía:",GARANTIAS,"Miles M"),paste("MEC+SEN:",MEC_SEN,"Miles M"),paste("Simultaneas:",SIMULTANEAS,"Miles M"),sep="\n"),
                VALOR_2=ifelse(GARANTIAS==0,0,ifelse(MEC_SEN>0,round(GARANTIAS/(MEC_SEN),2),NaN)),
                TEXTO_2=paste(paste("Periodo Liquidación:",VALOR_2,"días"),paste("Garantía:",GARANTIAS,"Miles M"),paste("MEC+SEN:",MEC_SEN,"Miles M"),sep="\n"),
                VALOR_3=ifelse(GARANTIAS==0,0,ifelse(SIMULTANEAS>0,round(GARANTIAS/(SIMULTANEAS),2),NaN)),
                TEXTO_3=paste(paste("Periodo Liquidación:",VALOR_3,"días"),paste("Garantía:",GARANTIAS,"Miles M"),paste("Simultaneas:",SIMULTANEAS,"Miles M"),sep="\n"),
                VALOR_4=ifelse(GARANTIAS_COVER_2==0,0,ifelse(MEC_SEN+SIMULTANEAS>0,round(GARANTIAS_COVER_2/(MEC_SEN+SIMULTANEAS),2),NaN)),
                TEXTO_4=paste(paste("Periodo Liquidación:",VALOR_4,"días"),paste("Garantía:",GARANTIAS_COVER_2,"Miles M"),paste("MEC+SEN:",MEC_SEN,"Miles M"),paste("Simultaneas:",SIMULTANEAS,"Miles M"),paste(TEXTO_COMPLEMENTO[GRUPO=="COVER 2"]),sep="\n"),
                VALOR_5=ifelse(GARANTIAS_COVER_2==0,0,ifelse(MEC_SEN>0,round(GARANTIAS_COVER_2/(MEC_SEN),2),NaN)),
                TEXTO_5=paste(paste("Periodo Liquidación:",VALOR_5,"días"),paste("Garantía:",GARANTIAS_COVER_2,"Miles M"),paste("MEC+SEN:",MEC_SEN,"Miles M"),paste(TEXTO_COMPLEMENTO[GRUPO=="COVER 2"]),sep="\n"),
                VALOR_6=ifelse(GARANTIAS_COVER_2==0,0,ifelse(SIMULTANEAS>0,round(GARANTIAS_COVER_2/(SIMULTANEAS),2),NaN)),
                TEXTO_6=paste(paste("Periodo Liquidación:",VALOR_6,"días"),paste("Garantía:",GARANTIAS_COVER_2,"Miles M"),paste("Simultaneas:",SIMULTANEAS,"Miles M"),paste(TEXTO_COMPLEMENTO[GRUPO=="COVER 2"]),sep="\n"),.groups="drop")

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      botones <- list(list(label = tipos$BOTON[i],method = "restyle",
                           args = list(list(boton_activo=tipos$BOTON[i],
                                            visible = as.logical(visible)))))
    }

    # Se grafica el ratio periodo de liquidación por título
    plot <- plot_ly(data=datos_completos, y=~ACTIVO_DESCRIPCION, x=~FECHA) %>%
      add_heatmap(z=~VALOR_1,text=~TEXTO_1,hoverinfo="text+x+y",visible=tipos$VISIBLE[1],showscale=tipos$VISIBLE[1],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_2,text=~TEXTO_2,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_3,text=~TEXTO_3,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_4,text=~TEXTO_4,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_5,text=~TEXTO_5,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_6,text=~TEXTO_6,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      layout(coloraxis=list(colorscale='YlGnBu',reversescale=F,cmin=0,cmax =2,colorbar=list(title =list(text="Días"))),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = "",type='date',tickformat = "%d-%b",showgrid = F, zeroline = F,fixedrange=fixedrange),
             yaxis = list(title = "",showgrid = F, zeroline = F,fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla)) %>% event_register("plotly_restyle")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}



#' Gráfica la garantía depositada vs volumen negociado por acción promedio diario (barras+puntos)
#'
#' Esta función crea la gráfica de la garantía depositada (barras) vs volumen negociado (puntos)
#' por acción promedio diario
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_promedio_diario}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' #' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("Efectivo", "Acciones", "Efectivo Cover 2", "Acciones Cover 2"). Por defecto NULL
#' @param completa clase boolean. TRUE si se desea mostar la gráfica completa.
#' FALSE si desea mostrar solo los valores de las garantias depositadas por título. Por defecto TRUE
#' @export

gt_gar_dep_vol_negociado_promedio_diario_por_accion<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,completa=TRUE){

  ## Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se verifica si la grafica se debe mostrar completa o parcial
    if (completa==TRUE) {

      # Verificación inputs
      if (is.null(boton_activo)) boton_activo <- "Efectivo"

      # Se crea el data.frame tipos
      tipos <- data.frame(TIPO=c("EFECTIVO","ACCIONES","EFECTIVO COVER 2","ACCIONES COVER 2"),
                          BOTON=c("Efectivo","Acciones","Efectivo Cover 2","Acciones Cover 2"),
                          POSICION=c(1,2,3,4)) %>%
        mutate(VISIBLE=BOTON==boton_activo)

      # Se crea el data.frame datos_completos
      datos_completos <- datos %>%
        group_by(GRUPO="GENERAL",ACTIVO_DESCRIPCION) %>%
        summarise(across(c(VOLUMEN_GARANTIA:IMPORTE_GARANTIA_HAIRCUT), ~round(sum(.x)/1e+9,6)),
                  across(c(VOLUMEN_CONTADO:IMPORTE_ADR), ~round(max(.x)/1e+9,6)),.groups="drop") %>%
        bind_rows(datos %>%
                    group_by(GRUPO="COVER 2",ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO) %>%
                    summarise(across(c(VOLUMEN_GARANTIA:IMPORTE_GARANTIA_HAIRCUT), ~round(sum(.x)/1e+9,6)),
                              across(c(VOLUMEN_CONTADO:IMPORTE_ADR), ~round(max(.x)/1e+9,6)),.groups="drop_last") %>%
                    arrange(desc(IMPORTE_GARANTIA_HAIRCUT)) %>% slice_head(n = 2) %>% ungroup()) %>%
        pivot_longer(c(VOLUMEN_GARANTIA, VOLUMEN_CONTADO, VOLUMEN_ADR,
                       IMPORTE_GARANTIA_DESPUES_HAIRCUT,
                       IMPORTE_GARANTIA_HAIRCUT, IMPORTE_CONTADO, IMPORTE_ADR),names_to ="ID",values_to = "VALOR") %>%
        mutate(TIPO=case_when(str_detect(ID,"VOLUMEN")==TRUE ~"ACCIONES", TRUE~"EFECTIVO"),
               TIPO=case_when(GRUPO=="COVER 2" ~paste(TIPO,GRUPO),TRUE~ TIPO),
               ID=case_when(ID =="IMPORTE_GARANTIA_DESPUES_HAIRCUT"~ "Garantia Con HC",
                            ID =="IMPORTE_GARANTIA_HAIRCUT"~ "HC Garantia",
                            ID =="VOLUMEN_GARANTIA" ~ "Garantia",
                            ID %in% c("VOLUMEN_CONTADO","IMPORTE_CONTADO")~ "Contado",
                            ID %in% c("VOLUMEN_ADR","IMPORTE_ADR")==TRUE~ "ADR"),.before="ID") %>%
        group_by(GRUPO,ACTIVO_DESCRIPCION,TIPO,ID) %>%
        summarise(TEXTO_COMPLEMENTO=paste(paste(MIEMBRO_ID_SEUDONIMO,VALOR,"Miles M"),collapse = "\n"),
                  VALOR=case_when(first(ID) %in% c("Contado","ADR") ~max(VALOR),TRUE~sum(VALOR)),
                  TEXTO=case_when(!(first(GRUPO)=="COVER 2" & first(ID) %in% c("Garantia Con HC","HC Garantia","Garantia"))~paste(VALOR,"Miles M"),
                                  TRUE~paste(paste(VALOR,"Miles M"),TEXTO_COMPLEMENTO,sep="\n")),.groups = "drop") %>%
        left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
        mutate(ID=fct_reorder(factor(ID),VALOR,.fun=mean,.desc=T),
               ACTIVO_DESCRIPCION=fct_reorder(factor(ACTIVO_DESCRIPCION),if_else(ID %in% c("Garantia Con HC","HC Garantia"),VALOR,0),.fun=sum,.desc=T),
               COLOR_ID=paste(POSICION,dt_num_char(ID),sep="-")) %>% arrange(COLOR_ID)

      # Se crean los botones
      botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
        visible <- tipos$BOTON[i]==tipos$BOTON
        list(list(label = tipos$BOTON[i],method = "update",
                  args = list(list(boton_activo=tipos$BOTON[i],
                                   visible = as.logical(c(rep(visible[1],2),
                                                          rep(visible[2],1),
                                                          rep(visible[3],2),
                                                          rep(visible[4],1),
                                                          rep(visible[1],2),
                                                          rep(visible[2],2),
                                                          rep(visible[3],2),
                                                          rep(visible[4],2)))),
                              list(yaxis=list(title=ifelse(i==1,"Miles de Millones-COP","Miles de Millones de Acciones"),fixedrange=fixedrange)))))
      }

      # Se crea el vector colores
      colores <- datos_completos %>% distinct(TIPO="UNIDAD",ID,COLOR_ID) %>%
        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

      # Se grafica la garantia depositada vs volumen negociado por acción promedio diario
      plot <- plot_ly(data= datos_completos %>% filter(str_detect(ID,"Garantia")==TRUE) ,x=~ACTIVO_DESCRIPCION,
                      color=~COLOR_ID,colors=colores,textposition = 'none') %>%
        add_bars(y=~VALOR,text=~TEXTO,visible=~VISIBLE,
                 name=~ID,hoverinfo="text+x+name") %>%
        add_data(data= datos_completos %>%  filter(str_detect(ID,"Garantia")!=TRUE)) %>%
        add_markers(y=~VALOR,text=~TEXTO,visible=~VISIBLE,
                    name=~ID,hoverinfo="text+x+name",stackgroup="1",fillcolor="transparent") %>%
        layout(barmode="stack",hovermode = 'compare',
               legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=1.05,tracegroupgap=0),
               updatemenus=list(
                 list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                      yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
               xaxis = list(title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
        config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

      return(plot)

    }else{

      # Verificación inputs
      if (is.null(boton_activo)) boton_activo <- "Efectivo"

      # Se crea el data.frame tipos
      tipos <- data.frame(TIPO=c("EFECTIVO","ACCIONES"),
                          BOTON=c("Efectivo","Acciones"),
                          POSICION=c(1,2)) %>%
        mutate(VISIBLE=BOTON==boton_activo)

      # Se crea el data.frame datos_completos
      datos_completos <- datos %>%
        group_by(GRUPO="GENERAL",ACTIVO_DESCRIPCION) %>%
        summarise(across(c(VOLUMEN_GARANTIA:IMPORTE_GARANTIA_HAIRCUT), ~round(sum(.x)/1e+9,6)),.groups="drop") %>%
        pivot_longer(c(VOLUMEN_GARANTIA,IMPORTE_GARANTIA_DESPUES_HAIRCUT,IMPORTE_GARANTIA_HAIRCUT),names_to ="ID",values_to = "VALOR") %>%
        mutate(TIPO=case_when(str_detect(ID,"VOLUMEN")==TRUE ~"ACCIONES",TRUE~"EFECTIVO"),
               ID=case_when(ID =="IMPORTE_GARANTIA_DESPUES_HAIRCUT"~ "Garantia Con HC",
                            ID =="IMPORTE_GARANTIA_HAIRCUT"~ "HC Garantia",
                            ID =="VOLUMEN_GARANTIA" ~ "Garantia"),.before="ID") %>%
        left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
        mutate(TEXTO=paste(VALOR,"Miles M"),
               ID=fct_reorder(factor(ID),VALOR,.fun=mean,.desc=T),
               ACTIVO_DESCRIPCION=fct_reorder(factor(ACTIVO_DESCRIPCION),if_else(ID %in% c("Garantia Con HC","HC Garantia"),VALOR,0),.fun=sum,.desc=T),
               COLOR_ID=paste(POSICION,dt_num_char(ID),sep="-")) %>% arrange(COLOR_ID)


      # Se crean los botones
      botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
        visible <- tipos$BOTON[i]==tipos$BOTON
        list(list(label = tipos$BOTON[i],method = "update",
                  args = list(list(boton_activo=tipos$BOTON[i],
                                   visible = as.logical(c(rep(visible[1],2),
                                                          rep(visible[2],1)))),
                              list(yaxis=list(title=ifelse(i==1,"Miles de Millones-COP","Miles de Millones de Acciones"),fixedrange=fixedrange)))))
      }

      # Se crea el vector colores
      colores <- datos_completos %>% distinct(TIPO="UNIDAD",ID,COLOR_ID) %>%
        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

      # Se grafica la garantia depositada vs volumen negociado por acción promedio diario
      plot <- plot_ly(data= datos_completos %>% filter(str_detect(ID,"Garantia")==TRUE) ,x=~ACTIVO_DESCRIPCION,
                      color=~COLOR_ID,colors=colores,textposition = 'none') %>%
        add_bars(y=~VALOR,text=~TEXTO,visible=~VISIBLE,
                 name=~ID,hoverinfo="text+x+name") %>%
        layout(barmode="stack",hovermode = 'compare',
               legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=1.05,tracegroupgap=0),
               updatemenus=list(
                 list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                      yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
               xaxis = list(title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
        config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

      return(plot)
    }
  }else{
    return(gt_mensaje_error)
  }
}


#' Gráfica la garantía depositada por acción, miembro y tipo cuenta promedio diario (treemap)
#'
#' Esta función crea la gráfica de la  garantía depositada promedio diario por acción, miembro y tipo cuenta en
#' formato treemap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_promedio_diario}} o tener una estructura igual a dichos datos
#' @export

gt_gar_dep_promedio_diario_por_accion_miembro_tipocuenta<- function(datos){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se filtra y modifica la granularidad de los datos
    datos <- datos %>% filter(IMPORTE_GARANTIA>0) %>%
      group_by(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_ID_SEUDONIMO,CUENTA_GARANTIA_TIPO) %>%
      summarise(VALOR=sum(IMPORTE_GARANTIA,na.rm = TRUE)/1e+9,.groups="drop")

    # Se crea el data.frame datos_completos
    datos_completos <- datos  %>% group_by(LABEL="Acciones",PARENT="") %>%
      summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M") ,.groups="drop")  %>%
      bind_rows(datos %>% group_by(LABEL=ACTIVO_DESCRIPCION,PARENT="Acciones") %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M"),.groups="drop")) %>%
      bind_rows(datos %>% group_by(LABEL=paste(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO),PARENT=ACTIVO_DESCRIPCION) %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M"),.groups="drop")) %>%
      bind_rows(datos %>% group_by(LABEL=paste(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_TIPO),PARENT=paste(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO)) %>%
                  summarise(VALOR_COMPLEMETARIO=VALOR,VALOR=sum(VALOR,na.rm = TRUE),
                            TEXTO_COMPLEMENTARIO=paste0(CUENTA_GARANTIA_ID_SEUDONIMO,": ",round(VALOR_COMPLEMETARIO,6)," Miles M / ",dt_porcentaje_caracter(VALOR_COMPLEMETARIO/VALOR)), .groups="keep") %>%
                  arrange(desc(VALOR_COMPLEMETARIO)) %>% slice_head(n = 10) %>%
                  summarise(VALOR=max(VALOR),TEXTO=paste0(round(VALOR,6)," Miles M","\n","\n","Cuentas Garantías Top 10:","\n",paste0(TEXTO_COMPLEMENTARIO,collapse="\n")),.groups="drop"))


    # Se grafica la garantia depositada por acción, miembro y tipo de cuenta promedio diario
    plot <- plot_ly(data = datos_completos,type="treemap",labels=~LABEL,parents=~PARENT,values=~VALOR,text=~TEXTO,
                    textinfo="text+label+percent parent+name",branchvalues="total",hoverinfo="label+percent parent",
                    maxdepth=2,domain=list(column=0)) %>% layout(margin=list(l=0, r=0, b=0, t=0)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la garantía depositada por miembro, acción y tipo cuenta promedio diario (treemap)
#'
#' Esta función crea la gráfica de la  garantía depositada promedio diario por miembro, acción y tipo cuenta en
#' formato treemap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_promedio_diario}} o tener una estructura igual a dichos datos
#' @export

gt_gar_dep_promedio_diario_por_miembro_accion_tipocuenta<- function(datos){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se filtra y modifica la granularidad de los datos
    datos <- datos %>% filter(IMPORTE_GARANTIA>0) %>%
      group_by(ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_ID_SEUDONIMO,CUENTA_GARANTIA_TIPO) %>%
      summarise(VALOR=sum(IMPORTE_GARANTIA,na.rm = TRUE)/1e+9,.groups="drop")

    # Se crea el data.frame datos_completos
    datos_completos <- datos  %>% group_by(LABEL="Miembros",PARENT="") %>%
      summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M") ,.groups="drop")  %>%
      bind_rows(datos %>% group_by(LABEL=MIEMBRO_ID_SEUDONIMO,PARENT="Miembros") %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M"),.groups="drop"))  %>%
      bind_rows(datos %>% group_by(LABEL=paste(MIEMBRO_ID_SEUDONIMO,ACTIVO_DESCRIPCION),PARENT=MIEMBRO_ID_SEUDONIMO) %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Miles M"),.groups="drop")) %>%
      bind_rows(datos %>% group_by(LABEL=paste(MIEMBRO_ID_SEUDONIMO,ACTIVO_DESCRIPCION,CUENTA_GARANTIA_TIPO),PARENT=paste(MIEMBRO_ID_SEUDONIMO,ACTIVO_DESCRIPCION)) %>%
                  summarise(VALOR_COMPLEMETARIO=VALOR,VALOR=sum(VALOR,na.rm = TRUE),
                            TEXTO_COMPLEMENTARIO=paste0(CUENTA_GARANTIA_ID_SEUDONIMO,": ",round(VALOR_COMPLEMETARIO,6)," Miles M / ",dt_porcentaje_caracter(VALOR_COMPLEMETARIO/VALOR)), .groups="keep") %>%
                  arrange(desc(VALOR_COMPLEMETARIO)) %>% slice_head(n = 10) %>%
                  summarise(VALOR=max(VALOR),TEXTO=paste0(round(VALOR,6)," Miles M","\n","\n","Cuentas Garantías Top 10:","\n",paste0(TEXTO_COMPLEMENTARIO,collapse="\n")),.groups="drop"))

    # Se grafica la garantia depositada por miembro, acción y tipo de cuenta promedio diario
    plot <- plot_ly(data = datos_completos,type="treemap",labels=~LABEL,parents=~PARENT,values=~VALOR,text=~TEXTO,
                    textinfo="text+label+percent parent+name",branchvalues="total",hoverinfo="label+percent parent",
                    maxdepth=2,domain=list(column=0)) %>% layout(margin=list(l=0, r=0, b=0, t=0)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica el ratio periodo de liquidación por título (heatmap)
#'
#' Esta función crea la gráfica del ratio periodo de liquidación por título en formato heatmap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_acciones}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("General", "Contado", "ADR", "General Cover 2", "Contado Cover 2", "ADR Cover 2").
#' Por defecto NULL
#' @export

gt_gar_dep_ratio_liquidacion_por_accion<- function(datos,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Verificación inputs
    if (is.null(boton_activo)) {boton_activo <- "General"}

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","CONTADO","ADR","GENERAL COVER 2","CONTADO COVER 2","ADR COVER 2"),
                        BOTON=c("General","Contado","ADR","General Cover 2","Contado Cover 2","ADR Cover 2")) %>%
      mutate(VISIBLE=BOTON==boton_activo)


    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,GRUPO="GENERAL",ACTIVO_DESCRIPCION) %>%
      summarise(across(c(IMPORTE_GARANTIA), ~round(sum(.x)/1e+9,6)),
                across(c(IMPORTE_CONTADO:IMPORTE_ADR), ~round(max(.x)/1e+9,6)),.groups="drop") %>%
      bind_rows(datos %>%
                  group_by(FECHA,GRUPO="COVER 2",ACTIVO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO) %>%
                  summarise(across(c(IMPORTE_GARANTIA), ~round(sum(.x)/1e+9,6)),
                            across(c(IMPORTE_CONTADO:IMPORTE_ADR), ~round(max(.x)/1e+9,6)),.groups="drop_last") %>%
                  arrange(desc(IMPORTE_GARANTIA)) %>% slice_head(n = 2) %>% ungroup() %>%
                  group_by(FECHA,GRUPO,ACTIVO_DESCRIPCION) %>%
                  summarise(TEXTO_COMPLEMENTO=paste(paste(MIEMBRO_ID_SEUDONIMO,IMPORTE_GARANTIA,"Miles M"),collapse = "\n"),
                            across(c(IMPORTE_GARANTIA), ~sum(.x)),
                            across(c(IMPORTE_CONTADO:IMPORTE_ADR), ~max(.x)),.groups="drop")) %>%
      group_by(FECHA,ACTIVO_DESCRIPCION) %>%
      summarise(GARANTIAS=sum(IMPORTE_GARANTIA[GRUPO=="GENERAL"]),
                GARANTIAS_COVER_2=sum(IMPORTE_GARANTIA[GRUPO=="COVER 2"]),
                CONTADO=max(IMPORTE_CONTADO),
                ADR=max(IMPORTE_ADR),
                VALOR_1=ifelse(GARANTIAS==0,0,ifelse(CONTADO+ADR>0,round(GARANTIAS/(CONTADO+ADR),2),NaN)),
                TEXTO_1=paste(paste("Periodo Liquidación:",VALOR_1,"días"),paste("Garantía:",GARANTIAS,"Miles M"),paste("Contado:",CONTADO,"Miles M"),paste("ADR:",ADR,"Miles M"),sep="\n"),
                VALOR_2=ifelse(GARANTIAS==0,0,ifelse(CONTADO>0,round(GARANTIAS/(CONTADO),2),NaN)),
                TEXTO_2=paste(paste("Periodo Liquidación:",VALOR_2,"días"),paste("Garantía:",GARANTIAS,"Miles M"),paste("Contado:",CONTADO,"Miles M"),sep="\n"),
                VALOR_3=ifelse(GARANTIAS==0,0,ifelse(ADR>0,round(GARANTIAS/(ADR),2),NaN)),
                TEXTO_3=paste(paste("Periodo Liquidación:",VALOR_3,"días"),paste("Garantía:",GARANTIAS,"Miles M"),paste("ADR:",ADR,"Miles M"),sep="\n"),
                VALOR_4=ifelse(GARANTIAS_COVER_2==0,0,ifelse(CONTADO+ADR>0,round(GARANTIAS_COVER_2/(CONTADO+ADR),2),NaN)),
                TEXTO_4=paste(paste("Periodo Liquidación:",VALOR_4,"días"),paste("Garantía:",GARANTIAS_COVER_2,"Miles M"),paste("Contado:",CONTADO,"Miles M"),paste("ADR:",ADR,"Miles M"),paste(TEXTO_COMPLEMENTO[GRUPO=="Cover 2"]),sep="\n"),
                VALOR_5=ifelse(GARANTIAS_COVER_2==0,0,ifelse(CONTADO>0,round(GARANTIAS_COVER_2/(CONTADO),2),NaN)),
                TEXTO_5=paste(paste("Periodo Liquidación:",VALOR_5,"días"),paste("Garantía:",GARANTIAS_COVER_2,"Miles M"),paste("Contado:",CONTADO,"Miles M"),paste(TEXTO_COMPLEMENTO[GRUPO=="Cover 2"]),sep="\n"),
                VALOR_6=ifelse(GARANTIAS_COVER_2==0,0,ifelse(ADR>0,round(GARANTIAS_COVER_2/(ADR),2),NaN)),
                TEXTO_6=paste(paste("Periodo Liquidación:",VALOR_6,"días"),paste("Garantía:",GARANTIAS_COVER_2,"Miles M"),paste("ADR:",ADR,"Miles M"),paste(TEXTO_COMPLEMENTO[GRUPO=="Cover 2"]),sep="\n"),.groups="drop")

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      botones <- list(list(label = tipos$BOTON[i],method = "restyle",
                           args = list(list(boton_activo=tipos$BOTON[i],
                                            visible = as.logical(visible)))))
    }

    # Se grafica el ratio periodo de liquidación por acción
    plot <- plot_ly(data=datos_completos, y=~ACTIVO_DESCRIPCION, x=~FECHA) %>%
      add_heatmap(z=~VALOR_1,text=~TEXTO_1,hoverinfo="text+x+y",visible=tipos$VISIBLE[1],showscale=tipos$VISIBLE[1],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_2,text=~TEXTO_2,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_3,text=~TEXTO_3,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_4,text=~TEXTO_4,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_5,text=~TEXTO_5,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      add_heatmap(z=~VALOR_6,text=~TEXTO_6,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],showscale=tipos$VISIBLE[2],coloraxis = 'coloraxis',colorbar = list(title =NA)) %>%
      layout(coloraxis=list(colorscale='YlGnBu',reversescale=F,cmin=0,cmax =10,colorbar=list(title =list(text="Días"))),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = "",type='date',tickformat = "%d-%b",showgrid = F, zeroline = F,fixedrange=fixedrange),
             yaxis = list(title = "",showgrid = F, zeroline = F,fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


#' Tabla garantía depositada remuneración resumen
#'
#' Esta función crea la tabla garantía depositada remuneración en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_remuneracion_resumen}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_gar_dep_remuneracion_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <- datos %>% bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado",CUENTA_GARANTIA_TIPO="Consolidado")) %>%
    group_by(FECHA,SEGMENTO_NOMBRE,CUENTA_GARANTIA_TIPO) %>%
    summarise(GARANTIA_REMUNERADA=sum(GARANTIA[ESTADO=="Garantia Remunerada"],na.rm=TRUE),
              GARANTIA_NO_REMUNERADA=sum(GARANTIA[ESTADO=="Garantia No Remunerada"],na.rm=TRUE),.groups="drop") %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
    group_by(SEGMENTO_NOMBRE,CUENTA_GARANTIA_TIPO) %>%
    summarise(GARANTIA_REMUNERADA_DIARIA=sum(GARANTIA_REMUNERADA[FECHA==fecha_analisis]),
              GARANTIA_REMUNERADA_DIARIA_PROMEDIO_MENSUAL=mean(GARANTIA_REMUNERADA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              GARANTIA_REMUNERADA_DIARIA_PROMEDIO_PERIODO=mean(GARANTIA_REMUNERADA),
              GARANTIA_NO_REMUNERADA_DIARIA=sum(GARANTIA_NO_REMUNERADA[FECHA==fecha_analisis]),
              GARANTIA_NO_REMUNERADA_DIARIA_PROMEDIO_MENSUAL=mean(GARANTIA_NO_REMUNERADA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              GARANTIA_NO_REMUNERADA_DIARIA_PROMEDIO_PERIODO=mean(GARANTIA_NO_REMUNERADA) ,.groups="drop") %>%
    arrange(desc(GARANTIA_REMUNERADA_DIARIA+GARANTIA_NO_REMUNERADA_DIARIA)) %>%
    transmute(Segmento=SEGMENTO_NOMBRE,"Cuenta"=CUENTA_GARANTIA_TIPO,
              "% Garantía Remunerada"=GARANTIA_REMUNERADA_DIARIA/(GARANTIA_REMUNERADA_DIARIA+GARANTIA_NO_REMUNERADA_DIARIA),
              "Garantía Remunerada Último Día M-COP"=GARANTIA_REMUNERADA_DIARIA/1e+6,
              "Garantía No Remunerada Último Día M-COP"=GARANTIA_NO_REMUNERADA_DIARIA/1e+6,
              "Garantía Remunerada Promedio Diario Último Mes M-COP"=GARANTIA_REMUNERADA_DIARIA_PROMEDIO_MENSUAL/1e+6,
              "Garantía No Remunerada Promedio Diario Último Mes M-COP"=GARANTIA_NO_REMUNERADA_DIARIA_PROMEDIO_MENSUAL/1e+6,
              "Garantía Remunerada Promedio Diario Periodo M-COP"=GARANTIA_REMUNERADA_DIARIA_PROMEDIO_PERIODO/1e+6,
              "Garantía No Remunerada Promedio Diario Periodo M-COP"=GARANTIA_NO_REMUNERADA_DIARIA_PROMEDIO_PERIODO/1e+6)

  # Se crea la tabla
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatPercentage(3,digits = 2) %>% formatCurrency(c(4,5,6,7,8,9), '$',digits = 0)

  return(table)
}



#' Gráfica la garantía depositada vs. utilidad remuneración vs. tarifa remuneración por miembro de una fecha de análisis (barras)
#'
#' Esta función crea la gráfica de la garantía depositada vs. utilidad remuneración vs. tarifa remuneración por miembro para una
#' fecha de análisis en formato de barras
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_remuneracion_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_gar_dep_remuneracion_por_miembro_fecha <- function(datos,colores,fixedrange=FALSE){

  # Se filtran los datos
  datos <-  datos %>% filter(GARANTIA>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO), GARANTIA,.fun=sum,.desc=T),
             VALOR_1=GARANTIA,VALOR_2=UTILIDAD_REMUNERACION,VALOR_3=TARIFA_REMUNERACION)  %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO="ESTADO",ID=ESTADO) %>%
      summarise(across(VALOR_1, ~round(sum(.x)/1e+9,6)),across(c(VALOR_2:VALOR_3), ~round(sum(.x)/1e+6,6)),.groups="drop_last")%>%
      mutate(TEXTO_1=paste(VALOR_1,"Miles Millones","/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1))),
             TEXTO_2=paste(VALOR_2,"Millones","/",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2))),
             TEXTO_3=paste(VALOR_3,"Millones","/",dt_porcentaje_caracter(VALOR_3/sum(VALOR_3)))) %>% ungroup() %>%
      mutate(COLOR_ID=dt_num_char(fct_reorder(factor(paste(TIPO,ID,sep="-")),VALOR_1,.fun=mean,.desc=T))) %>%
      arrange(COLOR_ID)

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la garantia depositada por miembro (Fecha Especifica)
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,name=~ID,
               legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,name=~ID,
               legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_bars(y=~VALOR_3,text=~TEXTO_3,name=~ID,
               legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      subplot(nrows = 3,shareX = TRUE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Garantía Miles M-COP",fixedrange=fixedrange),
             yaxis2 = list(title = "Utilidad M-COP",fixedrange=fixedrange),
             yaxis3 = list(title = "Tarifa M-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}


#' Gráfica la garantía depositada vs. utilidad remuneración vs. tarifa remuneración diaria (lineas)
#'
#' Esta función crea la gráfica de la garantía depositada vs. utilidad remuneración vs. tarifa remuneración
#'  diaria en formato de lineas
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_remuneracion_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' #' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_gar_dep_remuneracion_diaria<- function(datos,colores,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      mutate(VALOR_1=GARANTIA,VALOR_2=UTILIDAD_REMUNERACION,VALOR_3=TARIFA_REMUNERACION) %>%
      group_by(TIPO="ESTADO",ID=ESTADO,FECHA) %>%
      summarise(across(VALOR_1, ~round(sum(.x)/1e+9,6)),across(c(VALOR_2:VALOR_3), ~round(sum(.x)/1e+6,6)),.groups="drop_last")%>%
      mutate(across(c(VALOR_1:VALOR_3),~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA,TIPO)  %>% group_by(FECHA,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Miles Millones","/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)),"P /",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Millones","/",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2,"C"),
             TEXTO_3=paste(VALOR_3,"Millones","/",dt_porcentaje_caracter(VALOR_3/sum(VALOR_3)),"P /",CAMBIO_VALOR_3,"C")) %>% ungroup() %>%
      mutate(COLOR_ID=dt_num_char(fct_reorder(factor(paste(TIPO,ID,sep = "-")),VALOR_1,.fun=mean,.desc=T))) %>%
      arrange(COLOR_ID)

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la garantia depositada diaria
    plot <- plot_ly(data= datos_completos ,x=~FECHA,colors = colores,color=~COLOR_ID,alpha=1,
                    textposition = 'none') %>%
      add_lines(y=~VALOR_1,text=~TEXTO_1,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_lines(y=~VALOR_2,text=~TEXTO_2,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_lines(y=~VALOR_3,text=~TEXTO_3,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      subplot(nrows = 3,shareX = TRUE) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Garantía Miles M-COP",fixedrange=fixedrange),
             yaxis2 = list(title = "Utilidad M-COP",fixedrange=fixedrange),
             yaxis3 = list(title = "Tarifa M-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}


#' Gráfica la garantía depositada vs. utilidad remuneración vs. tarifa remuneración promedio diario por (Mes o Año) (barras)
#'
#' Esta función crea la gráfica de la garantía depositada vs. utilidad remuneración vs. tarifa remuneración promedio diario en formato de barras.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_cm_remuneracion_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' #' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param promedio clase character. "m" si se desea promediar por mes y "y" si se desea promediar por año. Por defecto "m"
#' @export

gt_gar_dep_remuneracion_promedio_diario<- function(datos,colores,fixedrange=FALSE,promedio="m"){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se define la granularidad del promedio
    if (promedio=="m"){
      fecha_formato <- list(FORMATO_DATOS="%Y-%m",FORMATO_TIPO_GRAFICA="date",FORMATO_GRAFICA="%b-%Y")
    }else{
      fecha_formato <- list(FORMATO_DATOS="%Y",FORMATO_TIPO_GRAFICA=NULL,FORMATO_GRAFICA=NULL)
    }

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      mutate(FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),
             VALOR_1=GARANTIA,VALOR_2=UTILIDAD_REMUNERACION,VALOR_3=TARIFA_REMUNERACION)  %>%
      group_by(TIPO="ESTADO",ID=ESTADO,FECHA_FORMATO,FECHA) %>%
      summarise(across(VALOR_1, ~sum(.x)/1e+9),across(c(VALOR_2:VALOR_3), ~sum(.x)/1e+6),.groups="drop_last") %>%
      summarise(across(c(VALOR_1:VALOR_3), ~round(mean(.x),6)),.groups="drop_last")%>%
      mutate(across(c(VALOR_1:VALOR_3),~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Miles Millones","/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)),"P /",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Millones","/",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2,"C"),
             TEXTO_3=paste(VALOR_3,"Millones","/",dt_porcentaje_caracter(VALOR_3/sum(VALOR_3)),"P /",CAMBIO_VALOR_3,"C")) %>% ungroup() %>%
      mutate(COLOR_ID=dt_num_char(fct_reorder(factor(paste(TIPO,ID,sep="-")),VALOR_1,.fun=mean,.desc=T))) %>%
      arrange(COLOR_ID)

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la garantia depositada promedio diario por (Mes o Año)
    plot <- plot_ly(data= datos_completos ,x=~FECHA_FORMATO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,name=~ID,legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_bars(y=~VALOR_3,text=~TEXTO_3,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      subplot(nrows = 3,shareX = TRUE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type=fecha_formato$FORMATO_TIPO_GRAFICA,tickformat = fecha_formato$FORMATO_GRAFICA,title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Garantía Miles M-COP",fixedrange=fixedrange),
             yaxis2 = list(title = "Utilidad M-COP",fixedrange=fixedrange),
             yaxis3 = list(title = "Tarifa M-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

