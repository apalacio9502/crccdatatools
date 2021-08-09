#' Tabla posición abierta por tramo resumen
#'
#' Esta función crea la posición abierta por tramo en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_rf_pa_por_tramo_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <- datos %>%
    group_by(FECHA,TRAMO) %>%
    summarise(POSICION_COMPRADORA_VALORADA=sum(POSICION_COMPRADORA_VALORADA),.groups="drop") %>%
    complete(FECHA,nesting(TRAMO),fill = list(POSICION_COMPRADORA_VALORADA=0)) %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
    group_by(TRAMO) %>%
    summarise(POSICION_ABIERTA_ULTIMO_DIA=sum(POSICION_COMPRADORA_VALORADA[FECHA==fecha_analisis]),
              POSICION_ABIERTA_PROMEDIO_DIARIO_ULTIMO_MES=mean(POSICION_COMPRADORA_VALORADA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              POSICION_ABIERTA_PROMEDIO_DIARIO_PERIODO=mean(POSICION_COMPRADORA_VALORADA),.groups="drop") %>%
    arrange(TRAMO) %>%
    transmute("Tramo"=TRAMO,
              "Posición Abierta Último Día M-COP"=POSICION_ABIERTA_ULTIMO_DIA/1e+6,
              "Posición Abierta Promedio Diario Último Mes M-COP"=POSICION_ABIERTA_PROMEDIO_DIARIO_ULTIMO_MES/1e+6,
              "Posición Abierta Promedio Diario Periodo M-COP"=POSICION_ABIERTA_PROMEDIO_DIARIO_PERIODO/1e+6)

  # Se crea la tabla volumen operado
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(dom="t",searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatCurrency(c(2,3,4), '$',digits = 0)

  return(table)
}

#' Grafica el resumen por tramo y miembro (heatmap)
#'
#' Esta función crea la gráfica del resumen por tramo y miembro
#' en formato de heatmap.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @export

gt_rf_resumen_tramo_por_miembro<- function(datos,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Verificación boton_activo
    if (is.null(boton_activo)) {boton_activo <- "Posición Neta"}

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("POSICION NETA","PV01","GARANTIAS","PBS CUBIERTOS"),
                        BOTON=c("Posición Neta","PV01","Garantías","PBS Cubiertos"),
                        UNIDAD=c("Miles M","Millones","Miles M","PBS")) %>%
      mutate(VISIBLE=BOTON==boton_activo)

    # Se modifica el data.frame datos_completos
    datos_completos <-datos %>% group_by(MIEMBRO_ID_SEUDONIMO,TRAMO) %>%
      summarise(across(c("POSICION_NETA_VALORADA","PV01","GARANTIA_EXIGIDA"), ~sum(.x,na.rm = TRUE)),.groups="drop") %>%
      mutate(VALOR_1=round(POSICION_NETA_VALORADA/1e+9,6),
             TEXTO_1=paste(VALOR_1,"Miles-M"),
             VALOR_2=round(PV01/1e+6,6),
             TEXTO_2=paste(VALOR_2,"Millones"),
             VALOR_3=round(GARANTIA_EXIGIDA/1e+9,6),
             TEXTO_3=paste(VALOR_3,"Miles-M"),
             VALOR_4=abs(ifelse(GARANTIA_EXIGIDA==0,0,ifelse(PV01!=0,round(GARANTIA_EXIGIDA/(PV01),2),NaN))),
             TEXTO_4=paste(ifelse(is.nan(VALOR_4),"Indeterminado",VALOR_4),"PBS"),
             MIEMBRO_ID_SEUDONIMO = fct_reorder(factor(MIEMBRO_ID_SEUDONIMO),VALOR_2,.fun = sum,.asc = TRUE))

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      botones <- list(list(label = tipos$BOTON[i],method = "restyle",
                           args = list(list(boton_activo=tipos$BOTON[i],
                                            visible = as.logical(visible)))))
    }


    # Se grafica el resumen por tramo y miembro
    plot <- plot_ly(data=datos_completos, y=~MIEMBRO_ID_SEUDONIMO, x=~TRAMO) %>%
      add_heatmap(z=~VALOR_1,text=~TEXTO_1,hoverinfo="text+x+y",visible=tipos$VISIBLE[1],colorscale='YlGnBu',colorbar=list(len =1,y =1,title =list(text=tipos$UNIDAD[1]))) %>%
      add_heatmap(z=~VALOR_2,text=~TEXTO_2,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],colorscale='YlGnBu',colorbar=list(len =1,y =1,title =list(text=tipos$UNIDAD[2]))) %>%
      add_heatmap(z=~VALOR_3,text=~TEXTO_3,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],colorscale='YlGnBu',colorbar=list(len =1,y =1,title =list(text=tipos$UNIDAD[3]))) %>%
      add_heatmap(z=~VALOR_4,text=~TEXTO_4,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],colorscale='YlGnBu',colorbar=list(len =1,y =1,title =list(text=tipos$UNIDAD[4]))) %>%
      layout(updatemenus=list(
        list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
             yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
        xaxis = list(title = "",fixedrange=fixedrange),
        yaxis = list(title = "",showgrid = F, zeroline = F,fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica los dias al vencimiento por miembro (barras)
#'
#' Esta función crea la gráfica de los días al vencimiento por miembro
#' en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @export

gt_rf_dias_al_vencimiento_por_miembro<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Verificación boton_activo
    if (is.null(boton_activo)) {boton_activo <- "General"}

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","TRAMO"),
                        BOTON=c("General","Tramo")) %>%
      mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% group_by(MIEMBRO_ID_SEUDONIMO) %>% mutate(GENERAL="General") %>%
      select(c("MIEMBRO_ID_SEUDONIMO",tipos$TIPO,"POSICION_COMPRADORA_VALORADA","POSICION_VENDEDORA_VALORADA",
               "DUR_POSICION_COMPRADORA_VALORADA","DUR_POSICION_VENDEDORA_VALORADA")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO,ID) %>%
      summarise(VALOR_1=sum(DUR_POSICION_COMPRADORA_VALORADA,na.rm = TRUE),
                VALOR_2=sum(DUR_POSICION_VENDEDORA_VALORADA,na.rm = TRUE),
                VALOR_3=sum(POSICION_COMPRADORA_VALORADA,na.rm = TRUE),
                VALOR_4=sum(POSICION_VENDEDORA_VALORADA,na.rm = TRUE),.groups="drop_last") %>%
      mutate(TEXTO_1=paste(round(VALOR_1/VALOR_3,2)),
             TEXTO_2=paste(-round(VALOR_2/VALOR_4,2)),
             VALOR_1=VALOR_1/sum(VALOR_3),
             VALOR_2=-VALOR_2/sum(VALOR_4)) %>% ungroup() %>%
      replace_na(list(VALOR_1=0,VALOR_2=0)) %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(ID=fct_reorder(factor(ID),ID,.fun=max,.desc=F),
             COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(ID),sep="-")) %>% arrange(COLOR_ID)

    # Se el data.frame n_dist
    n_dist<- datos_completos %>% group_by(TIPO) %>% summarise(N=n_distinct(ID),.groups="drop") %>% spread(TIPO,N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(c(rep(c(rep(visible[1],n_dist$GENERAL),
                                                              rep(visible[2],n_dist$TRAMO)),2),
                                                        rep(visible[1],2)))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica los dias al vencimiento por miembro
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors=colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,name=~ID,visible=~VISIBLE_1,
               legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,name=~ID,visible=~VISIBLE_2,
               legendgroup=~ID,hoverinfo="text+x+name",showlegend=FALSE)%>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                name="Maximo",visible=tipos$VISIBLE_1[1],legendgroup="Maximo") %>%
      add_lines(y=~min(VALOR_2),line = list(color="black",dash = "dash"),
                name="Maximo",visible=tipos$VISIBLE_2[1],
                legendgroup="Maximo",showlegend=FALSE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Días al Vencimiento",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica la duración modificada por miembro (barras)
#'
#' Esta función crea la gráfica de la duración modificada por miembro
#' en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @export

gt_rf_dur_mod_por_miembro<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Verificación boton_activo
    if (is.null(boton_activo)) {boton_activo <- "General"}

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","TRAMO"),
                        BOTON=c("General","Tramo")) %>%
      mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% group_by(MIEMBRO_ID_SEUDONIMO) %>% mutate(GENERAL="General") %>%
      select(c("MIEMBRO_ID_SEUDONIMO",tipos$TIPO,"POSICION_COMPRADORA_VALORADA","POSICION_VENDEDORA_VALORADA",
               "DUR_MOD_POSICION_COMPRADORA_VALORADA","DUR_MOD_POSICION_VENDEDORA_VALORADA")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>% group_by(MIEMBRO_ID_SEUDONIMO,TIPO,ID) %>%
      summarise(VALOR_1=sum(DUR_MOD_POSICION_COMPRADORA_VALORADA,na.rm = TRUE),
                VALOR_2=sum(DUR_MOD_POSICION_VENDEDORA_VALORADA,na.rm = TRUE),
                VALOR_3=sum(POSICION_COMPRADORA_VALORADA,na.rm = TRUE),
                VALOR_4=sum(POSICION_VENDEDORA_VALORADA,na.rm = TRUE),
                VALOR_5=(if_else(VALOR_1/VALOR_3=="NaN",0,VALOR_1/VALOR_3)*(VALOR_3/(VALOR_3+VALOR_4)))+
                  (-if_else(VALOR_2/VALOR_4=="NaN",0,VALOR_2/VALOR_4)*(VALOR_4/(VALOR_3+VALOR_4))),.groups="drop_last") %>%
      mutate(TEXTO_1=paste(round(VALOR_1/VALOR_3,2)),
             TEXTO_2=paste(-round(VALOR_2/VALOR_4,2)),
             VALOR_1=VALOR_1/sum(VALOR_3),
             VALOR_2=-VALOR_2/sum(VALOR_4)) %>% ungroup() %>%
      replace_na(list(VALOR_1=0,VALOR_2=0,VALOR_5=0)) %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(ID=fct_reorder(factor(ID),ID,.fun=max,.desc=F),
             COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(ID),sep="-")) %>% arrange(COLOR_ID)

    # Se crea el data.frame n_dist
    n_dist<- datos_completos %>% group_by(TIPO) %>% summarise(N=n_distinct(ID),.groups="drop") %>% spread(TIPO,N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(c(rep(c(rep(visible[1],n_dist$GENERAL),
                                                              rep(visible[2],n_dist$TRAMO)),2),
                                                        rep(visible[1],3)))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la duración modificada por miembro
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors=colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,name=~ID,visible=~VISIBLE_1,
               legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,name=~ID,visible=~VISIBLE_2,
               legendgroup=~ID,hoverinfo="text+x+name",showlegend=FALSE) %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_markers(y=~VALOR_5,marker = list(color = "black"),name="Dur. Neta",visible=tipos$VISIBLE_1[1]) %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                name="Maximo",visible=tipos$VISIBLE_1[1],legendgroup="Maximo") %>%
      add_lines(y=~min(VALOR_2),line = list(color="black",dash = "dash"),
                name="Maximo",visible=tipos$VISIBLE_2[1],
                legendgroup="Maximo",showlegend=FALSE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Duaración Modificada",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica los días al vencimiento y la duración modificada diaria (lineas)
#'
#' Esta función crea la gráfica de los días al vencimiento y la duración modificada diaria
#' en formato de lineas
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @export

gt_rf_dias_al_vecimiento_dur_mod_diaria<- function(datos,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {
    # Verificación boton_activo

    if (is.null(boton_activo)) {boton_activo <- "Días al Vencimiento"}

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("DIAS_VENCIMIENTO","DURACION_MODIFICADA"),
                        BOTON=c("Días al Vencimiento","Duración Modificada")) %>%
      mutate(VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% group_by(FECHA) %>%
      summarise(VALOR_1=round(sum(DUR_POSICION_COMPRADORA_VALORADA,na.rm = TRUE)/sum(POSICION_COMPRADORA_VALORADA,na.rm = TRUE),2),
                VALOR_2=round(sum(DUR_MOD_POSICION_COMPRADORA_VALORADA,na.rm = TRUE)/sum(POSICION_COMPRADORA_VALORADA,na.rm = TRUE),2),.groups="drop")

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      botones <- list(list(label = tipos$BOTON[i],method = "restyle",
                           args = list(list(boton_activo=tipos$BOTON[i],
                                            visible = as.logical(rep(visible,2))))))
    }



    # Se grafica los dias al vencimiento y duración modificada por periodo
    plot <- plot_ly(data= datos_completos ,x=~FECHA) %>%
      add_lines(y=~VALOR_1,visible=~tipos$VISIBLE[1],name="Duración",fill = 'tozeroy') %>%
      add_lines(y=~VALOR_2,visible=~tipos$VISIBLE[2],name="Duración Modificada",fill = 'tozeroy') %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[1],name="Maximo",legendgroup="Maximo") %>%
      add_lines(y=~max(VALOR_2),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[2],name="Maximo", legendgroup="Maximo",showlegend=FALSE) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Valor",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica la curva tes (lineas + barras)
#'
#' Esta función crea la gráfica de la curva tes en formato de lineas y barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' \code{\link{dt_dv_curva_fwd}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export


gt_rf_curva_tes<- function(datos,fecha_analisis,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos %>% filter(FECHA==fecha_analisis))>0) {

    # Se modifica el data.frame datos
    datos <- datos %>% mutate(DISTANCIA_HOY_DIAS=as.numeric(difftime(fecha_analisis,FECHA,units="days")),.after="DURACION_ANOS")

    # Se crea el data.frame datos_base
    datos_base <-datos  %>% filter(DISTANCIA_HOY_DIAS==0) %>%
      transmute(NEMOTECNICO,TASA_HOY=TASA,DURACION_ANOS_HOY=DURACION_ANOS) %>%
      arrange(DURACION_ANOS_HOY)

    # Se crea el plot con los datos_base
    plot <- plot_ly(data = datos_base,textposition = 'none') %>%
      add_trace(x =~ DURACION_ANOS_HOY, y =~ TASA_HOY/100, text =~ NEMOTECNICO, name = "Curva Hoy",type = 'scatter',mode = 'lines+markers')

    # Se crea la lista_plaza
    lista_plazos <- c()

    # Se agrega programaticamente las curvas
    for(i in c(7,30,60,90)){

      # Se crea el data.frame datos_provisional
      datos_provisional <- datos %>% filter(DISTANCIA_HOY_DIAS>=i)

      # Se verifica si el data.frame datos_provisional contiene datos
      if (nrow(datos_provisional)>0) {

        # Se modifica el data.frame datos_provisional
        datos_provisional <- datos_base %>%
          full_join(datos_provisional %>%
                      mutate(FECHA_MAXIMA=max(FECHA)) %>% filter(FECHA==FECHA_MAXIMA) %>%
                      transmute(CURVA="0D",NEMOTECNICO,TASA,DURACION_ANOS),by = "NEMOTECNICO") %>%
          mutate(PBS_VAR_TASA = (TASA_HOY - TASA)*100) %>% arrange(DURACION_ANOS)

        # Se agrea el plazo a la lista_plazos
        lista_plazos <- c(lista_plazos,i)

        # Se agregan los traces relacionados con los datos_provisional
        plot <- plot %>% add_data(data = datos_provisional) %>%
          add_trace(x =~ DURACION_ANOS, y=~ TASA/100, text =~ NEMOTECNICO, name = glue("Curva {i}D"),visible=ifelse(i==7,TRUE,FALSE),type = 'scatter',mode = 'lines+markers') %>%
          add_bars(x =~ DURACION_ANOS_HOY,y=~ PBS_VAR_TASA, text =~ NEMOTECNICO, name = "Var pbs", visible=ifelse(i==7,TRUE,FALSE),yaxis = "y2")
      }
    }

    # Se verifica si la grafica tendra botones
    if (length(lista_plazos)>0) {
      # Se crean los botones
      botones <- foreach(i=lista_plazos,.combine = append) %do% {
        visible <- i==lista_plazos
        list(list(label = paste0(i,"D"),method = "restyle",
                  args = list(list(visible = as.logical(c(TRUE,rep(visible,2)))))))
      }

      # Se le agrega el layout al plot
      plot <- plot %>%
        subplot(nrows = 2, shareX = T,shareY = F,heights = c(0.8, 0.20)) %>%
        layout(hovermode="x",legend = list(orientation = 'h',xanchor = "center",x = 0.5),
               updatemenus=list(
                 list(active = 0,type= 'dropdown',direction = "down",xanchor = 'center',
                      yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
               xaxis = list(dtick = 3, tick0 = 1, tickmode = "linear", title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Curva",tickformat=".2%",fixedrange=fixedrange),
               yaxis2 = list(title = "PBS",fixedrange=fixedrange))%>%
        config(displaylogo = F,locale = "es")

    }else{

      # Se le agrega el layout al plot
      plot <- plot  %>%
        layout(hovermode="x",legend = list(orientation = 'h',xanchor = "center",x = 0.5),
               xaxis = list(dtick = 3, tick0 = 1, tickmode = "linear", title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Curva",tickformat=".2%",fixedrange=fixedrange))%>%
        config(displaylogo = F,locale = "es")

    }

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica la posición abierta neta diaria por tramo  (lineas)
#'
#' Esta función crea la gráfica de la posición abierta neta diaria por tramo
#' en formato de lineas
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_rf_pa_neta_diaria_por_tramo<- function(datos,colores,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,TIPO="TRAMO",ID=TRAMO) %>%
      summarise(VALOR=round(sum(POSICION_NETA_VALORADA,na.rm=TRUE)/1e+9,6),
                VALOR_1=pmax(VALOR,0),
                VALOR_2=pmin(VALOR,0),.groups="drop") %>%
      complete(FECHA,nesting(TIPO,ID), fill = list(VALOR=0,VALOR_1=0,VALOR_2=0)) %>%
      group_by(FECHA) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Miles-M","/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1))),
             TEXTO_2=paste(VALOR_2,"Miles-M","/",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)))) %>% ungroup() %>%
      mutate(COLOR_ID=paste0(dt_num_char(factor(ID)),sep="-")) %>% arrange(COLOR_ID)

    # Se crea el data.frame datos_complemento
    datos_complemento <- datos_completos %>%
      group_by(FECHA) %>%
      summarise(VALOR=sum(VALOR,na.rm=TRUE),.groups="drop") %>%
      mutate(TEXTO=paste(VALOR,"Miles-M")) %>% ungroup()

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la posicion neta por tramo
    plot <- plot_ly(data= datos_completos  ,x=~FECHA,colors=colores,hoverinfo="text+x+name",alpha = 1) %>%
      add_lines(color=~COLOR_ID,y=~VALOR_1,text=~TEXTO_1,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID) %>%
      add_lines(color=~COLOR_ID,y=~VALOR_2,text=~TEXTO_2,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="2",legendgroup=~ID,showlegend=FALSE) %>%
      add_data(datos_complemento) %>%
      add_lines(y=~VALOR,text=~TEXTO,
                name="Posición Neta",line = list(color = 'black')) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el pv01 diario por tramo (lineas)
#'
#' Esta función crea la gráfica del pv01 diario por tramo
#' en formato de lineas
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_rf_pv01_diario_por_tramo<- function(datos,colores,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,TIPO="TRAMO",ID=TRAMO) %>%
      summarise(VALOR=round(sum(PV01,na.rm=TRUE)/1e+6,6),
                VALOR_1=pmax(VALOR,0),
                VALOR_2=pmin(VALOR,0),.groups="drop") %>%
      complete(FECHA,nesting(TIPO,ID), fill = list(VALOR_1=0,VALOR_2=0)) %>%
      group_by(FECHA) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Millones","/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1))),
             TEXTO_2=paste(VALOR_2,"Millones","/",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)))) %>% ungroup() %>%
      mutate(COLOR_ID=paste0(dt_num_char(factor(ID)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el data.frame datos_complemento
    datos_complemento <- datos_completos %>%
      group_by(FECHA) %>%
      summarise(VALOR=sum(VALOR,na.rm=TRUE),.groups="drop") %>%
      mutate(TEXTO=paste(VALOR,"Millones")) %>% ungroup()

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica el PV01 por tramo
    plot <- plot_ly(data= datos_completos  ,x=~FECHA,colors=colores,hoverinfo="text+x+name",alpha = 1) %>%
      add_lines(color=~COLOR_ID,y=~VALOR_1,text=~TEXTO_1,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID) %>%
      add_lines(color=~COLOR_ID,y=~VALOR_2,text=~TEXTO_2,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="2",legendgroup=~ID,showlegend=FALSE) %>%
      add_data(datos_complemento) %>%
      add_lines(y=~VALOR,text=~TEXTO,
                name="PV01 Neto",line = list(color = 'black')) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Millones-COP (PV01)",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica la garantía exigida diaria por tramo (lineas)
#'
#' Esta función crea la gráfica de la garantía exigida diaria por tramo
#' en formato de lineas
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_rf_gar_exi_diaria_por_tramo<- function(datos,colores,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,TIPO="TRAMO",ID=TRAMO) %>%
      summarise(VALOR=round(sum(GARANTIA_EXIGIDA,na.rm=TRUE)/1e+9,6),.groups="drop") %>%
      complete(FECHA,nesting(TIPO,ID), fill = list(VALOR=0)) %>%
      group_by(FECHA) %>%
      mutate(TEXTO=paste(VALOR,"Miles-M","/",dt_porcentaje_caracter(VALOR/sum(VALOR)))) %>% ungroup() %>%
      mutate(COLOR_ID=paste0(dt_num_char(factor(ID)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el data.frame datos_complemento
    datos_complemento <- datos_completos %>%
      group_by(FECHA) %>%
      summarise(VALOR=sum(VALOR,na.rm=TRUE) ,.groups="drop") %>%
      mutate(TEXTO=paste(VALOR,"Miles-M")) %>% ungroup()

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se graficala garantía exigida por tramo
    plot <- plot_ly(data= datos_completos  ,x=~FECHA,colors=colores,hoverinfo="text+x+name",alpha = 1) %>%
      add_lines(color=~COLOR_ID, y=~VALOR,text=~TEXTO,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID) %>%
      add_data(datos_complemento) %>%
      add_lines(y=~VALOR,text=~TEXTO,
                name="Garantía Exigida",line = list(color = 'black')) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica los PBS cubiertos diarios por tramo (lineas)
#'
#' Esta función crea la gráfica de los PBS cubiertos diarios por tramo
#' en formato de lineas
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_rf_pbs_cubiertos_diarios_por_tramo<- function(datos,colores,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,TIPO="TRAMO",ID=TRAMO) %>%
      summarise(VALOR= round(abs(sum(GARANTIA_EXIGIDA,na.rm=TRUE)/sum(PV01,na.rm=TRUE)),2),.groups="drop") %>%
      complete(FECHA,nesting(TIPO,ID), fill = list(VALOR=0)) %>%
      group_by(FECHA) %>%
      mutate(TEXTO=paste(VALOR,"PBS","/",dt_porcentaje_caracter(VALOR/sum(VALOR)))) %>% ungroup() %>%
      mutate(COLOR_ID=paste0(dt_num_char(factor(ID)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el data.frame datos_complemento
    datos_complemento <- datos_completos %>%
      group_by(FECHA) %>%
      summarise(VALOR=mean(VALOR,na.rm=TRUE),.groups="drop") %>%
      mutate(TEXTO=paste(VALOR,"PBS")) %>% ungroup()

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica el volumen operado mensual promedio
    plot <- plot_ly(data= datos_completos  ,x=~FECHA,colors=colores,hoverinfo="text+x+name",alpha = 1) %>%
      add_lines(color=~COLOR_ID,y=~VALOR,text=~TEXTO,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID) %>%
      add_data(datos_complemento) %>%
      add_lines(y=~VALOR,text=~TEXTO,name="Promedio PBS cubiertos",line = list(color = 'black')) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "PBS",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}


