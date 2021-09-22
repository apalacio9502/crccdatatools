#' Grafica el reverse gap por miembro (barras)
#'
#' Esta función crea la gráfica del reverse gap por miembro en formato de barras
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_dv_pa_por_rango}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("Vencimiento hasta 1 día", "Vencimiento hasta 2 días"). Por defecto NULL
#' @export


gt_dv_reverse_gap_por_miembro<- function(datos,fixedrange=FALSE,boton_activo=NULL){

  # Se filtran los datos
  datos <- datos %>% filter(RIESGO_T1=="SI" | RIESGO_T2 =="SI")

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Verificación inputs
    if (is.null(boton_activo)) {boton_activo <- "Vencimiento hasta 1 día"}

    # Se crea la lista vencimientos
    vencimientos <- c("Vencimiento hasta 1 día","Vencimiento hasta 2 días")

    # Se modifica el data.frame datos
    datos <- datos  %>% group_by(MIEMBRO_ID_SEUDONIMO) %>%
      summarise(VALUE_1=round(sum(abs(DELTA_GARANTIA[RIESGO_T1=="SI"]))/1e+6,6),
                VALUE_2=round(sum(abs(REVERSE_GAP_GARANTIA_T1[RIESGO_T1=="SI"]))/1e+6,6),
                VALUE_3=round(sum(abs(DELTA_GARANTIA[RIESGO_T2=="SI"]))/1e+6,6),
                VALUE_4=round(sum(abs(REVERSE_GAP_GARANTIA_T2[RIESGO_T2=="SI"]))/1e+6,6),
                TEXT_1=paste(VALUE_1,"Millones"),
                TEXT_2=paste(VALUE_2,"Millones"),
                TEXT_3=paste(VALUE_3,"Millones"),
                TEXT_4=paste(VALUE_4,"Millones"),.groups="drop")

    # Se crean los botones
    botones <- foreach(i=1:length(vencimientos),.combine = append) %do% {
      visible <- vencimientos[i]==vencimientos
      list(list(label = vencimientos[i],method = "restyle",
                args = list(list(boton_activo=vencimientos[i],
                                 visible = as.logical(c(rep(visible[1],2),rep(visible[2],2)))))))
    }

    # Se grafica el Reverse GAP
    plot <- plot_ly(data= datos ,x=~MIEMBRO_ID_SEUDONIMO,colors=c("#1f77b4","#e34a33"),
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    hoverinfo="text+x+name",textposition = 'none') %>%
      add_bars(y=~VALUE_1,text=~TEXT_1,name="Delta Garantía",visible=vencimientos[1]==boton_activo,color="1") %>%
      add_bars(y=~VALUE_2,text=~TEXT_2,name="Reverse GAP",visible=vencimientos[1]==boton_activo,color="2") %>%
      add_bars(y=~VALUE_3,text=~TEXT_3,name="Delta Garantía",visible=vencimientos[2]==boton_activo,color="1") %>%
      add_bars(y=~VALUE_4,text=~TEXT_4,name="Reverse GAP",visible=vencimientos[2]==boton_activo,color="2") %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2),
             updatemenus=list(
               list(active = which(vencimientos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Millones-USD",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


#' Tabla posición abierta por rango resumen
#'
#' Esta función crea la posición abierta por rango en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_dv_pa_por_rango}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_dv_pa_por_rango_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <- datos %>%
    mutate(RANGO=factor(RANGO,c("0S-1S","1S-2S","2S-1M","1M-2M","2M-3M","3M-6M","6M-9M","9M-12M","12M-18M"))) %>%
    group_by(FECHA,PRODUCTO_TIPO,RANGO) %>%
    summarise(POSICION_COMPRADORA_VALORADA=sum(POSICION_COMPRADORA_VALORADA),.groups="drop") %>%
    complete(FECHA,nesting(PRODUCTO_TIPO,RANGO),fill = list(POSICION_COMPRADORA_VALORADA=0)) %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
    group_by(PRODUCTO_TIPO,RANGO) %>%
    summarise(POSICION_ABIERTA_ULTIMO_DIA=sum(POSICION_COMPRADORA_VALORADA[FECHA==fecha_analisis]),
              POSICION_ABIERTA_PROMEDIO_DIARIO_ULTIMO_MES=mean(POSICION_COMPRADORA_VALORADA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              POSICION_ABIERTA_PROMEDIO_DIARIO_PERIODO=mean(POSICION_COMPRADORA_VALORADA),.groups="drop") %>%
    arrange(PRODUCTO_TIPO,RANGO) %>%
    transmute("Producto"=PRODUCTO_TIPO,
              "Rango Meses"=RANGO,
              "Posición Abierta Último Día M-COP"=POSICION_ABIERTA_ULTIMO_DIA/1e+6,
              "Posición Abierta Promedio Diario Último Mes M-COP"=POSICION_ABIERTA_PROMEDIO_DIARIO_ULTIMO_MES/1e+6,
              "Posición Abierta Promedio Diario Periodo M-COP"=POSICION_ABIERTA_PROMEDIO_DIARIO_PERIODO/1e+6)

  # Se crea la tabla volumen operado
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatCurrency(c(3,4,5), '$',digits = 0)

  return(table)
}


#' Grafica la posición abierta neta por rango y miembro (barras)
#'
#' Esta función crea la gráfica de la posición abierta neta por rango y miembro en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_dv_pa_por_rango}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("F. TES", "OIS", "TRM"). Por defecto NULL
#' @export

gt_dv_pa_neta_por_rango_miembro<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se convierte el PRODUCTO_TIPO en un factor
    datos <- datos %>% filter(POSICION_NETA_VALORADA!=0) %>% mutate(PRODUCTO_TIPO=factor(PRODUCTO_TIPO))%>%
      arrange(PRODUCTO_TIPO)

    # Se crea el vector productos
    productos <- levels(datos$PRODUCTO_TIPO)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% productos)boton_activo <- productos[1]

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO=PRODUCTO_TIPO,ID=RANGO) %>%
      summarise(VALOR=round(POSICION_NETA_VALORADA/1e+12,6),
                TEXTO=paste(VALOR,"Billones"),.groups = "drop") %>%
      mutate(ID=fct_drop(factor(ID,c("0S-1S","1S-2S","2S-1M","1M-2M","2M-3M","3M-6M","6M-9M","9M-12M","12M-18M"))),
             COLOR_ID=paste0(dt_num_char(TIPO),"-",
                             dt_num_char(ID)),
             VISIBLE=if_else(TIPO==boton_activo,TRUE,FALSE)) %>% arrange(COLOR_ID)

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO) %>% summarise(N=n_distinct(ID),.groups="drop") %>% arrange(TIPO) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:length(productos),.combine = append) %do% {
      visible <- productos[i]==productos
      botones <- list(list(label = productos[i],method = "restyle",
                           args = list(list(visible = as.logical(c(rep(visible,n_dist)))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO="RANGO",ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la posicion neta por rango y miembro
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,
                    colors = colores,color=~COLOR_ID,hoverinfo="text+x+name",
                    textposition = 'none') %>%
      add_bars(y=~VALOR,text=~TEXTO,name=~ID,visible=~VISIBLE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2),
             updatemenus=list(
               list(active = which(productos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")


    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la distribución de la posición abierta neta diaria por miembro (boxplot)
#'
#' Esta función crea la gráfica de la distribución de la posición abierta neta
#' diaria por miembro en formato de boxplot
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_dv_pa_por_rango}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("F. TES", "OIS", "TRM"). Por defecto NULL
#' @export

gt_dv_distribucion_pa_neta_por_miembro <-  function(datos,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se convierte el PRODUCTO_TIPO en un factor
    datos <- datos %>% mutate(PRODUCTO_TIPO=factor(PRODUCTO_TIPO)) %>%
      arrange(PRODUCTO_TIPO)

    # Se crea el vector productos
    productos <- levels(datos$PRODUCTO_TIPO)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% productos)boton_activo <- productos[1]

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,MIEMBRO_ID_SEUDONIMO,PRODUCTO_TIPO) %>%
      summarise(VALOR=round(sum(POSICION_NETA_VALORADA)/1e+12,6),.groups = "drop") %>%
      mutate(VISIBLE=if_else(PRODUCTO_TIPO==boton_activo,TRUE,FALSE)) %>%
      arrange(PRODUCTO_TIPO)

    # Se crea el data.frame datos_ultimos
    datos_ultimos <- datos_completos %>% arrange(FECHA) %>%
      group_by(MIEMBRO_ID_SEUDONIMO,PRODUCTO_TIPO,VISIBLE) %>%
      summarise(VALOR_LAST=last((VALOR)),.groups="drop") %>%
      arrange(PRODUCTO_TIPO)

    # Se crean los botones
    botones <- foreach(i=1:length(productos),.combine = append) %do% {
      visible <- productos[i]==productos
      botones <- list(list(label = productos[i],method = "restyle",
                           args = list(list(visible = visible))))
    }

    # Se grafica la posicion neta por rango y miembro
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,
                    split = ~PRODUCTO_TIPO) %>%
      add_boxplot(y=~VALOR,name="PA Neta",visible=~VISIBLE) %>%
      add_data(datos_ultimos) %>%
      add_markers(split = ~PRODUCTO_TIPO,y=~VALOR_LAST,name="PA Neta Último Dato",
                  marker = list(color = "black"),visible=~VISIBLE) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2),
             updatemenus=list(
               list(active = which(productos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")


    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica la posición abierta neta diaria por miembro (heatmap)
#'
#' Esta función crea la gráfica de la posición abierta neta por miembro en formato de heatmap.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_dv_pa_por_rango}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("F. TES", "OIS", "TRM"). Por defecto NULL
#' @export

gt_dv_pa_neta_diaria_por_miembro<- function(datos,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se convierte el PRODUCTO_TIPO en un factor
    datos <- datos %>% mutate(PRODUCTO_TIPO=factor(PRODUCTO_TIPO))%>%
      arrange(PRODUCTO_TIPO)

    # Se crea el vector productos
    productos <- levels(datos$PRODUCTO_TIPO)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% productos)boton_activo <- productos[1]

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,MIEMBRO_ID_SEUDONIMO,PRODUCTO_TIPO) %>%
      summarise(VALOR=round(sum(POSICION_NETA_VALORADA,na.rm = TRUE)/1e+12,6),.groups = "drop") %>%
      complete(PRODUCTO_TIPO,nesting(FECHA, MIEMBRO_ID_SEUDONIMO)) %>%
      mutate(TEXTO=if_else(is.na(VALOR),"",paste(VALOR,"Billones")),
             VISIBLE=if_else(PRODUCTO_TIPO==boton_activo,TRUE,FALSE)) %>%
      arrange(PRODUCTO_TIPO)

    # Se crean los botones
    botones <- foreach(i=1:length(productos),.combine = append) %do% {
      visible <- productos[i]==productos
      botones <- list(list(label = productos[i],method = "restyle",
                           args = list(list(visible = visible))))
    }

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos,split=~PRODUCTO_TIPO, x=~FECHA,
                    y=~MIEMBRO_ID_SEUDONIMO,hoverinfo="text+x+y") %>%
      add_heatmap(z=~VALOR,text=~TEXTO,coloraxis = 'coloraxis',visible=~VISIBLE,showscale=~VISIBLE) %>%
      layout(coloraxis=list(colorscale='YlGnBu',reversescale=F,colorbar=list(title =list(text="Billones"))),
             updatemenus=list(
               list(active = which(productos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,showgrid = F, zeroline = F,fixedrange=fixedrange),
             yaxis = list(title = NA,showgrid = F, zeroline = F,fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")


    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}



#' Grafica la curva forward (lineas + barras)
#'
#' Esta función crea la gráfica de la curva forward en formato de lineas y barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' \code{\link{dt_dv_curva_fwd}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_dv_curva_forward<- function(datos,fecha_analisis,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos %>% filter(FECHA==fecha_analisis))>0) {

    # Se modifica el data.frame datos
    datos <- datos %>% mutate(DISTANCIA_HOY_DIAS=as.numeric(difftime(fecha_analisis,FECHA,units="days")),
                              NODO=fct_reorder(factor(NODO),.fun = min,NODO_DIAS))

    # Se crea el data.frame datos_base
    datos_base <-datos  %>% filter(DISTANCIA_HOY_DIAS==0) %>%
      transmute(NODO=NODO,PF_HOY=PF,DEVALUACION_HOY=DEVALUACION)

    # Se crea el plot con los datos_base
    plot <- plot_ly(data = datos_base,textposition = 'none',x =~ NODO,colors=c("#1f77b4","#e34a33")) %>%
      add_trace(y =~ DEVALUACION_HOY, text =~ paste(round(PF_HOY,2),"PF"),
                name = "Curva Hoy",type = 'scatter',mode = 'lines+markers')

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
                      transmute(CURVA="0D",NODO,PF,DEVALUACION),by = "NODO") %>%
          mutate(PBS_VAR_TASA = ((1+DEVALUACION_HOY)/(1+DEVALUACION)-1)*10000)

        # Se agrea el plazo a la lista_plazos
        lista_plazos <- c(lista_plazos,i)

        # Se agregan los traces relacionados con los datos_provisional
        plot <- plot %>% add_data(data = datos_provisional) %>%
          add_trace(y=~ DEVALUACION, text =~ paste(round(PF,2),"PF"), name = glue("Curva {i}D"),
                    visible=ifelse(i==7,TRUE,FALSE),type = 'scatter',mode = 'lines+markers',color="2") %>%
          add_bars(y=~ PBS_VAR_TASA, name = "Var pbs", visible=ifelse(i==7,TRUE,FALSE),yaxis = "y2",color="1")
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
        subplot(nrows = 2, shareX = T,shareY = F,heights = c(0.6, 0.40)) %>%
        layout(hovermode="x",legend = list(orientation = 'h',xanchor = "center",x = 0.5),
               updatemenus=list(
                 list(active = 0,type= 'dropdown',direction = "down",xanchor = 'center',
                      yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
               xaxis = list(title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Curva",tickformat=".2%",fixedrange=fixedrange),
               yaxis2 = list(title = "PBS",fixedrange=fixedrange))%>%
        config(displaylogo = F,locale = "es")

    }else{

      # Se le agrega el layout al plot
      plot <- plot  %>%
        layout(hovermode="x",legend = list(orientation = 'h',xanchor = "center",x = 0.5),
               xaxis = list( title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Curva",tickformat=".2%",fixedrange=fixedrange))%>%
        config(displaylogo = F,locale = "es")

    }

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


#' Grafica las liquidaciones de derivados por miembro liquidador  (barras)
#'
#' Esta función crea la gráfica de las liquidaciones de derivados por miembro liquidador en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_dv_liq_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Tipo Producto", "Subtipo Producto", "Origen Producto", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_dv_liq_por_miembro_liq<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(LIQUIDACION!=0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("General","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar."))  %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

    # Se crea el data.frame datos_completos
    datos_completos <- datos  %>%
      mutate(MIEMBRO_LIQ_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_LIQ_ID_SEUDONIMO), LIQUIDACION,.fun=sum,.desc=F),GENERAL="General") %>%
      select(c("MIEMBRO_LIQ_ID_SEUDONIMO",tipos$TIPO,"LIQUIDACION")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>% group_by(MIEMBRO_LIQ_ID_SEUDONIMO,TIPO,ID) %>%
      summarise(VALOR=round(sum(LIQUIDACION,na.rm = TRUE)/1e+9,6),
                VALOR_1=pmax(VALOR,0),VALOR_2=pmin(VALOR,0),.groups="drop_last") %>%
      mutate(TEXTO_1=paste(VALOR_1,"M Miles","/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1))),
             TEXTO_2=paste(VALOR_2,"M Miles","/",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)))) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea la lista n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(c(rep(rep(visible,n_dist),2),
                                                        rep(visible[1],2)))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
                        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se grafica la liquidación diaria por miembro
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_LIQ_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,name=~ID,visible=~VISIBLE_1,
               legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,name=~ID,visible=~VISIBLE_2,
               legendgroup=~ID,hoverinfo="text+x+name",showlegend=FALSE)%>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                name="Máximo",visible=tipos$VISIBLE_1[1],legendgroup="Máximo") %>%
      add_lines(y=~min(VALOR_2),line = list(color="black",dash = "dash"),
                name="Máximo",visible=tipos$VISIBLE_2[1],
                legendgroup="Máximo",showlegend=FALSE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica las liquidaciones de derivados diarias  (lineas)
#'
#' Esta función crea la gráfica de las liquidaciones de derivados diarias en formato de lineas.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_dv_liq_resumen}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_dv_liq_diarias<- function(datos,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,MIEMBRO_LIQ_ID_SEUDONIMO) %>%
      summarise(VALOR=pmax(round(sum(LIQUIDACION,na.rm = TRUE)/1e+9,6),0),.groups="drop_last") %>%
      summarise(VALOR=sum(VALOR),.groups="drop") %>%
      mutate(CAMBIO_VALOR=dt_porcentaje_variacion(VALOR),
             TEXTO=paste(VALOR,"Miles M /",CAMBIO_VALOR,"C"))

    # Se grafica la liquidación diaria
    plot <- plot_ly(data= datos_completos ,x=~FECHA,alpha = 1) %>%
      add_lines(y=~VALOR,text=~TEXTO,name="General",line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",hoverinfo="text+x+name") %>%
      add_lines(y=~max(VALOR),line = list(color="black",dash = "dash"),name="Máximo") %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el indicador de liquidez  (barras)
#'
#' Esta función crea la gráfica del indicador de liquidez en formato de barras.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_dv_indicador_liquidez}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_dv_indicador_liquidez<- function(datos,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se grafica el indicador de liquidez
    plot <- plot_ly(datos,
                    colors=c("#66c2a5","#8da0cb","#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A")) %>%
      add_bars(x="Liquidación",color="0-1",y=~LIQUIDACION_1/1e+12, name="M1",legendgroup="1")%>%
      add_bars(x="Liquidación",color="0-2",y=~LIQUIDACION_2/1e+12, name="M2",legendgroup="2") %>%
      add_bars(x="Liquidación Estresada",color="0-1",y=~LIQUIDACION_ESTRESADA_1/1e+12, name="M1",legendgroup="1",showlegend=FALSE)%>%
      add_bars(x="Liquidación Estresada",color="0-2",y=~LIQUIDACION_ESTRESADA_2/1e+12, name="M2",legendgroup="2",showlegend=FALSE) %>%
      add_bars(x="Recursos Liquidos",color="0-3",y=~GARANTIAS_CASH_1/1e+12, name="Gar. Cash M1") %>%
      add_bars(x="Recursos Liquidos",color="0-4",y=~GARANTIAS_CASH_2/1e+12, name="Gar. Cash M2") %>%
      add_bars(x="Recursos Liquidos",color="0-5",y=~GARANTIAS_CASH_DISPONIBLES/1e+12, name="Gar. Cash Disp.") %>%
      add_bars(x="Recursos Liquidos",color="0-6",y=~GARANTIAS_TES_1/1e+12, name="Gar. TES M1") %>%
      add_bars(x="Recursos Liquidos",color="0-7",y=~GARANTIAS_TES_2/1e+12, name="Gar. TES M2") %>%
      add_bars(x="Recursos Liquidos",color="0-8",y=~TITULOS_APT/1e+12, name="Títulos APT") %>%
      add_bars(x="Recursos Liquidos",color="0-9",y=~CTA_APT/1e+12, name="CTA APT") %>%
      add_bars(x="Lineas de Credito",color="1-0",y=~LNC_BCO_BOGOTA/1e+12, name="LNC BCO BOGOTA") %>%
      add_bars(x="Lineas de Credito",color="1-1",y=~LNC_BANCOLOMBIA/1e+12, name="LNC BANCOLOMBIA") %>%
      add_bars(x="Lineas de Credito",color="1-2",y=~LNC_DAVIVIENDA/1e+12, name="LNC DAVIVIENDA") %>%
      layout(barmode="stack",hovermode = 'compare',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5),
             xaxis = list(title = NA,categoryorder = "array",categoryarray = c("Liquidación","Liquidación Estresada","Recursos Liquidos","Lineas de Credito"),
                          fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}




