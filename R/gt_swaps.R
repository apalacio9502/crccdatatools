#' Tabla posición abierta por rango resumen
#'
#' Esta función crea la posición abierta por rango en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_sw_sen_pa}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_sw_pa_por_rango_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <- datos %>%
    mutate(RANGO=factor(RANGO,c("0Y-1Y","1Y-2Y","2Y-5Y","5Y-10Y","10Y-15Y"))) %>%
    group_by(FECHA,RANGO) %>%
    summarise(POSICION_COMPRADORA_VALORADA=sum(POSICION_COMPRADORA_VALORADA),.groups="drop") %>%
    complete(FECHA,nesting(RANGO),fill = list(POSICION_COMPRADORA_VALORADA=0)) %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
    group_by(RANGO) %>%
    summarise(POSICION_ABIERTA_ULTIMO_DIA=sum(POSICION_COMPRADORA_VALORADA[FECHA==fecha_analisis]),
              POSICION_ABIERTA_PROMEDIO_DIARIO_ULTIMO_MES=mean(POSICION_COMPRADORA_VALORADA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              POSICION_ABIERTA_PROMEDIO_DIARIO_PERIODO=mean(POSICION_COMPRADORA_VALORADA),.groups="drop") %>%
    arrange(RANGO) %>%
    transmute("Rango Años"=RANGO,
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

#' Grafica la sensibilidad y posición abierta neta por nodo y miembro (barras)
#'
#' Esta función crea la gráfica la sensibilidad y posición abierta neta por nodo y miembro
#' en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_sw_sen_pa}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("Sensibilidad", "Posición Abierta Neta"). Por defecto NULL
#' @export

gt_sw_sen_pa_neta_por_nodo_miembro<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL){

    # Se verifica si existen datos
    if (nrow(datos)>0) {

      # Verificación inputs
      if (is.null(boton_activo)) {boton_activo <- "Sensibilidad"}

      # Se crea el data.frame tipos
      tipos <- data.frame(TIPO=c("SENSIBILIDAD","POSICION_NETA_VALORADA"),
                          BOTON=c("Sensibilidad","Posición Abierta Neta"),
                          UNIDAD=c("Millones-COP","Miles M-COP")) %>%
        mutate(VISIBLE=BOTON==boton_activo)

      # Se crea el data.frame datos_completos
      datos_completos <- datos %>%
        transmute(MIEMBRO_ID_SEUDONIMO,ID_1=factor(NODO,c("1Y","2Y","5Y","10Y","15Y")),
                  ID_2=factor(RANGO,c("0Y-1Y","1Y-2Y","2Y-5Y","5Y-10Y","10Y-15Y")),
                  VALOR_1=round(SENSIBILIDAD/1e+6,6),
                  VALOR_2=round(POSICION_NETA_VALORADA/1e+9,6),
                  TEXTO_1=paste(VALOR_1,"Miles M"),
                  TEXTO_2=paste(VALOR_2,"Miles M"),
                  COLOR_ID=dt_num_char(ID_1),TIPO="NODO")

      # Se crean los botones
      botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
        visible <- tipos$BOTON[i]==tipos$BOTON
        botones <- list(list(label = tipos$BOTON[i],method = "update",
                             args = list(list(boton_activo=tipos$BOTON[i],
                                              visible = as.logical(c(rep(visible[1],5),rep(visible[2],5)))),
                                         list(yaxis=list(title=tipos$UNIDAD[i],fixedrange=fixedrange)))))
      }

      # Se crea el vecto colores
      colores <-datos_completos %>% distinct(TIPO,ID=ID_1,COLOR_ID) %>%
        left_join(colores,by = c("ID", "TIPO")) %>% arrange(COLOR_ID) %>% pull(COLOR)

      # Se crea la gráfica
      plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
                      transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                      textposition = 'none') %>%
        add_bars(y=~VALOR_1,text=~TEXTO_1,name=~ID_1,hoverinfo="text+x+name",visible=tipos$VISIBLE[1]) %>%
        add_bars(y=~VALOR_2,text=~TEXTO_2,name=~ID_2,hoverinfo="text+x+name",visible=tipos$VISIBLE[2]) %>%
        layout(barmode="relative",hovermode = 'x',
               legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2),
               updatemenus=list(
                 list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                      yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
               xaxis = list(title = NA,fixedrange=fixedrange),
               yaxis = list(title = ifelse(tipos$VISIBLE[1]==TRUE,tipos$UNIDAD[1],tipos$UNIDAD[2]),fixedrange=fixedrange)) %>%
        config(displaylogo = F,locale = "es")

      return(plot)

    }else{
      return(gt_mensaje_error)
    }
  }

#' Gráfica la distribución de la sensibilidad y posición abierta neta diaria por miembro (boxplot)
#'
#' Esta función crea la gráfica de la distribución de la sensibilidad y posición abierta neta
#' diaria por miembro en formato de boxplot
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_sw_sen_pa}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("Sensibilidad", "Posición Abierta Neta"). Por defecto NULL
#' @export

gt_sw_distribucion_sen_pa_neta_por_miembro <-  function(datos,fixedrange=FALSE,boton_activo=NULL){

  # Preprosemiento

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Verificación inputs
    if (is.null(boton_activo)) {boton_activo <- "Sensibilidad"}

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("SENSIBILIDAD","POSICION_NETA_VALORADA"),
                        BOTON=c("Sensibilidad","Posición Abierta Neta"),
                        UNIDAD=c("Millones-COP","Miles M-COP")) %>%
      mutate(VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,MIEMBRO_ID_SEUDONIMO) %>%
      summarise(VALOR_1=round(sum(SENSIBILIDAD)/1e+6,6),
                VALOR_2=round(sum(POSICION_NETA_VALORADA)/1e+9,6),.groups="drop")

    # Se crea el data.frame datos_ultimos
    datos_ultimos <- datos_completos %>% arrange(FECHA) %>%
      group_by(MIEMBRO_ID_SEUDONIMO) %>%
      summarise(VALOR_LAST_1=last((VALOR_1)),
                VALOR_LAST_2=last((VALOR_2)),.groups="drop")

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      botones <- list(list(label = tipos$BOTON[i],method = "update",
                           args = list(list(boton_activo=tipos$BOTON[i],
                                            visible = as.logical(rep(visible,2))),
                                       list(yaxis=list(title=tipos$UNIDAD[i],fixedrange=fixedrange)))))
    }

    # Se grafica el comportamiento de la sensibilidad
    plot <- plot_ly(data=datos_completos, x=~MIEMBRO_ID_SEUDONIMO) %>%
      add_boxplot(y=~VALOR_1,name="Sensibilidad",visible=tipos$VISIBLE[1]) %>%
      add_boxplot(y=~VALOR_2,name="PA Neta",visible=tipos$VISIBLE[2]) %>%
      add_data(datos_ultimos) %>%
      add_markers(y=~VALOR_LAST_1,name="Sensibilidad Último Dato",marker = list(color = "black"),
                  visible=tipos$VISIBLE[1]) %>%
      add_markers(y=~VALOR_LAST_2,name="PA Neta Último Dato",marker = list(color = "black"),
                  visible=tipos$VISIBLE[2]) %>%
      layout(legend = list(orientation = 'h',xanchor = "center",x = 0.5),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = "",fixedrange=fixedrange),
             yaxis = list(title =ifelse(tipos$VISIBLE[1]==TRUE,tipos$UNIDAD[1],tipos$UNIDAD[2]),fixedrange=fixedrange))%>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica la sensibilidad y posición abierta neta diaria por miembro (heatmap)
#'
#' Esta función crea la gráfica la sensibilidad y posición abierta neta diaria por miembro
#' en formato de heatmap.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_sw_sen_pa}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("Sensibilidad", "Posición Abierta Neta"). Por defecto NULL
#' @export

gt_sw_sen_pa_neta_diaria_por_miembro<- function(datos,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Verificación inputs
    if (is.null(boton_activo)) {boton_activo <- "Sensibilidad"}

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("SENSIBILIDAD","POSICION_NETA_VALORADA"),
                        BOTON=c("Sensibilidad","Posición Abierta Neta"),
                        UNIDAD=c("Millones-COP","Miles M-COP")) %>%
      mutate(VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      group_by(FECHA,MIEMBRO_ID_SEUDONIMO) %>%
      summarise(VALOR_1=round(sum(SENSIBILIDAD)/1e+6,6),VALOR_2=round(sum(POSICION_NETA_VALORADA)/1e+9,6),
                TEXTO_1=paste(VALOR_1,"Millones"),
                TEXTO_2=paste(VALOR_2,"Miles M"),.groups="drop")

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      botones <- list(list(label = tipos$BOTON[i],method = "restyle",
                           args = list(list(boton_activo=tipos$BOTON[i],
                                            visible = as.logical(visible)))))
    }

    # Se crea la gráfica
    plot <- plot_ly(data=datos_completos, y=~MIEMBRO_ID_SEUDONIMO, x=~FECHA) %>%
      add_heatmap(z=~VALOR_1,text=~TEXTO_1,hoverinfo="text+x+y",visible=tipos$VISIBLE[1],colorscale='YlGnBu',
                  colorbar=list(len =1,y =1,title =list(text=tipos$UNIDAD[1]))) %>%
      add_heatmap(z=~VALOR_2,text=~TEXTO_2,hoverinfo="text+x+y",visible=tipos$VISIBLE[2],colorscale='YlGnBu',
                  colorbar=list(len =1,y =1,title =list(text=tipos$UNIDAD[2]))) %>%
      layout(updatemenus=list(
        list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
             yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
        xaxis = list(type='date',tickformat = "%d-%b",title = "",showgrid = F, zeroline = F,fixedrange=fixedrange),
        yaxis = list(title = "",showgrid = F, zeroline = F,fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


#' Grafica la curva swaps (lineas + barras)
#'
#' Esta función crea la gráfica de la curva swaps en formato de lineas y barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' \code{\link{dt_sw_curva}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_sw_curva<- function(datos,fecha_analisis,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos %>% filter(FECHA==fecha_analisis))>0) {

    # Se modifica el data.frame datos
    datos <- datos %>% mutate(DISTANCIA_HOY_DIAS=as.numeric(difftime(fecha_analisis,FECHA,units="days")),
                              NODO=fct_reorder(factor(NODO),.fun = min,NODO_DIAS))

    # Se crea el data.frame datos_base
    datos_base <-datos  %>% filter(DISTANCIA_HOY_DIAS==0) %>%
      transmute(NODO=NODO,TASA_HOY=TASA)

    # Se crea el plot con los datos_base
    plot <- plot_ly(data = datos_base,textposition = 'none',x =~ NODO,colors=c("#1f77b4","#e34a33")) %>%
      add_trace(y =~ TASA_HOY, name = "Curva Hoy",type = 'scatter',mode = 'lines+markers',color="1")

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
                      transmute(CURVA=glue("Curva {i}D"),NODO,TASA),by = "NODO") %>%
          mutate(PBS_VAR_TASA = (TASA_HOY - TASA)*10000)

        # Se agrea el plazo a la lista_plazos
        lista_plazos <- c(lista_plazos,i)

        # Se agregan los traces relacionados con los datos_provisional
        plot <- plot %>% add_data(data = datos_provisional) %>%
          add_trace(y=~ TASA, name = ~CURVA,visible=ifelse(i==7,TRUE,FALSE),type = 'scatter',mode = 'lines+markers',color="2") %>%
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


    saveWidget(plot,"curva_swap_3.html")
    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

