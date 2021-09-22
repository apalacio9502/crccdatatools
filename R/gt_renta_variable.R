#' Grafica el volumen operado (lineas)
#'
#' Esta función crea la gráfica el volumen operado (eje y1) y el volumen operado promedio movil (eje y2)
#' en formato de lineas
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_rv_vol}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_rv_volumen_diario<- function(datos,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% group_by(FECHA) %>%
      summarise(VALOR_1=round(sum(EFECTIVO_COL,na.rm = TRUE)/1e+9,6),
                VALOR_2=round(sum(EFECTIVO_COL_MA_3M,na.rm = TRUE)/1e+9,6),
                VALOR_3=round(sum(EFECTIVO_COL_MA_6M,na.rm = TRUE)/1e+9,6),
                VALOR_4=round(sum(EFECTIVO_COL_MA_12M,na.rm = TRUE)/1e+9,6),
                TEXTO_1=paste(VALOR_1,"Miles M"),
                TEXTO_2=paste(VALOR_2,"Miles M"),
                TEXTO_3=paste(VALOR_3,"Miles M"),
                TEXTO_4=paste(VALOR_4,"Miles M"),.groups="drop")

    # Se crea la gráfica
    plot <-plot_ly(datos_completos, x = ~FECHA,hoverinfo="text+x+name")  %>%
      add_lines(y=~VALOR_1,text=~TEXTO_1,name="Diario") %>%
      add_lines(y=~VALOR_2,text=~TEXTO_2, yaxis = "y2",name = 'Prom. 3m') %>%
      add_lines(y=~VALOR_3,text=~TEXTO_3, yaxis = "y2",name = 'Prom. 6m') %>%
      add_lines(y=~VALOR_4,text=~TEXTO_4, yaxis = "y2",name = 'Prom. 12m') %>%
      subplot(nrows = 2, shareX = T,shareY = F,heights = c(0.7, 0.3)) %>%
      layout(hovermode='x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = 0,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10))),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = list(text="Diario \n (Miles de Millones-COP)",font=list(size=12)),fixedrange=fixedrange),
             yaxis2 = list(title =list(text="Promedio Movil \n  (Miles de Millones-COP)",font=list(size=12)),fixedrange=fixedrange))%>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el volumen operado por activo (lineas)
#'
#' Esta función crea la gráfica el volumen operado por activo (eje y1), la disperción VMD (eje y2) y el porcentaje de marcación (eje y3)
#' en formato de lineas
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_rv_vol}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_rv_pa_volumen_por_activo_diario<- function(datos,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea la gráfica
    plot <- plot_ly(data = datos,x=~FECHA)%>%
      add_lines(y=~EFECTIVO_TOTAL_MA_MINIMO/1e+6,text=~paste(round(EFECTIVO_TOTAL_MA_MINIMO/1e+6,6), "Millones"),hoverinfo="text+name",name="Efectivo") %>%
      add_lines(y = 500000000/1e+6,text=paste(round(500000000/1e+6,6), "Millones"),hoverinfo="text+x+name",name = 'Efectivo Minimo',showlegend=FALSE, line = list(dash = 'dot')) %>%
      add_lines(y = ~DISPERSION_VMD, name = 'Dispersión VMD', yaxis = "y2") %>%
      add_lines(y = 0.6, yaxis = "y2",name = 'Dispersión Minima',showlegend=FALSE, line = list(dash = 'dot') ) %>%
      add_lines(y= ~PORCENTAJE_MARC_PRECIO, name = 'Porcentaje Marcación', yaxis = "y3") %>%
      add_lines(y = 0.8, yaxis = "y3",name = 'Porcentaje Maracación Minima',showlegend=FALSE, line = list(dash = 'dot') ) %>%
      subplot(nrows = 3, shareX = T,shareY = F,heights = c(0.3, 0.3, 0.3)) %>%
      layout(hovermode='x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = 0,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10))),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = list(text="Efectivo \n Negociado",font=list(size=12)),fixedrange=fixedrange),
             yaxis2 = list(title =list(text="Dispersión \n VMD",font=list(size=12)) ,tickformat='%',fixedrange=fixedrange),
             yaxis3 = list(title=list(text= "Marcación \n Precio",font=list(size=12)),tickformat='%',fixedrange=fixedrange))%>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Tabla de los activos elegibles repo
#'
#' Esta función crea la tabla de los activos elegibles repo en formato html.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_cumplimiento_presupuesto}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_rv_activos_elegibles_repo<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <- datos %>%
    filter(format(FECHA,"%Y-%m")==format(fecha_analisis,"%Y-%m")) %>%
    arrange(desc(ELEGIBLE),desc(EFECTIVO_TOTAL_MA_MINIMO)) %>%
    transmute("Activo"=ACTIVO_DESCRIPCION,"Elegible"=ELEGIBLE,
              "Efectivo Negociado Promedio Movil"=EFECTIVO_TOTAL_MA_MINIMO,
              "Disperción VMD"=DISPERSION_VMD,"Porcentaje Marcación Precio"=PORCENTAJE_MARC_PRECIO)

  # Se crea la tabla cumplimento del presupuesto
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatCurrency(c(3), '$',digits = 0) %>%
    formatStyle(c(2),color = styleInterval(-0, c('orange', 'white'))) %>%
    formatStyle(c(3),color = styleInterval(500*1e+6, c('orange', 'white'))) %>%
    formatStyle(c(4),color = styleInterval( 0.6, c('orange', 'white'))) %>%
    formatStyle(c(5),color = styleInterval( 0.8, c('orange', 'white')))

  return(table)
}

#' Grafica la posición abierta valorada vs el importe (lineas)
#'
#' Esta función crea la gráfica la posición abierta valorada vs el importe diario
#' en formato de lineas.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_rv_concentracion_repos_por_activo}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param top_terceros clase boolean. TRUE si se desea mostrar la lista de botones del top 10. Por defecto FALSE
#' @export

gt_rv_pa_valorada_importe_diario<- function(datos,fixedrange=FALSE,top_terceros=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%   arrange(FECHA) %>%
      mutate(ORDENADOR=as.numeric(fct_reorder(factor(CUENTA_GARANTIA_TITULAR),.x = IMPORTE_EFECTIVO_REPOS,.fun = last,.desc = TRUE)),
             CUENTA_GARANTIA_TITULAR=if_else(as.numeric(ORDENADOR)>10,"Otros",CUENTA_GARANTIA_TITULAR)) %>%
      bind_rows(datos %>% mutate(CUENTA_GARANTIA_TITULAR="General")) %>%
      filter(case_when(top_terceros==FALSE~CUENTA_GARANTIA_TITULAR=="General",
                       TRUE~CUENTA_GARANTIA_TITULAR!="")) %>%
      group_by(FECHA,CUENTA_GARANTIA_TITULAR) %>%
      summarise(VALOR_1=round(sum(TITULOS_OBJETO_OPERACION,na.rm = TRUE)/1e+9,6),
                VALOR_2=round(sum(IMPORTE_EFECTIVO_REPOS,na.rm = TRUE)/1e+9,6),
                VALOR_3=(1-(VALOR_2/VALOR_1)),
                TEXTO_1=paste(VALOR_1,"Miles M"),
                TEXTO_2=paste(VALOR_2,"Miles M"),
                TEXTO_3=dt_porcentaje_caracter(VALOR_3),.groups = "drop") %>%
      mutate(CUENTA_GARANTIA_TITULAR=fct_reorder(factor(CUENTA_GARANTIA_TITULAR),.x = VALOR_2,.fun = last,.desc = TRUE),
             VISIBLE=ifelse(CUENTA_GARANTIA_TITULAR=="General",TRUE,FALSE)) %>%
      arrange(CUENTA_GARANTIA_TITULAR)

    # Se crea el vector de titulares
    titulares <- levels(datos_completos$CUENTA_GARANTIA_TITULAR)

    # Se verifica si se debe crear el updatemenus
    if (length(titulares)>1) {
      # Se crean los botones
      botones <- foreach(i=1:length(titulares),.combine = append) %do% {
        visible <- titulares[i]==titulares
        list(list(label = titulares[i],method = "restyle",
                  args = list(list(visible = as.logical(rep(visible,3))))))
      }

      # Se crea el updatemenus
      updatemenus <- list(
        list(active = 0,type= 'dropdown',direction = "down",xanchor = 'center',
             yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones))
    }else{
      # Se crea el updatemenus
      updatemenus <- NULL
    }

    # Se crea la grafica
    plot <- plot_ly(data= datos_completos ,x=~FECHA,split=~dt_num_char(CUENTA_GARANTIA_TITULAR),hoverinfo="text+x+name") %>%
      add_lines(y=~VALOR_1, text=~TEXTO_1,visible=~VISIBLE, name="Títulos Objeto Operación") %>%
      add_lines(y=~VALOR_2, text=~TEXTO_2,visible=~VISIBLE, name="Importe Efectivo Títulos") %>%
      add_lines(y=~VALOR_3, text=~TEXTO_3,visible=~VISIBLE, name="Haircut",yaxis="y2") %>%
      subplot(nrows = 2,heights = c(0.6,0.3),shareX = TRUE) %>%
      layout(hovermode = 'x',legend = list(orientation = 'h',xanchor = "center",x = 0.5),
             updatemenus=updatemenus,
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange),
             yaxis2 = list(title = "% Haricut",tickformat=".2%",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la posición abierta por activo, miembro y titular garantía (treemap)
#'
#' Esta función crea la gráfica de la  posición abierta por activo, miembro y titular garantía en
#' formato treemap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_rv_pa_repos}} o tener una estructura igual a dichos datos
#' @export

gt_rv_pa_repos_por_activo_miembro_titular_garantia<- function(datos){

  # Se verifica si existen datos
  if (nrow(datos %>% filter(MIEMBRO_ID_SEUDONIMO!="NA"))>0) {

    # Se filtra y modifica la granularidad de los datos
    datos <- datos %>% filter(POSICION_COMPRADORA_IMPORTE>0 ) %>%
      mutate(VALOR=POSICION_COMPRADORA_IMPORTE/1e+6)

    # Se crea el data.frame datos_completos
    datos_completos <- datos  %>% group_by(LABEL="Posición Repos",PARENT="") %>%
      summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Millones"),.groups = "drop") %>%
      bind_rows(datos %>% group_by(PARENT = "Posición Repos", LABEL = CONTRATO_DESCRIPCION) %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Millones"),.groups = "drop")) %>%
      bind_rows(datos %>% group_by(LABEL = paste(CONTRATO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO, sep = "\n"),PARENT = CONTRATO_DESCRIPCION) %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Millones"),.groups = "drop")) %>%
      bind_rows(datos %>% group_by(LABEL = paste(CONTRATO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO,CUENTA_GARANTIA_TITULAR, sep = "\n"),PARENT = paste(CONTRATO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO, sep = "\n")) %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),TEXTO=paste(round(VALOR,6),"Millones"), .groups = "drop"))


    # Se grafica la garantia depositada por titulo, miembro y tipo de cuenta promedio diario
    plot <- plot_ly(data = datos_completos,type="treemap",labels=~LABEL,parents=~PARENT,values=~VALOR,text=~TEXTO,
                    textinfo="text+label+percent parent+name",branchvalues="total",hoverinfo="label+percent parent",
                    maxdepth=2,domain=list(column=0)) %>% layout(margin=list(l=0, r=0, b=0, t=0)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la posición abierta por titular garantía, activo y miembro  (treemap)
#'
#' Esta función crea la gráfica de la  posición abierta por titular garantía, activo y miembro en
#' formato treemap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_rv_pa_repos}} o tener una estructura igual a dichos datos
#' @export

gt_rv_pa_repos_por_titular_garantia_activo_miembro<- function(datos){

  # Se verifica si existen datos
  if (nrow(datos %>% filter(MIEMBRO_ID_SEUDONIMO!="NA"))>0) {

    # Se filtra y modifica la granularidad de los datos
    datos <- datos %>% filter(POSICION_COMPRADORA_IMPORTE>0 ) %>%
      mutate(VALOR=POSICION_COMPRADORA_IMPORTE/1e+6)

    # Se genera el top 10
    datos <- datos %>%
      left_join(datos %>% group_by(CUENTA_GARANTIA_TITULAR) %>%
                  summarise(VALOR=sum(VALOR,na.rm = TRUE),.groups="drop") %>%
                  slice_max(n=10,order_by =VALOR ) %>% transmute(CUENTA_GARANTIA_TITULAR,TOP_10=TRUE),
                by="CUENTA_GARANTIA_TITULAR") %>%
      mutate(CUENTA_GARANTIA_TITULAR=ifelse(is.na(TOP_10),"Otros",CUENTA_GARANTIA_TITULAR),.keep="unused")

    # Se crea el data.frame datos_completos
    datos_completos <- datos%>% group_by(LABEL="Posición Repos",PARENT="") %>%
      summarise(VALOR=sum(VALOR),TEXTO=paste(round(VALOR,6),"Millones"),.groups = "drop") %>%
      bind_rows(datos %>% group_by(LABEL = CUENTA_GARANTIA_TITULAR,PARENT = "Posición Repos") %>%
                  summarise(VALOR=sum(VALOR),TEXTO=paste(round(VALOR,6),"Millones"),.groups = "drop")) %>%
      bind_rows(datos %>% group_by( LABEL = paste(CUENTA_GARANTIA_TITULAR,CONTRATO_DESCRIPCION, sep = "\n"),PARENT = CUENTA_GARANTIA_TITULAR) %>%
                  summarise(VALOR=sum(VALOR),TEXTO=paste(round(VALOR,6),"Millones"),.groups = "drop")) %>%
      bind_rows(datos %>% group_by( LABEL = paste(CUENTA_GARANTIA_TITULAR,CONTRATO_DESCRIPCION,MIEMBRO_ID_SEUDONIMO, sep = "\n"),PARENT = paste(CUENTA_GARANTIA_TITULAR,CONTRATO_DESCRIPCION, sep = "\n")) %>%
                  summarise(VALOR=sum(VALOR),TEXTO=paste(round(VALOR,6),"Millones"),.groups = "drop"))

    # Se grafica la garantia depositada por titulo, miembro y tipo de cuenta promedio diario
    plot <- plot_ly(data = datos_completos,type="treemap",labels=~LABEL,parents=~PARENT,values=~VALOR,text=~TEXTO,
                    textinfo="text+label+percent parent+name",branchvalues="total",hoverinfo="label+percent parent",
                    maxdepth=2,domain=list(column=0)) %>% layout(margin=list(l=0, r=0, b=0, t=0)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el haircut por activo (lineas)
#'
#' Esta función crea la gráfica el haircut  (eje y1), el volumen y la garantía (eje y2) por activo
#' en formato de lineas
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_rv_vol}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_rv_vol_haircut_por_activo_diario<- function(datos,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea la gráfica
    plot <- plot_ly(data=datos,x=~FECHA) %>%
      add_lines(y = ~HAIRCUT, name = 'Haircut', yaxis = "y1") %>%
      add_lines(y = ~HAIRCUT_MA, yaxis = "y1",name = 'Haircut Prom. 3M',showlegend=TRUE ) %>%
      add_lines(y=~EFECTIVO_TOTAL_MA_MINIMO/1e+6,text=~paste(round(EFECTIVO_TOTAL_MA_MINIMO/1e+6,6), "Millones"),
                hoverinfo="text+x+name",name="Efectivo", yaxis = "y2") %>%
      add_lines(y = ~IMPORTE_ANTES_HAIRCUT/1e+6,text=~paste(round(IMPORTE_ANTES_HAIRCUT/1e+6,6), "Millones"),
                hoverinfo="text+x+name",name = 'Garantías Dep.', yaxis = "y2") %>%
      subplot(nrows = 2, shareX = T,shareY = F,heights = c(0.6, 0.4)) %>%
      layout(hovermode='x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = 0,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10))),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title =list(text="Porcentaje",font=list(size=12)) ,tickformat='%',fixedrange=fixedrange) ,
             yaxis2 =list(title = list(text="Millones-COP",font=list(size=12)),fixedrange=fixedrange))%>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}
