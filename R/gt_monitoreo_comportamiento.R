#' Tabla monitoreo comportamiento resumen
#'
#' Esta función crea la tabla monitoreo comportamiento en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_div_monitoreo_comportamiento_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <-  datos %>% group_by(MIEMBRO_ID_SEUDONIMO) %>%
    summarise(ATIPICO_DIARIO=sum(ATIPICO[FECHA==fecha_analisis]),
              ACUMULADO_ATIPICOS_PERIODO=sum(ATIPICO),
              ACUMULADO_TIPICOS_PERIODO=n()-sum(ATIPICO),
              PORCENTAJE_ATIPICOS_PERIODO=ACUMULADO_ATIPICOS_PERIODO/n(),
              PORCENTAJE_TIPICOS_PERIODO=ACUMULADO_TIPICOS_PERIODO/n(),.groups = "drop") %>%
    arrange(desc(ACUMULADO_ATIPICOS_PERIODO,ATIPICO_DIARIO)) %>%
    transmute("Miembro ID"=MIEMBRO_ID_SEUDONIMO,
              "Atípico Último Día"=ATIPICO_DIARIO,
              "Atípicos Acumulados Periodo"=ACUMULADO_ATIPICOS_PERIODO,
              "Típicos Acumulados Periodo"=ACUMULADO_TIPICOS_PERIODO,
              "% Atípicos Acumulados Periodo"=PORCENTAJE_ATIPICOS_PERIODO,
              "% Típicos Acumulados Periodo"=PORCENTAJE_TIPICOS_PERIODO)

  # Se crea la tabla ingresos
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatPercentage(c(5,6),digits = 2)

  return(table)
}

#' Grafica los datos atípicos diarios por miembro (puntos)
#'
#' Esta función crea la gráfica de loslos datos atípicos diarios por miembro en formato de puntos.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_div_monitoreo_comportamiento}} o tener una estructura igual a dichos datos
#' (Solo deben incluir los datos de un miembro)
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_div_monitoreo_comportamiento_diario_por_miembro<- function(datos,fixedrange=FALSE){

  # Se filtran los datos
  datos <- datos %>% filter(ATIPICO==1)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea la gráfica
    plot <- plot_ly(data= datos ,x=~FECHA) %>%
      add_markers(y=~MIEMBRO_ID_SEUDONIMO,name="Atípico") %>%
      layout(hovermode = 'x',
             showlegend=T,
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = NA,fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))


    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}



#' Grafica el volumen, liquidación, número operaciones diaras y participación
#' de un miembro para una fecha de análisis vs. su comportamiento típico (radar)
#'
#' Esta función crea la gráfica del volumen, liquidación, número operaciones diaras y participación
#' de un miembro para una fecha de análisis vs. su comportamiento típico (radar) en formato de radar.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_div_monitoreo_comportamiento}} o tener una estructura igual a dichos datos
#' (Solo deben incluir los datos de un miembro)
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @export

gt_div_monitoreo_comportamiento_miembro<- function(datos,fecha_analisis){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se modifica el data.frame datos
    datos <- datos %>% mutate(across(VOLUMEN_COMPRA:PARTICIPACION_VOLUMEN_VENTA,~normalize(.x,na.rm = T),.names="N_{.col}"),.after="PARTICIPACION_VOLUMEN_VENTA")

    # Se crea el data.frame datos_completos
    datos_completos <-datos %>% filter(FECHA==fecha_analisis) %>% select(c(VOLUMEN_COMPRA:N_PARTICIPACION_VOLUMEN_VENTA))%>%
      bind_rows(datos  %>% filter(FECHA<fecha_analisis,ATIPICO!=1) %>% select(c(VOLUMEN_COMPRA:N_PARTICIPACION_VOLUMEN_VENTA)) %>%
                  group_by() %>% summarise(across(VOLUMEN_COMPRA:N_PARTICIPACION_VOLUMEN_VENTA,~quantile(.x,probs=0.95,na.rm = T)),.groups="drop")) %>%
      mutate(across(c(VOLUMEN_COMPRA:LIQUIDACION_VENTA),~paste(round(.x/1e+6,6),"Millones USD"),.names="TEXTO_{.col}"),
             across(c(NUMERO_OPERACIONES_COMPRA:NUMERO_OPERACIONES_VENTA),~paste(.x,"Op."),.names="TEXTO_{.col}"),
             across(c(PARTICIPACION_VOLUMEN_COMPRA:PARTICIPACION_VOLUMEN_VENTA),~dt_porcentaje_caracter(.x),.names="TEXTO_{.col}"))

    # Se crea la gráfica
    plot <- plot_ly(type = 'scatterpolar',fill = 'toself',mode="markers") %>%
      add_trace(r =datos_completos[1,c(9,11,13,15,10,12,14,16)] %>% t() %>% as.vector(),
                theta=c("Volumen CM", "Liquidación CM", "Número Op. CM", "Participancion CM", "Volumen VT", "Liquidación VT","Número Op. VT", "Participación VT"),
                text=datos_completos[1,c(17,19,21,23,18,20,22,24)] %>% t() %>% as.vector(),name = 'Comportamiento Día') %>%
      add_trace(r =datos_completos[2,c(9,11,13,15,10,12,14,16)] %>% t() %>% as.vector(),
                theta=c("Volumen CM", "Liquidación CM", "Número Op. CM", "Participancion CM", "Volumen VT", "Liquidación VT","Número Op. VT", "Participación VT"),
                text=datos_completos[2,c(17,19,21,23,18,20,22,24)] %>% t() %>% as.vector(),name = 'Comportamiento Tipico') %>%
      layout(margin = list(l=70,r=70),legend = list(orientation = 'h',xanchor = "center",x = 0.5),
             polar = list(radialaxis = list(visible = T,range = c(0,1)))) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))


    return(plot)

  }else{
    return(gt_mensaje_error)
  }

}


#' Grafica los datos atípicos de un miembro sobre el volumen, liquidación, número operaciones diaras y participación (lineas+puntos)
#'
#' Esta función crea la gráfica de los datos atípicos de un miembro sobre el volumen (eje y1),
#' liquidación (eje y2), número operaciones diarias (eje y3) y participación (eje y4) en formato de lineas y puntos.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_div_monitoreo_comportamiento}} o tener una estructura igual a dichos datos
#' (Solo deben incluir los datos de un miembro)
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_div_monitoreo_comportamiento_diario_miembro<- function(datos,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      mutate(FECHA,VALOR_1=VOLUMEN_COMPRA,VALOR_2=-VOLUMEN_VENTA,
             VALOR_3=LIQUIDACION_COMPRA, VALOR_4=-LIQUIDACION_VENTA,
             VALOR_5=NUMERO_OPERACIONES_COMPRA,VALOR_6=-NUMERO_OPERACIONES_VENTA,
             VALOR_7=PARTICIPACION_VOLUMEN_COMPRA,VALOR_8=-PARTICIPACION_VOLUMEN_VENTA,
             across(VALOR_1:VALOR_4, ~round(.x/1e+6,6)),
             across(VALOR_1:VALOR_8,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"),
             TEXTO_1=paste(VALOR_1,"Millones/",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Millones /",CAMBIO_VALOR_2,"C"),
             TEXTO_3=paste(VALOR_3,"Millones/",CAMBIO_VALOR_3,"C"),
             TEXTO_4=paste(VALOR_4,"Millones /",CAMBIO_VALOR_4,"C"),
             TEXTO_5=paste(VALOR_5,"op. /",CAMBIO_VALOR_5,"C"),
             TEXTO_6=paste(VALOR_6,"Op. /",CAMBIO_VALOR_6),
             TEXTO_7=paste(dt_porcentaje_caracter(VALOR_7),"Participación /",CAMBIO_VALOR_7),
             TEXTO_8=paste(dt_porcentaje_caracter(VALOR_8),"Participación /",CAMBIO_VALOR_8))

    # Se crea el data.frame datos_complemento
    datos_complemento <- datos %>% filter(ATIPICO==1) %>% select(FECHA) %>% left_join(datos_completos,by="FECHA")

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA,colors=c("#1f77b4","#000000"),alpha=1,
                    textposition = 'none') %>%
      add_lines(y=~VALOR_1,text=~TEXTO_1,name="Datos",line = list(color = 'transparent'),color="1",
                fill = 'tonexty',legendgroup="1",showlegend=FALSE,hoverinfo="text+x+name") %>%
      add_lines(y=~VALOR_2,text=~TEXTO_2,name="Datos",line = list(color = 'transparent'),color="1",
                fill = 'tonexty',legendgroup="1",showlegend=FALSE,hoverinfo="text+x+name") %>%
      add_lines(y=~VALOR_3,text=~TEXTO_3,name="Datos",line = list(color = 'transparent'),color="1",
                fill = 'tonexty',legendgroup="1",showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_lines(y=~VALOR_4,text=~TEXTO_4,name="Datos",line = list(color = 'transparent'),color="1",
                fill = 'tonexty',legendgroup="1",showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_lines(y=~VALOR_5,text=~TEXTO_5,name="Datos",line = list(color = 'transparent'),color="1",
                fill = 'tonexty',legendgroup="1",showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      add_lines(y=~VALOR_6,text=~TEXTO_6,name="Datos",line = list(color = 'transparent'),color="1",
                fill = 'tonexty',legendgroup="1",showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      add_lines(y=~VALOR_7,text=~TEXTO_7,name="Datos",line = list(color = 'transparent'),color="1",
                fill = 'tonexty',legendgroup="1",showlegend=FALSE,hoverinfo="text+x+name",yaxis="y4") %>%
      add_lines(y=~VALOR_8,text=~TEXTO_8,name="Datos",line = list(color = 'transparent'),color="1",
                fill = 'tonexty',legendgroup="1",showlegend=FALSE,hoverinfo="text+x+name",yaxis="y4") %>%
      add_data(datos_complemento) %>%
      add_markers(y=~VALOR_1,text=~TEXTO_1,name="Atipico",color="2",legendgroup="1", hoverinfo="text+x+name") %>%
      add_markers(y=~VALOR_2,text=~TEXTO_2,name="Atipico",color="2",legendgroup="1",showlegend=FALSE, hoverinfo="text+x+name") %>%
      add_markers(y=~VALOR_3,text=~TEXTO_3,name="Atipico",color="2",legendgroup="1",showlegend=FALSE, hoverinfo="text+x+name",yaxis="y2") %>%
      add_markers(y=~VALOR_4,text=~TEXTO_4,name="Atipico",color="2",legendgroup="1",showlegend=FALSE, hoverinfo="text+x+name",yaxis="y2") %>%
      add_markers(y=~VALOR_5,text=~TEXTO_5,name="Atipico",color="2",legendgroup="1",showlegend=FALSE, hoverinfo="text+x+name",yaxis="y3") %>%
      add_markers(y=~VALOR_6,text=~TEXTO_6,name="Atipico",color="2",legendgroup="1",showlegend=FALSE, hoverinfo="text+x+name",yaxis="y3") %>%
      add_markers(y=~VALOR_7,text=~TEXTO_7,name="Atipico",color="2",legendgroup="1",showlegend=FALSE, hoverinfo="text+x+name",yaxis="y4") %>%
      add_markers(y=~VALOR_8,text=~TEXTO_8,name="Atipico",color="2",legendgroup="1",showlegend=FALSE, hoverinfo="text+x+name",yaxis="y4") %>%
      subplot(nrows = 4,shareX = TRUE) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Volumen \n M-USD",fixedrange=fixedrange),
             yaxis2 = list(title = "Liquidación \n M-USD",fixedrange=fixedrange),
             yaxis3 = list(title = "Número \n Op.",fixedrange=fixedrange),
             yaxis4 = list(title = "Participación",tickformat=".1%",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))


    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}
