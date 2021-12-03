#' Tabla volumen, liquidacion y número operaciones resumen
#'
#' Esta función crea la tabla volumen, liquidacion y número operaciones en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_pa_resumen}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_div_vol_liq_op_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  if (sum(datos$VOLUMEN_COMPRA)==sum(datos$VOLUMEN_VENTA) & sum(datos$LIQUIDACION_COMPRA)==sum(datos$LIQUIDACION_VENTA) &
      sum(datos$NUMERO_OPERACIONES_COMPRA)==sum(datos$NUMERO_OPERACIONES_VENTA)) {
    # Manipulación de datos
    datos <- datos  %>% bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado")) %>%
      group_by(FECHA,SEGMENTO_NOMBRE) %>%
      summarise(VOLUMEN_COMPRA=sum(VOLUMEN_COMPRA,na.rm=TRUE),
                LIQUIDACION_COMPRA=sum(LIQUIDACION_COMPRA,na.rm=TRUE),
                NUMERO_OPERACIONES_COMPRA=sum(NUMERO_OPERACIONES_COMPRA,na.rm=TRUE),.groups="drop") %>%
      mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
      group_by(SEGMENTO_NOMBRE) %>%
      summarise(VOLUMEN_DIARIO=sum(VOLUMEN_COMPRA[FECHA==fecha_analisis]),
                VOLUMEN_PROMEDIO_PERIODO=mean(VOLUMEN_COMPRA),
                LIQUIDACION_DIARIA=sum(LIQUIDACION_COMPRA[FECHA==fecha_analisis]),
                LIQUIDACION_PROMEDIO_PERIODO=mean(LIQUIDACION_COMPRA),
                NUMERO_OPERACIONES_DIARIO=sum(NUMERO_OPERACIONES_COMPRA[FECHA==fecha_analisis]),
                NUMERO_OPERACIONES_PROMEDIO_PERIODO=mean(NUMERO_OPERACIONES_COMPRA),.groups = "drop")   %>%
      arrange(desc(VOLUMEN_DIARIO)) %>%
      transmute(Segmento=SEGMENTO_NOMBRE,
                "Volumen Último Día M-USD"=VOLUMEN_DIARIO/1e+6,
                "Volumen Promedio Diario Periodo M-USD"=VOLUMEN_PROMEDIO_PERIODO/1e+6,
                "Liquidación Último Día M-USD"=LIQUIDACION_DIARIA/1e+6,
                "Liquidación Promedio Diario Periodo M-USD"=LIQUIDACION_PROMEDIO_PERIODO/1e+6,
                "Número Op. Último Día M-USD"=NUMERO_OPERACIONES_DIARIO,
                "Número Op. Promedio Diario Periodo M-USD"=NUMERO_OPERACIONES_PROMEDIO_PERIODO)

    # Se crea la tabla
    table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                       options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                      columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% formatCurrency(c(2,3,4,5), '$',digits = 0) %>%
      formatRound(c(6,7),digits = 0)

  }else{
    # Manipulación de datos
    datos <- datos  %>% bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado")) %>%
      group_by(FECHA,SEGMENTO_NOMBRE) %>%
      summarise(VOLUMEN_COMPRA=sum(VOLUMEN_COMPRA,na.rm=TRUE),
                VOLUMEN_VENTA=sum(VOLUMEN_VENTA,na.rm=TRUE),
                LIQUIDACION_COMPRA=sum(LIQUIDACION_COMPRA,na.rm=TRUE),
                LIQUIDACION_VENTA=sum(LIQUIDACION_VENTA,na.rm=TRUE),
                NUMERO_OPERACIONES_COMPRA=sum(NUMERO_OPERACIONES_COMPRA,na.rm=TRUE),
                NUMERO_OPERACIONES_VENTA=sum(NUMERO_OPERACIONES_VENTA,na.rm=TRUE),.groups="drop") %>%
      mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
      group_by(SEGMENTO_NOMBRE) %>%
      summarise(VOLUMEN_COMPRA_DIARIO=sum(VOLUMEN_COMPRA[FECHA==fecha_analisis]),
                VOLUMEN_VENTA_DIARIO=sum(VOLUMEN_VENTA[FECHA==fecha_analisis]),
                VOLUMEN_COMPRA_PROMEDIO_PERIODO=mean(VOLUMEN_COMPRA),
                VOLUMEN_VENTA_PROMEDIO_PERIODO=mean(VOLUMEN_VENTA),
                LIQUIDACION_COMPRA_DIARIA=sum(LIQUIDACION_COMPRA[FECHA==fecha_analisis]),
                LIQUIDACION_VENTA_DIARIA=sum(LIQUIDACION_VENTA[FECHA==fecha_analisis]),
                LIQUIDACION_COMPRA_PROMEDIO_PERIODO=mean(LIQUIDACION_COMPRA),
                LIQUIDACION_VENTA_PROMEDIO_PERIODO=mean(LIQUIDACION_VENTA),
                NUMERO_OPERACIONES_COMPRA_DIARIO=sum(NUMERO_OPERACIONES_COMPRA[FECHA==fecha_analisis]),
                NUMERO_OPERACIONES_VENTA_DIARIO=sum(NUMERO_OPERACIONES_VENTA[FECHA==fecha_analisis]),
                NUMERO_OPERACIONES_COMPRA_PROMEDIO_PERIODO=mean(NUMERO_OPERACIONES_COMPRA),
                NUMERO_OPERACIONES_VENTA_PROMEDIO_PERIODO=mean(NUMERO_OPERACIONES_VENTA),.groups = "drop")   %>%
      arrange(desc(VOLUMEN_COMPRA_DIARIO)) %>%
      transmute(Segmento=SEGMENTO_NOMBRE,
                "Volumen Compra Último Día M-USD"=VOLUMEN_COMPRA_DIARIO/1e+6,
                "Volumen Venta Último Día M-USD"=VOLUMEN_VENTA_DIARIO/1e+6,
                "Volumen Compra Promedio Diario Periodo M-USD"=VOLUMEN_COMPRA_PROMEDIO_PERIODO/1e+6,
                "Volumen Venta Promedio Diario Periodo M-USD"=VOLUMEN_VENTA_PROMEDIO_PERIODO/1e+6,
                "Liquidación Compra Último Día M-USD"=LIQUIDACION_COMPRA_DIARIA/1e+6,
                "Liquidación Venta Último Día M-USD"=LIQUIDACION_VENTA_DIARIA/1e+6,
                "Liquidación Compra Promedio Diario Periodo M-USD"=LIQUIDACION_COMPRA_PROMEDIO_PERIODO/1e+6,
                "Liquidación Venta Promedio Diario Periodo M-USD"=LIQUIDACION_VENTA_PROMEDIO_PERIODO/1e+6,
                "Número Op. Compra Último Día M-USD"=NUMERO_OPERACIONES_COMPRA_DIARIO,
                "Número Op. Venta Último Día M-USD"=NUMERO_OPERACIONES_VENTA_DIARIO,
                "Número Op. Compra Promedio Diario Periodo M-USD"=NUMERO_OPERACIONES_COMPRA_PROMEDIO_PERIODO,
                "Número Op. Venta Promedio Diario Periodo M-USD"=NUMERO_OPERACIONES_VENTA_PROMEDIO_PERIODO)

    # Se crea la tabla
    table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                       options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                      columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% formatCurrency(c(2,3,4,5,6,7,8,9), '$',digits = 0) %>%
      formatRound(c(10,11,12,13),digits = 0)

  }

  return(table)
}

#' Grafica el volumen vs. liquidación vs. número operaciones por miembro (barras)
#'
#' Esta función crea la gráfica del volumen (eje y1) vs. liquidación (eje y2) vs. número operaciones por miembro
#' en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_pa_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento"). Por defecto c()
#' @export

gt_div_vol_liq_op_por_miembro<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(VOLUMEN_COMPRA>0 | VOLUMEN_VENTA>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE"),
                        BOTON=c("General","Segmento")) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <-datos %>%
      mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO), if_else(VOLUMEN_COMPRA>LIQUIDACION_VENTA,VOLUMEN_COMPRA,-LIQUIDACION_VENTA),.fun=max,.desc=T),
             GENERAL="General",VALOR_1=VOLUMEN_COMPRA,VALOR_2=-VOLUMEN_VENTA,
             VALOR_3=LIQUIDACION_COMPRA, VALOR_4=-LIQUIDACION_VENTA,
             VALOR_5=NUMERO_OPERACIONES_COMPRA,VALOR_6=-NUMERO_OPERACIONES_VENTA) %>%
      select(c("MIEMBRO_ID_SEUDONIMO",tipos$TIPO,"VALOR_1":"VALOR_6")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO,ID) %>%  summarise(across(VALOR_1:VALOR_4, ~round(sum(.x)/1e+6,6)),across(VALOR_5:VALOR_6, ~sum(.x)),.groups="drop_last") %>%
      mutate(TEXTO_1=paste(VALOR_1,"Millones/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P"),
             TEXTO_2=paste(VALOR_2,"Millones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P"),
             TEXTO_3=paste(VALOR_3,"Millones/",dt_porcentaje_caracter(VALOR_3/sum(VALOR_3)), "P"),
             TEXTO_4=paste(VALOR_4,"Millones /",dt_porcentaje_caracter(VALOR_4/sum(VALOR_4)),"P"),
             TEXTO_5=paste(VALOR_5,"op. /",dt_porcentaje_caracter(VALOR_5/sum(VALOR_5)), "P /",ifelse(is.nan(VALOR_1/VALOR_5),0,round(VALOR_1/VALOR_5,2)),"M Ticker Prom."),
             TEXTO_6=paste(VALOR_6,"Op. /",dt_porcentaje_caracter(VALOR_6/sum(VALOR_6)),"P /","C /",ifelse(is.nan(VALOR_2/VALOR_6),0,round(VALOR_2/VALOR_6,2)),"M Ticker Prom.")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(rep(c(rep(rep(visible,n_dist),2),rep(visible[1],2)),3))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none')  %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE,name=~ID,legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_3,text=~TEXTO_3,visible=~VISIBLE,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_bars(y=~VALOR_4,text=~TEXTO_4,visible=~VISIBLE,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_bars(y=~VALOR_5,text=~TEXTO_5,visible=~VISIBLE,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      add_bars(y=~VALOR_6,text=~TEXTO_6,visible=~VISIBLE,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[1],name="Máximo",legendgroup="Máximo") %>%
      add_lines(y=~min(VALOR_2),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE) %>%
      add_lines(y=~max(VALOR_3),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[1],name="Máximo",legendgroup="Máximo",showlegend=FALSE,yaxis="y2") %>%
      add_lines(y=~min(VALOR_4),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE,yaxis="y2") %>%
      add_lines(y=~max(VALOR_5),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[1],name="Máximo",legendgroup="Máximo",showlegend=FALSE,yaxis="y3") %>%
      add_lines(y=~min(VALOR_6),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE,yaxis="y3") %>%
      subplot(nrows = 3,shareX = TRUE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Volumen \n M-USD",fixedrange=fixedrange),
             yaxis2 = list(title = "Liquidación \n M-USD",fixedrange=fixedrange),
             yaxis3 = list(title = "Número \n Op.",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


#' Grafica el volumen vs. liquidación vs. número operaciones diaras (lines)
#'
#' Esta función crea la gráfica del volumen (eje y1) vs. liquidación (eje y2) vs. número operaciones diarias en formato de lineas.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_pa_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param dash_board clase boolean. TRUE si se desea monstrar en la agrupaciones simetricas unicamente el eje positivo. Por defecto TRUE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Miembro"). Por defecto c()
#' @export

gt_div_vol_liq_op_diario<- function(datos,colores,fixedrange=FALSE,dash_board=TRUE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","MIEMBRO_TIPO"),
                        BOTON=c("General","Segmento","Tipo Miembro"),
                        AMBOS=c(dash_board==T,dash_board==T,T)) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=as.logical(VISIBLE_1*AMBOS))

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(GENERAL="General",VALOR_1=VOLUMEN_COMPRA,VALOR_2=-VOLUMEN_VENTA,
                                        VALOR_3=LIQUIDACION_COMPRA, VALOR_4=-LIQUIDACION_VENTA,
                                        VALOR_5=NUMERO_OPERACIONES_COMPRA,VALOR_6=-NUMERO_OPERACIONES_VENTA) %>%
      group_by(across(c("FECHA",tipos$TIPO)))  %>% summarise(across(VALOR_1:VALOR_6,.fns = sum),.groups = "drop") %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(TIPO,ID,FECHA) %>% summarise(across(VALOR_1:VALOR_4, ~round(sum(.x)/1e+6,6)),across(VALOR_5:VALOR_6, ~sum(.x)),.groups="drop_last") %>%
      mutate(across(VALOR_1:VALOR_6,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Millones/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Millones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2,"C"),
             TEXTO_3=paste(VALOR_3,"Millones/",dt_porcentaje_caracter(VALOR_3/sum(VALOR_3)), "P /",CAMBIO_VALOR_3,"C /",dt_porcentaje_caracter(ifelse(is.nan(VALOR_3/VALOR_1),0,1-(VALOR_3/VALOR_1))),"Ahorro Liq."),
             TEXTO_4=paste(VALOR_4,"Millones /",dt_porcentaje_caracter(VALOR_4/sum(VALOR_4)),"P /",CAMBIO_VALOR_4,"C /",dt_porcentaje_caracter(ifelse(is.nan(VALOR_4/VALOR_2),0,1-(VALOR_4/VALOR_2))), "Ahorro Liq."),
             TEXTO_5=paste(VALOR_5,"op. /",dt_porcentaje_caracter(VALOR_5/sum(VALOR_5)), "P /",CAMBIO_VALOR_5,"C /",ifelse(is.nan(VALOR_1/VALOR_5),0,round(VALOR_1/VALOR_5,2)),"M Ticker Prom."),
             TEXTO_6=paste(VALOR_6,"Op. /",dt_porcentaje_caracter(VALOR_6/sum(VALOR_6)),"P /",CAMBIO_VALOR_6,"C /",ifelse(is.nan(VALOR_2/VALOR_6),0,round(VALOR_2/VALOR_6,2)),"M Ticker Prom.")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)


    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(rep(c(rep(visible,n_dist),rep(visible*tipos$AMBOS,n_dist),
                                                            visible[1],visible[1]*tipos$AMBOS[1]),3))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA,colors = colores,color=~COLOR_ID,alpha=1,
                    textposition = 'none') %>%
      add_lines(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE_1,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_lines(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE_2,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="2",legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name") %>%
      add_lines(y=~VALOR_3,text=~TEXTO_3,visible=~VISIBLE_1,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="3",legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_lines(y=~VALOR_4,text=~TEXTO_4,visible=~VISIBLE_2,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="4",legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_lines(y=~VALOR_5,text=~TEXTO_5,visible=~VISIBLE_1,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="3",legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      add_lines(y=~VALOR_6,text=~TEXTO_6,visible=~VISIBLE_2,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="4",legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_1[1],name="Máximo",legendgroup="Máximo") %>%
      add_lines(y=~min(VALOR_2),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_2[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE) %>%
      add_lines(y=~max(VALOR_3),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_1[1],name="Máximo",legendgroup="Máximo",showlegend=FALSE,yaxis="y2") %>%
      add_lines(y=~min(VALOR_4),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_2[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE,yaxis="y2") %>%
      add_lines(y=~max(VALOR_5),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_1[1],name="Máximo",legendgroup="Máximo",showlegend=FALSE,yaxis="y3") %>%
      add_lines(y=~min(VALOR_6),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_2[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE,yaxis="y3") %>%
      subplot(nrows = 3,shareX = TRUE) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Volumen \n Millones-USD",fixedrange=fixedrange),
             yaxis2 = list(title = "Liquidación \n Millones-USD",fixedrange=fixedrange),
             yaxis3 = list(title = "Número \n Op.",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))


    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el volumen vs. liquidación vs. número operaciones promedio diario por (Mes o Año) (barras)
#'
#' Esta función crea la gráfica del volumen (eje y1) vs. liquidación (eje y2) vs. número operaciones promedio diario en formato de barras
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_pa_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param dash_board clase boolean. TRUE si se desea monstrar en la agrupaciones simetricas unicamente el eje positivo. Por defecto TRUE
#' @param promedio clase character. "m" si se desea promediar por mes y "y" si se desea promediar por año. Por defecto "m"
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Miembro"). Por defecto c()
#' @export

gt_div_vol_liq_op_promedio_diario<- function(datos,colores,fixedrange=FALSE,dash_board=TRUE,promedio="m",boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","MIEMBRO_TIPO"),
                        BOTON=c("General","Segmento","Tipo Miembro"),
                        AMBOS=c(dash_board==T,dash_board==T,T)) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=as.logical(VISIBLE_1*AMBOS))

    # Se define la granularidad del promedio
    if (promedio=="m"){
      fecha_formato <- list(FORMATO_DATOS="%Y-%m",FORMATO_TIPO_GRAFICA="date",FORMATO_GRAFICA="%b-%Y")
    }else{
      fecha_formato <- list(FORMATO_DATOS="%Y",FORMATO_TIPO_GRAFICA=NULL,FORMATO_GRAFICA=NULL)
    }

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(GENERAL="General",FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),
                                        VALOR_1=VOLUMEN_COMPRA,VALOR_2=-VOLUMEN_VENTA,
                                        VALOR_3=LIQUIDACION_COMPRA, VALOR_4=-LIQUIDACION_VENTA,
                                        VALOR_5=NUMERO_OPERACIONES_COMPRA,VALOR_6=-NUMERO_OPERACIONES_VENTA) %>%
      group_by(across(c("FECHA","FECHA_FORMATO",tipos$TIPO)))  %>% summarise(across(VALOR_1:VALOR_6,.fns = sum),.groups = "drop") %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(TIPO,ID,FECHA_FORMATO,FECHA) %>% summarise(across(VALOR_1:VALOR_4, ~sum(.x)/1e+6),across(VALOR_5:VALOR_6, ~sum(.x)),.groups="drop_last") %>%
      summarise(across(VALOR_1:VALOR_4, ~round(mean(.x),6)),across(VALOR_5:VALOR_6, ~round(mean(.x),2)),.groups="drop_last") %>%
      mutate(across(VALOR_1:VALOR_6,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Millones/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Millones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2,"C"),
             TEXTO_3=paste(VALOR_3,"Millones/",dt_porcentaje_caracter(VALOR_3/sum(VALOR_3)), "P /",CAMBIO_VALOR_3,"C /",dt_porcentaje_caracter(ifelse(is.nan(VALOR_3/VALOR_1),0,1-(VALOR_3/VALOR_1))),"Ahorro Liq."),
             TEXTO_4=paste(VALOR_4,"Millones /",dt_porcentaje_caracter(VALOR_4/sum(VALOR_4)),"P /",CAMBIO_VALOR_4,"C /",dt_porcentaje_caracter(ifelse(is.nan(VALOR_4/VALOR_2),0,1-(VALOR_4/VALOR_2))), "Ahorro Liq."),
             TEXTO_5=paste(VALOR_5,"op. /",dt_porcentaje_caracter(VALOR_5/sum(VALOR_5)), "P /",CAMBIO_VALOR_5,"C /",ifelse(is.nan(VALOR_1/VALOR_5),0,round(VALOR_1/VALOR_5,2)),"M Ticker Prom."),
             TEXTO_6=paste(VALOR_6,"Op. /",dt_porcentaje_caracter(VALOR_6/sum(VALOR_6)),"P /",CAMBIO_VALOR_6,"C /",ifelse(is.nan(VALOR_2/VALOR_6),0,round(VALOR_2/VALOR_6,2)),"M Ticker Prom.")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)


    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(rep(c(rep(visible,n_dist),rep(visible*tipos$AMBOS,n_dist),
                                                            visible[1],visible[1]*tipos$AMBOS[1]),3))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA_FORMATO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none')  %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE_1,name=~ID,legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE_2,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_3,text=~TEXTO_3,visible=~VISIBLE_1,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_bars(y=~VALOR_4,text=~TEXTO_4,visible=~VISIBLE_2,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_bars(y=~VALOR_5,text=~TEXTO_5,visible=~VISIBLE_1,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      add_bars(y=~VALOR_6,text=~TEXTO_6,visible=~VISIBLE_2,name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y3") %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_1[1],name="Máximo",legendgroup="Máximo") %>%
      add_lines(y=~min(VALOR_2),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_2[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE) %>%
      add_lines(y=~max(VALOR_3),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_1[1],name="Máximo",legendgroup="Máximo",showlegend=FALSE,yaxis="y2") %>%
      add_lines(y=~min(VALOR_4),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_2[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE,yaxis="y2") %>%
      add_lines(y=~max(VALOR_5),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_1[1],name="Máximo",legendgroup="Máximo",showlegend=FALSE,yaxis="y3") %>%
      add_lines(y=~min(VALOR_6),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_2[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE,yaxis="y3") %>%
      subplot(nrows = 3,shareX = TRUE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type=fecha_formato$FORMATO_TIPO_GRAFICA,tickformat = fecha_formato$FORMATO_GRAFICA,title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Volumen \n M-USD",fixedrange=fixedrange),
             yaxis2 = list(title = "Liquidación \n M-USD",fixedrange=fixedrange),
             yaxis3 = list(title = "Número \n Op.",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}
