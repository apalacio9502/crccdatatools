#' Tabla volumen operado resumen
#'
#' Esta función crea la tabla volumen operado en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_vol_resumen<- function(datos,fecha_analisis,pageLength=100,fillContainer=FALSE,style="bootstrap4"){


  if (sum(datos$EFECTIVO_COMPRA)==sum(datos$EFECTIVO_VENTA)) {
    # Manipulación de datos
    datos <- datos  %>% bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado",PRODUCTO_NOMBRE="Consolidado",PRODUCTO_TIPO="Consolidado")) %>%
      group_by(FECHA,SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
      summarise(EFECTIVO_COMPRA=sum(EFECTIVO_COMPRA,na.rm=TRUE),.groups="drop") %>%
      mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
      group_by(SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
      summarise(VOLUMEN_DIARIO=sum(EFECTIVO_COMPRA[FECHA==fecha_analisis]),
                VOLUMEN_DIARIO_PROMEDIO_MENSUAL=mean(EFECTIVO_COMPRA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
                VOLUMEN_ACUMULADO_MENSUAL=sum(EFECTIVO_COMPRA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
                VOLUMEN_DIARIO_PROMEDIO_PERIODO=mean(EFECTIVO_COMPRA),
                VOLUMEN_ACUMULADO_PERIODO=sum(EFECTIVO_COMPRA),.groups = "drop")  %>%
      arrange(desc(VOLUMEN_DIARIO)) %>%
      transmute(Segmento=SEGMENTO_NOMBRE,"Tipo Producto"=PRODUCTO_TIPO,
                "Producto"=PRODUCTO_NOMBRE,
                "%"=if_else(SEGMENTO_NOMBRE=="Consolidado",1,VOLUMEN_DIARIO/sum(VOLUMEN_DIARIO[SEGMENTO_NOMBRE!="Consolidado"])),
                "Volumen Último Día M-COP"=VOLUMEN_DIARIO/1e+6,
                "Volumen Promedio Diario Último Mes M-COP"=VOLUMEN_DIARIO_PROMEDIO_MENSUAL/1e+6,
                "Volumen Acumulado Último Mes M-COP"=VOLUMEN_ACUMULADO_MENSUAL/1e+6,
                "Volumen Promedio Diario Periodo M-COP"=VOLUMEN_DIARIO_PROMEDIO_PERIODO/1e+6,
                "Volumen Acumulado Periodo M-COP"=VOLUMEN_ACUMULADO_PERIODO/1e+6)

    # Se crea la tabla
    table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                       options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                      columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
      formatPercentage(4,digits = 2) %>% formatCurrency(c(5,6,7,8,9), '$',digits = 0)

  }else{
    # Manipulación de datos
    datos <- datos  %>% bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado",PRODUCTO_NOMBRE="Consolidado",PRODUCTO_TIPO="Consolidado")) %>%
      group_by(FECHA,SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
      summarise(EFECTIVO_COMPRA=sum(EFECTIVO_COMPRA,na.rm=TRUE),
                EFECTIVO_VENTA=sum(EFECTIVO_VENTA,na.rm=TRUE),.groups="drop") %>%
      mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
      group_by(SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
      summarise(VOLUMEN_COMPRA_DIARIO=sum(EFECTIVO_COMPRA[FECHA==fecha_analisis]),
                VOLUMEN_VENTA_DIARIO=sum(EFECTIVO_VENTA[FECHA==fecha_analisis]),
                VOLUMEN_COMPRA_DIARIO_PROMEDIO_MENSUAL=mean(EFECTIVO_COMPRA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
                VOLUMEN_VENTA_DIARIO_PROMEDIO_MENSUAL=mean(EFECTIVO_VENTA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
                VOLUMEN_COMPRA_DIARIO_PROMEDIO_PERIODO=mean(EFECTIVO_COMPRA),
                VOLUMEN_VENTA_DIARIO_PROMEDIO_PERIODO=mean(EFECTIVO_VENTA),.groups = "drop")  %>%
      arrange(desc(VOLUMEN_COMPRA_DIARIO)) %>%
      transmute(Segmento=SEGMENTO_NOMBRE,"Tipo Producto"=PRODUCTO_TIPO,
                "Producto"=PRODUCTO_NOMBRE,
                "Volumen Compra Último Día M-COP"=VOLUMEN_COMPRA_DIARIO/1e+6,
                "Volumen Venta Último Día M-COP"=VOLUMEN_VENTA_DIARIO/1e+6,
                "Volumen Compra Promedio Diario Último Mes M-COP"=VOLUMEN_COMPRA_DIARIO_PROMEDIO_MENSUAL/1e+6,
                "Volumen Venta Promedio Diario Último Mes M-COP"=VOLUMEN_VENTA_DIARIO_PROMEDIO_MENSUAL/1e+6,
                "Volumen Compra Promedio Diario Periodo M-COP"=VOLUMEN_COMPRA_DIARIO_PROMEDIO_PERIODO/1e+6,
                "Volumen Venta Promedio Diario Periodo M-COP"=VOLUMEN_VENTA_DIARIO_PROMEDIO_PERIODO/1e+6)

    # Se crea la tabla
    table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                       options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                      columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
      formatCurrency(c(4,5,6,7,8,9), '$',digits = 0)

  }

  return(table)
}

#' Grafica el volumen operado (barras h)
#'
#' Esta función crea la gráfica del volumen operado (compra y venta)
#' en formato de barras horizantales.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto",
#' "Origen Producto", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_vol<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(EFECTIVO_COMPRA>0 | EFECTIVO_VENTA>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar."))  %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]


    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(VALOR_1=EFECTIVO_COMPRA,VALOR_2=EFECTIVO_VENTA) %>%
      select(c(tipos$TIPO,"VALOR_1","VALOR_2")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>% group_by(TIPO,ID) %>%
      summarise(across(VALOR_1:VALOR_2, ~round(sum(.x)/1e+12,6)),.groups="drop_last")%>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)),"P"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P"),
             VALOR_1=VALOR_1/sum(VALOR_1),
             VALOR_2=VALOR_2/sum(VALOR_2)) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>% summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(rep(rep(visible,n_dist),2))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos%>% arrange(COLOR_ID),colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    source="MOD_GE_SUB_IO_C1_D_plot_1") %>%
      add_bars(y="COMPRA",x=~VALOR_1,text=~TEXTO_1,name=~ID,visible=~VISIBLE_1,
               legendgroup=~ID,hoverinfo="text+name",textposition = 'none') %>%
      add_bars(y="VENTA",x=~VALOR_2,text=~TEXTO_2,name=~ID,visible=~VISIBLE_2,
               legendgroup=~ID,hoverinfo="text+name",textposition = 'none',showlegend=FALSE) %>%
      layout(barmode = 'stack',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             yaxis = list(title = "",tickangle = 270,fixedrange=fixedrange,showgrid = FALSE,showline = FALSE,zeroline = FALSE),
             xaxis=list(title="",tickformat = ".2%",fixedrange=fixedrange,showgrid = FALSE,showline = FALSE,zeroline = FALSE)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }

}

#' Grafica el volumen operado por miembro (barras)
#'
#' Esta función crea la gráfica del volumen operado (compra eje positivo y venta eje negativo) por miembro
#' en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto",
#'  "Origen Producto", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_vol_por_miembro<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(EFECTIVO_COMPRA>0 | EFECTIVO_VENTA>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("General","Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar.")) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% group_by(MIEMBRO_ID_SEUDONIMO) %>%
      mutate(ORDENADOR=if_else(sum(EFECTIVO_COMPRA)>sum(EFECTIVO_VENTA),sum(EFECTIVO_COMPRA),-sum(EFECTIVO_VENTA))) %>% ungroup() %>%
      mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO), ORDENADOR,.fun=max,.desc=T),
             GENERAL="General", VALOR_1=EFECTIVO_COMPRA,VALOR_2=-EFECTIVO_VENTA) %>%
      select(c("MIEMBRO_ID_SEUDONIMO",tipos$TIPO,"VALOR_1","VALOR_2")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO,ID) %>% summarise(across(VALOR_1:VALOR_2, ~round(sum(.x)/1e+12,6)),.groups="drop_last")%>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)),"P"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P")) %>% ungroup() %>%
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
                                 visible = as.logical(c(rep(rep(visible,n_dist),2),
                                                        rep(visible[1],2)))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
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
      layout(barmode="relative",hovermode="x",
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el volumen operado diario (lineas)
#'
#' Esta función crea la gráfica del volumen operado diario (compra eje positivo y venta eje negativo) en formato de lineas.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param dash_board clase boolean. TRUE si se desea monstrar en la agrupaciones simetricas unicamente el eje positivo. Por defecto TRUE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto",
#' "Origen Producto", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_vol_diario<- function(datos,colores,fixedrange=FALSE,dash_board=TRUE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("General","Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar."),
                        AMBOS=c(dash_board==T,dash_board==T,dash_board==T,dash_board==T,dash_board==T,T)) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=as.logical(VISIBLE_1*AMBOS))

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(GENERAL="General",VALOR_1=EFECTIVO_COMPRA,VALOR_2=-EFECTIVO_VENTA) %>%
      group_by(across(c("FECHA",tipos$TIPO)))  %>% summarise(across(VALOR_1:VALOR_2,.fns = sum),.groups = "drop") %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(TIPO,ID,FECHA) %>% summarise(across(VALOR_1:VALOR_2, ~round(sum(.x)/1e+12,6)),.groups="drop_last")%>%
      mutate(across(VALOR_1:VALOR_2,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2,"C")) %>% ungroup() %>%
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
                                 visible = as.logical(c(rep(visible,n_dist),rep(visible*tipos$AMBOS,n_dist),
                                                        visible[1],visible[1]*tipos$AMBOS[1]))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA,colors = colores,color=~COLOR_ID,alpha = 1) %>%
      add_lines(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE_1,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_lines(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE_2,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="2",legendgroup=~ID,hoverinfo="text+x+name",showlegend=FALSE) %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_1[1],name="Máximo",legendgroup="Máximo") %>%
      add_lines(y=~min(VALOR_2),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_2[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE) %>%
      layout(hovermode="x",
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el volumen operado promedio diario por (Mes o Año) (barras)
#'
#' Esta función crea la gráfica del volumen operado promedio diario (compra eje positivo y venta eje negativo) en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param dash_board clase boolean. TRUE si se desea monstrar en la agrupaciones simetricas unicamente el eje positivo. Por defecto TRUE
#' @param promedio clase character. "m" si se desea promediar por mes y "y" si se desea promediar por año. Por defecto "m"
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto",
#' "Origen Producto", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_vol_promedio_diario<- function(datos,colores,fixedrange=FALSE,dash_board=TRUE,promedio="m",boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("General","Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar."),
                        AMBOS=c(dash_board==T,dash_board==T,dash_board==T,dash_board==T,dash_board==T,T)) %>%
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
    datos_completos <- datos %>%
      mutate(GENERAL="General",FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),VALOR_1=EFECTIVO_COMPRA,VALOR_2=-EFECTIVO_VENTA) %>%
      group_by(across(c("FECHA","FECHA_FORMATO",tipos$TIPO)))  %>% summarise(across(VALOR_1:VALOR_2,.fns = sum),.groups = "drop") %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(TIPO,ID,FECHA_FORMATO,FECHA) %>% summarise(across(VALOR_1:VALOR_2, ~sum(.x)/1e+12),.groups="drop_last")%>%
      summarise(across(VALOR_1:VALOR_2, ~round(mean(.x),6)),.groups="drop_last")%>%
      mutate(across(VALOR_1:VALOR_2,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1, "C"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2, "C")) %>% ungroup() %>%
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
                                 visible = as.logical(c(rep(visible,n_dist),rep(visible*tipos$AMBOS,n_dist),
                                                        visible[1],visible[1]*tipos$AMBOS[1]))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA_FORMATO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE_1,
               name=~ID,legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE_2,
               name=~ID,legendgroup=~ID,hoverinfo="text+x+name",showlegend=FALSE) %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_1[1],name="Máximo",legendgroup="Máximo") %>%
      add_lines(y=~min(VALOR_2),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE_2[1],name="Máximo", legendgroup="Máximo",showlegend=FALSE) %>%
      layout(barmode="relative",hovermode="x",
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type=fecha_formato$FORMATO_TIPO_GRAFICA,tickformat = fecha_formato$FORMATO_GRAFICA,title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)


  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el volumen operado promedio diario por (Mes o Año) y tipo de cuenta (barras)
#'
#' Esta función crea la gráfica del volumen operado promedio diario (compra eje positivo y venta eje negativo)
#' por tipo de cuenta en formato de barras. La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param promedio clase character. "m" si se desea promediar por mes y "y" si se desea promediar por año. Por defecto "m"
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("Posición Propia", "Posición Terceros", "Posición Cartera"). Por defecto NULL
#' @export

gt_vol_promedio_diario_tipocuenta<- function(datos,colores,fixedrange=FALSE,promedio="m",boton_activo=NULL){

  # Preprosemiento

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("PP","PT","PC"),
                        BOTON=c("Posición Propia","Posición Terceros","Posición Cartera")) %>%
      filter(TIPO %in% unique(datos$CUENTA_GARANTIA_TIPO))

    # Verificación inputs
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se agrega el condicional visible al data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

    # Se define la granularidad del promedio
    if (promedio=="m"){
      fecha_formato <- list(FORMATO_DATOS="%Y-%m",FORMATO_TIPO_GRAFICA="date",FORMATO_GRAFICA="%b-%Y")
    }else{
      fecha_formato <- list(FORMATO_DATOS="%Y",FORMATO_TIPO_GRAFICA=NULL,FORMATO_GRAFICA=NULL)
    }

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% rename(TIPO="CUENTA_GARANTIA_TIPO",ID="PRODUCTO_SUBTIPO")  %>%
      mutate(FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),VALOR_1=EFECTIVO_COMPRA,VALOR_2=-EFECTIVO_VENTA) %>%
      select(c("FECHA","FECHA_FORMATO","TIPO","ID","VALOR_1","VALOR_2")) %>%
      group_by(TIPO,ID,FECHA_FORMATO,FECHA)  %>% summarise(across(VALOR_1:VALOR_2, ~sum(.x)/1e+12),.groups="drop_last")%>%
      summarise(across(VALOR_1:VALOR_2, ~round(mean(.x),6)),.groups="drop_last") %>%
      mutate(across(VALOR_1:VALOR_2,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2,"C")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1-VALOR_2,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO) %>% summarise(N=n_distinct(ID),.groups="drop") %>% spread(TIPO,N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = rep(foreach(i=1:nrow(tipos),.combine = append) %do% {
                                   rep(visible[i],n_dist[[tipos$TIPO[i]]][1])},2)))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      mutate(TIPO="PRODUCTO_SUBTIPO") %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA_FORMATO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE_1,
               name=~ID,legendgroup=~COLOR_ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE_2,
               name=~ID,legendgroup=~COLOR_ID,hoverinfo="text+x+name",showlegend=FALSE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.15,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type=fecha_formato$FORMATO_TIPO_GRAFICA,tickformat = fecha_formato$FORMATO_GRAFICA,title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el ranking de los miembros por volumen operado (barras)
#'
#' Esta función crea la gráfica el ranking de los miembros por volumen operado en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("General", "Derivados OTC", "Divisas",
#' "Estandarizado", "C. Renta Variable", "Repos y TTV", "Simultaneas"). Por defecto NULL
#' @export

gt_vol_ranking_miembros <- function(datos,colores,fixedrange=FALSE,boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    datos <- datos %>% bind_rows(datos %>% mutate(PRODUCTO_ORIGEN="General"))

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("General","Derivados OTC","Divisas","Estandarizado","C. Renta Variable","Repos y TTV","Simultaneas"),
                        BOTON=c("General","Derivados OTC","Divisas","Estandarizado","C. Renta Variable","Repos y TTV","Simultaneas")) %>%
      filter(TIPO %in% unique(datos$PRODUCTO_ORIGEN))

    # Verificación inputs
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) {
      boton_activo <- tipos$BOTON[1]
    }

    # Se agrega el condicional visible al data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      rename(TIPO="PRODUCTO_ORIGEN",ID="MIEMBRO_TIPO")  %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO,ID) %>% summarise(VALOR=round((sum(EFECTIVO_COMPRA)+sum(EFECTIVO_VENTA))/1e+12,6),.groups="drop") %>%
      mutate(TEXTO=paste(VALOR,"Billones")) %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
      mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO),if_else(TIPO=="General",VALOR,0),.fun=sum,.desc=T),
             COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(factor(paste0(TIPO,"-",ID))),sep="-")) %>% arrange(COLOR_ID)

    # Se crea el data.frame n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(rep(visible,n_dist))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      mutate(TIPO="MIEMBRO_TIPO") %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = '>',value = 0)),
                    textposition = 'none',hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR,text=~TEXTO,visible=~VISIBLE,name=~ID,legendgroup=~COLOR_ID) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.15),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.1,pad = list('r'= 0, 't'= 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica el número de miembros operando en cada segmento por mes (barras)
#'
#' Esta función crea la gráfica el ranking de los miembros por volumen operado.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_miembros_operando<- function(datos,colores,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      mutate(SEGMENTO_NOMBRE=case_when(SEGMENTO_NOMBRE=="C. Renta Variable" ~ "Renta Variable",SEGMENTO_NOMBRE=="ND Divisas" ~ "C. Divisas",TRUE ~ SEGMENTO_NOMBRE)) %>%
      bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="General")) %>%
      group_by(FECHA_ANO_MES=format(FECHA,"%Y-%m"),TIPO="SEGMENTO_NOMBRE",ID=SEGMENTO_NOMBRE) %>%
      summarise(NUMERO_MIEMBROS=n_distinct(MIEMBRO_ID_SEUDONIMO),.groups = "drop")

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(ID) %>% pull(COLOR)

    # Se grafica los el numero de miembros operando por segmento y mes
    plot <- plot_ly(data= datos_completos ,x=~FECHA_ANO_MES,colors = colores,color=~ID) %>%
      add_bars(y=~NUMERO_MIEMBROS) %>%
      layout(legend = list(orientation = 'h',xanchor = "center",x = 0.5),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Número Miembros",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))
    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}
