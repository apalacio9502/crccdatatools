#' Tabla posición abierta resumen
#'
#' Esta función crea la tabla posición abierta en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_pa_resumen}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_pa_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  if (sum(datos$POSICION_COMPRADORA_VALORADA)==sum(datos$POSICION_VENDEDORA_VALORADA)) {
  # Manipulación de datos
  datos <- datos  %>% bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado",PRODUCTO_NOMBRE="Consolidado",PRODUCTO_TIPO="Consolidado")) %>%
    group_by(FECHA,SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
    summarise(POSICION_COMPRADORA_VALORADA=sum(POSICION_COMPRADORA_VALORADA,na.rm=TRUE),.groups="drop") %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
    group_by(SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
    summarise(POSICION_ABIERTA_DIARIA=sum(POSICION_COMPRADORA_VALORADA[FECHA==fecha_analisis]),
              POSICION_ABIERTA_PROMEDIO_MENSUAL=mean(POSICION_COMPRADORA_VALORADA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              POSICION_ABIERTA_PROMEDIO_PERIODO=mean(POSICION_COMPRADORA_VALORADA),.groups = "drop")  %>%
    arrange(desc(POSICION_ABIERTA_DIARIA)) %>%
    transmute(Segmento=SEGMENTO_NOMBRE,"Tipo Producto"=PRODUCTO_TIPO,
              "Producto"=PRODUCTO_NOMBRE,
              "%"=if_else(SEGMENTO_NOMBRE=="Consolidado",1,POSICION_ABIERTA_DIARIA/sum(POSICION_ABIERTA_DIARIA[SEGMENTO_NOMBRE!="Consolidado"])),
              "Posición Abierta Último Día M-COP"=POSICION_ABIERTA_DIARIA/1e+6,
              "Posición Abierta Promedio Diario Último Mes M-COP"=POSICION_ABIERTA_PROMEDIO_MENSUAL/1e+6,
              "Posición Abierta Promedio Diario Periodo M-COP"=POSICION_ABIERTA_PROMEDIO_PERIODO/1e+6)

  # Se crea la tabla
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatPercentage(4,digits = 2) %>% formatCurrency(c(5,6,7), '$',digits = 0)

  }else{
    # Manipulación de datos
    datos <- datos  %>% bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado",PRODUCTO_NOMBRE="Consolidado",PRODUCTO_TIPO="Consolidado")) %>%
      group_by(FECHA,SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
      summarise(POSICION_COMPRADORA_VALORADA=sum(POSICION_COMPRADORA_VALORADA,na.rm=TRUE),
                POSICION_VENDEDORA_VALORADA=sum(POSICION_VENDEDORA_VALORADA,na.rm=TRUE),.groups="drop") %>%
      mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
      group_by(SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
      summarise(POSICION_ABIERTA_COMPRADORA_DIARIA=sum(POSICION_COMPRADORA_VALORADA[FECHA==fecha_analisis]),
                POSICION_ABIERTA_VENDEDORA_DIARIA=sum(POSICION_VENDEDORA_VALORADA[FECHA==fecha_analisis]),
                POSICION_ABIERTA_COMPRADORA_PROMEDIO_MENSUAL=mean(POSICION_COMPRADORA_VALORADA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
                POSICION_ABIERTA_VENDEDORA_PROMEDIO_MENSUAL=mean(POSICION_VENDEDORA_VALORADA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
                POSICION_ABIERTA_COMPRADORA_PROMEDIO_PERIODO=mean(POSICION_COMPRADORA_VALORADA),
                POSICION_ABIERTA_VENDEDORA_PROMEDIO_PERIODO=mean(POSICION_VENDEDORA_VALORADA),.groups = "drop")  %>%
      arrange(desc(POSICION_ABIERTA_COMPRADORA_DIARIA)) %>%
      transmute(Segmento=SEGMENTO_NOMBRE,"Tipo Producto"=PRODUCTO_TIPO,
                "Producto"=PRODUCTO_NOMBRE,
                "Posición Abierta Compradora Último Día M-COP"=POSICION_ABIERTA_COMPRADORA_DIARIA/1e+6,
                "Posición Abierta Vendedora Último Día M-COP"=POSICION_ABIERTA_VENDEDORA_DIARIA/1e+6,
                "Posición Abierta Compradora Promedio Diario Último Mes M-COP"=POSICION_ABIERTA_COMPRADORA_PROMEDIO_MENSUAL/1e+6,
                "Posición Abierta Vendedora Promedio Diario Último Mes M-COP"=POSICION_ABIERTA_VENDEDORA_PROMEDIO_MENSUAL/1e+6,
                "Posición Abierta Compradora Promedio Diario Periodo M-COP"=POSICION_ABIERTA_COMPRADORA_PROMEDIO_PERIODO/1e+6,
                "Posición Abierta Vendedora Promedio Diario Periodo M-COP"=POSICION_ABIERTA_VENDEDORA_PROMEDIO_PERIODO/1e+6)

    # Se crea la tabla
    table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                       options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                      columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
      formatCurrency(c(4,5,6,7,8,9), '$',digits = 0)

  }

  return(table)
}

#' Gráfica la posición abierta (barras h)
#'
#' Esta función crea la gráfica del posición abierta (compradora y vendedora)
#' en formato de barras horizantales.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_pa_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", Tipo Producto", "Subtipo Producto", "Origen Producto",
#' "Tipo Miembro", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_pa<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(POSICION_COMPRADORA_VALORADA>0 | POSICION_VENDEDORA_VALORADA>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","MIEMBRO_TIPO","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Miembro","Tipo Cuenta Gar."))  %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(VALOR_1=POSICION_COMPRADORA_VALORADA,VALOR_2=POSICION_VENDEDORA_VALORADA) %>%
      select(c(tipos$TIPO,"VALOR_1","VALOR_2")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>% group_by(TIPO,ID) %>%
      summarise(across(VALOR_1:VALOR_2, ~round(sum(.x)/1e+12,6)),.groups="drop_last")%>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)),"P"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P"),
             VALOR_1=VALOR_1/sum(VALOR_1),
             VALOR_2=VALOR_2/sum(VALOR_2)) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

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
    plot <- plot_ly(data= datos_completos,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none',hoverinfo="text+name") %>%
      add_bars(y="COMPRADORA",x=~VALOR_1,text=~TEXTO_1,name=~ID,visible=~VISIBLE_1,legendgroup=~ID) %>%
      add_bars(y="VENDEDORA",x=~VALOR_2,text=~TEXTO_2,name=~ID,visible=~VISIBLE_2,legendgroup=~ID,showlegend=FALSE) %>%
      layout(barmode = 'stack',legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
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

#' Gráfica la posicón abierta por miembro (barras)
#'
#' Esta función crea la gráfica de la posicón abierta (compradora eje positivo y vendedora eje negativo) por miembro
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
#' en la gráfica ("Segmento", "Tipo Producto",
#' "Subtipo Producto", "Origen Producto", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_pa_por_miembro<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(POSICION_COMPRADORA_VALORADA>0 | POSICION_VENDEDORA_VALORADA>0)

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
      mutate(ORDENADOR=if_else(sum(POSICION_COMPRADORA_VALORADA)>sum(POSICION_VENDEDORA_VALORADA),sum(POSICION_COMPRADORA_VALORADA),-sum(POSICION_VENDEDORA_VALORADA))) %>% ungroup() %>%
      mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO), ORDENADOR,.fun=max,.desc=T),
             GENERAL="General", VALOR_1=POSICION_COMPRADORA_VALORADA,VALOR_2=-POSICION_VENDEDORA_VALORADA) %>%
      select(c("MIEMBRO_ID_SEUDONIMO",tipos$TIPO,"VALOR_1","VALOR_2")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO,ID) %>% summarise(across(VALOR_1:VALOR_2, ~round(sum(.x)/1e+12,6)),.groups="drop_last")%>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)),"P"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
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
               legendgroup=~ID,hoverinfo="text+x+name" ) %>%
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

#' Gráfica la posición abierta diaria (lines)
#'
#' Esta función crea la gráfica la posición abierta diaria (compradora eje positivo y vendedora eje negativo) en formato de lineas.
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
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto",
#' "Origen Producto", "Tipo Miembro", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_pa_diaria<- function(datos,colores,fixedrange=FALSE,dash_board=TRUE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","MIEMBRO_TIPO","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("General","Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Miembro","Tipo Cuenta Gar."),
                        AMBOS=c(dash_board==T,dash_board==T,dash_board==T,dash_board==T,dash_board==T,T,T)) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=as.logical(VISIBLE_1*AMBOS))

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(GENERAL="General",VALOR_1=POSICION_COMPRADORA_VALORADA,VALOR_2=-POSICION_VENDEDORA_VALORADA) %>%
      group_by(across(c("FECHA",tipos$TIPO)))  %>% summarise(across(VALOR_1:VALOR_2,.fns = sum),.groups = "drop") %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(TIPO,ID,FECHA) %>% summarise(across(VALOR_1:VALOR_2, ~round(sum(.x)/1e+12,6)),.groups="drop_last")%>%
      mutate(across(VALOR_1:VALOR_2,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2,"C")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>% arrange(COLOR_ID)

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
                fill = 'tonexty',stackgroup="1",legendgroup=~ID,hoverinfo="text+x+name",textposition = 'none') %>%
      add_lines(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE_2,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="2",legendgroup=~ID,hoverinfo="text+x+name",textposition = 'none',showlegend=FALSE) %>%
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

#' Gráfica la posición abierta promedio diario por (Mes o Año) (barras)
#'
#' Esta función crea la gráfica de la posición abierta promedio diario (compradora eje positivo y vendedora eje negativo) en formato de barras.
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
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto",
#' "Origen Producto", "Tipo Miembro", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_pa_promedio_diario<- function(datos,colores,fixedrange=FALSE,dash_board=TRUE,promedio="m",boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","MIEMBRO_TIPO","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("General","Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Miembro","Tipo Cuenta Gar."),
                        AMBOS=c(dash_board==T,dash_board==T,dash_board==T,dash_board==T,dash_board==T,T,T)) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) {
      boton_activo <- tipos$BOTON[1]
    }

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
      mutate(GENERAL="General",FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),VALOR_1=POSICION_COMPRADORA_VALORADA,VALOR_2=-POSICION_VENDEDORA_VALORADA) %>%
      group_by(across(c("FECHA","FECHA_FORMATO",tipos$TIPO)))  %>% summarise(across(VALOR_1:VALOR_2,.fns = sum),.groups = "drop") %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(TIPO,ID,FECHA_FORMATO,FECHA) %>% summarise(across(VALOR_1:VALOR_2, ~sum(.x)/1e+12),.groups="drop_last")%>%
      summarise(across(VALOR_1:VALOR_2, ~round(mean(.x),6)),.groups="drop_last")%>%
      mutate(across(VALOR_1:VALOR_2,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2,"C")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
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
      layout(barmode="relative",hovermode = "x",
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

#' Gráfica la posición abierta promedio diario por (Mes o Año) y tipo de cuenta (barras)
#'
#' Esta función crea la gráfica de la posición abierta promedio diario (compradora eje positivo y vendedora eje negativo)
#' por tipo de cuenta en formato de barras. La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_pa_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param promedio clase character. "m" si se desea promediar por mes y "y" si se desea promediar por año. Por defecto "m"
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("Posición Propia", "Posición Terceros", "Posición Cartera"). Por defecto NULL
#' @export

gt_pa_promedio_diario_tipocuenta<- function(datos,colores,fixedrange=FALSE,promedio="m",boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("PP","PT","PC"),
                        BOTON=c("Posición Propia","Posición Terceros","Posición Cartera"),
                        POSICION=c(1,2,3),
                        AMBOS=c(T,T,T)) %>% filter(TIPO %in% datos$CUENTA_GARANTIA_TIPO)

    # Verificación inputs
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) {
      boton_activo <- tipos$BOTON[1]
    }

    # Se agrega el condicional visible al data.frame tipos
    tipos <- tipos %>% mutate(VISIBLE_1=BOTON==boton_activo,VISIBLE_2=as.logical(VISIBLE_1*AMBOS))

    # Se define la granularidad del promedio
    if (promedio=="m"){
      fecha_formato <- list(FORMATO_DATOS="%Y-%m",FORMATO_TIPO_GRAFICA="date",FORMATO_GRAFICA="%b-%Y")
    }else{
      fecha_formato <- list(FORMATO_DATOS="%Y",FORMATO_TIPO_GRAFICA=NULL,FORMATO_GRAFICA=NULL)
    }

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% rename(TIPO="CUENTA_GARANTIA_TIPO",ID="PRODUCTO_SUBTIPO")  %>%
      mutate(FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),VALOR_1=POSICION_COMPRADORA_VALORADA,VALOR_2=-POSICION_VENDEDORA_VALORADA) %>%
      select(c("FECHA","FECHA_FORMATO","TIPO","ID","VALOR_1","VALOR_2")) %>%
      group_by(TIPO,ID,FECHA_FORMATO,FECHA)  %>% summarise(across(VALOR_1:VALOR_2, ~sum(.x)/1e+12),.groups="drop_last")%>%
      summarise(across(VALOR_1:VALOR_2, ~round(mean(.x),6)),.groups="drop_last") %>%
      mutate(across(VALOR_1:VALOR_2,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1,"C"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2),"C") %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1-VALOR_2,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO) %>% summarise(N=n_distinct(ID),.groups="drop") %>% spread(TIPO,N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = rep(foreach(i=1:nrow(tipos),.combine = append) %do% {
                                   rep(visible[i],n_dist[[tipos$TIPO[i]]][1])
                                 },2)))))
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
      layout(barmode="relative",hovermode = "x",
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
