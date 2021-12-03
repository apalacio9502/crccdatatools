#' Tabla ingresos resumen
#'
#' Esta función crea la tabla ingresos en formato html
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_ing_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <- datos  %>%
    bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado",PRODUCTO_NOMBRE="Consolidado",PRODUCTO_TIPO="Consolidado")) %>%
    group_by(FECHA,SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
    summarise(TARIFA=sum(TARIFA,na.rm=TRUE),.groups="drop") %>%
    mutate(FECHA_ANO_MES=format(FECHA, "%Y-%m"),.after="FECHA") %>%
    group_by(SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
    summarise(INGRESO_DIARIO=sum(TARIFA[FECHA==fecha_analisis]),
              INGRESO_DIARIO_PROMEDIO_MENSUAL=mean(TARIFA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              INGRESO_ACUMULADO_MENSUAL=sum(TARIFA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              INGRESO_DIARIO_PROMEDIO_PERIODO=mean(TARIFA),
              INGRESO_ACUMULADO_PERIODO=sum(TARIFA),.groups = "drop")  %>%
    arrange(desc(INGRESO_DIARIO)) %>%
    transmute(Segmento=SEGMENTO_NOMBRE,"Tipo Producto"=PRODUCTO_TIPO,
              "Producto"=PRODUCTO_NOMBRE,
              "%"=if_else(SEGMENTO_NOMBRE=="Consolidado",1,INGRESO_DIARIO/sum(INGRESO_DIARIO[SEGMENTO_NOMBRE!="Consolidado"])),
              "Ingresos Último Día"=INGRESO_DIARIO,
              "Ingresos Promedio Diario Último Mes"=INGRESO_DIARIO_PROMEDIO_MENSUAL,
              "Ingresos Acumulados Último Mes"=INGRESO_ACUMULADO_MENSUAL,
              "Ingresos Promedio Diario Periodo"=INGRESO_DIARIO_PROMEDIO_PERIODO,
              "Ingresos Acumulados Periodo"=INGRESO_ACUMULADO_PERIODO)

  # Se crea la tabla ingresos
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatPercentage(4,digits = 2) %>% formatCurrency(c(5,6,7,8,9), '$',digits = 0)

  return(table)
}

#' Grafica los ingresos (pie)
#'
#' Esta función crea la gráfica de los ingresos en formato de pie.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto", "Origen Producto",
#' "Tipo Cuenta Gar.", "Concepto Tarifa"). Por defecto c()
#' @export

gt_ing<- function(datos,colores,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(TARIFA>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO","TARIFA_CONCEPTO"),
                        BOTON=c("Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar.","Concepto Tarifa")) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON)boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(VALOR=TARIFA) %>%
      select(c(tipos$TIPO,"VALOR")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>% group_by(TIPO,ID) %>%
      summarise(across(VALOR, ~round(sum(.x)/1e+6,6)),.groups="drop_last")%>%
      mutate(TEXTO=paste(VALOR,"Millones /",dt_porcentaje_caracter(VALOR/sum(VALOR)),"P")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T)),sep="-"),
             TIPO=factor(TIPO,levels = tipos$TIPO)) %>% arrange(COLOR_ID)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(c(visible))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,split = ~TIPO,labels=~ID) %>%
      add_pie(values=~VALOR,text=~TEXTO,visible=~VISIBLE,textinfo='percent',hoverinfo="text",
              marker = list(colors =colores),domain = list(x = c(0, 1), y = c(0.1, 0.95))) %>%
      layout(legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             margin=list("l"=50,"r"=50),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones))) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica los ingresos por miembro  (barras)
#'
#' Esta función crea la gráfica de los ingresos por miembro en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto",
#' "Subtipo Producto", "Origen Producto", "Tipo Cuenta Gar.", "Concepto Tarifa"). Por defecto c()
#' @export

gt_ing_por_miembro<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(TARIFA>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO","TARIFA_CONCEPTO"),
                        BOTON=c("General","Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar.","Concepto Tarifa")) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO), TARIFA,.fun=sum,.desc=T),
             GENERAL="General", VALOR=TARIFA) %>%
      select(c("MIEMBRO_ID_SEUDONIMO",tipos$TIPO,"VALOR")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO,ID) %>% summarise(across(VALOR, ~round(sum(.x)/1e+6,6)),.groups="drop_last")%>%
      mutate(TEXTO=paste(VALOR,"Millones /",dt_porcentaje_caracter(VALOR/sum(VALOR)),"P")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(c(rep(visible,n_dist),
                                                        visible[1]))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
                        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR,text=~TEXTO,name=~ID,visible=~VISIBLE,
               legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR),line = list(color="black",dash = "dash"),
                name="Maximo",visible=tipos$VISIBLE[1],legendgroup="Maximo") %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica los ingresos promedio diario por miembro (barras)
#'
#' Esta función crea la gráfica de los ingresos promedio diario en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto",
#' "Origen Producto", "Tipo Cuenta Gar.", "Concepto Tarifa"). Por defecto c()
#' @export

gt_ing_promedio_diario_por_miembro<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO","TARIFA_CONCEPTO"),
                        BOTON=c("General","Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar.","Concepto Tarifa")) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      mutate(MIEMBRO_ID_SEUDONIMO=factor(MIEMBRO_ID_SEUDONIMO),GENERAL="General", VALOR=TARIFA) %>%
      select(c("FECHA","MIEMBRO_ID_SEUDONIMO",tipos$TIPO,"VALOR")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID",
                   names_transform = list(TIPO = factor),
                   values_transform = list(ID = factor)) %>%
      group_by(MIEMBRO_ID_SEUDONIMO,TIPO,ID,FECHA) %>% summarise(across(VALOR, ~round(sum(.x)/1e+6,6)),.groups="drop_last")%>%
      summarise(across(VALOR, ~round(mean(.x),6)),.groups="drop_last")%>%
      mutate(TEXTO=paste(VALOR,"Millones /",dt_porcentaje_caracter(VALOR/sum(VALOR)),"P")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
      mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(MIEMBRO_ID_SEUDONIMO,VALOR,.fun=sum,.desc=T),
             ID=as.character(ID),
             COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(c(rep(visible,n_dist),
                                                        visible[1]))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR,text=~TEXTO,name=~ID,visible=~VISIBLE,
               legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR),line = list(color="black",dash = "dash"),
                name="Maximo",visible=tipos$VISIBLE[1],legendgroup="Maximo") %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica los ingresos diarios (lines)
#'
#' Esta función crea la gráfica de los ingresos diarios en formato de lineas.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto",
#' "Origen Producto", "Tipo Cuenta Gar.", "Concepto Tarifa"). Por defecto c()
#' @export

gt_ing_diarios<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO","TARIFA_CONCEPTO"),
                        BOTON=c("General","Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar.","Concepto Tarifa")) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos  %>%
      mutate(GENERAL="General",VALOR=TARIFA) %>%
      group_by(across(c("FECHA",tipos$TIPO)))  %>% summarise(across(VALOR,.fns = sum),.groups = "drop") %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(TIPO,ID,FECHA) %>% summarise(across(VALOR, ~round(sum(.x)/1e+6,6)),.groups="drop_last")%>%
      mutate(across(VALOR,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA,TIPO) %>%
      mutate(TEXTO=paste(VALOR,"Millones /",dt_porcentaje_caracter(VALOR/sum(VALOR)), "P /",CAMBIO_VALOR,"C")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(c(rep(visible,n_dist),
                                                        visible[1]))))))
    }

    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos,x=~FECHA,
                    colors = colores,color=~COLOR_ID,alpha = 1,
                    textposition = 'none') %>%
      add_lines(y=~VALOR,text=~TEXTO,visible=~VISIBLE,name=~ID,line = list(color = 'transparent'),
                fill = 'tonexty',stackgroup="1",legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[1],name="Maximo",legendgroup="Maximo") %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica los ingresos promedio diario por (Mes o Año) (barras)
#'
#' Esta función crea la gráfica de los ingresos promedio diario en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param promedio clase character. "m" si se desea promediar por mes y "y" si se desea promediar por año. Por defecto "m"
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto", "Subtipo Producto",
#' "Origen Producto", "Tipo Cuenta Gar.", "Concepto Tarifa"). Por defecto c()
#' @export

gt_ing_promedio_diario<- function(datos,colores,fixedrange=FALSE,promedio="m",boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_SUBTIPO","PRODUCTO_ORIGEN","CUENTA_GARANTIA_TIPO","TARIFA_CONCEPTO"),
                        BOTON=c("General","Segmento","Tipo Producto","Subtipo Producto","Origen Producto","Tipo Cuenta Gar.","Concepto Tarifa")) %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE=BOTON==boton_activo)

    # Se define la granularidad del promedio
    if (promedio=="m"){
      fecha_formato <- list(FORMATO_DATOS="%Y-%m",FORMATO_TIPO_GRAFICA="date",FORMATO_GRAFICA="%b-%Y")
    }else{
      fecha_formato <- list(FORMATO_DATOS="%Y",FORMATO_TIPO_GRAFICA=NULL,FORMATO_GRAFICA=NULL)
    }

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      mutate(GENERAL="General",FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),VALOR=TARIFA) %>%
      group_by(across(c("FECHA","FECHA_FORMATO",tipos$TIPO)))  %>% summarise(across(VALOR,.fns = sum),.groups = "drop") %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(TIPO,ID,FECHA_FORMATO,FECHA) %>% summarise(across(VALOR, ~sum(.x)/1e+6),.groups="drop_last")%>%
      summarise(across(VALOR, ~round(mean(.x),6)),.groups="drop_last")%>%
      mutate(across(VALOR,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO=paste(VALOR,"Millones /",dt_porcentaje_caracter(VALOR/sum(VALOR)), "P /",CAMBIO_VALOR, "C")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)


    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(c(rep(visible,n_dist),
                                                        visible[1]))))))
    }


    # Se crea el vector colores
    colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA_FORMATO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR,text=~TEXTO,visible=~VISIBLE,
               name=~ID,legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_data(data=datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR),line = list(color="black",dash = "dash"),
                visible=tipos$VISIBLE[1],name="Maximo",legendgroup="Maximo") %>%
      layout(barmode="stack",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type=fecha_formato$FORMATO_TIPO_GRAFICA,tickformat = fecha_formato$FORMATO_GRAFICA,title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Grafica los ingresos promedio diario por (Mes o Año) y tipo de cuenta (barras)
#'
#' Esta función crea la gráfica de los ingresos promedio diario por tipo de cuenta en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_adm_gen_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param promedio clase character. "m" si se desea promediar por mes y "y" si se desea promediar por año. Por defecto "m"
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico ("Posición Propia", "Posición Terceros", "Posición Cartera"). Por defecto NULL
#' @export

gt_ing_promedio_diario_tipocuenta<- function(datos,colores,fixedrange=FALSE,promedio="m",boton_activo=NULL){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("PP","PT","PC"),
                        BOTON=c("Posición Propia","Posición Terceros","Posición Cartera"),
                        POSICION=c(1,2,3)) %>% filter(TIPO %in% unique(datos$CUENTA_GARANTIA_TIPO))

    # Verificación inputs
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se agrega el condicional visible al data.frame tipos
    tipos <- tipos %>% mutate(VISIBLE=BOTON==boton_activo)

    # Se define la granularidad del promedio
    if (promedio=="m"){
      fecha_formato <- list(FORMATO_DATOS="%Y-%m",FORMATO_TIPO_GRAFICA="date",FORMATO_GRAFICA="%b-%Y")
    }else{
      fecha_formato <- list(FORMATO_DATOS="%Y",FORMATO_TIPO_GRAFICA=NULL,FORMATO_GRAFICA=NULL)
    }

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% filter(CUENTA_GARANTIA_TIPO!="GE") %>%
      rename(TIPO="CUENTA_GARANTIA_TIPO",ID="PRODUCTO_SUBTIPO")  %>%
      mutate(FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),VALOR=TARIFA) %>%
      select(c("FECHA","FECHA_FORMATO","TIPO","ID","VALOR")) %>%
      group_by(TIPO,ID,FECHA_FORMATO,FECHA)  %>% summarise(across(VALOR, ~sum(.x)/1e+6),.groups="drop_last")%>%
      summarise(across(VALOR, ~round(mean(.x),6)),.groups="drop_last") %>%
      mutate(across(VALOR,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO=paste(VALOR,"Millones /",dt_porcentaje_caracter(VALOR/sum(VALOR)), "P /",CAMBIO_VALOR,"C")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
      mutate(COLOR_ID=paste(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)

    # Se crea el vector n_dist
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
                        mutate(ID=ID, TIPO="PRODUCTO_SUBTIPO") %>%
                        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)


    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA_FORMATO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR,text=~TEXTO,visible=~VISIBLE,
               name=~ID,legendgroup=~COLOR_ID,hoverinfo="text+x+name") %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis=list(type=fecha_formato$FORMATO_TIPO_GRAFICA,tickformat = fecha_formato$FORMATO_GRAFICA,title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Millones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Tabla cumplimiento presupuesto resumen
#'
#' Esta función crea la tabla cumlimplimiento presupuesto en formato html.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_cumplimiento_presupuesto}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param pageLength clase number. Número de filas por hoja que alojara
#' la tabla. Por defecto 100
#' @param style clase character. Estilo boostrap que se debe utilizar
#' para renderizar la tabla. Por defecto "bootstrap4"
#' @export

gt_ing_cumplimiento_presupuesto_resumen<- function(datos,fecha_analisis,pageLength=100,style="bootstrap4"){

  # Manipulación de datos
  datos <- datos %>%
    bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado",PRODUCTO_NOMBRE="Consolidado",PRODUCTO_TIPO="Consolidado")) %>%
    group_by(FECHA,FECHA_ANO_MES,SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
    summarise(PROYECCION=sum(PROYECCION_DIARIA,na.rm = TRUE),
              TARIFA=sum(TARIFA,na.rm=TRUE),.groups="drop") %>%
    group_by(SEGMENTO_NOMBRE,PRODUCTO_NOMBRE,PRODUCTO_TIPO) %>%
    summarise(CUMPLIMENTO_DIARIO=sum(TARIFA[FECHA==fecha_analisis])-sum(PROYECCION[FECHA==fecha_analisis]),
              CUMPLIMENTO_MENSUAL=sum(TARIFA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")])-sum(PROYECCION[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
              CUMPLIMENTO_PERIODO=sum(TARIFA)-sum(PROYECCION),
              .groups = "drop") %>%
    transmute(Segmento=SEGMENTO_NOMBRE,"Tipo Producto"=PRODUCTO_TIPO,
              "Producto"=PRODUCTO_NOMBRE,
              "Cumplimiento Último Día"=CUMPLIMENTO_DIARIO,
              "Cumplimiento Último Mes"=CUMPLIMENTO_MENSUAL,
              "Cumplimiento Periodo"=CUMPLIMENTO_PERIODO)

  if (nrow(datos)>0) {
    datos <- datos %>%
      mutate(Segmento=relevel(factor(Segmento),"Consolidado"),
             Producto=relevel(factor(Producto),"Consolidado"),
             `Tipo Producto`=relevel(factor(`Tipo Producto`),"Consolidado")) %>%
      arrange(Segmento)
  }

  # Se crea la tabla cumplimento del presupuesto
  table <- datatable(datos,rownames = FALSE,style=style,fillContainer=FALSE,extensions = 'Responsive',
                     options = list(searching = F,processing=T,language = gt_espanol,pageLength = pageLength, lengthChange = F,searching = F,
                                    columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    formatCurrency(c(4,5,6), '$',digits = 0) %>% formatStyle(c(4,5,6),color = styleInterval(-0, c('orange', 'white')))

  return(table)
}

#' Grafica el resumen del cumplimiento del presupuesto
#'
#' Esta función crea la gráfica resumen del cumplimiento del presupuesto para el utlimo mes
#' y periodo(fecha min a fecha max de los datos) (barras).
#' La información se muestra acorde la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_cumplimiento_presupuesto}} o tener una estructura igual a dichos datos
#' @param fecha_analisis clase date. Fecha en la que se realiza el análisis (Último día de los datos)
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Producto", "Origen Producto", "Producto"). Por defecto c()
#' @export

gt_ing_cumplimiento_presupuesto<- function(datos,fecha_analisis,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("SEGMENTO_NOMBRE","PRODUCTO_TIPO","PRODUCTO_ORIGEN","PRODUCTO_NOMBRE"),
                        BOTON=c("Segmento", "Tipo Producto", "Origen Producto", "Producto"))  %>%
      filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE=BOTON==boton_activo)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% filter(SEGMENTO_NOMBRE!="General") %>%
      bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="Consolidado",PRODUCTO_TIPO="Consolidado",
                                 PRODUCTO_NOMBRE="Consolidado",PRODUCTO_ORIGEN="Consolidado")) %>%
      select(c("FECHA","FECHA_ANO_MES",tipos$TIPO,"TARIFA","PROYECCION_DIARIA")) %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(FECHA,FECHA_ANO_MES,TIPO,ID) %>%
      summarise(PROYECCION_DIARIA=sum(PROYECCION_DIARIA,na.rm = TRUE),
                INGRESO_DIARIO=sum(TARIFA,na.rm=TRUE),.groups="drop") %>% group_by(TIPO,ID) %>%
      summarise(INGRESO_ULTIMO_DIA=sum(INGRESO_DIARIO[FECHA==fecha_analisis]),
                INGRESO_ULTIMO_MES=sum(INGRESO_DIARIO[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
                INGRESO_PERIODO=sum(INGRESO_DIARIO),
                PROYECCION_ULTIMO_DIA=sum(PROYECCION_DIARIA[FECHA==fecha_analisis]),
                PROYECCION_ULTIMO_MES=sum(PROYECCION_DIARIA[FECHA_ANO_MES==format(fecha_analisis,"%Y-%m")]),
                PROYECCION_PERIODO=sum(PROYECCION_DIARIA),.groups = "drop") %>%
      mutate(VALOR_1=INGRESO_ULTIMO_DIA/PROYECCION_ULTIMO_DIA,
             VALOR_2=INGRESO_ULTIMO_MES/PROYECCION_ULTIMO_MES,
             VALOR_3=INGRESO_PERIODO/PROYECCION_PERIODO,
             TEXTO_1=paste(dt_porcentaje_caracter(VALOR_1),"/",round((INGRESO_ULTIMO_DIA-PROYECCION_ULTIMO_DIA)/1e+6,6),"Millones"),
             TEXTO_2=paste(dt_porcentaje_caracter(VALOR_2),"/",round((INGRESO_ULTIMO_MES-PROYECCION_ULTIMO_MES)/1e+6,6),"Millones"),
             TEXTO_3=paste(dt_porcentaje_caracter(VALOR_3),"/",round((INGRESO_PERIODO-PROYECCION_PERIODO)/1e+6,6),"Millones")) %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE),by="TIPO") %>%
      mutate(ORDENADOR=paste(dt_num_char(POSICION),dt_num_char(relevel(factor(ID),"Consolidado")),sep="-")) %>%
      arrange(ORDENADOR) %>%
      replace_na(list(VALOR_1=0,VALOR_2=0,VALOR_3=0))

    # Se crea el vector n_dist
    n_dist<- datos_completos %>% group_by(TIPO,POSICION) %>%
      summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION) %>% pull(N)

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(rep(visible,3))))))
    }


    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos,split=~POSICION,x=~ORDENADOR,textposition = 'none',colors=c("#af8dc3","#66c2a5","#8da0cb")) %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE,hoverinfo="text+x",name="Último Día",color="1") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE,hoverinfo="text+x",name="Último Mes",color="2") %>%
      add_bars(y=~VALOR_3,text=~TEXTO_3,visible=~VISIBLE,hoverinfo="text+x",name="Periodo",color="3") %>%
      layout(legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis=list(title=NA,fixedrange=fixedrange,tickmode="array",tickvals=datos_completos$ORDENADOR,
                        ticktext =datos_completos$ID),
             yaxis = list(title = "Cumplimiento",tickformat = ".2%",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}


#' Gráfica el comportamiento de los ingresos promedio diario por mes, producto y miembro (treemap)
#'
#' Esta función crea la gráfica del comportamiento de los ingresos promedio diario por mes, producto y miembro en
#' formato treemap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @export

gt_ing_promedio_diario_por_mes_producto_miembro<- function(datos){

  # Se filtran los datos y se crea la columna FECHA_ANO_MES
  datos <- datos %>% filter(SEGMENTO_ID!="GE") %>%
    mutate(FECHA_ANO_MES=format(FECHA,"%Y-%m"))

  # Se verifica si existen datos
  if (nrow(datos)>0 & length(datos %>% distinct(FECHA_ANO_MES) %>% pull(FECHA_ANO_MES))>1) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%  mutate(VALOR=TARIFA) %>%
      group_by(MIEMBRO_ID_SEUDONIMO,PRODUCTO_SUBTIPO,FECHA_ANO_MES,FECHA)  %>%
      summarise(across(VALOR, ~sum(.x)/1e+6),.groups="drop_last") %>%
      summarise(across(VALOR, ~mean(.x)),.groups="drop_last")%>%
      mutate(VALOR_ANTERIOR=lag(VALOR),CAMBIO_VALOR=VALOR-VALOR_ANTERIOR) %>% ungroup() %>%
      filter(FECHA_ANO_MES!=min(FECHA_ANO_MES)) %>%
      group_by(FECHA_ANO_MES,PRODUCTO_SUBTIPO) %>%
      mutate(TOTAL_CAMBIO_VALOR=sum(CAMBIO_VALOR,na.rm=TRUE)) %>% ungroup() %>%
      mutate(GRUPO_CAMBIO=case_when(TOTAL_CAMBIO_VALOR>0~"Incremento",TOTAL_CAMBIO_VALOR<0~"Decrecimiento", TRUE~"Estable"),
             SUBGRUPO_CAMBIO=case_when(TOTAL_CAMBIO_VALOR>0 & TOTAL_CAMBIO_VALOR<=0.5~"0 a 0.5 Millones",
                                       TOTAL_CAMBIO_VALOR>0.5 & TOTAL_CAMBIO_VALOR<=1~"0.5 a 1 Millones",
                                       TOTAL_CAMBIO_VALOR>1 & TOTAL_CAMBIO_VALOR<=3~"1 a 3 Millones",
                                       TOTAL_CAMBIO_VALOR>3 & TOTAL_CAMBIO_VALOR<=5~"3 a 5 Millones",
                                       TOTAL_CAMBIO_VALOR>5 ~"> 5 Millones",
                                       TOTAL_CAMBIO_VALOR<0 & TOTAL_CAMBIO_VALOR>=(-0.5)~"0 a -0.5 Millones",
                                       TOTAL_CAMBIO_VALOR<(-0.5) & TOTAL_CAMBIO_VALOR>=(-1)~"-0.5 a -1 Millones",
                                       TOTAL_CAMBIO_VALOR<(-1) & TOTAL_CAMBIO_VALOR>=(-3)~"-1 a -3 Millones",
                                       TOTAL_CAMBIO_VALOR<(-3) & TOTAL_CAMBIO_VALOR>=(-5)~"-3 a -5 Millones",
                                       TOTAL_CAMBIO_VALOR<(-5) ~"< -5 Millones",TRUE~"0 Millones"))

    # Se modifica el data.frame datos_completos
    datos_completos <- datos_completos  %>% group_by(LABEL="Ingresos",PARENT="") %>%
      summarise(NIVEL=1,N=n_distinct(FECHA_ANO_MES,PRODUCTO_SUBTIPO),.groups="drop") %>%
      bind_rows(datos_completos %>% group_by(LABEL=FECHA_ANO_MES,PARENT="Ingresos") %>%
                  summarise(NIVEL=2,N=n_distinct(PRODUCTO_SUBTIPO),
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop")) %>%
      bind_rows(datos_completos %>% group_by(LABEL=paste(FECHA_ANO_MES,GRUPO_CAMBIO),PARENT=FECHA_ANO_MES) %>%
                  summarise(NIVEL=3,N=n_distinct(PRODUCTO_SUBTIPO),
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop")) %>%
      bind_rows(datos_completos %>% group_by(LABEL=paste(FECHA_ANO_MES,SUBGRUPO_CAMBIO),PARENT=paste(FECHA_ANO_MES,GRUPO_CAMBIO)) %>%
                  summarise(NIVEL=4,N=n_distinct(PRODUCTO_SUBTIPO),
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop")) %>%
      bind_rows(datos_completos %>% group_by(LABEL=paste(FECHA_ANO_MES,PRODUCTO_SUBTIPO),PARENT=paste(FECHA_ANO_MES,SUBGRUPO_CAMBIO)) %>%
                  summarise(NIVEL=5,N=1,
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop")) %>%
      bind_rows(datos_completos %>% group_by(LABEL=paste(FECHA_ANO_MES,MIEMBRO_ID_SEUDONIMO,PRODUCTO_SUBTIPO),PARENT=paste(FECHA_ANO_MES,PRODUCTO_SUBTIPO)) %>%
                  summarise(NIVEL=6,N=1,
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop") %>%
                  group_by(PARENT) %>% mutate(N=N/sum(N)) %>% ungroup()) %>%
      mutate(TEXTO=case_when(NIVEL<=1 ~ "",NIVEL<=4 ~paste0("Num Productos: ",N,"\n","Prom Diario: ",VALOR," Millones","\n","Variación: ",CAMBIO_VALOR," Millones / ",dt_porcentaje_caracter(CAMBIO_VALOR_P) ),
                             TRUE ~paste0("Prom Diario: ",VALOR," Millones","\n","Variación: ",CAMBIO_VALOR," Millones/ ",dt_porcentaje_caracter(CAMBIO_VALOR_P))),
             COLOR=case_when(NIVEL<=1 ~ "#FFFFFF",CAMBIO_VALOR>0 ~"#2ca25f",CAMBIO_VALOR<0 ~"#e34a33",CAMBIO_VALOR==0 ~"#bdbdbd"))

    # Se crea la gráfica
    plot <- plot_ly(data = datos_completos,type="treemap",labels=~LABEL,parents=~PARENT,values=~N,text=~TEXTO,
                    textinfo="text+label+name",branchvalues="total",hoverinfo="label+percent parent",
                    maxdepth=2,domain=list(column=0),
                    marker=list(colors=~COLOR)) %>%
      layout(margin=list(l=0, r=0, b=0, t=0)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}



#' Gráfica el comportamiento de los ingresos promedio diario por mes, miembro y producto (treemap)
#'
#' Esta función crea la gráfica del comportamiento de los ingresos promedio diario por mes, miembro y producto en
#' formato treemap
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_ing_resumen}} o tener una estructura igual a dichos datos
#' @export

gt_ing_promedio_diario_por_mes_miembro_producto<- function(datos){

  # Se filtran los datos y se crea la columna FECHA_ANO_MES
  datos <- datos %>% filter(SEGMENTO_ID!="GE") %>%
    mutate(FECHA_ANO_MES=format(FECHA,"%Y-%m"))

  # Se verifica si existen datos
  if (nrow(datos)>0 & length(datos %>% distinct(FECHA_ANO_MES) %>% pull(FECHA_ANO_MES))>1) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(VALOR=TARIFA) %>%
      group_by(MIEMBRO_ID_SEUDONIMO,PRODUCTO_SUBTIPO,FECHA_ANO_MES,FECHA)  %>%
      summarise(across(VALOR, ~sum(.x)/1e+6),.groups="drop_last") %>%
      summarise(across(VALOR, ~mean(.x)),.groups="drop_last")%>%
      mutate(VALOR_ANTERIOR=lag(VALOR),CAMBIO_VALOR=VALOR-VALOR_ANTERIOR) %>% ungroup() %>%
      filter(FECHA_ANO_MES!=min(FECHA_ANO_MES)) %>%
      group_by(FECHA_ANO_MES,MIEMBRO_ID_SEUDONIMO) %>%
      mutate(TOTAL_CAMBIO_VALOR=sum(CAMBIO_VALOR,na.rm=TRUE)) %>% ungroup() %>%
      mutate(GRUPO_CAMBIO=case_when(TOTAL_CAMBIO_VALOR>0~"Incremento",TOTAL_CAMBIO_VALOR<0~"Decrecimiento", TRUE~"Estable"),
             SUBGRUPO_CAMBIO=case_when(TOTAL_CAMBIO_VALOR>0 & TOTAL_CAMBIO_VALOR<=0.5~"0 a 0.5 Millones",
                                       TOTAL_CAMBIO_VALOR>0.5 & TOTAL_CAMBIO_VALOR<=1~"0.5 a 1 Millones",
                                       TOTAL_CAMBIO_VALOR>1 & TOTAL_CAMBIO_VALOR<=3~"1 a 3 Millones",
                                       TOTAL_CAMBIO_VALOR>3 & TOTAL_CAMBIO_VALOR<=5~"3 a 5 Millones",
                                       TOTAL_CAMBIO_VALOR>5 ~"> 5 Millones",
                                       TOTAL_CAMBIO_VALOR<0 & TOTAL_CAMBIO_VALOR>=(-0.5)~"0 a -0.5 Millones",
                                       TOTAL_CAMBIO_VALOR<(-0.5) & TOTAL_CAMBIO_VALOR>=(-1)~"-0.5 a -1 Millones",
                                       TOTAL_CAMBIO_VALOR<(-1) & TOTAL_CAMBIO_VALOR>=(-3)~"-1 a -3 Millones",
                                       TOTAL_CAMBIO_VALOR<(-3) & TOTAL_CAMBIO_VALOR>=(-5)~"-3 a -5 Millones",
                                       TOTAL_CAMBIO_VALOR<(-5) ~"< -5 Millones",TRUE~"0 Millones"))

    # Se modifica el data.frame datos_completos
    datos_completos <- datos_completos  %>% group_by(LABEL="Ingresos",PARENT="") %>%
      summarise(NIVEL=1,N=n_distinct(FECHA_ANO_MES,MIEMBRO_ID_SEUDONIMO),.groups="drop") %>%
      bind_rows(datos_completos %>% group_by(LABEL=FECHA_ANO_MES,PARENT="Ingresos") %>%
                  summarise(NIVEL=2,N=n_distinct(MIEMBRO_ID_SEUDONIMO),
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop")) %>%
      bind_rows(datos_completos %>% group_by(LABEL=paste(FECHA_ANO_MES,GRUPO_CAMBIO),PARENT=FECHA_ANO_MES) %>%
                  summarise(NIVEL=3,N=n_distinct(MIEMBRO_ID_SEUDONIMO),
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop")) %>%
      bind_rows(datos_completos %>% group_by(LABEL=paste(FECHA_ANO_MES,SUBGRUPO_CAMBIO),PARENT=paste(FECHA_ANO_MES,GRUPO_CAMBIO)) %>%
                  summarise(NIVEL=4,N=n_distinct(MIEMBRO_ID_SEUDONIMO),
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop")) %>%
      bind_rows(datos_completos %>% group_by(LABEL=paste(FECHA_ANO_MES,MIEMBRO_ID_SEUDONIMO),PARENT=paste(FECHA_ANO_MES,SUBGRUPO_CAMBIO)) %>%
                  summarise(NIVEL=5,N=1,
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop")) %>%
      bind_rows(datos_completos %>% group_by(LABEL=paste(FECHA_ANO_MES,MIEMBRO_ID_SEUDONIMO,PRODUCTO_SUBTIPO),PARENT=paste(FECHA_ANO_MES,MIEMBRO_ID_SEUDONIMO)) %>%
                  summarise(NIVEL=6,N=1,
                            VALOR=round(sum(VALOR),6),
                            VALOR_ANTERIOR=round(sum(VALOR),6),
                            CAMBIO_VALOR=round(sum(CAMBIO_VALOR),6),
                            CAMBIO_VALOR_P=CAMBIO_VALOR/VALOR_ANTERIOR,.groups="drop") %>%
                  group_by(PARENT) %>% mutate(N=N/sum(N)) %>% ungroup()) %>%
      mutate(TEXTO=case_when(NIVEL<=1 ~ "",NIVEL<=4 ~paste0("Num Miembros: ",N,"\n","Prom Diario: ",VALOR," Millones","\n","Variación: ",CAMBIO_VALOR," Millones / ",dt_porcentaje_caracter(CAMBIO_VALOR_P) ),
                             TRUE ~paste0("Prom Diario: ",VALOR," Millones","\n","Variación: ",CAMBIO_VALOR," Millones/ ",dt_porcentaje_caracter(CAMBIO_VALOR_P))),
             COLOR=case_when(NIVEL<=1 ~ "#FFFFFF",CAMBIO_VALOR>0 ~"#2ca25f",CAMBIO_VALOR<0 ~"#e34a33",CAMBIO_VALOR==0 ~"#bdbdbd"))

    # Se crea la gráfica
    plot <- plot_ly(data = datos_completos,type="treemap",labels=~LABEL,parents=~PARENT,values=~N,text=~TEXTO,
                    textinfo="text+label+name",branchvalues="total",hoverinfo="label+percent parent",
                    maxdepth=2,domain=list(column=0),
                    marker=list(colors=~COLOR)) %>%
      layout(margin=list(l=0, r=0, b=0, t=0)) %>%
      config(displaylogo = F,locale = "es",modeBarButtonsToAdd = list(gt_mbb_minimizar_pantalla,gt_mbb_maximizar_pantalla))

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}
