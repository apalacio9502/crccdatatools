#' Gráfica la garantía depositada vs. exigida (barras h)
#'
#' Esta función crea la gráfica de la garantía depositada vs. exigida
#' en formato de barras horizantales.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_exi_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica ("Segmento", "Tipo Miembro", "Tipo Cuenta Gar."). Por defecto c()
#' @export

gt_gar_dep_exi<- function(datos,colores,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(GARANTIA_DEPOSITADA>0 | GARANTIA_EXIGIDA>0 )

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("SEGMENTO_NOMBRE","MIEMBRO_TIPO","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("Segmento","Tipo Miembro","Tipo Cuenta Gar.")) %>% filter(!BOTON %in% botones_inactivos)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON)boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% mutate(VALOR_1=GARANTIA_DEPOSITADA,VALOR_2=GARANTIA_EXIGIDA) %>%
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
    plot <- plot_ly(data= datos_completos%>% arrange(COLOR_ID),colors = colores,color=~COLOR_ID,
                    textposition = 'none') %>%
      add_bars(y="Depositadas",x=~VALOR_1,text=~TEXTO_1,name=~ID,visible=~VISIBLE_1,
               legendgroup=~ID,hoverinfo="text+name") %>%
      add_bars(y="Exigidas",x=~VALOR_2,text=~TEXTO_2,name=~ID,visible=~VISIBLE_2,
               legendgroup=~ID,hoverinfo="text+name",showlegend=FALSE) %>%
      layout(barmode = 'stack',legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             yaxis = list(title = "",tickangle = 270,categoryarray = c("Exigidas","Depositadas"),fixedrange=fixedrange,showgrid = FALSE,showline = FALSE,zeroline = FALSE),
             xaxis=list(title="",tickformat = "%",fixedrange=fixedrange,showgrid = FALSE,showline = FALSE,zeroline = FALSE)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la garantía depositada vs. exigida por miembro (barras)
#'
#' Esta función crea la gráfica de la garantía depositada (eje y1) vs. exigida (eje y2) por miembro
#' en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_exi_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param dos_ejes clase boolean. TRUE si se desea que la garantia depositada y exigida se muestren en ejes distintos. Por defecto TRUE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica. Si 'dos_ejes' es igual a true los botones son ("Segmento","Tipo Cuenta Gar."). En caso
#' contrario los botones son (General,D-sss--s-s-s-s-s-s--s). Por defecto c()
#' @export

gt_gar_dep_exi_por_miembro<- function(datos,colores,fixedrange=FALSE,dos_ejes=TRUE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos %>% filter(GARANTIA_DEPOSITADA>0 | GARANTIA_EXIGIDA>0 )

  # Se verifica si existen datos
  if (nrow(datos)>0) {
    # Se verifica el tipo de la grafica
    if (dos_ejes==TRUE) {

      # Se crea el data.frame tipos
      tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","CUENTA_GARANTIA_TIPO"),
                          BOTON=c("General","Segmento","Tipo Cuenta Gar.")) %>% filter(!BOTON %in% botones_inactivos)

      # Verificación boton_activo
      if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

      # Se modifica el data.frame tipos
      tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

      # Se crea el data.frame datos_completos
      datos_completos <- datos %>%
        mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO), GARANTIA_DEPOSITADA,.fun=sum,.desc=T,na.rm = TRUE),
               GENERAL="General", VALOR_1=GARANTIA_DEPOSITADA,VALOR_2=GARANTIA_EXIGIDA) %>%
        select(c("MIEMBRO_ID_SEUDONIMO",tipos$TIPO,"VALOR_1","VALOR_2")) %>%
        pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
        group_by(MIEMBRO_ID_SEUDONIMO,TIPO,ID) %>% summarise(across(VALOR_1:VALOR_2, ~round(sum(.x)/1e+12,6)),VALOR_3=VALOR_1/VALOR_2,.groups="drop_last")%>%
        mutate(TEXTO_1=paste(VALOR_1,"Billones","/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "C /",dt_porcentaje_caracter(VALOR_3), "RC"),
               TEXTO_2=paste(VALOR_2,"Billones","/",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)), "C")) %>% ungroup() %>%
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
                                   visible = as.logical(rep(c(rep(visible,n_dist),visible[1]),2))))))
      }

      # Se crea el vector colores
      colores <- datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)


      # Se crea la gráfica
      plot <- plot_ly(data= datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,colors = colores,color=~COLOR_ID,
                      textposition = 'none') %>%
        add_bars(y=~VALOR_1,text=~TEXTO_1,name=~ID,visible=~VISIBLE_1,legendgroup=~ID,
                 hoverinfo="text+x+name") %>%
        add_bars(y=~VALOR_2,text=~TEXTO_2,name=~ID,visible=~VISIBLE_2,yaxis="y2",legendgroup=~ID,
                 showlegend=FALSE,hoverinfo="text+x+name") %>%
        add_data(data= datos_completos %>% filter(TIPO=="GENERAL")) %>%
        add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),
                  name="Máximo",visible=tipos$VISIBLE_1[1],legendgroup="Máximo") %>%
        add_lines(y=~max(VALOR_2),line = list(color="black",dash = "dash"),
                  name="Máximo",visible=tipos$VISIBLE_2[1],yaxis="y2",legendgroup="Máximo",showlegend=FALSE) %>%
        subplot(nrows = 2, shareX = TRUE) %>%
        layout(barmode="relative",hovermode = 'x',
               legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
               updatemenus=list(
                 list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                      yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
               xaxis = list(title = NA,fixedrange=fixedrange),
               yaxis = list(title = "B-COP (Dep.)",fixedrange=fixedrange),
               yaxis2 = list(title = "B-COP (Exi.)",fixedrange=fixedrange)) %>%
        config(displaylogo = F,locale = "es")

      return(plot)
    }else{

      # Se convierte el SEGMENTO_NOMBRE en una factor
      datos <- datos %>% filter(SEGMENTO_NOMBRE!="General") %>%
        bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="General")) %>%
        filter(!SEGMENTO_NOMBRE %in% botones_inactivos) %>%
        mutate(SEGMENTO_NOMBRE=relevel(factor(SEGMENTO_NOMBRE),"General")) %>%
        arrange(SEGMENTO_NOMBRE)

      # Se crea el vector segmetos
      segmentos <- levels(datos$SEGMENTO_NOMBRE)

      # Verificación inputs
      if (is.null(boton_activo) || !boton_activo %in% segmentos) {
        boton_activo <- segmentos[1]
      }

      # Se crea el data.frame datos_completos
      datos_completos<- datos %>% group_by(MIEMBRO_ID_SEUDONIMO,SEGMENTO_NOMBRE) %>%
        summarise(VALOR_1=round(sum(GARANTIA_DEPOSITADA,na.rm = TRUE)/1e+9,6),
                  VALOR_2=round(sum(GARANTIA_EXIGIDA,na.rm = TRUE)/1e+9,6),
                  TEXTO_1=paste(VALOR_1,"Miles M /",dt_porcentaje_caracter(VALOR_1/VALOR_2)),
                  TEXTO_2=paste(VALOR_2,"Miles M"),.groups = "drop")%>%
        mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO),VALOR_1,.fun=max,.desc=T),
               VISIBLE=if_else(SEGMENTO_NOMBRE==boton_activo,TRUE,FALSE)) %>% arrange(SEGMENTO_NOMBRE)

      # Se verifica si se debe crear el updatemenus
      if (length(segmentos)>1) {
        # Se crean los botones
        botones <- foreach(i=1:length(segmentos),.combine = append) %do% {
          visible <- segmentos[i]==segmentos
          list(list(label = segmentos[i],method = "restyle",
                    args = list(list(boton_activo=segmentos[i],
                                     visible = as.logical(rep(visible,2))))))
        }

        # Se crea el updatemenus
        updatemenus <- list(
          list(active = which(segmentos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
               yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones))
      }else{
        # Se crea el updatemenus
        updatemenus <- NULL
      }

      # Se crea la gráfica
      plot <- plot_ly(data=datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,split=~as.numeric(SEGMENTO_NOMBRE),
                      hoverinfo="text+x+name",textposition = 'none') %>%
        add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE,name="Garantía Depositada") %>%
        add_bars(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE,name="Garantía Exigida") %>%
        layout(hovermode = 'x',
               legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2),
               updatemenus=updatemenus,
               xaxis = list(title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange)) %>%
        config(displaylogo = F,locale = "es")

      return(plot)
    }



  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la garantía depositada vs. exigida diaria (lineas)
#'
#' Esta función crea la gráfica de la garantía depositada (eje y1) vs. exigida (eje y2) diaria  en formato de lineas.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_exi_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param dos_ejes clase boolean. TRUE si se desea que la garantia depositada y exigida se muestren en ejes distintos. Por defecto TRUE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica. Si 'dos_ejes' es igual a true los botones son ("Segmento", "Tipo Miembro", "Tipo Cuenta Gar."). En caso
#' contrario los botones son (General,D-sss--s-s-s-s-s-s--s). Por defecto c()
#' @export

gt_gar_dep_exi_diaria<- function(datos,colores,fixedrange=FALSE,dos_ejes=TRUE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {
    # Se verifica el tipo de la grafica
    if (dos_ejes==1) {

      # Se crea el data.frame tipos
      tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","MIEMBRO_TIPO","CUENTA_GARANTIA_TIPO"),
                          BOTON=c("General","Segmento","Tipo Miembro","Tipo Cuenta Gar.")) %>%
        filter(!BOTON %in% botones_inactivos)

      # Verificación boton_activo
      if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

      # Se modifica el data.frame tipos
      tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

      # Se crea el data.frame datos_completos
      datos_completos <- datos %>% mutate(GENERAL="General",VALOR_1=GARANTIA_DEPOSITADA,VALOR_2=GARANTIA_EXIGIDA) %>%
        group_by(across(c("FECHA",tipos$TIPO)))  %>% summarise(across(VALOR_1:VALOR_2,.fns = sum),.groups = "drop") %>%
        pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
        group_by(TIPO,ID,FECHA) %>% summarise(across(VALOR_1:VALOR_2, ~round(sum(.x)/1e+12,6)),VALOR_3=VALOR_1/VALOR_2,.groups="drop_last")%>%
        mutate(across(VALOR_1:VALOR_2,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA,TIPO) %>%
        mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1, "C /",dt_porcentaje_caracter(VALOR_3), "RC"),
               TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)),"P /",CAMBIO_VALOR_2, "C")) %>% ungroup() %>%
        left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
        mutate(COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
        arrange(COLOR_ID)

      # Se crea el vector n_dist
      n_dist<- (datos_completos %>% group_by(TIPO,POSICION) %>% summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION))$N

      # Se crean los botones
      botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
        visible <- tipos$BOTON[i]==tipos$BOTON
        list(list(label = tipos$BOTON[i],method = "restyle",
                  args = list(list(boton_activo=tipos$BOTON[i],
                                   visible = as.logical(rep(c(rep(visible,n_dist),visible[1]),2))))))
      }

      # Se crea el vector colores
      colores <- (datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
                          left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID))$COLOR

      # Se crea la gráfica
      plot <- plot_ly(data= datos_completos ,x=~FECHA,colors = colores,color=~COLOR_ID,alpha=1,
                      textposition = 'none') %>%
        add_lines(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE_1,name=~ID,line = list(color = 'transparent'),
                  fill = 'tonexty',stackgroup="1",legendgroup=~ID,hoverinfo="text+x+name") %>%
        add_lines(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE_2,name=~ID,line = list(color = 'transparent'),
                  fill = 'tonexty',stackgroup="2",legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
        add_data(data= datos_completos %>% filter(TIPO=="GENERAL")) %>%
        add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),name="Máximo",visible=tipos$VISIBLE_1[1],legendgroup="Máximo") %>%
        add_lines(y=~max(VALOR_2),line = list(color="black",dash = "dash"),name="Máximo",visible=tipos$VISIBLE_2[1],yaxis="y2",legendgroup="Máximo",showlegend=FALSE) %>%
        subplot(nrows = 2,shareX = TRUE) %>%
        layout(hovermode = 'x',
               legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
               updatemenus=list(
                 list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                      yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
               xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
               yaxis = list(title = "B-COP (Dep.)",fixedrange=fixedrange),
               yaxis2 = list(title = "B-COP (Exi.)",fixedrange=fixedrange)) %>%
        config(displaylogo = F,locale = "es")

      return(plot)
    }else{

      # Se convierte el SEGMENTO_NOMBRE en una factor
      datos <- datos %>% filter(SEGMENTO_NOMBRE!="General") %>%
        bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="General")) %>%
        filter(!SEGMENTO_NOMBRE %in% botones_inactivos) %>%
        mutate(SEGMENTO_NOMBRE=relevel(factor(SEGMENTO_NOMBRE),"General")) %>%
        arrange(SEGMENTO_NOMBRE)

      # Se crea el vector segmetos
      segmentos <- levels(datos$SEGMENTO_NOMBRE)

      # Verificación inputs
      if (is.null(boton_activo) || !boton_activo %in% segmentos) boton_activo <- segmentos[1]

      # Se crea el data.frame datos_completos
      datos_completos<- datos %>% group_by(FECHA,SEGMENTO_NOMBRE) %>%
        summarise(VALOR_1=round(sum(GARANTIA_DEPOSITADA,na.rm = TRUE)/1e+12,6),
                  VALOR_2=round(sum(GARANTIA_EXIGIDA,na.rm = TRUE)/1e+12,6),
                  TEXTO_1=paste(VALOR_1,"Billones","/",dt_porcentaje_caracter(VALOR_1/VALOR_2)),
                  TEXTO_2=paste(VALOR_2,"Billones"),.groups = "drop")%>%
        mutate(VISIBLE=if_else(SEGMENTO_NOMBRE==boton_activo,TRUE,FALSE)) %>% arrange(SEGMENTO_NOMBRE)

      # Se verifica si se debe crear el updatemenus
      if (length(segmentos)>1) {
        # Se crean los botones
        botones <- foreach(i=1:length(segmentos),.combine = append) %do% {
          visible <- segmentos[i]==segmentos
          list(list(label = segmentos[i],method = "restyle",
                    args = list(list(boton_activo=segmentos[i],
                                     visible = as.logical(rep(visible,2))))))
        }

        # Se crea el updatemenus
        updatemenus <- list(
          list(active = which(segmentos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
               yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones))
      }else{
        # Se crea el updatemenus
        updatemenus <- NULL
      }

      # Se crea la gráfica
      plot <- plot_ly(data=datos_completos ,x=~FECHA,split=~as.numeric(SEGMENTO_NOMBRE),
                      hoverinfo="text+x+name") %>%
        add_lines(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE,name="Garantía Depositada") %>%
        add_lines(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE,name="Garantía Exigida") %>%
        layout(hovermode = 'x',
               legend = list(orientation = 'h',xanchor = "center",x = 0.5),
               updatemenus=updatemenus,
               xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
               yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
        config(displaylogo = F,locale = "es")

      return(plot)


    }

  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la garantía depositada vs. exigida promedio diario (barras)
#'
#' Esta función crea la gráfica de la garantía depositada (eje y1) vs. exigida (eje y2) promedio diario en formato de barras
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_exi_resumen}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param promedio clase character. "m" si se desea promediar por mes y "y" si se desea promediar por año. Por defecto "m"
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica. Si 'dos_ejes' es igual a true los botones son ("Segmento", "Tipo Miembro", "Tipo Cuenta Gar."). En caso
#' contrario los botones son (General,D-sss--s-s-s-s-s-s--s). Por defecto c()
#' @export

gt_gar_dep_exi_promedio_diario<- function(datos,colores,fixedrange=FALSE,promedio="m",boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame tipos
    tipos <- data.frame(TIPO=c("GENERAL","SEGMENTO_NOMBRE","MIEMBRO_TIPO","CUENTA_GARANTIA_TIPO"),
                        BOTON=c("General","Segmento","Tipo Miembro","Tipo Cuenta Gar.")) %>%
      filter(!BOTON %in% botones_inactivos)
    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% tipos$BOTON) boton_activo <- tipos$BOTON[1]

    # Se modifica el data.frame tipos
    tipos <- tipos %>% mutate(POSICION=row_number(),VISIBLE_1=BOTON==boton_activo,VISIBLE_2=VISIBLE_1)

    # Se define la granularidad del promedio
    if (promedio=="m"){
      fecha_formato <- list(FORMATO_DATOS="%Y-%m",FORMATO_TIPO_GRAFICA="date",FORMATO_GRAFICA="%b-%Y")
    }else{
      fecha_formato <- list(FORMATO_DATOS="%Y",FORMATO_TIPO_GRAFICA=NULL,FORMATO_GRAFICA=NULL)
    }

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>%
      mutate(GENERAL="General",FECHA_FORMATO=format(FECHA,fecha_formato$FORMATO_DATOS),VALOR_1=GARANTIA_DEPOSITADA,VALOR_2=GARANTIA_EXIGIDA) %>%
      group_by(across(c("FECHA","FECHA_FORMATO",tipos$TIPO)))  %>% summarise(across(VALOR_1:VALOR_2,.fns = sum),.groups = "drop") %>%
      pivot_longer(tipos$TIPO,names_to ="TIPO",values_to = "ID") %>%
      group_by(TIPO,ID,FECHA_FORMATO,FECHA) %>% summarise(across(VALOR_1:VALOR_2, ~sum(.x)/1e+12),.groups="drop_last")%>%
      summarise(across(VALOR_1:VALOR_2, ~round(mean(.x),6)),VALOR_3=VALOR_1/VALOR_2,.groups="drop_last")%>%
      mutate(across(VALOR_1:VALOR_2,~ dt_porcentaje_variacion(.x),.names="CAMBIO_{.col}"))%>% group_by(FECHA_FORMATO,TIPO) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones /",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1)), "P /",CAMBIO_VALOR_1, "C /",dt_porcentaje_caracter(VALOR_3), "RC"),
             TEXTO_2=paste(VALOR_2,"Billones /",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2)), "P /",CAMBIO_VALOR_2, "C")) %>% ungroup() %>%
      left_join(tipos %>% select(TIPO,POSICION,VISIBLE_1,VISIBLE_2),by="TIPO") %>%
      mutate(COLOR_ID=paste0(dt_num_char(POSICION),dt_num_char(fct_reorder(factor(paste0(TIPO,"-",ID)),VALOR_1,.fun=mean,.desc=T)),sep="-")) %>%
      arrange(COLOR_ID)


    # Se crea el vector n_dist
    n_dist<- (datos_completos %>% group_by(TIPO,POSICION) %>% summarise(N=n_distinct(ID),.groups="drop") %>% arrange(POSICION))$N

    # Se crean los botones
    botones <- foreach(i=1:nrow(tipos),.combine = append) %do% {
      visible <- tipos$BOTON[i]==tipos$BOTON
      list(list(label = tipos$BOTON[i],method = "restyle",
                args = list(list(boton_activo=tipos$BOTON[i],
                                 visible = as.logical(rep(c(rep(visible,n_dist),visible[1]),2))))))
    }

    # Se crea el vector colores
    colores <- (datos_completos %>% distinct(TIPO,ID,COLOR_ID) %>%
                        left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID))$COLOR

    # Se crea la gráfica
    plot <- plot_ly(data= datos_completos ,x=~FECHA_FORMATO,colors = colores,color=~COLOR_ID,
                    transforms = list(list(type = 'filter',target = 'y',operation = ')(',value = 0)),
                    textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE_1,
               name=~ID,legendgroup=~ID,hoverinfo="text+x+name") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE_2,
               name=~ID,legendgroup=~ID,showlegend=FALSE,hoverinfo="text+x+name",yaxis="y2") %>%
      add_data(data= datos_completos %>% filter(TIPO=="GENERAL")) %>%
      add_lines(y=~max(VALOR_1),line = list(color="black",dash = "dash"),name="Máximo",visible=tipos$VISIBLE_1[1],legendgroup="Máximo") %>%
      add_lines(y=~max(VALOR_2),line = list(color="black",dash = "dash"),name="Máximo",visible=tipos$VISIBLE_2[1],yaxis="y2",legendgroup="Máximo",showlegend=FALSE) %>%
      subplot(nrows = 2,shareX = TRUE) %>%
      layout(barmode="relative",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,tracegroupgap=0),
             updatemenus=list(
               list(active = which(tipos$BOTON == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
                    yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones)),
             xaxis = list(type=fecha_formato$FORMATO_TIPO_GRAFICA,tickformat = fecha_formato$FORMATO_GRAFICA,title = NA,fixedrange=fixedrange),
             yaxis = list(title = "B-COP (Dep.)",fixedrange=fixedrange),
             yaxis2 = list(title = "B-COP (Exi.)",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)
  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la garantía depositada (GGL) vs. exigida (individual + FGC) por miembro liquidador (barras+puntos)
#'
#' Esta función crea la gráfica de la garantía depositada (GGL) vs. exigida (individual + FGC) por miembro liquidador
#' en formato de barras y puntos.
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_ggl_ind_fgc_liq}} o tener una estructura igual a dichos datos
#' @param colores clase data.frame. Debe contener los datos generados
#' por la función \code{\link{dt_colores}}
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_gar_ggl_ind_fgc_por_miembro_liq<- function(datos,colores,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_garantias_exigidas
    datos_garantias_exigidas <- datos %>% filter(GARANTIA_TIPO!="GGL")  %>%
      group_by(MIEMBRO_LIQ_ID_SEUDONIMO) %>%
      mutate(VALUE=round(IMPORTE/1e+12,6),
             TEXT=paste(VALUE,"Billones /",dt_porcentaje_caracter(VALUE/sum(VALUE)),"P")) %>% ungroup() %>%
      mutate(MIEMBRO_LIQ_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_LIQ_ID_SEUDONIMO), VALUE,.fun=sum,.desc=T),
             ID=fct_reorder(factor(SEGMENTO_NOMBRE),VALUE,.fun=mean,.desc=T),
             COLOR_ID=paste0(as.numeric(ID)),TIPO="SEGMENTO_NOMBRE",
             NAME=if_else(GARANTIA_TIPO=="MF","Garantías IND",paste("FGC",SEGMENTO_NOMBRE)))

    # Se crea el data.frame datos_garantias_exigidas
    datos_garantias_depositadas <- datos %>% filter(GARANTIA_TIPO=="GGL") %>%
      mutate(MIEMBRO_LIQ_ID_SEUDONIMO=factor(MIEMBRO_LIQ_ID_SEUDONIMO,levels(datos_garantias_exigidas$MIEMBRO_LIQ_ID_SEUDONIMO)),
             VALUE=round(IMPORTE/1e+12,6),
             TEXT=paste(VALUE,"Billones"),
             NAME=GARANTIA_TIPO)

    # Se crea el vector colores
    colores <- datos_garantias_exigidas %>% distinct(TIPO,ID,COLOR_ID) %>%
      left_join(colores,by = c("TIPO", "ID")) %>% arrange(COLOR_ID) %>% pull(COLOR)

    # Se crea la gráfica
    plot <- plot_ly(data=datos_garantias_exigidas,colors = colores,x=~MIEMBRO_LIQ_ID_SEUDONIMO,textposition = 'none') %>%
      add_bars(y=~VALUE,color=~COLOR_ID,text=~TEXT,hoverinfo="text+x+name",name=~NAME) %>%
      add_data(data=datos_garantias_depositadas) %>%
      add_markers(y=~VALUE,text=~TEXT,hoverinfo="text+x+name",name=~NAME,marker = list(color = "black")) %>%
      layout(barmode="stack",hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Billones-COP",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la posición abierta bruta (dos puntas) vs garantía exigida por miembro (barras)
#'
#' Esta función crea la gráfica de la posición abierta bruta (eje y1) vs. garantía exigida (eje y2)
#' y el ratio de cobertura (eje y3) por miembro en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_exi_resumen}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica (....). Por defecto c()
#' @export

gt_pa_bruta_gar_exi_por_miembro <- function(datos,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se filtran los datos
  datos <- datos  %>% filter(GARANTIA_DEPOSITADA>0 | GARANTIA_EXIGIDA>0 | POSICION_BRUTA_VALORADA>0)

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se filtran los datos
    datos <- datos %>% filter(SEGMENTO_NOMBRE!="General")

    # Se convierte el SEGMENTO_NOMBRE en una factor
    datos <- datos %>%
      bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="General")) %>%
      filter(!SEGMENTO_NOMBRE %in% botones_inactivos) %>%
      mutate(SEGMENTO_NOMBRE=relevel(factor(SEGMENTO_NOMBRE),"General")) %>%
      arrange(SEGMENTO_NOMBRE)

    # Se crea el vector segmetos
    segmentos <- levels(datos$SEGMENTO_NOMBRE)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% segmentos) boton_activo <- segmentos[1]

    # Se crea el data.frame datos_completos
    datos_completos<- datos %>% group_by(SEGMENTO_NOMBRE,MIEMBRO_ID_SEUDONIMO) %>%
      summarise(VALOR_1=round(sum(POSICION_BRUTA_VALORADA)/1e+12,6),
                VALOR_2=round(sum(GARANTIA_EXIGIDA)/1e+12,6),
                VALOR_3=VALOR_2/VALOR_1,.groups = "drop_last")%>%
      replace_na(list(VALOR_3=0)) %>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones","/",dt_porcentaje_caracter(VALOR_1/sum(VALOR_1))),
             TEXTO_2=paste(VALOR_2,"Billones","/",dt_porcentaje_caracter(VALOR_2/sum(VALOR_2))),
             TEXTO_3=dt_porcentaje_caracter(VALOR_3)) %>% ungroup() %>%
      mutate(MIEMBRO_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_ID_SEUDONIMO),VALOR_1,.fun=max,.desc=T),
             VISIBLE=if_else(SEGMENTO_NOMBRE==boton_activo,TRUE,FALSE),) %>% arrange(SEGMENTO_NOMBRE)

    # Se verifica si se debe crear el updatemenus
    if (length(segmentos)>1) {
      # Se crean los botones
      botones <- foreach(i=1:length(segmentos),.combine = append) %do% {
        visible <- segmentos[i]==segmentos
        list(list(label = segmentos[i],method = "restyle",
                  args = list(list(boton_activo=segmentos[i],
                                   visible = as.logical(rep(visible,3))))))
      }

      # Se crea el updatemenus
      updatemenus <- list(
        list(active = which(segmentos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
             yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones))
    }else{
      # Se crea el updatemenus
      updatemenus <- NULL
    }

    # Se crea la gráfica
    plot <- plot_ly(data=datos_completos ,x=~MIEMBRO_ID_SEUDONIMO,split=~as.numeric(SEGMENTO_NOMBRE),
                    hoverinfo="text+x+name",textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE,name="Posición Dos Puntas",yaxis="y1",color="1") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE,name="Garantía Exigida",yaxis="y2",color="2") %>%
      add_bars(y=~VALOR_3,text=~TEXTO_3,visible=~VISIBLE,name="Ratio Cobertura",yaxis="y3",color="3") %>%
      subplot(nrows = 3, shareX = TRUE) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2,tracegroupgap=0),
             updatemenus=updatemenus,
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title="B-COP",fixedrange=fixedrange),
             yaxis2 = list(title="B-COP",fixedrange=fixedrange),
             yaxis3 = list(title="Porcentaje",tickformat="%",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la posición abierta vs garantía exigida vs riesgo en situación de estrés por miembro liquidador (barras)
#'
#' Esta función crea la gráfica de la posición abierta (eje y1) vs. garantía exigida (eje y2) vs.
#' riesgo en situación de estrés (eje y3) por miembro liquidador en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_dep_exi_liq}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica (....). Por defecto c()
#' @export

gt_pa_gar_exi_rss_por_miembro_liq <- function(datos,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se filtran los datos
    datos <- datos %>% filter(SEGMENTO_NOMBRE!="General")

    # Se convierte el SEGMENTO_NOMBRE en una factor
    datos <- datos %>%
      bind_rows(datos %>% mutate(SEGMENTO_NOMBRE="General")) %>%
      filter(!SEGMENTO_NOMBRE %in% botones_inactivos) %>%
      mutate(SEGMENTO_NOMBRE=relevel(factor(SEGMENTO_NOMBRE),"General")) %>%
      arrange(SEGMENTO_NOMBRE)

    # Se crea el vector segmetos
    segmentos <- levels(datos$SEGMENTO_NOMBRE)

    # Verificación inputs
    if (is.null(boton_activo) || !boton_activo %in% segmentos) boton_activo <- segmentos[1]

    # Se crea el data.frame datos_completos
    datos_completos<- datos %>% group_by(SEGMENTO_NOMBRE,MIEMBRO_LIQ_ID_SEUDONIMO) %>%
      summarise(VALOR_1=round(sum(POSICION_COMPRADORA_VALORADA)/1e+12,6),
                TEXTO_1=paste(VALOR_1,"Billones"),
                VALOR_2=round(sum(POSICION_VENDEDORA_VALORADA)/1e+12,6),
                TEXTO_2=paste(VALOR_2,"Billones"),
                VALOR_3=round(sum(GARANTIA_EXIGIDA)/1e+12,6),
                TEXTO_3=paste(VALOR_3,"Billones"),
                VALOR_4=round(sum(RIESGO_ST)/1e+12,6),
                TEXTO_4=paste(VALOR_4,"Billones","/",dt_porcentaje_caracter(VALOR_4/round(sum(PATRIMONIO)/1e+12,6)),"RSS/PT"),.groups = "drop") %>%
      mutate(MIEMBRO_LIQ_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_LIQ_ID_SEUDONIMO),if_else(SEGMENTO_NOMBRE=="General",VALOR_1+VALOR_2,0),.fun=max,.desc=T),
             VISIBLE=if_else(SEGMENTO_NOMBRE==boton_activo,TRUE,FALSE)) %>% arrange(SEGMENTO_NOMBRE)

    # Se verifica si se debe crear el updatemenus
    if (length(segmentos)>1) {
      # Se crean los botones
      botones <- foreach(i=1:length(segmentos),.combine = append) %do% {
        visible <- segmentos[i]==segmentos
        list(list(label = segmentos[i],method = "restyle",
                  args = list(list(boton_activo=segmentos[i],
                                   visible = as.logical(rep(visible,4))))))
      }

      # Se crea el updatemenus
      updatemenus <- list(
        list(active = which(segmentos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
             yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones))
    }else{
      # Se crea el updatemenus
      updatemenus <- NULL
    }

    # Se crea la gráfica
    plot <- plot_ly(data=datos_completos ,x=~MIEMBRO_LIQ_ID_SEUDONIMO,split=~as.numeric(SEGMENTO_NOMBRE),
                    hoverinfo="text+x+name",textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE,name="Posición",yaxis="y1",color="1",legendgroup="1") %>%
      add_bars(y=~-VALOR_2,text=~TEXTO_2,visible=~VISIBLE,name="Posición",yaxis="y1",color="1",legendgroup="1",showlegend=FALSE) %>%
      add_bars(y=~VALOR_3,text=~TEXTO_3,visible=~VISIBLE,name="Garantía Exigida",yaxis="y2",color="2") %>%
      add_bars(y=~VALOR_4,text=~TEXTO_4,visible=~VISIBLE,name="RSS",yaxis="y3",color="3") %>%
      subplot(nrows = 3, shareX = TRUE) %>%
      layout(hovermode = 'x',barmode="relative",
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=-0.2),
             updatemenus=updatemenus,
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title=NA,fixedrange=fixedrange),
             yaxis2 = list(title="Billones-COP",fixedrange=fixedrange),
             yaxis3 = list(title=NA,fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}

#' Gráfica la posición abierta bruta (dos puntas) vs garantía exigida por producto (barras)
#'
#' Esta función crea la gráfica de la posición abierta bruta (eje 1) vs. garantía exigida (eje y2) vs.
#' y el ratio de cobertura (eje y3) por producto en formato de barras.
#' La información se muestra acorde a la agrupación relacionada con cada botón
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_gar_exi_resumen}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @param boton_activo clase character. Si se desea que la gráfica se inicialice
#' con un botón seleccionado en especifico. Por defecto NULL
#' @param botones_inactivos clase vector character. Vector de los nombres de los botones a desactivar
#' en la gráfica (....). Por defecto c()
#' @export

gt_pa_bruta_gar_exi_por_producto <- function(datos,fixedrange=FALSE,boton_activo=NULL,botones_inactivos=c()){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se convierte el SEGMENTO_NOMBRE en un factor
    datos <- datos %>% filter(!SEGMENTO_NOMBRE %in% botones_inactivos) %>%
      mutate(SEGMENTO_NOMBRE=factor(SEGMENTO_NOMBRE)) %>%
      arrange(SEGMENTO_NOMBRE)

    # Se crea el vector segmetos
    segmentos <- levels(datos$SEGMENTO_NOMBRE)

    # Verificación boton_activo
    if (is.null(boton_activo) || !boton_activo %in% segmentos)boton_activo <- segmentos[1]

    # Se crea el data.frame datos_completos
    datos_completos<- datos %>% group_by(SEGMENTO_NOMBRE,PRODUCTO_DETALLE) %>%
      summarise(VALOR_1=round(sum(POSICION_BRUTA_VALORADA)/1e+12,6),
                VALOR_2=round(sum(GARANTIA_EXIGIDA)/1e+12,6),
                VALOR_3=VALOR_2/VALOR_1,.groups = "drop_last") %>%
      mutate(TEXTO_1=paste(VALOR_1,"Billones","/",dt_porcentaje_caracter(proportions(VALOR_1))),
             TEXTO_2=paste(VALOR_2,"Billones","/",dt_porcentaje_caracter(proportions(VALOR_2))),
             TEXTO_3=dt_porcentaje_caracter(VALOR_2/VALOR_1)) %>% ungroup() %>%
      mutate(ORDENADOR=as.numeric(fct_reorder(factor(PRODUCTO_DETALLE),VALOR_1,.fun=max)),
             ORDENADOR=factor(paste0(as.numeric(SEGMENTO_NOMBRE),"-",if_else(nchar(as.numeric(ORDENADOR))==1,"0",""),as.numeric(ORDENADOR))),
             PRODUCTO_DETALLE=fct_reorder(factor(PRODUCTO_DETALLE),as.numeric(ORDENADOR),.fun=max,.desc = TRUE),
             VISIBLE=if_else(SEGMENTO_NOMBRE==boton_activo,TRUE,FALSE)) %>% arrange(SEGMENTO_NOMBRE)


    # Se verifica si se debe crear el updatemenus
    if (length(segmentos)>1) {
      # Se crean los botones
      botones <- foreach(i=1:length(segmentos),.combine = append) %do% {
        visible <- segmentos[i]==segmentos
        list(list(label = segmentos[i],method = "restyle",
                  args = list(list(boton_activo=segmentos[i],
                                   visible = as.logical(rep(visible,3))))))
      }

      # Se crea el updatemenus
      updatemenus <- list(
        list(active = which(segmentos == boton_activo)-1,type= 'dropdown',direction = "down",xanchor = 'center',
             yanchor = "top",x=0.5,y=1.2,pad = list('r'= 0, 't'= 10, 'b' = 10),buttons = botones))
    }else{
      # Se crea el updatemenus
      updatemenus <- NULL
    }



    # Se crea la gráfica
    plot <- plot_ly(data=datos_completos,split=~SEGMENTO_NOMBRE,x=~PRODUCTO_DETALLE,
                    hoverinfo="text+x+name",textposition = 'none') %>%
      add_bars(y=~VALOR_1,text=~TEXTO_1,visible=~VISIBLE,name="Posición Dos Puntas",yaxis="y1",color="1") %>%
      add_bars(y=~VALOR_2,text=~TEXTO_2,visible=~VISIBLE,name="Garantía Exigida",yaxis="y2",color="2") %>%
      add_bars(y=~VALOR_3,text=~TEXTO_3,visible=~VISIBLE,name="Ratio Cobertura",yaxis="y3",color="3") %>%
      subplot(nrows = 3,heights =c(0.3,0.3,0.3),  shareX = TRUE) %>%
      layout(hovermode = 'x',
             legend = list(orientation = 'h',xanchor = "center",x = 0.5,y=1.05,tracegroupgap=0),
             updatemenus=updatemenus,
             xaxis = list(title = NA,fixedrange=fixedrange),
             yaxis = list(title="B-COP",fixedrange=fixedrange),
             yaxis2 = list(title="B-COP",fixedrange=fixedrange),
             yaxis3 = list(title="Porcentaje",tickformat="%",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


#' Gráfica la distribución consumo LMC por miembro (boxplot)
#'
#' Esta función crea la gráfica de la distribución del consumo del LMC por miembro en formato de boxplot
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_lmc_consumo}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_lmc_distribucion_consumo <- function(datos,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    datos <- datos %>% mutate(MIEMBRO_LIQ_ID_SEUDONIMO=fct_reorder(factor(MIEMBRO_LIQ_ID_SEUDONIMO),CONSUMO_LIMITE,.fun = max,.desc = TRUE))

    plot <- plot_ly(data = datos,x=~MIEMBRO_LIQ_ID_SEUDONIMO) %>%
      add_boxplot(y=~CONSUMO_LIMITE) %>%
      layout(xaxis = list(title = "",fixedrange=fixedrange),
             yaxis = list(title = "Porcentaje",tickformat="%",fixedrange=fixedrange))%>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}


