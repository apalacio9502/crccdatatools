#' Grafica la posición abierta valorada vs el importe (lineas)
#'
#' Esta función crea la gráfica la posición abierta valorada vs el importe diario
#' en formato de lineas
#' @param datos clase data.frame. Los datos deben ser los generados por la función
#' \code{\link{dt_gen_vol_resumen}} o tener una estructura igual a dichos datos
#' @param fixedrange clase boolean. TRUE si se desea desactivar la función de zoom en las gráficas. Por defecto FALSE
#' @export

gt_rv_pa_valorada_importe_diario<- function(datos,fixedrange=FALSE){

  # Se verifica si existen datos
  if (nrow(datos)>0) {

    # Se crea el data.frame datos_completos
    datos_completos <- datos %>% group_by(FECHA) %>%
      summarise(VALOR_1=round(sum(POSICION_COMPRADORA_VALORADA,na.rm = TRUE)/1e+9,6),
                VALOR_2=round(sum(POSICION_COMPRADORA_IMPORTE,na.rm = TRUE)/1e+9,6),
                VALOR_3=(1-(VALOR_2/VALOR_1)),
                TEXTO_1=paste(VALOR_1,"Miles M"),
                TEXTO_2=paste(VALOR_2,"Miles M"),
                TEXTO_3=dt_porcentaje_caracter(VALOR_3),.groups = "drop")

    # Se grafica la evoluacion de la posicón abierta repos
    plot <- plot_ly(data= datos_completos ,x=~FECHA,hoverinfo="text+x+name") %>%
      add_lines(y=~VALOR_1, text=~TEXTO_1, name="Valor Mercado Títulos") %>%
      add_lines(y=~VALOR_2, text=~TEXTO_2, name="Importe Efectivo Títulos") %>%
      add_lines(y=~VALOR_3, text=~TEXTO_3, name="Haircut",yaxis="y2") %>%
      subplot(nrows = 2,heights = c(0.6,0.3),shareX = TRUE) %>%
      layout(hovermode = 'x',legend = list(orientation = 'h',xanchor = "center",x = 0.5),
             xaxis = list(type='date',tickformat = "%d-%b",title = NA,fixedrange=fixedrange),
             yaxis = list(title = "Miles de Millones-COP",fixedrange=fixedrange),
             yaxis2 = list(title = "% Haricut",tickformat="%",fixedrange=fixedrange)) %>%
      config(displaylogo = F,locale = "es")

    return(plot)

  }else{
    return(gt_mensaje_error)
  }
}
