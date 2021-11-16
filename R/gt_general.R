#' Retorna la lista de configuración del idioma de las data.table (DT)
#' @export

gt_espanol <- list(
  sProcessing="Procesando...",
  sLengthMenu="Mostrar _MENU_ registros",
  sZeroRecords="No se encontraron resultados",
  sEmptyTable="Información no disponible",
  sInfo="Registros del _START_ al _END_ de un total de _TOTAL_",
  sInfoEmpty="Registros del 0 al 0 de un total de 0",
  sInfoFiltered="(filtrado de un total de _MAX_ registros)",
  sInfoPostFix="",
  sSearch="Buscar:",
  sUrl="",
  sInfoThousands=",",
  sLoadingRecords="Cargando...",
  oPaginate=list(
    sFirst="Primero",
    sLast="Último",
    sNext="Siguiente",
    sPrevious="Anterior"
  ),
  oAria=list(
    sSortAscending=": Activar para ordenar la columna de manera ascendente",
    sSortDescending=": Activar para ordenar la columna de manera descendente"
  )
)

#' Retorna la gráfica mensaje error
#' @export

gt_mensaje_error <- plot_ly() %>%
  add_text(x=1,y=1,text="Información no disponible",textfont=list(size = 20,color = toRGB("grey70"))) %>%
  layout(margin=list(l=0,r=0,b=0,t=0,pad=4),xaxis=list(visible=FALSE,fixedrange=TRUE),
         yaxis=list(visible=FALSE,fixedrange=TRUE),plot_bgcolor  = "transparent", paper_bgcolor = "transparent")%>%
  config(displayModeBar = F)



#' Retorna el modeBarButton maximizar pantalla
#' @export

gt_mbb_maximizar_pantalla <- list(
  name = "Maximizar Pantalla",
  icon = list(
    path = 'M14 0a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V2a2 2 0 0 1 2-2h12zM5.904 10.803 10 6.707v2.768a.5.5 0 0 0 1 0V5.5a.5.5 0 0 0-.5-.5H6.525a.5.5 0 1 0 0 1h2.768l-4.096 4.096a.5.5 0 0 0 .707.707z',
    transform = 'scale(0.9)'
  ),
  click = htmlwidgets::JS(
    "function plotZoom(el){
            el = $(el);
            if(el.attr('data-full_screen') !== 'true') {
            el.addClass('full-screen-plotly').trigger('resize').fadeOut().fadeIn();
                el.attr('data-full_screen', 'true');
            }
        }"
  )
)

#' Retorna el modeBarButton minimizar pantalla
#' @export

gt_mbb_minimizar_pantalla <- list(
  name = "Minimizar Pantalla",
  icon = list(
    path = 'M2 16a2 2 0 0 1-2-2V2a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H2zm8.096-10.803L6 9.293V6.525a.5.5 0 0 0-1 0V10.5a.5.5 0 0 0 .5.5h3.975a.5.5 0 0 0 0-1H6.707l4.096-4.096a.5.5 0 1 0-.707-.707z',
    transform = 'scale(0.9)'
  ),
  click = htmlwidgets::JS(
    "function plotZoom(el){
            el = $(el);
            if(el.attr('data-full_screen') === 'true') {
                el.removeClass('full-screen-plotly').trigger('resize').fadeOut().fadeIn();
                el.attr('data-full_screen', 'false');
            }
        }"
  )
)
