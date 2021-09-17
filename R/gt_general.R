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
