# ==============================================================================
# Gripe RIDE | Modulo: mapa de incidencia por municipio
#
# Sempre a RIDE-DF inteira (o mapa nao recorta, so destaca o municipio em
# foco). Respeita o filtro de AGENTE. Clique num municipio -> foco no painel.
#
# Incidencia acumulada no periodo = casos / pop * 100 mil, com a populacao do
# ano mais recente do periodo (somar pop ao longo das semanas multiplicaria o
# denominador).
#
# Bordas: municipios em cinza claro e, por cima, o contorno das UFs e da
# RIDE (CONTORNO_RIDE, em global.R) - sem isso, municipio na cor mais clara
# da escala some no fundo branco do card.
# ==============================================================================

COR_BORDA_MUN  <- "#CBD5DB"   # entre municipios
COR_BORDA_UF   <- "#A9B5BD"   # divisa entre estados
COR_BORDA_RIDE <- "#94A3AC"   # contorno externo da RIDE

#' Desenha o contorno das UFs e da RIDE por cima do mapa.
#'
#' As feicoes de contorno (CODIGOS_CONTORNO) estao no fim de MALHA_MAPA; aqui
#' elas ganham um item de dado cada, sem valor (o visualMap as ignora), sem
#' preenchimento e `silent` (clique, hover e tooltip atravessam para o
#' municipio de baixo) - so a borda. Como fazem parte do mesmo mapa,
#' acompanham zoom e arraste sem sincronizacao.
com_contorno <- function(e) {
  itens <- lapply(CODIGOS_CONTORNO, function(cod) list(
    name = cod, value = NULL,
    itemStyle = list(areaColor = "none",
                     borderColor = if (cod == "RIDE") COR_BORDA_RIDE else COR_BORDA_UF,
                     borderWidth = if (cod == "RIDE") 1.2 else 1),
    emphasis = list(disabled = TRUE), select = list(disabled = TRUE),
    tooltip = list(show = FALSE), silent = TRUE
  ))
  e$x$opts$series[[1]]$data <- c(e$x$opts$series[[1]]$data, itens)
  e
}

mod_mapa_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("metrica"), "Métrica",
                        c("Incidência /100 mil" = "inc", "Casos" = "casos",
                          "Óbitos" = "obitos"), selected = "inc"),
    shiny::radioButtons(ns("periodo"), "Período",
                        c("Últimas 12 semanas" = "12", "Últimas 52 semanas" = "52",
                          "Ano corrente" = "ano"), selected = "52"),
    shiny::helpText(shiny::tags$small("Clique em um município para focar o painel nele."))
  )
}

mod_mapa_saida <- function(id, height = "460px") {
  ns <- shiny::NS(id)
  shiny::div(
    class = "position-relative",
    shiny::actionButton(ns("limpar"), "Visão geral", icon = shiny::icon("rotate-left"),
                        class = "btn btn-sm btn-light shadow-sm",
                        style = "position:absolute; top:8px; left:8px; z-index:5; padding:.15rem .5rem; font-size:.8rem;"),
    echarts4r::echarts4rOutput(ns("mapa"), height = height)
  )
}

mod_mapa_server <- function(id, geo, agente) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dados <- shiny::reactive({
      ini <- switch(input$periodo %||% "52",
                    "12" = DATA_MAX - 11L * 7L, "52" = DATA_MAX - 51L * 7L,
                    as.Date(sprintf("%d-01-01", ANO_MAX)) - 3L)
      d <- filtrar(list(nivel = "base"), agentes_do_filtro(agente()))
      d <- d[d$data_ini_se >= ini, , drop = FALSE]
      agg <- d |>
        dplyr::group_by(municipio_geocodigo) |>
        dplyr::summarise(casos = sum(casos), obitos = sum(obitos), .groups = "drop")
      pop <- POP_MUN_ANO |> dplyr::filter(ano == max(ano)) |> dplyr::select(-ano)
      GEO_DICT |>
        dplyr::select(municipio_geocodigo, municipio_nome) |>
        dplyr::left_join(agg, by = "municipio_geocodigo") |>
        dplyr::left_join(pop, by = "municipio_geocodigo") |>
        dplyr::mutate(casos = dplyr::coalesce(casos, 0), obitos = dplyr::coalesce(obitos, 0),
                      inc = round(casos / pop * 1e5, 1),
                      chave = as.character(municipio_geocodigo))
    })

    output$mapa <- echarts4r::renderEcharts4r({
      d <- dados()
      met <- input$metrica %||% "inc"
      d$valor <- d[[met]]
      rot <- c(inc = "Incidência /100 mil", casos = "Casos", obitos = "Óbitos")[[met]]

      info <- jsonlite::toJSON(
        stats::setNames(lapply(seq_len(nrow(d)), function(i) list(
          n = d$municipio_nome[i], c = d$casos[i], o = d$obitos[i], inc = d$inc[i]
        )), d$chave), auto_unbox = TRUE)
      tt <- htmlwidgets::JS(sprintf("function(p){
        var i = (%s)[p.name]; if (!i) return p.name;
        var f = function(x){ return (x === null || x === undefined) ? '-' :
                 Number(x).toLocaleString('pt-BR', {maximumFractionDigits: 1}); };
        return '<b>' + i.n + '</b><br/>Casos: ' + f(i.c) + '<br/>Incidência: ' + f(i.inc) +
               ' /100 mil<br/>Óbitos: ' + f(i.o);
      }", info))

      clique <- htmlwidgets::JS(sprintf(
        "function(p){ if (p && p.name) { var a = p.name; setTimeout(function(){
           Shiny.setInputValue('%s', a, {priority: 'event'}); }, 60); } }", ns("clique")))

      sel <- if (identical(geo()$nivel, "municipio")) as.character(geo()$geocodigo) else ""

      d |>
        echarts4r::e_charts(chave) |>
        echarts4r::e_map_register("RIDE", MALHA_MAPA) |>
        echarts4r::e_map(valor, map = "RIDE", name = rot, nameProperty = "codarea",
                         roam = TRUE, label = list(show = FALSE),
                         itemStyle = list(borderColor = COR_BORDA_MUN, borderWidth = 0.6),
                         emphasis = list(itemStyle = list(areaColor = TONS_AZUL[["300"]]),
                                         label = list(show = FALSE)),
                         selectedMode = "single",
                         select = list(itemStyle = list(areaColor = PALETA[["azul_acao"]]),
                                       label = list(show = FALSE))) |>
        echarts4r::e_visual_map(valor, inRange = list(color = ESCALA_MAPA),
                                orient = "vertical", left = "left", bottom = "6%",
                                text = list("alto", "baixo"), calculable = TRUE) |>
        echarts4r::e_tooltip(trigger = "item", formatter = tt) |>
        echarts4r::e_on(query = NULL, handler = clique, event = "click") |>
        echarts4r::e_on(query = NULL, event = "finished", handler = htmlwidgets::JS(sprintf(
          "function(){ if (this.__selFeito) return; this.__selFeito = true;
             if ('%s') this.dispatchAction({type: 'select', seriesIndex: 0, name: '%s'}); }",
          sel, sel))) |>
        e_padrao() |>
        com_contorno()
    })

    pedido <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$clique, {
      cod <- suppressWarnings(as.integer(input$clique))
      shiny::req(!is.na(cod))
      pedido(list(geocodigo = cod, .ts = Sys.time()))
    })
    shiny::observeEvent(input$limpar, {
      pedido(list(geocodigo = NA_integer_, .ts = Sys.time()))
    })
    pedido
  })
}
