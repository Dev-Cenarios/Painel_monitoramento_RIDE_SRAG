# ==============================================================================
# Gripe RIDE | server.R
# Orquestra os modulos. Os dados ja estao em memoria (global.R): cada modulo
# filtra DADOS com filtrar(), sem consulta a banco.
# ==============================================================================

function(input, output, session) {

  # Clique no mapa (ou "Visão geral") -> filtros. O mapa so pede o foco; o
  # estado do recorte mora nos inputs do modulo de filtros.
  foco_mapa <- shiny::reactiveVal(NULL)

  filtros <- mod_filtros_server("filtros", foco_externo = foco_mapa)
  geo    <- filtros$geo
  agente <- filtros$agente

  # Serie semanal do recorte + agente: compartilhada por KPIs, serie, canal e
  # heatmap.
  serie <- shiny::reactive(serie_semanal(geo(), agentes_do_filtro(agente()))) |>
    shiny::bindCache(geo(), agente())

  mod_kpi_server("kpi", geo, agente, serie)

  pedido <- mod_mapa_server("mapa", geo, agente)
  shiny::observeEvent(pedido(), foco_mapa(pedido()), ignoreInit = TRUE)

  mod_serie_server("serie", geo, agente, serie)
  mod_rt_server("rt", agente, serie)
  mod_canal_server("canal", serie)
  mod_heatmap_server("heatmap", serie)
  mod_perfil_server("perfil", geo, agente)
  mod_analises_server("analises", agente)
}
