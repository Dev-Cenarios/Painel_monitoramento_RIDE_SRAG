# ==============================================================================
# Gripe RIDE | Modulo: Tendencia por municipio (quadrantes)
#
# Mesmo grafico do dengue_ride (R/mod_mapa_rt.R):
#   Eixo X = casos do municipio na janela atual / mediana historica do
#            municipio NESSA MESMA janela de semanas, em anos anteriores.
#   Eixo Y = casos da janela atual / casos da janela imediatamente anterior.
#   Cor = quadrante; forma = incidencia acumulada no ano; bolha = casos.
#
# Apesar do "Rt" no titulo (pedido para manter o nome do dengue_ride), NAO e
# o numero de reproducao: e anomalia sazonal + tendencia recente, para
# comparar os municipios entre si. O Rt de verdade esta em R/mod_rt.R.
#
# Diferencas para o dengue_ride:
#   - segue o filtro de agente etiologico; IGNORA o recorte geografico,
#     que so serve para destacar o municipio selecionado;
#   - a semana corrente fica de fora (pouco digitada) e as semanas
#     recentes sao corrigidas pelo nowcast (NOWCAST_F, o mesmo F(k) do
#     total da RIDE), senao o eixo Y sairia sempre "caindo";
#   - SRAG sao casos graves (internacoes): a tendencia e das internacoes.
#
# Arquivo identico nos tres paineis (chik/zika/gripe) exceto o bloco de
# configuracao abaixo.
# ==============================================================================

# -- configuracao do painel ----------------------------------------------------
TENDENCIA_JANELA_PADRAO <- 4L
TENDENCIA_JANELA_MAX    <- 12L

# Incidencia ACUMULADA no ano ate a semana de referencia (por 100 mil).
# SRAG (todos os agentes) na RIDE: ~60-290 por 100 mil no ano ate a
# SE 40 de 2026, mediana ~150. Com um agente so, cai para a 1a faixa.
LIMIARES_FORMA <- list(
  list(id = "triangle", teto = 50,  simbolo = "triangle", rotulo = "< 50 /100k"),
  list(id = "diamond",  teto = 120, simbolo = "diamond",  rotulo = "50–120 /100k"),
  list(id = "circle",   teto = 200, simbolo = "circle",   rotulo = "120–200 /100k"),
  list(id = "rect",     teto = Inf, simbolo = "rect",     rotulo = "> 200 /100k")
)
# -----------------------------------------------------------------------------

# Anos historicos minimos com dado na janela para a mediana valer.
MIN_ANOS_HISTORICO_RT <- 2L

# Escala divergente igual a do dengue_ride: abaixo do normal em azul, acima
# em terracota (fixa, nao a cor da doenca, para ler igual em todos os
# paineis); o tom mais forte e o pior cenario de cada lado.
CORES_QUADRANTE <- c(
  abaixo_estavel      = unname(TONS_AZUL[["700"]]),
  abaixo_crescendo    = unname(TONS_AZUL[["400"]]),
  acima_desacelerando = "#D97F55",
  acima_crescendo     = "#853B15",
  sem_base            = unname(PALETA[["sub"]])
)

ROTULOS_QUADRANTE <- c(
  abaixo_estavel      = "Abaixo do normal · caindo",
  abaixo_crescendo    = "Abaixo do normal · subindo",
  acima_desacelerando = "Acima do normal · caindo",
  acima_crescendo     = "Acima do normal · subindo",
  sem_base            = "Sem histórico suficiente"
)


mod_mapa_rt_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("janela"), "Janela de comparação (semanas)",
                       min = 1, max = TENDENCIA_JANELA_MAX,
                       value = TENDENCIA_JANELA_PADRAO, step = 1),
    shiny::checkboxInput(ns("mostrar_sem_base"),
                         "Mostrar municípios sem histórico suficiente", value = FALSE),
    shiny::helpText(shiny::tags$small(
      "Eixo X: casos da janela atual vs. mediana histórica da mesma janela ",
      "(outros anos). Eixo Y: janela atual vs. janela anterior. ",
      "Cor = quadrante; forma = incidência acumulada no ano; tamanho = casos na janela."
    ))
  )
}

mod_mapa_rt_saida <- function(id, height = "440px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}


#' @param geo     reactive do recorte (so para destacar o municipio).
#' @param classes reactive com o vetor de agentes do filtro.
mod_mapa_rt_server <- function(id, geo, classes) {
  shiny::moduleServer(id, function(input, output, session) {

    serie_mun <- shiny::reactive(serie_municipios(classes())) |>
      shiny::bindCache(classes())

    quadro <- shiny::reactive({
      shiny::req(input$janela)
      calcular_quadro_tendencia(serie_mun(), janela = input$janela)
    })

    output$grafico <- echarts4r::renderEcharts4r({
      q <- quadro()
      d <- q$dados
      shiny::validate(shiny::need(nrow(d) > 0, "Sem dados suficientes para montar o gráfico."))

      d$rotulo_curto <- sub("\\s.*$", "", d$municipio_nome)

      g <- geo()
      sel_cod <- if (identical(g$nivel, "municipio")) as.integer(g$geocodigo) else NA_integer_
      has_sel <- !is.na(sel_cod)
      cor_destaque <- unname(PALETA[["tinta"]])

      # bolha: raiz dos casos, reescalada para 12-40px
      raiz <- sqrt(pmax(d$casos_atual, 0))
      amplitude <- diff(range(raiz, na.rm = TRUE))
      d$bolha <- if (is.na(amplitude) || amplitude == 0) rep(20, nrow(d)) else
        12 + (raiz - min(raiz, na.rm = TRUE)) / amplitude * 28

      ponto <- function(r, x, y, cor, opac_base = 1, borda = 0) {
        sel_i <- has_sel && r$municipio_geocodigo == sel_cod
        list(
          value      = list(x, y),
          symbolSize = if (sel_i) r$bolha * 1.7 else r$bolha,
          name       = r$municipio_nome,
          itemStyle  = list(
            color       = if (sel_i) cor_destaque else cor,
            opacity     = if (sel_i) 1 else if (has_sel) min(opac_base, 0.3) else opac_base,
            borderColor = if (sel_i || borda == 0) PALETA[["superficie"]] else cor,
            borderWidth = if (sel_i) 2 else borda
          ),
          label = list(
            show            = TRUE,
            formatter       = if (sel_i) r$municipio_nome else r$rotulo_curto,
            position        = "top",
            fontSize        = if (sel_i) 13 else 9,
            fontWeight      = if (sel_i) "bold" else "normal",
            color           = PALETA[["tinta"]],
            opacity         = if (has_sel && !sel_i) 0.35 else 1,
            backgroundColor = if (sel_i) "rgba(255,255,255,0.92)" else "transparent",
            padding         = if (sel_i) c(2, 5, 2, 5) else 0,
            borderRadius    = if (sel_i) 3 else 0
          )
        )
      }

      # uma serie por FORMA (echarts exige symbol no nivel da serie)
      com_base <- d[d$quadrante != "sem_base", , drop = FALSE]
      series_forma <- lapply(LIMIARES_FORMA, function(f) {
        sub <- com_base[com_base$forma == f$id, , drop = FALSE]
        list(
          name = f$rotulo, type = "scatter", symbol = f$simbolo,
          itemStyle = list(color = PALETA[["sub"]]),
          data = lapply(seq_len(nrow(sub)), function(i) {
            r <- sub[i, ]
            ponto(r, r$x_val, r$taxa_cresc, CORES_QUADRANTE[[r$quadrante]])
          }),
          labelLayout = list(hideOverlap = TRUE),
          emphasis = list(itemStyle = list(shadowBlur = 10,
                                           shadowColor = cor_alpha(PALETA[["tinta"]], 0.35)))
        )
      })

      # "sem base": ocultos por padrao; o municipio em foco aparece mesmo assim
      e_sem_base <- d$quadrante == "sem_base"
      sel_sem_base <- has_sel & d$municipio_geocodigo == sel_cod
      sem_base <- d[e_sem_base & (isTRUE(input$mostrar_sem_base) | sel_sem_base), , drop = FALSE]
      n_ocultos <- sum(e_sem_base) - nrow(sem_base)
      y_faixa_sem_base <- {
        ymax <- suppressWarnings(max(com_base$taxa_cresc, na.rm = TRUE))
        if (!is.finite(ymax)) ymax <- 1
        round(ymax * 1.15 + 0.2, 2)
      }
      serie_sem_base <- if (nrow(sem_base) > 0) list(list(
        name = "Sem histórico suficiente", type = "scatter", symbol = "circle",
        data = lapply(seq_len(nrow(sem_base)), function(i) {
          r <- sem_base[i, ]
          ponto(r, dplyr::coalesce(r$x_val, 0), y_faixa_sem_base,
                CORES_QUADRANTE[["sem_base"]], opac_base = 0.5, borda = 1.5)
        })
      )) else list()

      linhas_ref <- list(list(
        name = "__ref__", type = "scatter", data = list(),
        silent = TRUE, tooltip = list(show = FALSE),
        markLine = list(
          silent = TRUE, symbol = list("none", "none"), animation = FALSE,
          data = list(
            list(xAxis = 1, lineStyle = list(color = PALETA[["sub"]], type = "dashed", width = 1.2),
                 label = list(formatter = "Mediana histórica", position = "insideEndTop",
                              color = PALETA[["sub"]], fontSize = 10)),
            list(yAxis = 1, lineStyle = list(color = PALETA[["tinta"]], type = "dashed", width = 1.5),
                 label = list(formatter = "Estável", position = "insideEndTop",
                              color = PALETA[["tinta"]], fontSize = 10))
          )
        )
      ))

      # tooltip le de uma tabela por nome (a faixa "sem base" tem y artificial)
      tabela_tt <- jsonlite::toJSON(
        stats::setNames(lapply(seq_len(nrow(d)), function(i) list(
          x     = round(d$x_val[i], 2),
          y     = round(d$taxa_cresc[i], 2),
          quad  = unname(ROTULOS_QUADRANTE[d$quadrante[i]]),
          casos = round(d$casos_atual[i]),
          ant   = round(d$casos_anterior[i]),
          med   = round(d$mediana_historica[i], 1),
          inc   = round(d$incidencia_acumulada[i], 1)
        )), d$municipio_nome),
        auto_unbox = TRUE, na = "null"
      )
      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(p){
           if (p.seriesName === '__ref__') return '';
           var r = (%s)[p.name];
           if (!r) return '';
           function f(v, d){ return (v===null||v===undefined) ? '—' :
             v.toLocaleString('pt-BR', {maximumFractionDigits: d}); }
           return '<b>' + p.name + '</b><br/>' + r.quad + '<br/>' +
                  'Vs. mediana histórica: <b>' + f(r.x, 2) + 'x</b> (mediana ' + f(r.med, 1) + ')<br/>' +
                  'Vs. janela anterior: <b>' + f(r.y, 2) + 'x</b> (' + f(r.ant, 0) + ' casos)<br/>' +
                  'Casos na janela: ' + f(r.casos, 0) + '<br/>' +
                  'Incidência acumulada no ano: ' + f(r.inc, 1) + ' /100k';
         }", tabela_tt))

      # Duas linhas fixas: o card tem ~550px e uma linha so quebraria por
      # cima da legenda de formas.
      notas <- c(if (q$corrigido) "semanas recentes corrigidas (nowcast)",
                 if (n_ocultos > 0) sprintf("%d sem histórico (ocultos)", n_ocultos))
      subtitulo <- sprintf(
        "Atual %s–%s · anterior %s–%s · incidência acumulada em %d até SE %02d%s",
        q$atual_ini, q$atual_fim, q$ant_ini, q$ant_fim, q$ano_recente, q$se_ref_semana,
        if (length(notas)) paste0("\n", paste(notas, collapse = " · ")) else ""
      )

      quadrantes_legenda <- names(CORES_QUADRANTE)
      if (nrow(sem_base) == 0) quadrantes_legenda <- setdiff(quadrantes_legenda, "sem_base")
      # O echarts so desenha na legenda nomes que existem como serie: uma
      # serie vazia por quadrante carrega a cor (a "sem base" ja e serie).
      series_legenda_cor <- lapply(setdiff(quadrantes_legenda, "sem_base"), function(k) list(
        name = ROTULOS_QUADRANTE[[k]], type = "scatter", data = list(),
        itemStyle = list(color = CORES_QUADRANTE[[k]]), silent = TRUE
      ))
      y_max_minimo <- if (nrow(sem_base) > 0) round(y_faixa_sem_base + 0.15, 2) else 0

      # Com poucos casos (fora do pico), e comum nenhum municipio ter base:
      # avisa no meio do grafico em vez de mostrar eixos vazios sem explicacao.
      aviso <- if (nrow(com_base) == 0 && nrow(sem_base) == 0) list(list(
        type = "text", left = "center", top = "middle",
        style = list(text = paste0("Nenhum município com casos suficientes para comparar.\n",
                                   "Aumente a janela ou mostre os municípios sem histórico."),
                     fill = PALETA[["sub"]], fontSize = 12, textAlign = "center")
      )) else list()

      echarts4r::e_charts() |>
        echarts4r::e_list(list(
          backgroundColor = "transparent",
          title = list(
            text = sprintf("Tendência por município — %d semana(s)", input$janela),
            subtext = subtitulo,
            textStyle    = list(color = PALETA[["tinta"]], fontSize = 14, fontWeight = "bold"),
            subtextStyle = list(color = PALETA[["sub"]], fontSize = 10, lineHeight = 14)
          ),
          graphic = aviso,
          legend = list(
            list(data = lapply(LIMIARES_FORMA, function(f) f$rotulo),
                 top = 74, left = "center",
                 textStyle = list(color = PALETA[["sub"]], fontSize = 10),
                 itemWidth = 14, itemHeight = 12, itemGap = 14),
            list(data = lapply(quadrantes_legenda, function(k) list(
                   name = ROTULOS_QUADRANTE[[k]], itemStyle = list(color = CORES_QUADRANTE[[k]]))),
                 bottom = 6, left = "center",
                 textStyle = list(color = PALETA[["tinta"]], fontSize = 10),
                 itemWidth = 14, itemHeight = 12, itemGap = 12)
          ),
          tooltip = list(trigger = "item", formatter = js_tooltip,
                         backgroundColor = "#FFFFFF", borderColor = PALETA[["linha"]], borderWidth = 1,
                         textStyle = list(color = PALETA[["tinta"]])),
          xAxis = list(
            type = "value", name = "Vs. mediana histórica (mediana = 1x)",
            nameLocation = "middle", nameGap = 26, min = 0,
            max = htmlwidgets::JS("function(v){ return Math.max(v.max*1.15, 1.3).toFixed(1); }"),
            nameTextStyle = list(color = PALETA[["sub"]], fontSize = 11),
            axisLabel = list(color = PALETA[["sub"]], fontSize = 10,
                             formatter = htmlwidgets::JS("function(v){ return v.toFixed(1)+'x'; }")),
            splitLine = list(lineStyle = list(color = PALETA[["linha"]], type = "dashed"))
          ),
          yAxis = list(
            type = "value", name = "Vs. janela anterior (estável = 1x)",
            nameLocation = "middle", nameGap = 40, min = 0,
            max = htmlwidgets::JS(sprintf(
              "function(v){ return Math.max(v.max*1.1, %s, 1.3).toFixed(1); }", y_max_minimo)),
            nameTextStyle = list(color = PALETA[["sub"]], fontSize = 11),
            axisLabel = list(color = PALETA[["sub"]], fontSize = 10,
                             formatter = htmlwidgets::JS(
                               "function(v){ return v.toLocaleString('pt-BR',{minimumFractionDigits:1}); }")),
            splitLine = list(lineStyle = list(color = PALETA[["linha"]], type = "dashed"))
          ),
          series = c(series_forma, serie_sem_base, linhas_ref, series_legenda_cor),
          dataZoom = list(
            list(type = "inside", xAxisIndex = 0, filterMode = "none"),
            list(type = "inside", yAxisIndex = 0, filterMode = "none")
          ),
          toolbox = list(feature = list(saveAsImage = list(title = "Salvar imagem"))),
          grid = list(left = 56, right = 24, top = 112, bottom = 96),
          textStyle = list(fontFamily = FONTE_GRAFICO)
        ))
    })
  })
}


#' Serie semanal por municipio (grade completa: semana sem notificacao = 0).
#' Diferente do InfoDengue, o SINAN so tem linha onde houve caso; sem a grade,
#' anos sem caso sumiriam do historico em vez de contar como zero.
serie_municipios <- function(classes) {
  d <- filtrar(list(nivel = "base"), classes) |>
    dplyr::group_by(municipio_geocodigo, data_ini_se) |>
    dplyr::summarise(casos = sum(casos), .groups = "drop")
  tidyr::expand_grid(municipio_geocodigo = GEO_DICT$municipio_geocodigo,
                     SEMANAS[, c("data_ini_se", "se")]) |>
    dplyr::left_join(d, by = c("municipio_geocodigo", "data_ini_se")) |>
    dplyr::mutate(casos = dplyr::coalesce(casos, 0),
                  ano = se %/% 100L, semana = se %% 100L) |>
    dplyr::left_join(POP_MUN_ANO, by = c("municipio_geocodigo", "ano")) |>
    dplyr::left_join(dplyr::distinct(GEO_DICT, municipio_geocodigo, municipio_nome),
                     by = "municipio_geocodigo")
}

#' Semana de referencia e correcao de atraso. Com nowcast (chik e gripe), a
#' semana corrente sai e as k semanas seguintes sao divididas por F(k);
#' sem nowcast (zika), usa os casos como estao.
referencia_tendencia <- function(serie_mun) {
  if (!exists("NOWCAST_F")) {
    return(list(data_ref = max(serie_mun$data_ini_se), corrigir = function(dd, casos) casos))
  }
  list(
    data_ref = min(max(serie_mun$data_ini_se), DATA_REF_ARQUIVO - 7L),
    corrigir = function(dd, casos) {
      k <- as.integer((DATA_REF_ARQUIVO - dd) / 7)
      f <- NOWCAST_F$f[match(k, NOWCAST_F$k)]
      ifelse(is.na(f), casos, casos / pmax(f, 0.05))
    }
  )
}

#' Monta o quadro de tendencia: uma linha por municipio. Mesma conta do
#' dengue_ride, exceto o historico (por deslocamento de 52 semanas).
calcular_quadro_tendencia <- function(serie_mun, janela = 4L) {
  ref <- referencia_tendencia(serie_mun)
  serie_mun <- serie_mun[serie_mun$data_ini_se <= ref$data_ref, , drop = FALSE]
  serie_mun$casos <- ref$corrigir(serie_mun$data_ini_se, serie_mun$casos)

  data_max      <- ref$data_ref
  se_ref        <- serie_mun$se[serie_mun$data_ini_se == data_max][1]
  ano_recente   <- se_ref %/% 100L
  se_ref_semana <- se_ref %% 100L

  atual_fim <- data_max
  atual_ini <- atual_fim - (janela - 1L) * 7
  ant_fim   <- atual_ini - 7
  ant_ini   <- ant_fim - (janela - 1L) * 7

  janela_atual <- serie_mun |>
    dplyr::filter(data_ini_se >= atual_ini, data_ini_se <= atual_fim) |>
    dplyr::group_by(municipio_geocodigo) |>
    dplyr::summarise(casos_atual = sum(casos), .groups = "drop")

  janela_anterior <- serie_mun |>
    dplyr::filter(data_ini_se >= ant_ini, data_ini_se <= ant_fim) |>
    dplyr::group_by(municipio_geocodigo) |>
    dplyr::summarise(casos_anterior = sum(casos), .groups = "drop")

  # Historico = a mesma janela deslocada 52, 104, ... semanas para tras (so
  # deslocamentos em que ela cabe inteira na serie). Por data, e nao por
  # numero de semana como no dengue_ride: assim a virada do ano nao desloca
  # nada e uma janela longa nunca casa com ela mesma.
  n_desloc <- as.integer(atual_ini - min(serie_mun$data_ini_se)) %/% 364L
  historico <- if (n_desloc < 1) {
    data.frame(municipio_geocodigo = integer(0), mediana_historica = numeric(0),
               n_anos_historico = integer(0))
  } else {
    lapply(seq_len(n_desloc), function(i) {
      serie_mun |>
        dplyr::filter(data_ini_se >= atual_ini - 364L * i, data_ini_se <= atual_fim - 364L * i) |>
        dplyr::group_by(municipio_geocodigo) |>
        dplyr::summarise(casos_janela_ano = sum(casos), .groups = "drop")
    }) |>
      dplyr::bind_rows() |>
      dplyr::group_by(municipio_geocodigo) |>
      dplyr::summarise(mediana_historica = stats::median(casos_janela_ano),
                       n_anos_historico  = dplyr::n(), .groups = "drop")
  }

  acumulado <- serie_mun |>
    dplyr::filter(ano == ano_recente, semana <= se_ref_semana) |>
    dplyr::group_by(municipio_geocodigo) |>
    dplyr::summarise(casos_acumulados = sum(casos),
                     pop_ref = suppressWarnings(max(pop, na.rm = TRUE)), .groups = "drop") |>
    dplyr::mutate(incidencia_acumulada = dplyr::if_else(
      is.finite(pop_ref) & pop_ref > 0, casos_acumulados / pop_ref * 1e5, NA_real_))

  dados <- dplyr::distinct(serie_mun, municipio_geocodigo, municipio_nome) |>
    dplyr::left_join(janela_atual,    by = "municipio_geocodigo") |>
    dplyr::left_join(janela_anterior, by = "municipio_geocodigo") |>
    dplyr::left_join(historico,       by = "municipio_geocodigo") |>
    dplyr::left_join(acumulado,       by = "municipio_geocodigo") |>
    dplyr::mutate(
      casos_atual    = dplyr::coalesce(casos_atual, 0),
      casos_anterior = dplyr::coalesce(casos_anterior, 0),
      sem_base = casos_anterior == 0 |
        is.na(mediana_historica) | mediana_historica == 0 |
        dplyr::coalesce(n_anos_historico, 0L) < MIN_ANOS_HISTORICO_RT,
      taxa_cresc = dplyr::if_else(casos_anterior > 0, casos_atual / casos_anterior, NA_real_),
      x_val      = dplyr::if_else(!is.na(mediana_historica) & mediana_historica > 0,
                                  casos_atual / mediana_historica, NA_real_),
      forma = dplyr::case_when(
        is.na(incidencia_acumulada)                     ~ LIMIARES_FORMA[[1]]$id,
        incidencia_acumulada < LIMIARES_FORMA[[1]]$teto ~ LIMIARES_FORMA[[1]]$id,
        incidencia_acumulada < LIMIARES_FORMA[[2]]$teto ~ LIMIARES_FORMA[[2]]$id,
        incidencia_acumulada < LIMIARES_FORMA[[3]]$teto ~ LIMIARES_FORMA[[3]]$id,
        TRUE                                            ~ LIMIARES_FORMA[[4]]$id
      ),
      quadrante = dplyr::case_when(
        sem_base                       ~ "sem_base",
        x_val < 1  & taxa_cresc < 1    ~ "abaixo_estavel",
        x_val < 1  & taxa_cresc >= 1   ~ "abaixo_crescendo",
        x_val >= 1 & taxa_cresc < 1    ~ "acima_desacelerando",
        TRUE                           ~ "acima_crescendo"
      )
    )

  list(
    dados         = dados,
    ano_recente   = ano_recente,
    se_ref_semana = se_ref_semana,
    corrigido     = exists("NOWCAST_F"),
    atual_ini     = format(atual_ini, "%d/%m"),
    atual_fim     = format(atual_fim, "%d/%m"),
    ant_ini       = format(ant_ini, "%d/%m"),
    ant_fim       = format(ant_fim, "%d/%m")
  )
}
