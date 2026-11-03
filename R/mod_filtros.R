# ==============================================================================
# Gripe RIDE | Modulo: filtros globais (recorte geografico + agente)
#
# Contrato de saida: lista de dois reactives
#   $geo     list(nivel = "base"|"uf"|"municipio", uf_ibge, geocodigo, rotulo)
#   $agente  chave de FILTROS_AGENTE ("todos", "influenza", ...)
#
# Entrada `foco_externo`: reactive com o geocodigo clicado no mapa (ou NA
# para voltar a visao geral). E o unico jeito de outro modulo mexer no
# recorte - o estado continua morando nos inputs deste modulo.
# ==============================================================================

mod_filtros_ui <- function(id) {
  ns <- shiny::NS(id)

  mun_choices <- lapply(split(GEO_DICT, GEO_DICT$uf_nome), function(g) {
    stats::setNames(as.character(g$municipio_geocodigo), g$municipio_nome)
  })

  bslib::layout_column_wrap(
    width = 1/4,
    gap   = "1rem",
    shiny::radioButtons(
      ns("nivel"), "Abrangência",
      choices  = c(stats::setNames("base", ROTULO_BASE), "Estado" = "uf", "Município" = "municipio"),
      selected = "base", inline = TRUE
    ),
    shiny::selectizeInput(
      ns("uf"), "Estado",
      choices  = stats::setNames(as.character(UF_DISPONIVEIS$uf_ibge),
                                 sprintf("%s (%s)", UF_DISPONIVEIS$uf_nome, UF_DISPONIVEIS$uf_sigla)),
      selected = "53", width = "100%",
      options  = list(dropdownParent = "body")
    ),
    shiny::selectizeInput(
      ns("municipio"), "Município",
      choices  = mun_choices, selected = "5300108", width = "100%",
      options  = list(placeholder = "Digite para buscar...", dropdownParent = "body")
    ),
    shiny::selectizeInput(
      ns("agente"), "Agente etiológico",
      choices = FILTROS_AGENTE, selected = "todos", width = "100%",
      options = list(dropdownParent = "body")
    )
  )
}

mod_filtros_resumo <- function(id) {
  shiny::textOutput(shiny::NS(id)("resumo"), inline = TRUE)
}

mod_filtros_server <- function(id, foco_externo = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    # Escolher um municipio no seletor leva a abrangencia para "Municipio";
    # escolher uma UF leva para "Estado". ignoreInit: a selecao inicial nao
    # tira o painel da visao geral.
    shiny::observeEvent(input$municipio, {
      if (!identical(input$nivel, "municipio")) {
        shiny::updateRadioButtons(session, "nivel", selected = "municipio")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$uf, {
      if (!identical(input$nivel, "uf")) {
        shiny::updateRadioButtons(session, "nivel", selected = "uf")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(foco_externo(), {
      cod <- foco_externo()$geocodigo
      if (is.na(cod)) {
        shiny::updateRadioButtons(session, "nivel", selected = "base")
      } else {
        shiny::updateSelectizeInput(session, "municipio", selected = as.character(cod))
        shiny::updateRadioButtons(session, "nivel", selected = "municipio")
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    geo <- shiny::reactive({
      shiny::req(input$nivel)
      switch(
        input$nivel,
        base = list(nivel = "base", uf_ibge = NA_integer_, geocodigo = NA_integer_,
                    rotulo = ROTULO_BASE),
        uf = {
          uf <- as.integer(input$uf)
          list(nivel = "uf", uf_ibge = uf, geocodigo = NA_integer_,
               rotulo = UF_DISPONIVEIS$uf_nome[UF_DISPONIVEIS$uf_ibge == uf])
        },
        municipio = {
          cod <- as.integer(input$municipio)
          g <- GEO_DICT[GEO_DICT$municipio_geocodigo == cod, ]
          list(nivel = "municipio", uf_ibge = g$uf_ibge, geocodigo = cod,
               rotulo = sprintf("%s (%s)", g$municipio_nome, g$uf_sigla))
        }
      )
    })

    agente <- shiny::reactive(input$agente %||% "todos")

    output$resumo <- shiny::renderText({
      sprintf("%s · %s", geo()$rotulo, rotulo_filtro_agente(agente()))
    })

    list(geo = geo, agente = agente)
  })
}
