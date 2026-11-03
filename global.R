# ==============================================================================
# Gripe RIDE | global.R
# Ambiente, dados (Parquets agregados do SIVEP-Gripe), dicionario geografico,
# agentes etiologicos, nowcast e helpers. Executado UMA vez, antes de ui.R e
# server.R.
#
# Diferenca de arquitetura para o painel de dengue: la o Parquet tem ~25 mil
# linhas por municipio/semana e as consultas descem ao DuckDB. Aqui o ETL ja
# entrega a base agregada (semana x municipio x agente x faixa etaria, ~60 mil
# linhas, < 1 MB) e ela fica inteira em memoria: todo filtro e um dplyr sobre
# um data.frame, sem banco no processo do app.
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(echarts4r)
library(DT)

if (dir.exists("app_cache") && file.access("app_cache", 2) == 0) {
  options(sass.cache = sass::sass_file_cache(file.path("app_cache", "sass")))
}

# ------------------------------------------------------------ fonte de dados --
PARQUET_SRAG   <- Sys.getenv("GRIPE_PARQUET", file.path("data", "srag_ride_df.parquet"))
PARQUET_ATRASO <- Sys.getenv("GRIPE_PARQUET_ATRASO", file.path("data", "srag_ride_atraso.parquet"))

for (.p in c(PARQUET_SRAG, PARQUET_ATRASO)) {
  if (!file.exists(.p)) {
    stop("Parquet nao encontrado em '", .p, "'. Rode `Rscript etl_gripe_ride.R` ",
         "(precisa do .Renviron com PG_*).", call. = FALSE)
  }
}
rm(.p)

DADOS <- arrow::read_parquet(PARQUET_SRAG) |>
  as.data.frame() |>
  dplyr::mutate(
    data_ini_se = as.Date(data_ini_se),
    se          = as.integer(se),
    ano         = se %/% 100L,
    semana      = se %%  100L,
    municipio_geocodigo = as.integer(municipio_geocodigo),
    uf_ibge     = as.integer(uf_ibge),
    dplyr::across(c(casos, obitos, uti, vacinado_gripe, coinfeccao), as.numeric)
  )

ATRASO <- arrow::read_parquet(PARQUET_ATRASO) |>
  as.data.frame() |>
  dplyr::mutate(data_ini_se = as.Date(data_ini_se))

ROTULO_BASE <- Sys.getenv("GRIPE_ROTULO_BASE", "RIDE-DF")

message(sprintf("[Gripe RIDE] Fonte: %s (%s casos de SRAG).",
                PARQUET_SRAG, format(sum(DADOS$casos), big.mark = ".", decimal.mark = ",")))

# ------------------------------------------------- dicionario geografico -----
UF_LOOKUP <- data.frame(
  uf_ibge  = c(31L, 52L, 53L),
  uf_sigla = c("MG", "GO", "DF"),
  uf_nome  = c("Minas Gerais", "Goiás", "Distrito Federal"),
  stringsAsFactors = FALSE
)

# Populacao por municipio e ano (o ETL repete o ultimo ano conhecido).
POP_MUN_ANO <- DADOS |>
  dplyr::distinct(municipio_geocodigo, ano, pop) |>
  dplyr::group_by(municipio_geocodigo, ano) |>
  dplyr::summarise(pop = max(pop, na.rm = TRUE), .groups = "drop")

GEO_DICT <- DADOS |>
  dplyr::distinct(uf_ibge, municipio_geocodigo, municipio_nome) |>
  dplyr::left_join(UF_LOOKUP, by = "uf_ibge") |>
  dplyr::arrange(uf_nome, municipio_nome)

UF_DISPONIVEIS <- GEO_DICT |> dplyr::distinct(uf_ibge, uf_sigla, uf_nome) |> dplyr::arrange(uf_nome)

# Regioes Administrativas do DF (o SIVEP registra a residencia por RA).
RA_DICT <- DADOS |>
  dplyr::filter(!is.na(ra_codigo)) |>
  dplyr::group_by(ra_codigo) |>
  dplyr::summarise(ra_nome = dplyr::first(stats::na.omit(ra_nome)), .groups = "drop") |>
  dplyr::mutate(ra_nome = tools::toTitleCase(tolower(ra_nome)))

# ------------------------------------------------- cobertura temporal --------
DATA_MIN <- min(DADOS$data_ini_se)
DATA_MAX <- max(DADOS$data_ini_se)
ANO_MIN  <- min(DADOS$ano)
ANO_MAX  <- max(DADOS$ano)

# ------------------------------------------------------ agentes etiologicos --
# Ordem = ordem de empilhamento nos graficos (de baixo para cima). Cores:
# familia violeta para influenza (o "dado" deste painel), vinho para covid,
# verde-agua para VSR, verdes/terrosos para os demais virus respiratorios e
# neutros para os casos sem agente definido - assim o que NAO tem
# diagnostico nunca compete visualmente com o que tem.
#
# Os "outros virus" (CLASSI_FIN = 2 sem VSR) vem quebrados pelo ETL, com
# hierarquia para coinfeccao (ver etl_gripe_ride.R). `grupo` agrega o que o
# KPI de agente predominante trata como um so (as tres influenzas).
AGENTES <- data.frame(
  agente = c("influenza_a", "influenza_b", "influenza_nd", "vsr", "covid",
             "rinovirus", "metapneumovirus", "adenovirus", "parainfluenza", "outros_virus",
             "outro_agente", "nao_especificado", "em_investigacao"),
  rotulo = c("Influenza A", "Influenza B", "Influenza (tipo n/i)", "VSR", "Covid-19",
             "Rinovírus", "Metapneumovírus", "Adenovírus", "Parainfluenza", "Outros vírus resp.",
             "Outro agente", "Não especificada", "Em investigação"),
  grupo  = c("influenza", "influenza", "influenza", "vsr", "covid",
             "rinovirus", "metapneumovirus", "adenovirus", "parainfluenza", "outros_virus",
             "outro", "nao_especificado", "em_investigacao"),
  cor    = c("#5B3F9E", "#9A80D0", "#7C66B8", "#1B9AAA", "#8E3B46",
             "#5A9E2E", "#B08A1E", "#2E5E8C", "#A3C962", "#7E9C8C",
             "#8C6D4F", "#B8C4CC", "#DDE5EA"),
  stringsAsFactors = FALSE
)
COR_AGENTE    <- stats::setNames(AGENTES$cor, AGENTES$agente)
ROTULO_AGENTE <- stats::setNames(AGENTES$rotulo, AGENTES$agente)
ROTULO_GRUPO  <- c(influenza = "Influenza", vsr = "VSR", covid = "Covid-19",
                   rinovirus = "Rinovírus", metapneumovirus = "Metapneumovírus",
                   adenovirus = "Adenovírus", parainfluenza = "Parainfluenza",
                   outros_virus = "Outros vírus", outro = "Outro agente")

# Virus respiratorios que nao sao influenza, VSR nem covid (CLASSI_FIN = 2).
OUTROS_VIRUS <- c("rinovirus", "metapneumovirus", "adenovirus", "parainfluenza", "outros_virus")

# Opcoes do seletor "Agente" (rotulo -> chave). Cada chave expande para um
# conjunto de agentes em agentes_do_filtro().
FILTROS_AGENTE <- c(
  "Toda SRAG"               = "todos",
  "Influenza (A + B)"       = "influenza",
  "Influenza A"             = "influenza_a",
  "Influenza B"             = "influenza_b",
  "VSR"                     = "vsr",
  "Covid-19"                = "covid",
  "Outros vírus (todos)"    = "outros_virus_todos",
  "Rinovírus"               = "rinovirus",
  "Metapneumovírus"         = "metapneumovirus",
  "Adenovírus"              = "adenovirus",
  "Parainfluenza"           = "parainfluenza",
  "SRAG não especificada"   = "nao_especificado"
)

agentes_do_filtro <- function(chave) {
  switch(
    chave,
    todos              = AGENTES$agente,
    influenza          = c("influenza_a", "influenza_b", "influenza_nd"),
    outros_virus_todos = OUTROS_VIRUS,
    chave
  )
}

rotulo_filtro_agente <- function(chave) names(FILTROS_AGENTE)[FILTROS_AGENTE == chave]

FAIXAS <- c("0-1", "2-4", "5-14", "15-49", "50-64", "65+")
ROTULO_FAIXA <- c(`0-1` = "< 2 anos", `2-4` = "2–4", `5-14` = "5–14",
                  `15-49` = "15–49", `50-64` = "50–64", `65+` = "65+", ign = "Ignorada")

# ------------------------------------------------------------------ paleta --
# Mesma interface dos paineis de dengue (azul Cenarios, Poppins/DM Sans). A
# rampa de magnitude troca a terracota da dengue por um violeta, cor desta
# doenca no painel - ColorBrewer BuPu, sequencial e segura para daltonismo.
TONS_AZUL <- c(
  `50`  = "#ECF7FE", `100` = "#CFE9F9", `200` = "#A8D5F1", `300` = "#7ABEE6",
  `400` = "#4FA4D3", `500` = "#3891C0", `600` = "#0E7BA8", `700` = "#00638C",
  `800` = "#004565", `900` = "#00324C"
)

TONS_GRIPE <- c(
  `50`  = "#F4F1FA", `100` = "#E3DCF2", `200` = "#C9BCE6", `300` = "#AC98D8",
  `400` = "#8F76C8", `500` = "#7459B4", `600` = "#5B3F9E", `700` = "#472E83",
  `800` = "#352066", `900` = "#24154A"
)

PALETA <- c(
  fundo       = "#F2F6F9",
  superficie  = "#FFFFFF",
  linha       = "#D8E3EA",
  tinta       = "#12222B",
  texto       = "#4A4A4A",
  sub         = "#7A8B95",
  neutro      = "#B8C4CC",
  azul        = TONS_AZUL[["500"]],
  azul_acao   = TONS_AZUL[["600"]],
  azul_titulo = TONS_AZUL[["700"]],
  gripe       = TONS_GRIPE[["600"]],
  gripe_forte = TONS_GRIPE[["800"]],
  nowcast     = "#E03131",
  ok          = "#17895B",
  atencao     = "#DCA744",
  grave       = "#C65D26",
  critico     = "#8F2639"
)

ESCALA_MAPA <- c("#EDF8FB", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C")

FONTE_GRAFICO <- "'DM Sans', system-ui, sans-serif"

cor_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], alpha)
}

# ------------------------------------------------------------------- malhas --
# GeoJSON do IBGE (mesmos arquivos do painel de dengue) recortados aos
# municipios da base. Parse uma vez no start.
GEO_DIR <- file.path("data", "geo")

MALHA_RIDE <- local({
  codigos <- as.character(GEO_DICT$municipio_geocodigo)
  feicoes <- list()
  for (uf in sort(unique(GEO_DICT$uf_ibge))) {
    arq <- file.path(GEO_DIR, sprintf("MUN_%d.geojson", uf))
    if (!file.exists(arq)) next
    js <- jsonlite::fromJSON(arq, simplifyVector = FALSE)
    feicoes <- c(feicoes, Filter(function(f) as.character(f$properties$codarea) %in% codigos,
                                 js$features))
  }
  list(type = "FeatureCollection", features = feicoes)
})

message(sprintf("[Gripe RIDE] Malha: %d de %d municípios.",
                length(MALHA_RIDE$features), nrow(GEO_DICT)))

# Contorno das UFs e da RIDE (gerado por prep/gerar_contorno.R), juntado ao
# FIM da malha do mapa: as feicoes extras sao desenhadas por ultimo, por cima
# dos municipios, e por serem parte do mesmo mapa acompanham zoom e arraste.
# Opcional: sem o arquivo, o mapa sai so com as bordas cinza dos municipios.
CONTORNO_RIDE <- local({
  arq <- file.path(GEO_DIR, "contorno_ride.geojson")
  if (!file.exists(arq)) return(list())
  Filter(function(f) is.character(f$properties$codarea),
         jsonlite::fromJSON(arq, simplifyVector = FALSE)$features)
})
CODIGOS_CONTORNO <- vapply(CONTORNO_RIDE, function(f) f$properties$codarea, "")
MALHA_MAPA <- list(type = "FeatureCollection",
                   features = c(MALHA_RIDE$features, CONTORNO_RIDE))

# ------------------------------------------------------------------ nowcast --
# O SIVEP traz a data de digitacao de cada caso, entao o atraso de
# notificacao e observavel (o InfoDengue do painel de dengue nao oferece
# isso). Metodo: "chain ladder" empirico.
#
#   F(k) = fracao dos casos de uma semana que ja estavam digitados k semanas
#          depois do inicio dela, medida nas semanas ja maduras (>= 26
#          semanas) das ultimas 104 semanas maduras.
#   estimado(semana) = observado / F(k), k = idade da semana na data do
#          arquivo.
#
# Intervalo: quantis 10% e 90% de F(k) entre as semanas de treino (a
# variabilidade real do atraso, nao um erro de modelo). Um F(k) baixo
# (atraso grande) gera a estimativa ALTA, entao o quantil 10% de F da o
# limite superior.
#
# O triangulo e do total da RIDE-DF; o mesmo F(k) vale para qualquer recorte
# e agente - aproximacao razoavel (o atraso e do sistema de notificacao), mas
# um municipio pequeno com padrao de digitacao proprio pode destoar.
NOWCAST_MAX_K <- 8L   # semanas mais recentes que recebem correcao

DATA_REF_ARQUIVO <- max(ATRASO$data_ini_se + 7L * ATRASO$atraso_sem)

NOWCAST_F <- local({
  maduras <- ATRASO |>
    dplyr::filter(data_ini_se <= DATA_REF_ARQUIVO - 26L * 7L) |>
    dplyr::filter(data_ini_se >  max(data_ini_se) - 104L * 7L)

  por_semana <- maduras |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(
      total = sum(casos),
      cum   = list(vapply(0:NOWCAST_MAX_K, function(k) sum(casos[atraso_sem <= k]), numeric(1))),
      .groups = "drop"
    ) |>
    dplyr::filter(total >= 20)

  mat <- do.call(rbind, por_semana$cum) / por_semana$total
  data.frame(
    k     = 0:NOWCAST_MAX_K,
    f     = colSums(do.call(rbind, por_semana$cum)) / sum(por_semana$total),
    f_q10 = apply(mat, 2, stats::quantile, 0.10),
    f_q90 = apply(mat, 2, stats::quantile, 0.90)
  )
})

message(sprintf("[Gripe RIDE] Nowcast: arquivo ate %s; F(1)=%.0f%%, F(4)=%.0f%%.",
                format(DATA_REF_ARQUIVO, "%d/%m/%Y"),
                100 * NOWCAST_F$f[2], 100 * NOWCAST_F$f[5]))

#' Aplica o nowcast a uma serie semanal (colunas data_ini_se, casos).
#' Acrescenta casos_est, casos_est_min, casos_est_max (NA fora da janela).
aplicar_nowcast <- function(d) {
  k <- as.integer((DATA_REF_ARQUIVO - d$data_ini_se) / 7)
  i <- match(k, NOWCAST_F$k)
  d$casos_est     <- ifelse(is.na(i), NA_real_, d$casos / NOWCAST_F$f[i])
  d$casos_est_min <- ifelse(is.na(i), NA_real_, d$casos / NOWCAST_F$f_q90[i])
  d$casos_est_max <- ifelse(is.na(i), NA_real_, d$casos / NOWCAST_F$f_q10[i])
  d
}

# ----------------------------------------------------------------- helpers ---

#' Filtra a base pelo recorte geografico e pelo conjunto de agentes.
filtrar <- function(geo, agentes = AGENTES$agente) {
  d <- DADOS[DADOS$agente %in% agentes, , drop = FALSE]
  switch(
    geo$nivel,
    base      = d,
    uf        = d[d$uf_ibge == geo$uf_ibge, , drop = FALSE],
    municipio = d[d$municipio_geocodigo == geo$geocodigo, , drop = FALSE],
    stop("Nivel geografico desconhecido: ", geo$nivel, call. = FALSE)
  )
}

#' Populacao do recorte por ano (soma dos municipios).
pop_recorte <- function(geo) {
  mun <- switch(
    geo$nivel,
    base      = GEO_DICT$municipio_geocodigo,
    uf        = GEO_DICT$municipio_geocodigo[GEO_DICT$uf_ibge == geo$uf_ibge],
    municipio = geo$geocodigo
  )
  POP_MUN_ANO |>
    dplyr::filter(municipio_geocodigo %in% mun) |>
    dplyr::group_by(ano) |>
    dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
}

#' Grade completa de semanas (sem buraco quando nao ha caso).
SEMANAS <- data.frame(data_ini_se = seq(DATA_MIN, DATA_MAX, by = 7)) |>
  dplyr::mutate(se = DADOS$se[match(data_ini_se, DADOS$data_ini_se)])
SEMANAS$se[is.na(SEMANAS$se)] <- local({
  # semanas sem nenhum caso na base inteira (raro): SE pela quarta-feira
  dd <- SEMANAS$data_ini_se[is.na(SEMANAS$se)]
  ano <- as.integer(format(dd + 3L, "%Y"))
  jan4 <- as.Date(sprintf("%d-01-04", ano))
  ano * 100L + as.integer((dd - (jan4 - as.integer(format(jan4, "%w")))) / 7) + 1L
})

#' Serie semanal (total do recorte e agentes), com incidencia e nowcast.
serie_semanal <- function(geo, agentes) {
  d <- filtrar(geo, agentes) |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(casos = sum(casos), obitos = sum(obitos), uti = sum(uti),
                     .groups = "drop")
  SEMANAS |>
    dplyr::left_join(d, by = "data_ini_se") |>
    dplyr::mutate(dplyr::across(c(casos, obitos, uti), ~ dplyr::coalesce(.x, 0)),
                  ano = se %/% 100L, semana = se %% 100L) |>
    dplyr::left_join(pop_recorte(geo), by = "ano") |>
    dplyr::mutate(incidencia = casos / pop * 1e5) |>
    aplicar_nowcast()
}

# ----------------------------------------------------------------------- Rt --
# Nao ha Rt pronto para SRAG no banco (o InfoDengue so cobre arboviroses),
# entao o painel estima o seu, a partir da serie semanal do recorte+agente:
#
#   1. r = taxa de crescimento diaria entre duas janelas de 2 semanas:
#        r = ln(S_t / S_{t-2}) / 14,   S_t = casos(t) + casos(t-1)
#      (2 semanas suavizam o ruido de uma semana isolada).
#   2. Rt = (1 + r * theta)^k  (Wallinga & Lipsitch, 2007), com o intervalo
#      de geracao gama de media m e desvio s: k = (m/s)^2, theta = s^2/m.
#   3. IC 95%: erro de Poisson das contagens, ep(ln S_t/S_{t-2}) =
#      sqrt(1/S_t + 1/S_{t-2}), propagado por r.
#
# As semanas recentes usam os casos corrigidos pelo nowcast; o Rt delas e
# preliminar (linha tracejada). A semana corrente fica de fora: so uma fracao
# pequena dela esta digitada. Janela com menos de RT_MIN_CASOS casos no total
# fica sem estimativa.
#
# SRAG sao casos GRAVES: o Rt mede o crescimento das internacoes, um
# indicador atrasado e parcial da transmissao na comunidade.
RT_MIN_CASOS <- 20

# Intervalo de geracao (dias) por agente. Media/desvio da literatura:
# influenza 3,6/1,6 (Cowling et al., 2009); covid-19 Omicron ~3,5/2,4;
# VSR ~7,5/2,1 (Crowcroft et al., 2008). Mistura de agentes ou outros virus:
# valor generico de virus respiratorio, 4,5/2,5.
RT_INTERVALO_GERACAO <- list(
  influenza   = c(media = 3.6, dp = 1.6),
  influenza_a = c(media = 3.6, dp = 1.6),
  influenza_b = c(media = 3.6, dp = 1.6),
  covid       = c(media = 3.5, dp = 2.4),
  vsr         = c(media = 7.5, dp = 2.1),
  generico    = c(media = 4.5, dp = 2.5)
)

intervalo_geracao <- function(chave) RT_INTERVALO_GERACAO[[chave]] %||% RT_INTERVALO_GERACAO$generico

#' Rt semanal de uma serie (saida de serie_semanal). Acrescenta rt, rt_min,
#' rt_max, rt_casos (casos nas duas janelas) e rt_prelim (usa nowcast).
estimar_rt <- function(s, chave_agente) {
  ig    <- intervalo_geracao(chave_agente)
  k     <- (ig[["media"]] / ig[["dp"]])^2
  theta <- ig[["dp"]]^2 / ig[["media"]]

  n   <- nrow(s)
  inc <- dplyr::coalesce(s$casos_est, s$casos)
  inc[n] <- NA_real_                       # semana corrente: incompleta demais
  S   <- inc + dplyr::lag(inc)             # janela de 2 semanas terminando em t
  S0  <- dplyr::lag(S, 2)                  # as 2 semanas anteriores
  ok  <- !is.na(S) & !is.na(S0) & S > 0 & S0 > 0 & (S + S0) >= RT_MIN_CASOS

  para_rt <- function(lr) {
    base <- 1 + (lr / 14) * theta
    ifelse(base > 0, base^k, NA_real_)
  }
  lr <- ifelse(ok, log(S / S0), NA_real_)
  ep <- ifelse(ok, sqrt(1 / S + 1 / S0), NA_real_)

  s$rt        <- para_rt(lr)
  s$rt_min    <- para_rt(lr - 1.96 * ep)
  s$rt_max    <- para_rt(lr + 1.96 * ep)
  s$rt_casos  <- ifelse(ok, S + S0, NA_real_)
  # preliminar = a janela (t-1, t) inclui semana corrigida pelo nowcast
  com_nc      <- !is.na(s$casos_est)
  s$rt_prelim <- com_nc | dplyr::lag(com_nc, default = FALSE)
  s
}

#' Expressao JS que formata "AAAA-MM-DD" como "dd/mm/aaaa" (sem new Date,
#' que interpreta como UTC e volta um dia em UTC-3).
JS_DATA_BR <- "(function(s){var p=String(s).split('-');return p[2]+'/'+p[1]+'/'+p[0];})"

fmt_int <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x), digits), big.mark = ".", decimal.mark = ",",
         nsmall = digits, scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x)) || !is.finite(x)) return("-")
  paste0(fmt_num(100 * x, digits), "%")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --------------------------------------------------------- modulos do app ----
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)
