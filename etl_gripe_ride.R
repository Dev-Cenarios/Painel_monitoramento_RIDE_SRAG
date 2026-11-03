# ==============================================================================
# Gripe RIDE | etl_gripe_ride.R
# Extrai do PostgreSQL ("Info_Gripe".srag, microdado SIVEP-Gripe/OpenDataSUS)
# os casos de SRAG de residentes nos 34 municipios da RIDE-DF e grava dois
# Parquets JA AGREGADOS, unica fonte de dados lida pelo app:
#
#   data/srag_ride_df.parquet    semana x municipio (x RA no DF) x agente x
#                                faixa etaria -> casos, obitos, UTI, vacinados
#   data/srag_ride_atraso.parquet semana de sintomas x atraso de digitacao
#                                (semanas) -> casos. Base do nowcast.
#
# Nenhuma linha de microdado sai do banco: toda a agregacao roda no Postgres.
#
# Uso (working directory = raiz do projeto):  Rscript etl_gripe_ride.R
#
# Particularidades do SIVEP-Gripe que o SQL abaixo trata:
#   - TODAS as colunas sao texto, e o mesmo codigo aparece como "1" ou "1.0"
#     conforme o ano do arquivo -> split_part(x, '.', 1).
#   - Datas como texto ISO ("2020-05-15T00:00:00.000Z") -> substr(1,10)::date.
#   - Municipio com 6 digitos (sem verificador). No DF a residencia vem por
#     REGIAO ADMINISTRATIVA (530010 Plano Piloto, 530040 Ceilandia, ...), nao
#     so 530010: todo 53xxxx vira o municipio 5300108 e o codigo da RA fica em
#     `ra_codigo`. Filtrar so 530010 perderia ~99% dos casos do DF.
#   - Semana epidemiologica pela data de inicio dos sintomas (dt_sin_pri),
#     domingo a sabado (calendario do MS), calculada em R a partir do domingo.
# ==============================================================================

library(DBI)
library(RPostgres)
library(arrow)
library(dplyr)

if (file.exists(".Renviron")) readRenviron(".Renviron")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("PG_HOST"),
  port     = as.integer(Sys.getenv("PG_PORT", "5432")),
  dbname   = Sys.getenv("PG_DB"),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASS"),  # credenciais no .Renviron (ver .Renviron.example)
  bigint   = "numeric"
)

# Data minima de inicio de sintomas. 2019 e o primeiro arquivo da base; antes
# disso so ha casos de dez/2018 que entraram no arquivo de 2019.
DATA_MIN <- Sys.getenv("GRIPE_DATA_MIN", "2018-12-30")

# Codigos IBGE oficiais da RIDE-DF (LC 163/2018) - 34 municipios. Mesma lista
# do etl_ride.R do painel de dengue.
ride_codes_oficiais <- c(
  5300108,                                   # Distrito Federal (1)
  3104502, 3109303, 3109451, 3170404,        # Minas Gerais (4)
  5200100, 5200175, 5200258, 5200308, 5200605, 5200803, 5203203, 5204003,
  5205307, 5205497, 5205513, 5205802, 5206206, 5207907, 5208004, 5208608,
  5212501, 5213053, 5214606, 5215231, 5215603, 5217302, 5217609, 5219753,
  5220009, 5220686, 5221858, 5222203, 5222302  # Goias (29)
)
mun6 <- substr(ride_codes_oficiais, 1, 6)
filtro_mun <- sprintf(
  "(co_mun_res IN (%s) OR co_mun_res LIKE '53%%')",
  paste0("'", mun6, "'", collapse = ", ")
)

# ------------------------------------------------------------- base comum --
# CTE reaproveitada pelas duas consultas. Classificacao do agente:
#   classi_fin 1 = influenza (tipo A/B por RT-PCR, senao antigeno)
#              2 = outro virus respiratorio, quebrado pelo resultado do
#                  painel de PCR/antigeno. ~1/6 desses casos tem 2+ virus
#                  detectados, entao vale a PRIMEIRA deteccao nesta ordem:
#                  VSR > metapneumovirus > adenovirus > parainfluenza 1-4 >
#                  rinovirus > outros (bocavirus, enterovirus, coronavirus
#                  sazonais...). O rinovirus vem por ultimo de proposito: e
#                  o mais detectado e o mais frequente como co-deteccao de
#                  baixa carga. A coinfeccao fica contada em `coinfeccao`.
#              3 = outro agente etiologico
#              4 = SRAG nao especificada
#              5 = covid-19
#              vazio = em investigacao (caso ainda nao encerrado)
sql_base <- sprintf("
WITH base AS (
  SELECT
    substr(dt_sin_pri, 1, 10)::date                       AS dt_sin,
    substr(NULLIF(dt_digita, ''), 1, 10)::date            AS dt_dig,
    co_mun_res,
    id_mn_resi,
    split_part(classi_fin, '.', 1)                        AS cf,
    split_part(tp_flu_pcr, '.', 1)                        AS flu_pcr,
    split_part(tp_flu_an,  '.', 1)                        AS flu_an,
    split_part(pcr_vsr,    '.', 1)                        AS vsr_pcr,
    split_part(an_vsr,     '.', 1)                        AS vsr_an,
    (split_part(pcr_metap, '.', 1) = '1')::int            AS v_metap,
    (split_part(pcr_adeno, '.', 1) = '1' OR split_part(an_adeno, '.', 1) = '1')::int AS v_adeno,
    (split_part(pcr_para1, '.', 1) = '1' OR split_part(pcr_para2, '.', 1) = '1' OR
     split_part(pcr_para3, '.', 1) = '1' OR split_part(pcr_para4, '.', 1) = '1' OR
     split_part(an_para1,  '.', 1) = '1' OR split_part(an_para2,  '.', 1) = '1' OR
     split_part(an_para3,  '.', 1) = '1')::int            AS v_para,
    (split_part(pcr_rino,  '.', 1) = '1')::int            AS v_rino,
    (split_part(pcr_boca,  '.', 1) = '1' OR split_part(pcr_outro, '.', 1) = '1' OR
     split_part(an_outro,  '.', 1) = '1')::int            AS v_outro,
    split_part(evolucao,   '.', 1)                        AS evol,
    split_part(uti,        '.', 1)                        AS uti_,
    split_part(vacina,     '.', 1)                        AS vac,
    split_part(tp_idade,   '.', 1)                        AS tp_id,
    NULLIF(split_part(nu_idade_n, '.', 1), '')            AS idade_txt
  FROM \"Info_Gripe\".srag
  WHERE %s
    AND dt_sin_pri <> ''
    AND substr(dt_sin_pri, 1, 10) >= '%s'
),
classif AS (
  SELECT
    dt_sin - EXTRACT(DOW FROM dt_sin)::int                AS data_ini_se,
    dt_dig,
    CASE WHEN co_mun_res LIKE '53%%' THEN '530010' ELSE co_mun_res END AS mun6,
    CASE WHEN co_mun_res LIKE '53%%' THEN co_mun_res END  AS ra_codigo,
    CASE WHEN co_mun_res LIKE '53%%' THEN id_mn_resi END  AS ra_nome,
    CASE
      WHEN cf = '1' AND (flu_pcr = '1' OR (flu_pcr NOT IN ('1','2') AND flu_an = '1')) THEN 'influenza_a'
      WHEN cf = '1' AND (flu_pcr = '2' OR (flu_pcr NOT IN ('1','2') AND flu_an = '2')) THEN 'influenza_b'
      WHEN cf = '1' THEN 'influenza_nd'
      WHEN cf = '5' THEN 'covid'
      WHEN cf = '2' AND (vsr_pcr = '1' OR vsr_an = '1') THEN 'vsr'
      WHEN cf = '2' AND v_metap = 1 THEN 'metapneumovirus'
      WHEN cf = '2' AND v_adeno = 1 THEN 'adenovirus'
      WHEN cf = '2' AND v_para  = 1 THEN 'parainfluenza'
      WHEN cf = '2' AND v_rino  = 1 THEN 'rinovirus'
      WHEN cf = '2' THEN 'outros_virus'
      WHEN cf = '3' THEN 'outro_agente'
      WHEN cf = '4' THEN 'nao_especificado'
      ELSE 'em_investigacao'
    END                                                   AS agente,
    CASE
      WHEN idade_txt IS NULL OR idade_txt !~ '^[0-9]+$' THEN 'ign'
      WHEN tp_id IN ('1','2') THEN '0-1'
      WHEN idade_txt::int < 2   THEN '0-1'
      WHEN idade_txt::int < 5   THEN '2-4'
      WHEN idade_txt::int < 15  THEN '5-14'
      WHEN idade_txt::int < 50  THEN '15-49'
      WHEN idade_txt::int < 65  THEN '50-64'
      ELSE '65+'
    END                                                   AS faixa_etaria,
    (evol = '2')::int                                     AS obito,
    (uti_ = '1')::int                                     AS uti,
    (vac  = '1')::int                                     AS vacinado_gripe,
    (cf = '2' AND ((vsr_pcr = '1' OR vsr_an = '1')::int + v_metap + v_adeno + v_para +
                   v_rino + v_outro) >= 2)::int           AS coinfeccao
  FROM base
)
", filtro_mun, DATA_MIN)

# ------------------------------------------------------ 1. serie agregada --
message("[ETL Gripe] Agregando SRAG da RIDE-DF no Postgres...")
t0 <- Sys.time()
agg <- DBI::dbGetQuery(con, paste0(sql_base, "
SELECT data_ini_se, mun6, ra_codigo, max(ra_nome) AS ra_nome, agente, faixa_etaria,
       count(*)             AS casos,
       sum(obito)           AS obitos,
       sum(uti)             AS uti,
       sum(vacinado_gripe)  AS vacinado_gripe,
       sum(coinfeccao)      AS coinfeccao
FROM classif
GROUP BY data_ini_se, mun6, ra_codigo, agente, faixa_etaria
"))
message(sprintf("[ETL Gripe] %d linhas agregadas (%s casos) em %.1fs.",
                nrow(agg), format(sum(agg$casos), big.mark = ".", decimal.mark = ","),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# ------------------------------------------------- 2. triangulo de atraso --
# Casos por semana de sintomas x atraso ate a digitacao no SIVEP (semanas).
# So o total da RIDE-DF: o atraso e uma propriedade do sistema de notificacao,
# e recortar por municipio deixaria o triangulo esparso demais.
message("[ETL Gripe] Montando triangulo de atraso...")
atraso <- DBI::dbGetQuery(con, paste0(sql_base, "
SELECT data_ini_se,
       LEAST(GREATEST(floor((dt_dig - data_ini_se) / 7.0)::int, 0), 52) AS atraso_sem,
       count(*) AS casos
FROM classif
WHERE dt_dig IS NOT NULL
GROUP BY 1, 2
"))

# -------------------------------------------- 3. populacao (InfoDengue) --
# O SIVEP nao traz populacao. A mesma estimativa anual do IBGE que o painel de
# dengue usa ja esta no banco, em "Info_Dengue".dengue - um valor por
# municipio e ano. Anos sem estimativa repetem o ultimo ano disponivel.
message("[ETL Gripe] Lendo populacao de Info_Dengue...")
pop <- DBI::dbGetQuery(con, sprintf("
SELECT municipio_geocodigo, municipio_nome, uf_ibge,
       EXTRACT(YEAR FROM data_ini_se)::int AS ano, max(pop) AS pop
FROM \"Info_Dengue\".dengue
WHERE municipio_geocodigo IN (%s)
GROUP BY 1, 2, 3, 4", paste(ride_codes_oficiais, collapse = ", ")))

# ----------------------------------------------- semana epidemiologica (MS) --
#' Semana epidemiologica a partir do domingo que a inicia.
#' A SE 1 de um ano e a semana (dom-sab) que contem o dia 4 de janeiro, ou
#' seja, o ano epidemiologico de uma semana e o ano da sua quarta-feira.
se_de_domingo <- function(domingo) {
  ano <- as.integer(format(domingo + 3L, "%Y"))
  jan4 <- as.Date(sprintf("%d-01-04", ano))
  inicio <- jan4 - as.integer(format(jan4, "%w"))
  semana <- as.integer((domingo - inicio) / 7) + 1L
  ano * 100L + semana
}

agg <- agg |>
  mutate(
    data_ini_se = as.Date(data_ini_se),
    se          = se_de_domingo(data_ini_se),
    across(c(casos, obitos, uti, vacinado_gripe, coinfeccao), as.integer)
  )
agg$municipio_geocodigo <- ride_codes_oficiais[match(agg$mun6, mun6)]

pop_full <- pop |>
  mutate(municipio_geocodigo = as.integer(municipio_geocodigo)) |>
  group_by(municipio_geocodigo) |>
  tidyr::complete(ano = seq(min(ano), max(as.integer(format(max(agg$data_ini_se), "%Y")), max(ano)))) |>
  tidyr::fill(pop, municipio_nome, uf_ibge, .direction = "downup") |>
  ungroup()

agg <- agg |>
  mutate(ano_pop = as.integer(format(data_ini_se + 3L, "%Y"))) |>
  left_join(pop_full, by = c("municipio_geocodigo", "ano_pop" = "ano")) |>
  transmute(
    data_ini_se, se,
    municipio_geocodigo = as.integer(municipio_geocodigo),
    municipio_nome, uf_ibge = as.integer(uf_ibge),
    ra_codigo = as.integer(ra_codigo), ra_nome,
    agente, faixa_etaria,
    casos, obitos, uti, vacinado_gripe, coinfeccao,
    pop = as.numeric(pop)
  ) |>
  arrange(data_ini_se, municipio_geocodigo, agente, faixa_etaria)

sem_pop <- unique(agg$municipio_geocodigo[is.na(agg$pop)])
if (length(sem_pop)) warning("[ETL Gripe] Municipios sem populacao: ", paste(sem_pop, collapse = ", "))

atraso <- atraso |>
  mutate(data_ini_se = as.Date(data_ini_se),
         se = se_de_domingo(data_ini_se),
         atraso_sem = as.integer(atraso_sem),
         casos = as.integer(casos)) |>
  arrange(data_ini_se, atraso_sem)

DBI::dbDisconnect(con)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
arrow::write_parquet(agg, "data/srag_ride_df.parquet")
arrow::write_parquet(atraso, "data/srag_ride_atraso.parquet")

message(sprintf(
  "[ETL Gripe] OK: %s casos de SRAG, %s a %s, %d municipios. Parquets em data/.",
  format(sum(agg$casos), big.mark = ".", decimal.mark = ","),
  format(min(agg$data_ini_se), "%d/%m/%Y"), format(max(agg$data_ini_se), "%d/%m/%Y"),
  dplyr::n_distinct(agg$municipio_geocodigo)
))
