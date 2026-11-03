# Gripe RIDE — Painel de monitoramento de SRAG e vírus respiratórios

Painel Shiny (bslib + echarts4r) irmão do **dengue_ride**, com os casos de
Síndrome Respiratória Aguda Grave (SRAG) de residentes nos 34 municípios da
RIDE-DF. Fonte: microdado do SIVEP-Gripe (OpenDataSUS), tabela
`"Info_Gripe".srag` do banco do projeto.

## 1. Estrutura

```
gripe_ride/
├── etl_gripe_ride.R     # Postgres -> data/*.parquet (tudo agregado no banco)
├── global.R             # dados em memória, agentes, paleta, malha, nowcast
├── ui.R / server.R
├── R/
│   ├── mod_filtros.R    # recorte (RIDE-DF / Estado / Município) + agente
│   ├── mod_kpi.R        # 5 indicadores do topo
│   ├── mod_mapa.R       # incidência por município (clique = foco)
│   ├── mod_serie.R      # casos semanais empilhados por agente + nowcast
│   ├── mod_rt.R         # Rt estimado pelo painel (sem Rt pronto no banco)
│   ├── mod_canal.R      # canal endêmico (ano calendário)
│   ├── mod_heatmap.R    # sazonalidade ano x SE
│   ├── mod_perfil.R     # faixa etária x agente; letalidade/UTI por faixa
│   ├── mod_analises.R   # ranking, DF por RA, achados calculados da base
│   └── utils_ui.R       # tema e cards
└── data/
    ├── srag_ride_df.parquet      # semana x município (x RA) x agente x faixa
    ├── srag_ride_atraso.parquet  # semana x atraso de digitação (nowcast)
    └── geo/MUN_{31,52,53}.geojson
```

## 2. Atualizar os dados

Precisa do `.Renviron` com `PG_HOST`, `PG_PORT`, `PG_DB`, `PG_USER`, `PG_PASS`
(o mesmo dos outros painéis; ver `.Renviron.example`).

```bash
cd /srv/shiny-server/gripe_ride && Rscript etl_gripe_ride.R && touch restart.txt
```

O ETL leva poucos segundos (o filtro por município roda no Postgres). Os
Parquets **não são versionados** (`.gitignore`): no servidor, rode o ETL
depois do primeiro `git pull`.

## 3. Decisões de tratamento

| Tema | Decisão |
|---|---|
| Tipos | Todas as colunas da tabela são texto, e o mesmo código aparece como `1` ou `1.0` conforme o ano → `split_part(x, '.', 1)`. |
| Residência no DF | O SIVEP registra o DF por **Região Administrativa** (530040 Ceilândia, 530180 Taguatinga…). Filtrar só `530010` perderia ~99% dos casos. Todo `53xxxx` vira o município 5300108; a RA fica em `ra_codigo`. |
| Semana | Semana epidemiológica (dom–sáb) do início dos sintomas, calculada no ETL. |
| Agente | `CLASSI_FIN`: 1 influenza (A/B pelo `TP_FLU_PCR`, senão `TP_FLU_AN`), 2 outro vírus — quebrado por PCR/antígeno em VSR, metapneumovírus, adenovírus, parainfluenza, rinovírus e outros (hierarquia nessa ordem para coinfecção), 3 outro agente, 4 não especificada, 5 covid-19, vazio = em investigação. |
| População | O SIVEP não traz; usa a estimativa anual do IBGE já presente em `"Info_Dengue".dengue`. |
| Canal endêmico | **Ano calendário** (SE 1–52), não o ano sanitário da dengue: a SRAG tem pico de outono-inverno, dentro do ano civil. Referência padrão: 2022 em diante (2020–21 = pandemia; 2019 subnotificado). |
| Rt | Não há Rt de SRAG no banco: estimado pela taxa de crescimento entre janelas de 2 semanas (casos com nowcast), convertida em Rt pelo intervalo de geração gama do agente (Wallinga & Lipsitch). IC 95% Poisson; mínimo de 20 casos na janela. Calculado da série agregada, vale igual em município, estado e RIDE. |
| Nowcast | Chain ladder empírico sobre `DT_DIGITA`: F(k) = fração digitada em até k semanas, medida em semanas maduras dos últimos 2 anos; últimas 8 semanas divididas por F(k). Faixa = quantis 10–90% de F(k). |

## 4. Achados da exploratória (out/2026)

Calculados pelo app a partir da base (aba **Análises**); resumo do que saiu
na primeira carga:

- **VSR antecede a influenza em ~5 semanas** (picos medianos SE 15 e SE 20,
  desde 2022). 81% do VSR é em menores de 2 anos.
- **2026 até a SE 41**: SRAG −30% vs 2025; influenza A −81%, influenza B
  +501% (481 casos, quase empatando com a A — incomum); covid −78%.
- **Covid** caiu de 94% dos casos com agente identificado (2021) para 3%
  (2026), mas segue a mais letal (12,7% vs 3,3% influenza e 0,8% VSR).
- **Influenza em 65+**: letalidade 9,6% e só 24% com vacina de gripe
  registrada na ficha (piso — campo muito em branco).
- **Atraso**: só 63% dos casos digitados ao fim da semana seguinte, 93% em
  4 semanas → sem nowcast a curva sempre "cai" no fim.
- **Etiologia**: SRAG não especificada variou de 20% a 60% por ano (30% em
  2026).
- **DF**: 74% dos casos com 65% da população; Ceilândia lidera entre as RAs.
- **"Outros vírus"** (quebrados por PCR desde 29/10): o **rinovírus** foi de
  174 casos (2023) para 1.831 (2024) e 2.482 (2025) e é o agente
  predominante nas últimas 8 semanas (54% dos casos com agente
  identificado); 65% em menores de 5 anos. O salto coincide com a ampliação
  dos painéis multiplex de PCR — provavelmente mais detecção que epidemia
  nova (a confirmar). O **metapneumovírus** está 7x acima de 2025 (626 vs 86
  casos até a SE 41). 14% dos casos de VSR/outros vírus têm 2+ vírus
  detectados; cada caso vai para um só agente, na ordem VSR >
  metapneumovírus > adenovírus > parainfluenza > rinovírus > outros
  (coinfecções contadas em `coinfeccao`).

## 5. Próximos passos possíveis

- Previsão (reaproveitar `forecasting/` do dengue_ride; série semanal de
  influenza/VSR com sazonalidade forte cabe bem em SARIMA/GAM).
- Nowcast por agente (o atraso de classificação final é maior que o de
  digitação) ou modelo bayesiano de triângulo de atraso (INLA) — aqui os
  dados por caso existem, ao contrário do InfoDengue.
- Contexto nacional/GO (comparar a RIDE com o Brasil por agente).
- População por RA para incidência no DF (Codeplan/PDAD).
