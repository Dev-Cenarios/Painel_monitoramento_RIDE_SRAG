# ==============================================================================
# Gera data/geo/contorno_ride.geojson: um poligono por UF (municipios
# dissolvidos, codarea "UF52" etc.) e um da RIDE inteira (codarea "RIDE").
# global.R junta essas feicoes ao fim da malha dos municipios e o mapa
# (R/mod_mapa.R) desenha so a borda delas, por cima, para o recorte nao
# "sumir" no fundo branco quando os municipios estao na cor mais clara.
#
# Roda so quando a malha mudar (o arquivo vai para o git, como os geojson):
#   Rscript prep/gerar_contorno.R
# Precisa de sf e geojsonsf - so aqui, o app nao depende deles.
# ==============================================================================

suppressMessages(source("global.R", encoding = "UTF-8"))

sf::sf_use_s2(FALSE)
g <- geojsonsf::geojson_sf(as.character(
  jsonlite::toJSON(MALHA_RIDE, auto_unbox = TRUE, digits = NA)))
g$uf <- substr(g$codarea, 1, 2)

ufs <- g |>
  dplyr::group_by(uf) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
  dplyr::transmute(codarea = paste0("UF", uf), geometry)

ride <- sf::st_sf(codarea = "RIDE", geometry = sf::st_union(g))

saida <- file.path(GEO_DIR, "contorno_ride.geojson")
unlink(saida)
sf::st_write(rbind(ufs, ride), saida, driver = "GeoJSON",
             layer_options = "COORDINATE_PRECISION=5", quiet = TRUE)
message(sprintf("%s: %d UF(s) + contorno da RIDE.", saida, nrow(ufs)))
