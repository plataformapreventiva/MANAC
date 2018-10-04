library(dbconnection)
library(tidyverse)
library(dbplyr)
library(odbc)

library(dotenv)

dotenv::load_dot_env("../.env")
# 
# devtools::install_github("plataformapreventiva/dbrsocial",
#                          ref = "develop",
#                          auth_token=Sys.getenv("GITHUB_PATH"),
#                          build_vignettes=TRUE)

con <- dbconnection::prev_connect()

## TODO 😬
query_marg <- "select cve_ent, cve_mun, indice_marginacion, grado_marginacion from raw.conapo_marginacion"
df_marg <- dbGetQuery(con, query_marg) %>%
  mutate(cve_muni = str_pad(cve_mun, width = 5, side = 'left', pad = '0')) %>%
  group_by(cve_muni) %>%
  summarise(grado_marginacion = first(grado_marginacion))

query_censo <- "select distinct ent, mun, tamloc from raw.intercensal_viviendas_2015"
df_censo <- dbGetQuery(con, query_censo) %>%
  mutate(cve_ent = str_pad(string = ent, width = 2, side = 'left', pad = '0'),
         cve_mun = str_pad(string = mun, width = 3, side = 'left', pad = '0'),
         cve_muni = paste0(cve_ent,cve_mun)) %>%
  select(cve_muni, tamloc) %>%
  group_by(cve_muni) %>%
  summarise(tamloc_mediana = median(tamloc))

tabla_mun <- df_censo %>%
  left_join(df_marg, by = 'cve_muni')

write_csv(x = tabla_mun, path = "datos/tabla_municipios.csv")

