library(tidyverse)
library(foreign)
library(curl)
library(rgdal)

url_loc <- "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292812_s.zip"
curl_download(url = url_loc, destfile = 'datos/mg_2010.zip', quiet = TRUE, mode = 'w')
system('mkdir datos/mg_2010')
system('unzip -o datos/mg_2010.zip -d datos/mg_2010')

system(paste("for archivo in $(find datos/mg_2010/ -iname '*.zip'); do",
                    'echo "$archivo"',
                    'unzip -o $archivo -d datos/mg_2010',
                  'done', 
             sep = '\n'))

loc_rurales <- readOGR(dsn = "datos/mg_2010", layer = "Localidades_rurales_2010_5")

directorio <- loc_rurales@data %>% as_tibble()
coordenadas <- loc_rurales@coords %>% as_tibble()
colnames(coordenadas) <- c("lon","lat")

crs_origen <- CRS("+init=epsg:6362")
crs_destino <- CRS("+init=epsg:4326")

coordenadas <- sp::SpatialPoints(coordenadas, proj4string = crs_origen)
coordenadas_proy <- spTransform(coordenadas, crs_destino)

datos <- bind_cols(directorio, coordenadas_proy %>% as_tibble())

datos <- datos %>%
  mutate(NOM_LOC = iconv(NOM_LOC, from = "latin1", to = "utf-8")) %>%
  select(-OID)

# EDO, MUN, AGEB, tipo (rural, urbana), LOCALIDAD, columnas en nuestro modelo
url_iter <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2010/microdatos/iter/00_nacional_2010_iter_dbf.zip"
curl_download(url = url_iter, destfile = 'datos/iter_2010.zip', quiet = TRUE, mode = 'w')
system('mkdir datos/iter_2010')
system('unzip -o datos/iter_2010.zip -d datos/iter_2010')

iter <- read.dbf("datos/iter_2010/ITER_NALDBF10.dbf") %>%
  filter(ENTIDAD != "00" & MUN != "000" & LOC != "0000" & LOC != "9999" & LOC != "9998") %>%
  mutate_at(.vars = vars(P15YM_SE,P15PRI_IN,P15PRI_CO,P15SEC_CO,P18YM_PB), 
            .funs = funs(ifelse(. == "*", 0, as.integer(.)))) %>%
  mutate(TAM_LOC = as.integer(TAM_LOC)) %>%
  transmute(ent=ENTIDAD, 
         mun=MUN, 
         loc=LOC,
         indigena=P3YM_HLI,
         jefe_sexo_h=HOGJEF_M,
         jefe_sexo_m=HOGJEF_F,
         graproes=GRAPROES,
         pisos=VPH_PISOTI,
         dis_agua=VPH_AGUADV,
         excus=VPH_EXCSA,
         drenaje=VPH_DRENAJ,
         servicio_celular=VPH_CEL,
         servicio_internet=VPH_INTER,
         automovil=VPH_AUTOM,
         VPH_SNBIEN=VPH_SNBIEN,
         tam_hog=PROM_OCUP,
         n_ocup=POCUPADA,
         maxnved= (0 * P15YM_SE + 1 * P15PRI_IN + 2 * P15PRI_CO + 3 * P15SEC_CO + 4 * P18YM_PB)/as.integer(POBTOT),
         tam_loc=ifelse(TAM_LOC <= 4, 1, ifelse(TAM_LOC <= 7, 2, ifelse(TAM_LOC <= 10, 3, 4))),
         P15YM_SE=P15YM_SE,
         P15PRI_IN=P15PRI_IN,
         P15PRI_CO=P15PRI_CO,
         P15SEC_CO=P15SEC_CO,
         P18YM_PB=P18YM_PB,
         pobtot=POBTOT,
         vivtot=VIVTOT,
         tvivhab=TVIVHAB,
         OCUPVIVPAR=OCUPVIVPAR
  )

datos_2 <- datos %>%
  left_join(iter, by = c("CVE_ENT"="ent","CVE_MUN"="mun","CVE_LOC"="loc"))


write_csv(x = datos_2, path = "/home/andreu//MANAC/datos/localidades_rurales.csv")


# library(dbrsocial)
# library(tidyverse)
# library(dbplyr)
# library(odbc)
# library(dotenv)
# 
# dotenv::load_dot_env("../.env")
# con <- dbrsocial::prev_connect()
# 
# # carencias
# query_hogs <- paste("select hogar_id,ic_rezago_educativo,ic_alimentacion,ic_asalud,",
#                     "ic_seguridad_social,ic_servicios_basicos,ic_vivienda",
#                     "from features.enigh_carencias",
#                     "where fecha_creacion like '%2016%'")
# df_hogs <- dbGetQuery(con, query_hogs) %>%
#   group_by(hogar_id) %>%
#   summarise_all(.funs = funs(mean))
# 
# 
# # enigh 
# query_enigh <- paste("select *",
#                      "from tbl_enigh")
# df_enigh <- dbGetQuery(con, query_enigh)
# 
# 
# enigh_nueva  <- df_enigh %>%
#   left_join(df_hogs, by = "hogar_id")
# 
# write_csv(x = enigh_nueva, path = "/home/andreu//MANAC/datos/enigh_nueva.csv")

