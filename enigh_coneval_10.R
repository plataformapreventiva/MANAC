library(tidyverse)
library(foreign)
library(curl)

# Descarga datos de ENIGH CONEVAL
urlconeval <- "https://www.coneval.org.mx/Medicion/MP/Documents/Programas_calculo_pobreza_10_12_14/R_2010.zip"
destino <- "Coneval_2010.zip"

curl_download(url = urlconeval, destfile = paste0('datos/',destino))
system(paste0('7z x -scs1252 ', paste0('datos/',destino), ' -o"datos/CONEVAL_2010/"'))

# poblacion (nivel persona)
poblacion <- read.dbf("datos/CONEVAL_2010/Bases de datos/poblacion.dbf") 

poblacion_2 <- poblacion %>%
  select(folioviv=FOLIOVIV,
         foliohog=FOLIOHOG,
         parentesco=PARENTESCO,
         sexo=SEXO,
         edad=EDAD,
         lenguaind=LENGUAIND,
         alfabe=ALFABE,
         asis_esc=ASIS_ESC,
         nivel=NIVEL,
         grado=GRADO,
         nivelaprob=NIVELAPROB,
         gradoaprob=GRADOAPROB,
         segpop=SEGPOP,
         atemed=ATEMED,
         imss=INST_1,
         issste=INST_2,
         issste_estatal=INST_3,
         pemex=INST_4,
         otro_ss=INST_5,
         trabajo=TRABAJO) %>%
  mutate(hogar_id = paste0(folioviv,'-',foliohog),
         menores_trabaja=sum((edad >= 12 & edad <= 15 & trabajo == 1),na.rm=T),
         adol_trabaja=sum((edad >= 16 & edad <= 18 & trabajo == 1),na.rm=T)) %>%
  group_by(hogar_id) %>%
  summarise(num_menores_trabaja=sum(menores_trabaja),
            num_adol_trabaja=sum(adol_trabaja),
            num_trabajan = sum(trabajo == 1),
            num_mujeres_15a49 = sum(sexo == 2 & edad >= 15 & edad <= 49),
            num_p65omas = sum(edad >= 65))

# concentrado (nivel vivienda)

concentrado = read.dbf("datos/CONEVAL_2010/Bases de datos/concentrado.dbf")

concentrado_2 <- concentrado %>%
  select(folioviv=FOLIOVIV,
         foliohog=FOLIOHOG,
         factor=FACTOR,
         tam_loc=TAM_LOC,
         ubica_geo=UBICA_GEO,
         conapo=CONAPO,
         est_dis=EST_DIS,
         upm=UPM,
         sexo=SEXO,
         edad=EDAD,
         ed_formal=ED_FORMAL,
         tam_hog=TAM_HOG,
         hombres=HOMBRES,
         mujeres=MUJERES,
         tot_resi=TOT_RESI,
         tot_hom=TOT_HOM,
         tot_muj=TOT_MUJ,
         mayores=MAYORES,
         menores=MENORES,
         p12_64=P12_64,
         p65mas=P65MAS,
         n_ocup=N_OCUP,
         ingcor=INGCOR) %>%
  mutate(hogar_id = paste0(folioviv,'-',foliohog))

# hogares (nivel vivienda)

hogares <- read.dbf("datos/CONEVAL_2010/Bases de datos/hogares.dbf")

hogares_2 <- hogares %>%
  select(folioviv=FOLIOVIV,
         foliohog=FOLIOHOG,
         claviv=CLAVIV,
         pared=PARED,
         techos=TECHOS,
         pisos=PISOS,
         cua_coc=CUA_COC,
         coc_duer=COC_DUER,
         dormi=DORMI,
         cuart=CUART,
         dis_agua=DIS_AGUA,
         dot_agua=DOT_AGUA,
         excus=EXCUS,
         sanitario_compartido=USO_COM,
         adm_ag=ADM_AG,
         drenaje=DRENAJE,
         elect=ELECT,
         focos=FOCOS,
         estufa=ESTUFA,
         linea_telefonica=SERV_1,
         servicio_celular=SERV_2,
         servicio_internet=SERV_4,
         automovil=VEHI1_N,
         motocicleta=VEHI4_N,
         estereo=EQH1_N,
         radiograbadora=EQH2_N,
         radio=EQH3_N,
         televisor=EQH4_N) %>%
  mutate(hogar_id = paste0(folioviv,'-',foliohog))

# carencias (nivel persona)

carencias <- read.dbf("datos/CONEVAL_2010/Base final/pobreza_10.dbf")

carencias_2 <- carencias %>%
  select(folioviv=FOLIOVIV,
         foliohog=FOLIOHOG,
         ic_rezedu,
         ic_asalud,
         ic_seguridad_social=ic_segsoc,
         ic_vivienda=ic_cv,
         ic_servicios_basicos=ic_sbv,
         ic_alimentacion=ic_ali) %>%
  mutate(hogar_id = paste0(folioviv,'-',foliohog))

# ingreso_features (nivel vivienda)

ingreso <- read.dbf("datos/CONEVAL_2010/Base final/pobreza_10.dbf")

ingreso_2 <- ingreso %>%
  mutate(hogar_id = paste0(FOLIOVIV,'-',FOLIOHOG)) %>% 
  group_by(hogar_id) %>%
  summarise(total_personas = n(),
            num_asiste_escuela_5a15 = sum((1-inas_esc)*(EDAD >= 5 & EDAD <= 15),na.rm=T),
            con_servicios_salud = sum(serv_sal %in% 1:6),
            con_seguro_popular = sum(serv_sal == 1),
            jefe_edad = max((EDAD)*(PARENTESCO == 101),na.rm=T),
            )








