library(tidyverse)
library(foreign)
library(curl)

# Descarga datos de ENIGH CONEVAL
urlconeval <- "https://www.coneval.org.mx/Medicion/MP/Documents/Programas_calculo_pobreza_10_12_14/R_2010.zip"
destino <- "Coneval_2010.zip"

curl_download(url = urlconeval, destfile = paste0('datos/',destino))
system(paste0('7z x datos/', destino, ' -o"datos/CONEVAL_2010/"'))

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
         trabajo=TRABAJO,
         edocony=EDOCONY) %>%
  mutate(hogar_id = paste0(folioviv,'-',foliohog)) %>%
  group_by(hogar_id) %>%
  mutate(trabajo=ifelse(is.na(trabajo) & edad <= 16, 2, trabajo),
         menores_trabaja=1*(edad >= 12 & edad <= 15 & trabajo == 1),
         adol_trabaja=1*(edad >= 16 & edad <= 18 & trabajo == 1),
         asis_esc = ifelse(is.na(asis_esc) & edad <= 2, 2, asis_esc),
         nivelaprob = ifelse(edad <= 2, 0, nivelaprob),
         trab_adul_may = 1*(edad >= 64 & trabajo == 1)) %>%
  ungroup()

poblacion_3 <- poblacion_2 %>%
  group_by(hogar_id) %>%
  summarise(num_menores_trabaja=sum(menores_trabaja, na.rm = T),
            num_adol_trabaja=sum(adol_trabaja, na.rm = T),
            num_trabajan = sum(trabajo == 1, na.rm = T),
            num_mujeres_15a49 = sum(sexo == 2 & edad >= 15 & edad <= 49),
            num_p65omas = sum(edad >= 65),
            numasesc = sum(asis_esc == 1),
            trab_adul_may = sum(trab_adul_may, na.rm = T),
            trabinf = 1*any(edad < 16 & trabajo == 1),
            maxnved = max(as.integer(nivelaprob)),
            may_64 = sum(edad > 64),
            nalfab = sum(alfabe == 1, na.rm = T),
            indigena = 1*any(!is.na(lenguaind))) %>%
  ungroup()

poblacion_4 <- poblacion_2 %>%
  filter(parentesco == 101 | parentesco == 102) %>%
  mutate(jcony = first(edocony),
         jnesc = as.integer(nivelaprob),
         jpea = 1*(edad >= 12 & trabajo == 1)) %>%
  select(hogar_id, jcony:jpea)

poblacion_5 <- poblacion_3 %>% inner_join(poblacion_4, by = "hogar_id")

# concentrado (nivel vivienda)

concentrado = read.dbf("datos/CONEVAL_2010/Bases de datos/concentrado.dbf")

concentrado_2 <- concentrado %>%
  select(folioviv=FOLIOVIV,
         foliohog=FOLIOHOG,
         #actor=FACTOR,
         tam_loc=TAM_LOC,
         # 1 Localidades con 100 000 y más habitantes
         # 2 Localidades con 15 000 a 99 999 habitantes 
         # 3 Localidades con 2 500 a 14 999 habitantes 
         # 4 Localidades con menos de 2500 habitantes
         ubica_geo=UBICA_GEO,
         conapo=CONAPO,
         #est_dis=EST_DIS,
         #upm=UPM,
         #sexo=SEXO,
         #edad=EDAD,
         ed_formal=ED_FORMAL, # Educación formal del jefe del hogar
         tam_hog=TAM_HOG, # Número de personas pertenecientes al hogar (sin considerar trabajadores domésticos ni huéspedes)
         hombres=HOMBRES, # Número de hombres en el hogar (sin considerar trabajadores domésticos ni huéspedes)
         mujeres=MUJERES, # Número de mujeres en el hogar (sin considerar trabajadores domésticos ni huéspedes)
         tot_resi=TOT_RESI, # Número de personas que habitan la vivienda del hogar.
         tot_hom=TOT_HOM, # Número de hombres que habitan la vivienda del hogar.
         tot_muj=TOT_MUJ, # Número de mujeres que habitan la vivienda del hogar.
         mayores=MAYORES, # Integrantes del hogar que tienen 12 o más años de edad.
         menores=MENORES, # Integrantes del hogar que tienen 11 o menos años de edad.
         p12_64=P12_64, # Integrantes del hogar cuya edad está entre los 12 y los 64 años.
         p65mas=P65MAS, # Integrantes del hogar que tienen 65 o más años de edad.
         n_ocup=N_OCUP,# Número de personas que tienen trabajo y 14 o más años de edad.
         ingcor=INGCOR) %>% # Suma del ingreso corriente monetario y del gasto no monetario.
  mutate(hogar_id = paste0(folioviv,'-',foliohog)) %>%
  select(-folioviv,-foliohog)

# hogares (nivel vivienda)

hogares <- read.dbf("datos/CONEVAL_2010/Bases de datos/hogares.dbf")

hogares_2 <- hogares %>%
  mutate(claviv = ifelse(is.na(CLAVIV), '-1', as.character(CLAVIV))) %>%
  select(folioviv=FOLIOVIV,
         foliohog=FOLIOHOG,
         claviv,
         # 1 Casa independiente
         # 2 Departamento en edificio
         # 3 Vivienda en vecindad
         # 4 Vivienda en cuarto de azotea
         # 5 Local no construido para habitación
         # -1 No especificado
         pared=PARED,
         # 1 Material de desecho
         # 2 Lámina de cartón
         # 3 Lámina metálica o de asbesto
         # 4 Carrizo bambú o palma
         # 5 Embarro o Bajareque
         # 6 Madera
         # 7 Adobe
         # 8 Tabique, ladrillo, block, piedra o concreto
         techos=TECHOS,
         pisos=PISOS,
         cua_coc=CUA_COC,
         coc_duer=COC_DUER, # Cocina utilizada de manera habitual también para dormir.
         dormi=DORMI, # Número de cuartos de la vivienda que son usados habitualmente para dormir
         cuart=CUART, # Número total de cuartos que tiene la vivienda, independientemente de su uso.
         dis_agua=DIS_AGUA, # Formas de abastecer el agua a la vivienda
         # 1 Agua entubada dentro de la vivienda
         # 2 Agua entubada fuera de la vivienda, pero dentro del terreno 3 Agua entubada de llave pública (o hidrante)
         # 4 Agua entubada que acarrean de otra vivienda
         # 5 Agua de pipa
         # 6 Agua de un pozo, río, lago, arroyo u otra
         dot_agua=DOT_AGUA, # Frecuencia con la que llega el agua de la red pública a la vivienda.
         # 1 Diario
         # 2 Cada tercer día
         # 3 Dos veces por semana 4 Una vez por semana
         # 5 De vez en cuando
         excus=EXCUS, 
         sanitario_compartido=USO_COM,
         adm_ag=ADM_AG, # Funcionamiento de la instalación sanitaria con o sin conexión de agua.
         # 1 Tiene descarga directa de agua 
         # 2 Le echan agua con cubeta
         # 3 No se le puede echar agua
         drenaje=DRENAJE,
         # 1 La red pública
         # 2 Una fosa séptica
         # 3 Una tubería que va a dar a una barranca o grieta 
         # 4 Una tubería que va a dar a un río, lago o mar
         # 5 No tiene drenaje
         elect=ELECT,
         # 1 Del servicio público
         # 2 De una planta particular 
         # 3 De panel solar
         # 4 De otra fuente
         # 5 No tiene luz eléctrica
         focos=FOCOS,
         #estufa=ESTUFA,
         linea_telefonica=SERV_1,
         servicio_celular=SERV_2,
         servicio_internet=SERV_4,
         automovil=VEHI1_N,
         motocicleta=VEHI4_N,
         estereo=EQH1_N,
         radiograbadora=EQH2_N,
         radio=EQH3_N,
         televisor=EQH4_N) %>%
  mutate(hogar_id = paste0(folioviv,'-',foliohog)) %>%
  select(-folioviv,-foliohog)

# carencias (nivel persona)

carencias <- read.dbf("datos/CONEVAL_2010/Base final/pobreza_10.dbf")

carencias_2 <- carencias %>%
  mutate(FOLIOVIV = str_pad(FOLIOVIV, width = 6, side = 'left', pad = '0')) %>%
  select(folioviv=FOLIOVIV,
         foliohog=FOLIOHOG,
         ic_rezedu, # nivel persona
         ic_asalud, # nivel persona
         ic_seguridad_social=ic_segsoc, # nivel persona
         ic_vivienda=ic_cv, # nivel vivienda
         ic_servicios_basicos=ic_sbv, # nivel vivienda
         ic_alimentacion=ic_ali) %>% # nivel vivienda
  mutate(hogar_id = paste0(folioviv,'-',foliohog)) %>% 
  group_by(hogar_id) %>%
  summarise(ic_rezedu = mean(ic_rezedu),
            ic_asalud = mean(ic_asalud),
            ic_seguridad_social = mean(ic_seguridad_social),
            ic_vivienda = first(ic_vivienda),
            ic_servicios_basicos = first(ic_servicios_basicos),
            ic_alimentacion = first(ic_alimentacion)) %>%
  ungroup()

# ingreso_features (nivel vivienda)

ingreso <- read.dbf("datos/CONEVAL_2010/Base final/pobreza_10.dbf")

ingreso_2 <- ingreso %>%
  mutate(FOLIOVIV = str_pad(FOLIOVIV, width = 6, side = 'left', pad = '0'),
         hogar_id = paste0(FOLIOVIV,'-',FOLIOHOG)) %>% 
  group_by(hogar_id) %>%
  summarise(total_personas = n(),
            num_asiste_escuela_5a15 = sum((1-inas_esc)*(EDAD >= 5 & EDAD <= 15),na.rm=T),
            con_servicios_salud = sum(serv_sal %in% 1:6),
            con_seguro_popular = sum(serv_sal == 1, na.rm = T),
            jefe_edad = max((EDAD)*(PARENTESCO == 101),na.rm=T),
            jefe_sexo = first((SEXO-1)*(PARENTESCO == 101))) %>%
  ungroup()

enigh_final <- poblacion_5 %>%
  inner_join(hogares_2, by = "hogar_id") %>%
  inner_join(concentrado_2, by = "hogar_id") %>%
  inner_join(carencias_2, by = "hogar_id") %>%
  inner_join(ingreso_2, by = "hogar_id")

write_csv(x = enigh_final, path = "datos/enigh_final.csv")
