library(dbconnection)
library(tidyverse)
library(dbplyr)
library(dotenv)

dotenv::load_dot_env("../.env")
# 
# devtools::install_github("plataformapreventiva/dbrsocial",
#                          ref = "develop",
#                          auth_token=Sys.getenv("GITHUB_PATH"),
#                          build_vignettes=TRUE)

con <- dbconnection::prev_connect()


