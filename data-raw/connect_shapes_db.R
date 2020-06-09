## CONEXIÓN ENTRE POSTGRESL Y R
# 1.0 Instalación y carga de paquetes
library(DBI)
library(RPostgres)
library(sf)

# 2.0 Parámetros de conexión a PostgreSQL
dvr <- RPostgres::Postgres()
db <- 'shapefiles'  ##Nombre de la BBDD
host_db <- '152.44.44.45'
db_port <- '5432'
#db_user <- .rs.askForPassword("usuario")  ##Tu usuario
#db_password <- .rs.askForPassword("contraseña") ##Tu contraseña

# 3.0 Conexión
con <- dbConnect(dvr, dbname = db, host=host_db, port=db_port,
                 user=.rs.askForPassword("usuario"), password=.rs.askForPassword("contraseña") )

dbListTables(con)

library(dbplyr)
library(dplyr)


z <- dplyr::tbl(con, "birds") %>%
  dplyr::filter(grepl("^Z", SCINAME)) %>%
  head(10) %>%
  dplyr::show_query()

# 5.0 Lectura de una tabla

bird <- sf::st_read(con, query = "
               SELECT * FROM \"birds\"
               WHERE ((\"SCINAME\") ~ ('^A'))
               LIMIT 5;")
dbDisconnect(con)


# 6.0 Plot --> Graficar los elementos de la tabla que comienzan con z
bird %>% plot

