system.time({
  devtools::install_github("alrobles/ecointeraction",
                           auth_token = "1d246d7725ebae0cfc417de923096cb4b608a6dc", dependencies = FALSE)

  library(ecointeraction)

  list.of.packages <- c("caret",
                        "modelgrid",
                        "ranger",
                        "glmnet",
                        "dplyr",
                        "tidymodels",
                        "sf",
                        "raster",
                        "fasterize",
                        "rasterVis",
                        "gridExtra")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, repos = "https://cloud.r-project.org/")
  sapply(list.of.packages, require, character.only = TRUE)
  library(ecointeraction)

  birdswnv_cutoff <- ecointeraction::birdswnv %>%
    ecointeraction::acummulate_incidence(species) %>%
    cutoff_incidence(accuracy = 6)

  # Se lee la bases de datos de mamiferos y flavivirus
  wnvData <-  prep_incidence_data(distance = birdsdistance, incidence = birdswnv_cutoff)

  data_split <-  dplyr::select( wnvData, -species) %>%
    initial_split(prop = 0.6, strata = incidence)

  data_recipe <- data_split %>%
    training() %>%
    recipe(incidence ~.) %>%
    step_interact(terms = ~ (matches("distance$"))^3) %>%
    #step_corr(all_predictors(), -item1, -item2) %>%
    prep()



  data_train <- juice(data_recipe)

  #
  data_test <- data_recipe %>%
    bake(testing(data_split))

  rm(mg)

  AutomodelGrid <- function(data_train, tunelength = 5){


    mg <- model_grid() %>%
      share_settings(
        y = data_train[["incidence"]],
        x = data_train %>%
          ungroup %>%
          dplyr::select(contains("distance")),
        metric = "ROC",
        trControl = trainControl(
          method = "repeatedcv",
          repeats = 10,
          number = 10,
          summaryFunction = twoClassSummary,
          classProbs = TRUE,
          savePredictions = TRUE
        )
      )

    mg <- mg %>%
      modelgrid::add_model(model_name = "Logistic Regression Baseline",
                           method = "glm",
                           family = binomial(link = "logit"))
    # mg <- mg %>%
    #   add_model(model_name = "RandomForest",
    #             method = "rf",
    #             tuneLength = tunelength)
    mg <- mg %>%
      modelgrid::add_model(model_name = "Ranger",
                           method = "ranger",
                           tuneLength = tunelength,
                           importance = 'impurity')

    mg <- mg %>%
      modelgrid::add_model(model_name = "glmnet",
                           method = "glmnet",
                           tuneLength = tunelength)

    mg <- caret::train(mg)
    return(mg)
  }
  mg <- AutomodelGrid(data_train, tunelength = 5)

  #variabe importance
  vip::vip(mg$model_fits$Ranger)

  #roc_auc metric
  mg$model_fits$Ranger %>%
    predict(data_test, "prob") %>%
    dplyr::bind_cols(data_test) %>%
    yardstick::roc_auc(incidence, susceptible)

  #model accuracy
  mg$model_fits$Ranger %>%
    predict(data_test) %>%
    table(data_test$incidence, .) %>%
    yardstick::accuracy()

  data_test <- data_recipe %>%
    bake(birdsdistance)
  predicted <- mg$model_fits$Ranger %>%
    predict(data_test, "prob") %>%
    dplyr::bind_cols(dplyr::select(birdsdistance, species), .) %>%
    dplyr::filter(susceptible > 0.5) %>%
    dplyr::select(-unknown)


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


  library(dbplyr)
  library(dplyr)


  output <- capture.output(dplyr::tbl(con, "birds") %>%
    dplyr::filter(SCINAME %in%  local(predicted$species) ) %>%
    dplyr::show_query())

  final_output <-  paste(tail(output, -1), collapse="")
  # 5.0 Lectura de una tabla

  bird <- sf::st_read(con, query = final_output )

  dbDisconnect(con)


  # 6.0 Plot --> Graficar los elementos de la tabla que comienzan con z
  bird <- bird %>% left_join(predicted, by = c("SCINAME" = "species"))



  library(raster)
  library(fasterize)
  r <- raster(bird, res = 1/32)
  crs(r) <- crs(bird)
  rsum <- fasterize(bird, r, field = "susceptible", fun = "sum")
  rcount <- fasterize(bird, r, field = "susceptible", fun = "count")
  rmean <- rsum/rcount

  rnorm <- rcount/max(rcount@data@values, na.rm = TRUE )
  plot(rcount)

 plot(rnorm > 0.6)

 plot(rnorm)

 wnvr_raw <- readxl::read_excel("data-raw/Tolsa2018Birds-WNV.xls") %>% sample_frac(0.65)
 wnv_long_lat <- wnvr_raw %>% dplyr::select(longitud, latitud)

 points(wnv_long_lat)


 wnv_extract_points_rnorm <- extract(rnorm, wnv_long_lat) %>% enframe()
 wnv_extract_points_rmean <- extract(rmean, wnv_long_lat) %>% enframe() %>% rename(valuemean = value)


 rand_pts <- sampleRandom(rnorm, size=4000, cells=TRUE, sp=TRUE)
 rand_pts <- rand_pts %>% as_tibble %>%
   rename(long = x, lat = y, value = layer) %>%
   mutate(susceptible = 0) %>%
   dplyr::select(susceptible, value, long, lat)

 wnv_extract_points_rnorm <-  wnv_extract_points_rnorm %>%
   bind_cols(wnv_long_lat) %>%
   mutate(susceptible = 1) %>%
   mutate(long = longitud, lat = latitud) %>%
   dplyr::select(susceptible, value, long, lat)
  wnv_pts_validation <-  bind_rows(rand_pts, wnv_extract_points_rnorm) %>%
   sample_frac(1L) %>% na.exclude()

  wnv_pts_validation %>%
    mutate(susceptible = as.factor(susceptible)) %>%
    group_by(susceptible) %>%
    ggplot()+
    geom_density(aes(value, col = susceptible))

  model_wnv_validation <- glm(susceptible ~ value - 1, data = wnv_pts_validation, family = binomial(link = "logit"))
  p <- predict(object = model_wnv_validation)

    results <- tibble(susceptible = wnv_pts_validation$susceptible, p)
  results %>% arrange(desc(p))
  plot(model_wnv_validation)
  wnv_extract_points_rnorm %>% filter(is.na(value))

 wnv_extract_points_rnorm %>%
   mutate(validate = ifelse(value > 0, 1, 0)) %>%
   group_by(validate) %>% count() %>% tidyr::spread(validate, n)

 wnv_extract_points_rnorm %>%
   left_join(wnv_extract_points_rmean) %>%
   bind_cols(wnv_long_lat) %>% dplyr::select(value, valuemean)  %>%
   na.exclude() %>% cor

 #añadir top 20
 predicted_top_20  <- predicted %>% arrange(desc(susceptible)) %>% head(20)
 con <- dbConnect(dvr, dbname = db, host=host_db, port=db_port,
                  user=.rs.askForPassword("usuario"), password=.rs.askForPassword("contraseña") )

 output <- capture.output(dplyr::tbl(con, "birds") %>%
                            dplyr::filter(SCINAME %in%  local(predicted_top_20$species) ) %>%
                            dplyr::show_query())
 final_output <-  paste(tail(output, -1), collapse="")
 # 5.0 Lectura de una tabla

 bird_20 <- sf::st_read(con, query = final_output )
 bird_20 <- bird_20 %>% left_join(predicted_top_20, by = c("SCINAME" = "species"))



 library(raster)
 library(fasterize)
 r_20 <- raster(bird_20, res = 1/32)
 crs(r_20) <- crs(bird_20)
 rsum_20 <- fasterize(bird_20, r_20, field = "susceptible", fun = "sum")
 rcount_20 <- fasterize(bird_20, r_20, field = "susceptible", fun = "count")
 rmean_20 <- rsum_20/rcount_20

 rnorm_20 <- rcount_20/max(rcount_20@data@values, na.rm = TRUE )
 plot(rnorm_20 )
 plot(rnorm_20)







})

