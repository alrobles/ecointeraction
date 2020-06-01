<<<<<<< HEAD
<<<<<<< HEAD
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
  mg <- AutomodelGrid(data_train, tunelength = 20)

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

  #generate maps
  tmpdir <- tempdir()

  BirdList <- paste0("Birds_", 1:4)

  predict_shapefile <- function(x, bucket = 'https://ecointeraction.s3.us-east-2.amazonaws.com/'){
    url <- paste0(bucket, x, '.zip')
    file <- basename(url)
    download.file(url, file)
    unzip(file, exdir = tmpdir )
    shapeFile <- paste0(tmpdir,"/", x)
    Birds <- sf::st_read(shapeFile) %>%
      inner_join(predicted,  by = c("SCINAME" = "species"))
    unlink(shapeFile)
    file.remove(file)
    gc()
    return(Birds)
  }
  BirdList <- purrr::map(BirdList, predict_shapefile) %>% purrr::reduce(rbind)
  gc()

  library(raster)
  library(fasterize)
  r <- raster(BirdList, res = 1/32)
  crs(r) <- crs(BirdList)
  rsum <- fasterize(BirdList, r, field = "susceptible", fun = "sum")
  rcount <- fasterize(BirdList, r, field = "susceptible", fun = "count")
  rmean <- rsum/rcount

 plot(rmean)
 DT::datatable(predicted)
})
=======
<<<<<<< HEAD
=======
>>>>>>> 39f72184407fb22584767f97e738e4348b27721d
=======
devtools::install_github("alrobles/ecointeraction",
                         auth_token = "1d246d7725ebae0cfc417de923096cb4b608a6dc", dependencies = FALSE)
>>>>>>> 3070c19f6ee2ebabe57bdfefc2879c0b8d652612

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
<<<<<<< HEAD
devtools::install_github("alrobles/ecointeraction",
                         auth_token = "1d246d7725ebae0cfc417de923096cb4b608a6dc", dependencies = FALSE)
=======
>>>>>>> 3070c19f6ee2ebabe57bdfefc2879c0b8d652612
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
mg <- AutomodelGrid(data_train, tunelength = 20)

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
