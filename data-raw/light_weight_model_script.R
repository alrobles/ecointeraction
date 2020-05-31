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

# Se lee la bases de datos de mamiferos y flavivirus
mammalvirusincidence <-  mammalvirus %>%
  ecointeraction::acummulate_incidence(mammal_species) %>%
  ecointeraction::cutoff_incidence() %>%
  dplyr::select(mammal_species) %>%
  dplyr::rename(species = mammal_species) %>%
  dplyr::mutate(species  = stringr::str_replace(species, "_", " ") )


mammalsData <-  prep_incidence_data(distance = mammalsdistance, incidence = mammalvirusincidence)

data_split <-  dplyr::select( mammalsData, -species) %>%
  initial_split(dataDist, prop = 0.6, strata = incidence)

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

mammals_predict <- mg$model_fits$Ranger %>%
  predict(recipes::bake(data_recipe, mammalsdistance), "prob") %>%
  dplyr::bind_cols(dplyr::select(mammalsdistance, species), .)

#Rename prediction to field in IUCN polygons
mammals_predict <- mammals_predict %>% rename(BINOMIAL = species)

#download UICN polygons

if(!dir.exists("data-raw/Mammals_Terrestrial")) {
  download.file(
    "https://ecointeraction.s3.us-east-2.amazonaws.com/Mammals_Terrestrial.zip",
    destfile = "data-raw/Mammals_Terrestrial.zip") # <-- 383 MB
  unzip("data-raw/Mammals_Terrestrial.zip", exdir = ".")
  unlink("data-raw/Mammals_Terrestrial.zip")
}
mammal_shapes <- sf::st_read("data-raw/Mammals_Terrestrial")

#join data
mammal_shapes_susceptible <- mammal_shapes %>%
  dplyr::select(BINOMIAL) %>%
  dplyr::inner_join(mammals_predict) %>%
  dplyr::filter(susceptible > 0.5)

mammal_raster <- raster::raster(mammal_shapes_susceptible, res = 1/6)

mammal_susceptibility_richness <- fasterize::fasterize(mammal_shapes_susceptible,
                                            mammal_raster,
                                            field = "susceptible",
                                            fun="count")
mammal_susceptibility_sum <- fasterize::fasterize(mammal_shapes_susceptible,
                                          mammal_raster,
                                          field = "susceptible",
                                          fun="sum")
mammal_susceptibility_mean <- mammal_susceptibility_sum/mammal_susceptibility_richness
p1 <- rasterVis::levelplot(mammal_susceptibility_richness,
                     par.settings = rasterVis::YlOrRdTheme(),
                     margin=FALSE,
                     main = "Richness susceptibility predicted for mammals - flavivirus assembly",  cex = 4)

p2 <- rasterVis::levelplot(mammal_susceptibility_mean,
                           par.settings = rasterVis::YlOrRdTheme(),
                           margin=FALSE,
                           main = "Mean susceptibility predicted for mammals - flavivirus assembly",  cex = 4)
p2 <-  plot(mammal_susceptibility_mean, main =  "Risk")
gridExtra::grid.arrange(p1, p2)
