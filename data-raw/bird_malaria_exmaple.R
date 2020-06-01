library(ecointeraction)
data <- mammalvirus %>%
  acummulate_incidence(mammal_species, incidence)
birdsplasmodiumrelictum <- birdsplasmodiumrelictum %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::select(id, species, incidence) %>%
  dplyr::mutate(cummulativesum = cumsum(incidence))
birdsplasmodium <- birdsplasmodium %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::select(id, species, incidence) %>%
  dplyr::mutate(cummulativesum = cumsum(incidence))

cummulative_rate(birdsplasmodiumrelictum, id, incidence, cummulativesum, accuracy = 7)
cutoff_incidence(birdsplasmodiumrelictum, accuracy = 4)

birdsInfected <- birdsplasmodiumrelictum %>%
  select(species) %>%
  mutate(infected = 1) %>%
  dplyr::inner_join(birdsdistance)

birdsNotInfected <- select(birdsInfected, species) %>% dplyr::anti_join(birdsdistance, .)
birdsNotInfected_sample <-birdsNotInfected %>%
  dplyr::sample_n(nrow(birdsInfected)*4)

library(tidyverse)
library(tidymodels)
dataset <- dplyr::full_join(birdsInfected, birdsNotInfected_sample) %>%
  mutate(infected = ifelse(is.na(infected), "unknown", "susceptible"))

data_split <- dataset %>%
  select(-species) %>%
  select(infected, everything()) %>%
  initial_split(., prop = 0.6, strata = infected)

data_recipe <- data_split %>%
  training() %>%
  recipe(infected ~.) %>%
  step_interact(terms = ~ (matches("distance$"))^3) %>%
  prep()

DataTrain <- juice(data_recipe) %>% na.exclude()


DataTest <- data_recipe %>%
  bake(testing(data_split))


library(modelgrid)
library(caret)
library(gbm)

rm(mg)

AutomodelGrid <- function(data, tunelength = 5){


  mg <- model_grid() %>%
    share_settings(
      y = data[["infected"]],
      x = data %>%
        ungroup %>%
        select(contains("distance")),
      preProc = c("nzv"),
      metric = "ROC",
      trControl = trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 10,
        summaryFunction = twoClassSummary,
        classProbs = TRUE,
        savePredictions = TRUE
        ,allowParallel = TRUE
      )
    )

  mg <- mg %>%
    add_model(model_name = "Logistic Regression Baseline",
              method = "glm",
              family = binomial(link = "logit"))
   mg <- mg %>%
     add_model(model_name = "RandomForest",
               method = "rf",
               tuneLength = tunelength)
  mg <- mg %>%
    add_model(model_name = "Ranger",
              method = "ranger",
              tuneLength = tunelength,
              importance = 'impurity')

    mg <- mg %>%
      add_model(model_name = "glmnet",
                method = "glmnet",
                tuneLength = tunelength)
  mg <- mg %>%
    add_model(model_name = "gam",
              method = "gam",
              tuneLength = tunelength)
   # mg <- mg %>%
   # add_model(model_name = "gbm",
   #             method = "gbm",
   #             tuneLength = tunelength)
   # mg <- mg %>%
   #    add_model(model_name = "xgbTree",
   #              method = "xgbTree",
   #              tuneLength = tunelength,
   #              nthread = 7)

  mg <- caret::train(mg)
  return(mg)
}
library(caret)
library(modelgrid)
library("parallel")
library("doParallel")

Mycluster = makeCluster(detectCores(logical = TRUE) -1)
registerDoParallel(Mycluster)
#

mg <- AutomodelGrid(data = DataTrain, tunelength = 25)
stopCluster(Mycluster)

resamplesModels <- mg$model_fits %>%
  caret::resamples()

bwplot(resamplesModels)


library(vip)
vip(mg$model_fits$Ranger)
firm_ranger <- vip::vi_firm(mg$model_fits$Ranger, names(mg$model_fits$Ranger$trainingData)[-8] )
library(pdp)
vip::add_sparklines(mg$model_fits$Ranger, names(mg$model_fits$Ranger$trainingData)[-8])
pdp::plotPartial(mg$model_fits$RandomForest, pred.data = DataTest, x.var = "env_distance")
library(MLeval)
pred_wrapper <- function(object, newdata){
  predict(object, x = (newdata)) %>%
    as.vector()
}
vip::vi_firm(mg$model_fits$RandomForest, names(mg$model_fits$RandomForest$trainingData)[-8])
vip::vip(mg$model_fits$RandomForest)
partial(mg$model_fits$Ranger, names(mg$model_fits$RandomForest$trainingData)[c(2, 5)], train = select(DataTest, -infected), ice = FALSE, prob= TRUE) %>%
  autoplot(alpha = 0.5)

purrr::map2_df( mg$model_fits, names(mg$model_fits), function(x, y){
  yardstick::accuracy(table(predict(x , DataTest), DataTest$infected)) %>%
     mutate(model = y)
}) %>%
  arrange(desc(.estimate))

 library(MLeval)
 # Select a parameter setting
 library(ranger)
res <- evalm(mg$model_fits, gnames = c(names(mg$model_fits)) )

library(pROC)
roc_gbm <- pROC::roc(class , probs$susceptible)
pROC::plot.roc(roc_gbm)
png(filename = "birdResultRoc.png", width = 640, height = 640)
res$roc
dev.off()
