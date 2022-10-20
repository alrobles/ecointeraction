
#' This function returns a list with models given a data frame with
#'
#'
#' @param listData A list with a sample of labeled species and distance
#' to predict possible interactions
#' @param distance_df  A data frame with mean phylogenetic geographical and
#' environmental distance from one target host species to other species.
#'
#' @importFrom caret train twoClassSummary trainControl
#' @importFrom modelgrid model_grid share_settings add_model
#' @importFrom recipes recipe step_interact prep juice bake
#' @importFrom rsample initial_split training testing
#' @importFrom tidyselect matches contains
#' @importFrom vip vi
#' @importFrom yardstick roc_auc accuracy
#' @importFrom rlang .data
#' @importFrom stats predict
#'
#' @return A list with the best fitted model, score of variable importance,
#' score of ROC-AUC value, score of accuracy, and predicted values for
#' each possible host.
#' @export
#'
#' @examples
#' if(FALSE){
#' automodel_replication(listData, birdsdistance_raw)
#' }

automodel_replication <- function(listData, distance_df)
{
  data_split <-  dplyr::select(listData, -.data$species) %>%
    rsample::initial_split(prop = 0.7, strata = .data$incidence)

  data_recipe <- data_split %>%
    rsample::training() %>%
    recipes::recipe(incidence ~.) %>%
    recipes::step_interact(terms = ~ (tidyselect::matches("distance$"))^2) %>%
    recipes::prep()



  data_train <- recipes::juice(data_recipe)

  #
  data_test <- data_recipe %>%
    recipes::bake(rsample::testing(data_split))

  AutomodelGrid <- function(data_train, tunelength = 5){

    mg <- modelgrid::model_grid() %>%
      modelgrid::share_settings(
        y = data_train[["incidence"]],
        x = data_train %>%
          dplyr::ungroup() %>%
          dplyr::select(tidyselect::contains("distance")),
        metric = "ROC",
        trControl = caret::trainControl(
          method = "repeatedcv",
          repeats = 10,
          number = 10,
          summaryFunction = caret::twoClassSummary,
          classProbs = TRUE,
          savePredictions = TRUE
        )
      )

    mg <- mg %>%
      modelgrid::add_model(model_name = "Ranger",
                           method = "ranger",
                           tuneLength = tunelength,
                           importance = 'impurity')

    mg <- caret::train(mg)
    return(mg)
  }

  mg <-suppressWarnings(AutomodelGrid(data_train, tunelength = 6))

  # #variabe importance
  #
  vi_score <- vip::vi(mg$model_fits$Ranger)

  # #roc_auc metric
  roc_score <- mg$model_fits$Ranger %>%
    predict(data_test, "prob") %>%
    dplyr::bind_cols(data_test) %>%
    yardstick::roc_auc(.data$incidence, .data$susceptible)

  #model accuracy
  accuracy_score <-
    table(data_test$incidence, predict(mg$model_fits$Ranger, data_test) ) %>%
    yardstick::accuracy()

  data_test_all <- data_recipe %>%
    recipes::bake(distance_df)

   predicted <-
     dplyr::bind_cols(dplyr::select(distance_df, .data$species),
                      predict(mg$model_fits$Ranger, data_test_all, "prob")) %>%
     dplyr::select(-.data$unknown)

  return(list(model = mg$model_fits$Ranger,
              vi = vi_score,
              roc = roc_score,
              accuracy = accuracy_score,
              predicted = predicted))
}
