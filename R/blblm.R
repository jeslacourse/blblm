
#' Linear Modeling with Little Bag of Bootstraps
#' @name blblm
#'
#' @import purrr
#' @import stats
#' @importFrom magrittr %>%
#' @importFrom utils "capture.output"
#'
#'
"_PACKAGE"


#' Bag of Little Bootstraps for Linear Models
#'
#' The  primary namesake function subsets the data into m sections,
#' then bootstraps each subsample B times.
#'
#' @param formula the formula to be modelled
#' @param data    the source data
#' @param m       the number of subsamples
#' @param B       the number of bootstraps performed per subsample
#'
#' @export
#'
#' @examples
#' fit <- blblm(Sepal.Length ~ Petal.Width + Petal.Length, data = iris, m = 10, B = 15)
#'
#' # An example with an interaction
#' blblm(Sepal.Length ~ Petal.Width + Petal.Length + Petal.Width * Petal.Length,
#'                           data = iris, m = 10, B = 15)
#'
blblm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m) # creates subsamples
  estimates <- map(                # creates a list of bootstraps list coefs, sigmas
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  )
  res <- list(estimates = estimates, formula = formula) # the result is the estimate lists & formula
  class(res) <- "blblm"                                 #
  invisible(res)                                        # return (invisible) results

} # end blblm




#' Split Data
#'
#' An internal function to split data into m parts of approximated equal sizes
#'
#' @param data  the source data
#' @param m     the number of subsamples
#'
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)

} # end split_data


#' Linear Regression Model of Each Subsample
#'
#' An internal function to compute linear regression model
#' estimates for a subsample
#'
#' @param formula the formula to be modelled
#' @param data    the source data
#' @param n       the sample size
#' @param B       the number of bootstraps performed
#'
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)

} # end lm_each_subsample




#' Linear Regression Model of Each Bootstrap
#'
#' An internal function to compute linear regression estimates
#' for each boot of a given bag of bootstraps
#'
#' @param formula the formula to be modelled
#' @param data    the source data
#' @param n       the sample size
#'
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)

} # end lm_each_boot




#' Linear Regression Model
#'
#' An internal function to compute linear regression estimates
#' based on given number of repetitions
#'
#' @param formula the formula to be modelled
#' @param data    the source data
#' @param freqs   multinomial dist weights
#'
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick wrong variables from a parent scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))

} # end lm1



#' Linear Regression Coefficients
#'
#' An internal function to pull linear regression
#' coefficient estimates from the linear model
#'
#' @param fit  the fitted lm object
#'
blbcoef <- function(fit) {

  # The coefficients are the intercept and regression slopes
  fit$coefficients # pulled directly from lm model

} # end blbcoef




#' Linear Regression Sigma
#'
#' An internal function to pull linear regression
#' coefficient sigma from the linear model
#'
#' @param fit  the fitted lm object
#'
#'
blbsigma <- function(fit) {
  p <- fit$rank         # features + 1
  y <- model.extract(fit$model, "response") # point residuals
  e <- fitted(fit) - y                      # residuals
  w <- fit$weights                          # weights

  summary(fit)$sigma # pulled directly from lm model


} # end blbsigma



#' Print
#'
#' Print fitted object model
#'
#' @param x   the print object
#' @param ... additional parameters related to print.lm
#'
#' @export
#' @method print blblm
#'
print.blblm <- function(x, ...) {
  cat("blblm model:",capture.output(x$formula)) # pull from

} # end print.blblm




#' Sigma of Bag of Little Bootstraps
#'
#' The mean sigma for the BLB
#'
#' @param object     the fitted object
#' @param confidence boolean request for confidence interval
#' @param level      confidence level
#' @param ...        additional parameters related to sigma.lm
#'
#' @export
#' @method sigma blblm
#'
#' @example
#' sigma(fit)
#'
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {

  # Create a sigma dataframe, collection of subset-boot sigmas
  sigma_df <- est_df(object,"sigma")

  # Calculate sigma
  sigma <-  sigma_df %>% mean  #calculate mean of all sigmas collected

  # If confidence interval also requested
  if(confidence == TRUE){
      # Return sigma ci
      se <- sigma_df %>% sd()                                       # calculate std error for sigma
      alpha <- 1 - level                                            # set alpha

      sigma_ci <- with(object, sigma) + qnorm(1-alpha/2) * c(-1, 1) * se  # create confidence interval
      data.frame("sigma"= sigma, "lwr"= sigma_ci[1], "upr"= sigma_ci[2])  # and return it as a dataframe

  # Otherwise just return sigma
  }else{
      sigma
  }
} # end sigma.blblm




#' Coefficients of Bag of Little Bootstraps
#'
#' The mean coefficients for the BLB
#'
#' @param object  the fitted object
#' @param ...     additional parameters related to coef.lm
#'
#' @export
#' @method coef blblm
#'
#'
coef.blblm <- function(object, ...) {

  #compute coef matrix, the mean of subset bootstrap coef means
  est_df(object,"coef") %>%  # Create a coefficient dataframe
    colMeans()               #  and return the coef means

} # end coef.blblm




#' Confidence Interval of Bag of Little Bootstraps
#'
#' The confidence interval of coefficients for the BLB
#'
#' @param object the fitted object
#' @param parm   parameters for requested conf interval
#' @param level  confidence level
#' @param ...    additional parameters related to confint.lm
#'
#' @export
#' @method confint blblm
#'
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  alpha <- 1 - level

  # Create a confidence interval about each coefficient
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")         # Use all parameters
  }else{
    parm +1                                                    # Return just requested
  }

  ci <- est_df(object, "coef") %>%                             # Start with coef df
      apply(MARGIN =  2,                                       #  and calculate the ci col wise
            function(x){
              mean(x) + qnorm(1-alpha/2)*c(-1, 1)*sd(x)}) %>%
      as.data.frame(row.names = c("lwr", "upr")) %>% t          # store as a dataframe
  ci[parm,]                                                     # return requested parameters

} # end confint.blblm




#' Predicting with Bag of Little Bootstraps
#'
#' Prediction with the mean estimates of the bag of boots
#' on new data.
#'
#' @param object     the fitted object
#' @param newdata    data used to predict
#' @param confidence boolean request for confidence interval
#' @param level      confidence level
#' @param ...        additional parameters related to predict.lm
#'
#' @export
#' @method predict blblm
#'
predict.blblm <- function(object, newdata, confidence = FALSE, level = 0.95, ...) {

  # Create a design matrix
  #  Refactor X using just the formula variables, set intercepts to 1
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), newdata)

  # Predictions are the dot product of coefs and X
  y_preds <- X %*% coef(object) %>% t # dot product, transposed for formatting



  if (confidence) {
    # YOUR CODE to compute the predictions and their confidence intervals
    se <- sigma(object)
    alpha <- 1-level
    ci <- y_preds %>%                                   # Start with predicted values
           apply(MARGIN =  2,                           #   and calculate the ci col wise
           function(x){
              mean(x) + qnorm(1-alpha/2)*c(-1, 1)*se}) %>%
           as.data.frame(row.names = c("lwr", "upr")) %>% t

    # return fit values with ci bounds
    data.frame(cbind(t(y_preds), ci)) %>% set_names(.,c("fit", "lwr", "upr"))

  } else {
     y_preds # Just return the y preds
  }
} # end predict.blblm


#### HELPER FUNCTIONS ###


#' Create Dataframe of Estimate Values
#'
#' An internal function to compute build a dataframe
#' based on requested estimate value
#'
#' @param object the fitted object
#' @param value the estimate value requested
#'
#' @export
#'
est_df<- function(object, value){
  object$estimates %>% unlist(recursive = FALSE) %>%   # Breakdown formatting in est list
    flatten %>% keep(names(.) == value) %>%             #  and keep the requested value
    do.call(rbind,.)                                    #  to create a dataframe
}

### END USER CREATED HELPER FUNCTIONS

