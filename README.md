# blblm

<!-- badges: start -->
<!-- badges: end -->

While both subsampling and bootstrapping are powerful tools, combining the two can increase accuracy through the creation of similar-yet-original datasets. `blblm` bootstraps linear models. 

This package is designed for intuitive usage, particularly for users familiar with generalized linear modelling tools in R.


## Vignettes 

Several Vignettes Available for more information: 
`browseVignettes('blblm')`

**blblm Background and Motivation**
blblm_Background_and_Motivation

Background understanding of bootstrapping and subsamplings. 

**blblm Project Improvements and Features** 

`vignette("blblm_Project_Improvements_Features")`
Walkthrough of my project building process and Discussion.

**Linear Regression with Little Bag of Bootstraps**

`vignette("blblm")` 
Details of the `blblm` functions


**Generalized Linear Modeling with Little Bag of Bootstraps**

`vignette("blbglm")` 
Details of `blbglm` variant of `blblm`


## Examples

```
#
# blblm (Linear Regression)
#

fit <- blblm(Sepal.Length ~ Petal.Width + Petal.Length, data = iris, m = 10, B = 15

print(fit)
#> blblm model: Sepal.Length ~ Petal.Width + Petal.Length

sigma(fit, confidence =TRUE)
#>     sigma       lwr      upr
#> 1 1.35056 0.7269986 1.974122

coef(fit)
#>  (Intercept)  Petal.Width Petal.Length 
#>    4.2258393   -0.2271371    0.5063443

confint(fit)
#>                     lwr       upr
#> (Intercept)   3.7077705 4.7439082
#> Petal.Width  -0.9379363 0.4836620
#> Petal.Length  0.2151031 0.7975856

predict(fit, iris[4,])
#>             4
#> [1,] 4.939928

```


```
#
# blbglm (Gaussian Family GLM)
#

fit <- blbglm(Sepal.Length ~ Species, data = iris, m = 10, B = 10)

print(fit)
#> blbglm model: Sepal.Length ~ Species

sigma(fit, confidence =TRUE, level = .90)
#>      sigma       lwr      upr
#> 1 1.632753 0.7263946 2.539112

coef(fit)
#>       (Intercept) Speciesversicolor  Speciesvirginica 
#>         4.9322963         0.9944662         1.6553871

confint(fit)
#>                         lwr      upr
#> (Intercept)       4.4442349 5.420358
#> Speciesversicolor 0.1630075 1.825925
#> Speciesvirginica  0.9054076 2.405367

predict(fit, iris[4,])
#>             4
#> [1,] 4.932296

```
