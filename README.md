#HM\_4
================
by Tamires Amorim
4/5/2021

### Importing dataset

``` r
data<-data.frame(fread("insurance.csv"))
attach(data)
```

### Exploratory data analysis:

``` r
head(data)
```

    ##   age    sex    bmi children smoker    region   charges
    ## 1  19 female 27.900        0    yes southwest 16884.924
    ## 2  18   male 33.770        1     no southeast  1725.552
    ## 3  28   male 33.000        3     no southeast  4449.462
    ## 4  33   male 22.705        0     no northwest 21984.471
    ## 5  32   male 28.880        0     no northwest  3866.855
    ## 6  31 female 25.740        0     no southeast  3756.622

``` r
glimpse(data)
```

    ## Rows: 1,338
    ## Columns: 7
    ## $ age      <int> 19, 18, 28, 33, 32, 31, 46, 37, 37, 60, 25, 62, 23, 56, 27...
    ## $ sex      <chr> "female", "male", "male", "male", "male", "female", "femal...
    ## $ bmi      <dbl> 27.900, 33.770, 33.000, 22.705, 28.880, 25.740, 33.440, 27...
    ## $ children <int> 0, 1, 3, 0, 0, 0, 1, 3, 2, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0...
    ## $ smoker   <chr> "yes", "no", "no", "no", "no", "no", "no", "no", "no", "no...
    ## $ region   <chr> "southwest", "southeast", "southeast", "northwest", "north...
    ## $ charges  <dbl> 16884.924, 1725.552, 4449.462, 21984.471, 3866.855, 3756.6...

``` r
summary(data)
```

    ##       age            sex                 bmi           children    
    ##  Min.   :18.00   Length:1338        Min.   :15.96   Min.   :0.000  
    ##  1st Qu.:27.00   Class :character   1st Qu.:26.30   1st Qu.:0.000  
    ##  Median :39.00   Mode  :character   Median :30.40   Median :1.000  
    ##  Mean   :39.21                      Mean   :30.66   Mean   :1.095  
    ##  3rd Qu.:51.00                      3rd Qu.:34.69   3rd Qu.:2.000  
    ##  Max.   :64.00                      Max.   :53.13   Max.   :5.000  
    ##     smoker             region             charges     
    ##  Length:1338        Length:1338        Min.   : 1122  
    ##  Class :character   Class :character   1st Qu.: 4740  
    ##  Mode  :character   Mode  :character   Median : 9382  
    ##                                        Mean   :13270  
    ##                                        3rd Qu.:16640  
    ##                                        Max.   :63770

``` r
psych::describe(data)
```

    ##          vars    n     mean       sd  median  trimmed     mad     min      max
    ## age         1 1338    39.21    14.05   39.00    39.01   17.79   18.00    64.00
    ## sex*        2 1338     1.51     0.50    2.00     1.51    0.00    1.00     2.00
    ## bmi         3 1338    30.66     6.10   30.40    30.50    6.20   15.96    53.13
    ## children    4 1338     1.09     1.21    1.00     0.94    1.48    0.00     5.00
    ## smoker*     5 1338     1.20     0.40    1.00     1.13    0.00    1.00     2.00
    ## region*     6 1338     2.52     1.10    3.00     2.52    1.48    1.00     4.00
    ## charges     7 1338 13270.42 12110.01 9382.03 11076.02 7440.81 1121.87 63770.43
    ##             range  skew kurtosis     se
    ## age         46.00  0.06    -1.25   0.38
    ## sex*         1.00 -0.02    -2.00   0.01
    ## bmi         37.17  0.28    -0.06   0.17
    ## children     5.00  0.94     0.19   0.03
    ## smoker*      1.00  1.46     0.14   0.01
    ## region*      3.00 -0.04    -1.33   0.03
    ## charges  62648.55  1.51     1.59 331.07

``` r
#From the summary is observed that mean age is 39.21. The mean is 1 child per household. The charges goes from minimum of $1122.00 to $63770.00. The mean charge is $13270.00 (note that BMI stands for body mass index).
```

``` r
#checking for outlier in numeric variables: #bmi and charges have outlier that may affect the model later on.
meltData <- melt(data)
```

    ## Using sex, smoker, region as id variables

``` r
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")
```

![](HM_4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#correlation matrix: in theory the correlation between the independent variables should be zero. But, We expect a weak correlation between independent variables, and high correlation with the target variable (charges).
corrgram(data, order=TRUE)
```

![](HM_4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Regression Models:

``` r
#Spliting the data into training and test subsets. 

split <- initial_split(data, prop = .8)

data_train <- training(split)
data_test <- testing(split)

dim(data_train) #80% of the random observations is in the
```

    ## [1] 1071    7

``` r
#training dataset. 

#Regression models:
#model1: age as the dependent variable
#model2: age and sex as the dependent variables
#model3: age, sex, number of children covered by health insurance, BMI, smoker and region
#where the beneficiary lives. 

model1 <- lm(charges ~ age, data = data_train)
coef(model1)
```

    ## (Intercept)         age 
    ##   3200.3387    262.0196

``` r
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ age, data = data_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -8248  -6827  -6175   5629  47602 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3200.34    1063.03   3.011  0.00267 ** 
    ## age           262.02      25.53  10.262  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11760 on 1069 degrees of freedom
    ## Multiple R-squared:  0.08967,    Adjusted R-squared:  0.08882 
    ## F-statistic: 105.3 on 1 and 1069 DF,  p-value: < 2.2e-16

``` r
#From model1 we can interpret that the mean insurance charging price increases by 
# 282.13 dollars as people gets older. 

model2 <- lm(charges ~ age + sex, data = data_train)
coef(model2)
```

    ## (Intercept)         age     sexmale 
    ##   2388.2879    262.3914   1578.7535

``` r
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ age + sex, data = data_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -9029  -7156  -5642   5808  48049 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2388.29    1123.51   2.126   0.0338 *  
    ## age           262.39      25.49  10.294   <2e-16 ***
    ## sexmale      1578.75     717.70   2.200   0.0280 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11740 on 1068 degrees of freedom
    ## Multiple R-squared:  0.09378,    Adjusted R-squared:  0.09208 
    ## F-statistic: 55.26 on 2 and 1068 DF,  p-value: < 2.2e-16

``` r
#for males in the sample, there is an increase in charges by $1,189.84. But it is not as significant as the variable age in the model.

model3 <- lm(charges ~ ., data = data_train)
coef(model3)
```

    ##     (Intercept)             age         sexmale             bmi        children 
    ##    -12218.82508       250.94188       -68.43728       351.49280       477.89010 
    ##       smokeryes regionnorthwest regionsoutheast regionsouthwest 
    ##     23923.95724      -119.19658      -913.67974      -806.41957

``` r
summary(model3)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ ., data = data_train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11329.2  -2815.2   -946.3   1593.9  29906.2 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -12218.83    1119.00 -10.919  < 2e-16 ***
    ## age                250.94      13.41  18.718  < 2e-16 ***
    ## sexmale            -68.44     376.44  -0.182  0.85577    
    ## bmi                351.49      32.85  10.701  < 2e-16 ***
    ## children           477.89     156.10   3.062  0.00226 ** 
    ## smokeryes        23923.96     460.21  51.985  < 2e-16 ***
    ## regionnorthwest   -119.20     536.21  -0.222  0.82413    
    ## regionsoutheast   -913.68     548.66  -1.665  0.09615 .  
    ## regionsouthwest   -806.42     533.62  -1.511  0.13103    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6131 on 1062 degrees of freedom
    ## Multiple R-squared:  0.7543, Adjusted R-squared:  0.7525 
    ## F-statistic: 407.6 on 8 and 1062 DF,  p-value: < 2.2e-16

``` r
#the regions selected by default were all representing a decrease in charges, for people that answered yes for smoking, the increase in charges is very high, and significant, for smokers the rate by what the charges increases is 23,817.34.For body mass and children on the household there are also higher increases in the insurance.
#In this model, there is a negative sign for the men, different from model 2. Multiple R-squared:75%, meaning that age, bmi, children and smoker variables when put together, can explain 75% of the variance in the charges variable. This is not a very accurate model, but still good since is higher than 0.70.There is no multicollinearity in the data because the R-square and Adjusted R-squared do not have a large difference. 

model4 <- lm(charges ~ age + bmi + children + smoker, data = data_train)
summary(model4)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ age + bmi + children + smoker, data = data_train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11891.6  -2908.6   -958.3   1540.2  29430.7 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -12189.89    1065.26 -11.443  < 2e-16 ***
    ## age            251.39      13.40  18.761  < 2e-16 ***
    ## bmi            333.65      31.22  10.686  < 2e-16 ***
    ## children       476.80     155.98   3.057  0.00229 ** 
    ## smokeryes    23910.03     457.83  52.225  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6132 on 1066 degrees of freedom
    ## Multiple R-squared:  0.7533, Adjusted R-squared:  0.7524 
    ## F-statistic: 813.8 on 4 and 1066 DF,  p-value: < 2.2e-16

``` r
#From model 3, I extract all the variables that were not significant, and left only the ones with some degree of significance level, to observe the differences in the predicability of the linear regression between both models. 
```

### Comparing the models:

``` r
#Comparing models: this points that model4 is the best model, because it has the
# minimum AIC and BIC, both values measures "measure of fit + complexity penalty",
#known as penalized-likelihood criteria. 
AIC(model1)
```

    ## [1] 23119.89

``` r
BIC(model1)
```

    ## [1] 23134.82

``` r
AIC(model2)
```

    ## [1] 23117.05

``` r
BIC(model2)
```

    ## [1] 23136.95

``` r
AIC(model3)
```

    ## [1] 21731.09

``` r
BIC(model3)
```

    ## [1] 21780.85

``` r
AIC(model4)
```

    ## [1] 21727.49

``` r
BIC(model4)
```

    ## [1] 21757.35

``` r
names(model3) #just to check the vectors present on the models
```

    ##  [1] "coefficients"  "residuals"     "effects"       "rank"         
    ##  [5] "fitted.values" "assign"        "qr"            "df.residual"  
    ##  [9] "contrasts"     "xlevels"       "call"          "terms"        
    ## [13] "model"

``` r
rmse(actual = data_train$charges, predicted = model4$fitted.values)
```

    ## [1] 6118.126

``` r
rmse(actual = data_train$charges, predicted = model3$fitted.values)
```

    ## [1] 6105.567

``` r
rmse(actual = data_train$charges, predicted = model2$fitted.values)
```

    ## [1] 11726.42

``` r
rmse(actual = data_train$charges, predicted = model1$fitted.values)
```

    ## [1] 11752.96

``` r
#The model with the lower RMSE is considered a better model, in this case model 3.

#Linear Regression Assumptions: 
#1. Errors should follow normal distribution. From plots 1 and 2, the errors are skewed to the right, the plot 3 and 4, is somewhat normalized, slightly skewed to the right as well, probably because as we observed before, there were outliers.

par(mar=c(1,1,1,1)) #to fix the error figure margins too large

par(mfrow=c(2,2))
hist(model1$residuals)
hist(model2$residuals)

par(mfrow=c(2,2))
```

![](HM_4_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
hist(model3$residuals)
hist(model4$residuals)


#As observed before the models are not very close to zero, and do not follow a normal distribution.In these models we will have to deal with heteroskedasticity. 

par(mfrow=c(2,2))
```

![](HM_4_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
plot(model3,col="grey",pch=16,cex=1,lwd=1,lty=2)
```

![](HM_4_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
par(mfrow=c(2,2))
plot(model4,col="grey",pch=16,cex=1,lwd=1,lty=2)
```

![](HM_4_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
#Checking for multicollinearitty: a liner model assumes that the predictor variables do not correlate with each other, there is little or no multicollinearity with the variables in the models.
VIF(model3)
```

    ## [1] 4.070459

``` r
VIF(model1)
```

    ## [1] 1.098502

``` r
VIF(model2)
```

    ## [1] 1.103479

``` r
VIF(model4)
```

    ## [1] 4.053766

``` r
#there is no auto serial correlation (the error terms do not correlate with each other). However, we can see a negative correlation, since is a little higher than 2. 
dwtest(model4)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  model4
    ## DW = 2.0931, p-value = 0.9358
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
#Checking accuracy with the test dataset:

data_test$predictedcharges <- predict(model4,data_test)
head(data_test[,c("charges", "predictedcharges")])
```

    ##      charges predictedcharges
    ## 6   3756.622         4191.237
    ## 13  1826.843         5069.542
    ## 15 39611.758        32564.228
    ## 20 36837.467        31039.566
    ## 24 37701.877        31394.178
    ## 30 38711.000        32578.197

``` r
actual <- data_test$charges
preds <- data_test$predictedcharges
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq
```

    ## [1] 0.7298769

``` r
# When we compare model 3, model 4 and the test dataset, the accuracy for the test was 0.74128, which is close to both models 0.7466 (model3) and 0.7453 (model4).
```

\#\#Conclusions: The model 4 proved to be the most accurate, for
numerous reasons, one of them its that the variables were highly
significant. Something that could improve the models prediction is to
decrease the outliers to avoid heteroskedasticity.
