## Lesson 7 - Thursday 10/13/22

* We will spend most of our time in class this week going over last week's practice problems and discussing the mid-term exam.

## Mid-Term Exam - Due Monday 10/24/22

* Here is the R code you will use to set up your dataset for this week's exam:

```r
set.seed(229)
ncases <- 350
r <- runif(n=ncases,min=0,max=1)
trt <- rep(2,ncases)
trt[r<0.333] <- 1
trt[r>0.667] <- 3
arrest <- rep(0,ncases)
arrest[trt==1] <- 1
advise <- rep(0,ncases)
advise[trt==2] <- 1
table(trt,arrest)
table(trt,advise)
aggcirc <- rep(0,ncases)
s <- runif(n=ncases,min=0,max=1)
aggcirc[s>0.7] <- 1
u <- runif(n=ncases,min=0,max=1)
e <- log(u/(1-u))
ystar <- -1.1260-0.9943*arrest-0.1425*advise-
  0.1470*aggcirc+0.1701*arrest*aggcirc-0.1070*
  advise*aggcirc+e
y <- rep(0,ncases)
y[ystar>0] <- 1
table(y,trt)
df <- data.frame(trt,aggcirc,y)
```

* For the exam you should change the seed to the last 3 digits of your UID number.
* Once you have this datset in hand, you should address the following problems.

Part 1: Let the binary arrest variable be the treatment (leave advise and separate in the same category) and let y be the outcome.

1. Estimate p(y=1|x=1) and p(y=1|x=0)
2. Calculate the difference between the fractions (delta)
3. Calculate the standard error of delta
4. Calculate the relative risk
5. Calculate the odds ratio, the log odds ratio, and the standard error of the log-odds ratio
6. Compare your results above to what you obtain from the linear probability and logistic regression models
7. Calculate and interpret the Yule's Q statistic for the table
8. Use the grid-search method to calculate the maximum likelihood estimate of the failure rate for the entire sample
9. Use the grid-search method to calculate the maximum likelihood estimate of the failure rates in the arrest and no-arrest groups
10. Using the methods described in lesson 4, calculate a likelihood ratio test of the hypothesis that the failure rates are equal between the arrest and no-arrest groups.
11. Verify the results you got in #10 with the results you get from standard logistic regression software.
12. Use AIC and BIC to consider whether the arrest and no-arrest groups have different failure rates.
13. Calculate the standard error of the failure rate for the entire sample using the normal approximation to the binomial.
14. Use the finite difference approach to calculate the standard error of the failure rate for the entire sample.
15. Calculate 95% confidence limits for the failure rate of the entire sample using both the normal approximation to the binomial and the finite difference approach.

Part 2: We now consider all three treatment groups separately.

16. Use the treatment and outcome data to construct a Pearson chi-square test of independence.
17. Compare the results of the Pearson chi-square test of independence to the likelihood ratio chi-square test.
18. Estimate a logistic regression model that includes both of the treatment group variables as independent variables.
19. Calculate the failure rates for each of the treatment groups using the contingency table approach compared to the rates implied by the logistic regression analysis estimated in 18.
20. Add the aggravating circumstances variable to the logistic regression model. Interpret the results.
21. Add an interaction term to the logit model you estimated in part 4 to get a saturated model.
22. Verify that the saturated model recovers the failure rates that can be calculated from the contingency table.
23. Examine the likelihood-ratio test, AIC, and BIC to discern whether the saturated model or the main-effects-only model is more consistent with the data.


### New Material (Final Exam Starts Here)

* We begin by reading in the nc78.csv dataset I sent you today.

```r
# read in our dataset

df <- read.csv(file="nc78.csv",sep=",",header=T)

# look at a random sample of cases

df[sample(nrow(df),size=25,replace=F), ]
```

and here is what we get (your sample will be different from mine):

```rout
> # read in our dataset
> 
> df <- read.csv(file="nc78.csv",sep=",",header=T)
> 
> # look at a random sample of cases
> 
> df[sample(nrow(df),size=25,replace=F), ]
        X male ageyears recid
4870 4870    1       57     1
7989 7989    1       34     0
8612 8612    1       21     0
1835 1835    0       33     1
5830 5830    1       44     0
331   331    1       38     0
4095 4095    1       40     0
68     68    0       30     0
1189 1189    1       30     0
2860 2860    1       30     0
7106 7106    1       31     1
6078 6078    1       31     0
6689 6689    1       19     1
1616 1616    1       34     0
3284 3284    1       26     1
4524 4524    1       34     0
4080 4080    1       25     0
6101 6101    1       17     1
2799 2799    1       25     1
684   684    1       19     0
8157 8157    1       18     1
1481 1481    1       54     1
1556 1556    1       60     0
2882 2882    1       24     1
5535 5535    1       18     0
> 
```

* Now, we are going to look at the marginal distributions of our variables.
* Let's begin with the dependent variable, recidivism (measured over a 70-month follow-up period):

```r
# recidivism (within 70 months)

table(df$recid,exclude=NULL)
table(df$recid)/nrow(df)
```

* Here are the results:

```rout
> # recidivism (within 70 months)
> 
> table(df$recid,exclude=NULL)

   0    1 
5800 3527 
> table(df$recid)/nrow(df)

        0         1 
0.6218505 0.3781495 
> 
```

* Next, we turn to the distribution of males/females in the data and the joint distribution of sex and recidivism:

```r
# sex distribution

table(df$male,exclude=NULL)
sd <- table(df$recid,df$male,exclude=NULL)
sd

py1x1 <- sd[2,2]/(sd[1,2]+sd[2,2])
py1x1
py1x0 <- sd[2,1]/(sd[1,1]+sd[2,1])
py1x0
py1x1-py1x0
```

* This what we get:

```rout
> # sex distribution
> 
> table(df$male,exclude=NULL)

   0    1 
 469 8858 
> sd <- table(df$recid,df$male,exclude=NULL)
> sd
   
       0    1
  0  367 5433
  1  102 3425
> 
> py1x1 <- sd[2,2]/(sd[1,2]+sd[2,2])
> py1x1
[1] 0.3866561
> py1x0 <- sd[2,1]/(sd[1,1]+sd[2,1])
> py1x0
[1] 0.217484
> py1x1-py1x0
[1] 0.1691721
> 
```

* Now, we estimate a bivariate logistic regression model to corroborate what we see in the table above:


```r
# estimate bivariate logistic regression model

summary(glm(recid~1+male,data=df,family=binomial(link="logit")))

logit.m <- -1.2804+1*0.8190
exp(logit.m)/(1+exp(logit.m))

logit.f <- -1.2804+0*0.8190
exp(logit.f)/(1+exp(logit.f))
```

* These are the logistic regression results:

```rout
> # estimate bivariate logistic regression model
> 
> summary(glm(recid~1+male,data=df,family=binomial(link="logit")))

Call:
glm(formula = recid ~ 1 + male, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.9888  -0.9888  -0.9888   1.3786   1.7468  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.2804     0.1119 -11.439  < 2e-16 ***
male          0.8190     0.1140   7.182 6.88e-13 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12370  on 9326  degrees of freedom
Residual deviance: 12312  on 9325  degrees of freedom
AIC: 12316

Number of Fisher Scoring iterations: 4

> 
> logit.m <- -1.2804+1*0.8190
> exp(logit.m)/(1+exp(logit.m))
[1] 0.3866538
> 
> logit.f <- -1.2804+0*0.8190
> exp(logit.f)/(1+exp(logit.f))
[1] 0.2174821
> 
```

* Now, we turn to the age distribution in the dataset. We start by opening a plot window and generating a barplot showing the age distribution of the persons released from North Carolina prisons in 1978:

```r
# open a 1-row, 3-column plot window

par(mfrow=c(1,3))

# age distribution

table(df$ageyears,exclude=NULL)
median(df$ageyears)
mean(df$ageyears)
barplot(table(df$ageyears),
  xlab="Age (in years)",
  ylab="Number of People")
```

* Here are the results:

```rout
> # open a 1-row, 3-column plot window
> 
> par(mfrow=c(1,3))
> 
> # age distribution
> 
> table(df$ageyears,exclude=NULL)

 16  17  18  19  20  21  22  23  24  25  26  27  28  29  30 
 19 161 492 480 624 599 580 468 537 443 432 338 415 292 324 
 31  32  33  34  35  36  37  38  39  40  41  42  43  44  45 
254 234 179 187 167 177 132 152 117 119  93 113 102  85  75 
 46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
 90  72  86  62  78  61  57  50  44  49  55  34  34  25  21 
 61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
 18  19  11  16   7   5  13   5   3   1   3   5   3   4   2 
 77  78 
  2   2 
> median(df$ageyears)
[1] 26
> mean(df$ageyears)
[1] 29.32787
> barplot(table(df$ageyears),
+   xlab="Age (in years)",
+   ylab="Number of People")
> 
```

* Next, we show how age is related to the fraction of persons who were observed to recidivate within 70 months:

```r
df$age.factor <- factor(df$ageyears,levels=16:78)
table(df$recid,df$age.factor,exclude=NULL)

fail.age <- table(df$recid,df$age.factor)[2,]
fail.age
n.age <- (table(df$recid,df$age.factor)[1,]+table(df$recid,df$age.factor)[2,])
n.age

pfail.age <- fail.age/n.age
pfail.age

plot(x=16:78,y=pfail.age,
  type="l",lty=1,lwd=2,
  xlab="Age at Release (in years)",
  ylab="Proportion Observed to Fail Within 70 Months")
abline(h=seq(from=0,to=1,by=0.1),lty=2,lwd=0.5)
abline(v=seq(from=0,to=80,by=10),lty=2,lwd=0.5)
```

* Here are the tabled results:

```rout
> df$age.factor <- factor(df$ageyears,levels=16:78)
> table(df$recid,df$age.factor,exclude=NULL)
   
     16  17  18  19  20  21  22  23  24  25  26  27  28  29
  0   9  78 242 246 338 362 339 311 313 279 262 235 251 173
  1  10  83 250 234 286 237 241 157 224 164 170 103 164 119
   
     30  31  32  33  34  35  36  37  38  39  40  41  42  43
  0 208 152 149 123 133 111 118  85 108  81  80  66  84  67
  1 116 102  85  56  54  56  59  47  44  36  39  27  29  35
   
     44  45  46  47  48  49  50  51  52  53  54  55  56  57
  0  58  52  64  52  60  43  54  42  39  37  35  36  39  27
  1  27  23  26  20  26  19  24  19  18  13   9  13  16   7
   
     58  59  60  61  62  63  64  65  66  67  68  69  70  71
  0  25  18  18  18  13   7  12   7   5  11   5   0   1   2
  1   9   7   3   0   6   4   4   0   0   2   0   3   0   1
   
     72  73  74  75  76  77  78
  0   5   2   4   2   0   2   2
  1   0   1   0   0   0   0   0
> 
> fail.age <- table(df$recid,df$age.factor)[2,]
> fail.age
 16  17  18  19  20  21  22  23  24  25  26  27  28  29  30 
 10  83 250 234 286 237 241 157 224 164 170 103 164 119 116 
 31  32  33  34  35  36  37  38  39  40  41  42  43  44  45 
102  85  56  54  56  59  47  44  36  39  27  29  35  27  23 
 46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
 26  20  26  19  24  19  18  13   9  13  16   7   9   7   3 
 61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
  0   6   4   4   0   0   2   0   3   0   1   0   1   0   0 
 76  77  78 
  0   0   0 
> n.age <- (table(df$recid,df$age.factor)[1,]+table(df$recid,df$age.factor)[2,])
> n.age
 16  17  18  19  20  21  22  23  24  25  26  27  28  29  30 
 19 161 492 480 624 599 580 468 537 443 432 338 415 292 324 
 31  32  33  34  35  36  37  38  39  40  41  42  43  44  45 
254 234 179 187 167 177 132 152 117 119  93 113 102  85  75 
 46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
 90  72  86  62  78  61  57  50  44  49  55  34  34  25  21 
 61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
 18  19  11  16   7   5  13   5   3   1   3   5   3   4   2 
 76  77  78 
  0   2   2 
> 
> pfail.age <- fail.age/n.age
> pfail.age
       16        17        18        19        20        21 
0.5263158 0.5155280 0.5081301 0.4875000 0.4583333 0.3956594 
       22        23        24        25        26        27 
0.4155172 0.3354701 0.4171322 0.3702032 0.3935185 0.3047337 
       28        29        30        31        32        33 
0.3951807 0.4075342 0.3580247 0.4015748 0.3632479 0.3128492 
       34        35        36        37        38        39 
0.2887701 0.3353293 0.3333333 0.3560606 0.2894737 0.3076923 
       40        41        42        43        44        45 
0.3277311 0.2903226 0.2566372 0.3431373 0.3176471 0.3066667 
       46        47        48        49        50        51 
0.2888889 0.2777778 0.3023256 0.3064516 0.3076923 0.3114754 
       52        53        54        55        56        57 
0.3157895 0.2600000 0.2045455 0.2653061 0.2909091 0.2058824 
       58        59        60        61        62        63 
0.2647059 0.2800000 0.1428571 0.0000000 0.3157895 0.3636364 
       64        65        66        67        68        69 
0.2500000 0.0000000 0.0000000 0.1538462 0.0000000 1.0000000 
       70        71        72        73        74        75 
0.0000000 0.3333333 0.0000000 0.3333333 0.0000000 0.0000000 
       76        77        78 
      NaN 0.0000000 0.0000000 
> 
```

* Next, we calculate a logistic regression model measuring the relationship between age-at-release and recidivism risk. Note that this code will add a purple smooth curve to the age-recidivism plot below:

```r
age.model <- glm(recid~1+ageyears,data=df,family=binomial(link="logit"))
summary(age.model)
x.age <- seq(from=16,to=78,by=1)
logit.age <- 0.250539-0.025866*x.age
pred.age <- exp(logit.age)/(1+exp(logit.age))
lines(x=x.age,y=pred.age,lty=1,lwd=2,col="purple")
```

* Here are the logistic regression results:

```rout
> age.model <- glm(recid~1+ageyears,data=df,family=binomial(link="logit"))
> summary(age.model)

Call:
glm(formula = recid ~ 1 + ageyears, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.1089  -1.0145  -0.8766   1.3153   1.9054  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  0.250539   0.065699   3.813 0.000137 ***
ageyears    -0.025866   0.002181 -11.860  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12370  on 9326  degrees of freedom
Residual deviance: 12221  on 9325  degrees of freedom
AIC: 12225

Number of Fisher Scoring iterations: 4

> x.age <- seq(from=16,to=78,by=1)
> logit.age <- 0.250539-0.025866*x.age
> pred.age <- exp(logit.age)/(1+exp(logit.age))
> lines(x=x.age,y=pred.age,lty=1,lwd=2,col="purple")
```

* And, then we calculate the derivative of the logistic response function (a model that only includes age):

```r
# calculate the derivative of the logistic response function at the median
# derivative of the logistic response function is b*p*(1-p) (King, 1989:109)

data.frame(x.age,logit.age,pred.age)

b <- -0.025866
p <- 0.3960438
derivative <- b*p*(1-p)
derivative
```

* Here are the results:

```rout
> # calculate the derivative of the logistic response function at the median
> # derivative of the logistic response function is b*p*(1-p) (King, 1989:109)
> 
> data.frame(x.age,logit.age,pred.age)
   x.age logit.age  pred.age
1     16 -0.163317 0.4592613
2     17 -0.189183 0.4528448
3     18 -0.215049 0.4464440
4     19 -0.240915 0.4400609
5     20 -0.266781 0.4336975
6     21 -0.292647 0.4273560
7     22 -0.318513 0.4210382
8     23 -0.344379 0.4147462
9     24 -0.370245 0.4084818
10    25 -0.396111 0.4022471
11    26 -0.421977 0.3960438
12    27 -0.447843 0.3898737
13    28 -0.473709 0.3837387
14    29 -0.499575 0.3776406
15    30 -0.525441 0.3715808
16    31 -0.551307 0.3655612
17    32 -0.577173 0.3595833
18    33 -0.603039 0.3536487
19    34 -0.628905 0.3477589
20    35 -0.654771 0.3419152
21    36 -0.680637 0.3361191
22    37 -0.706503 0.3303720
23    38 -0.732369 0.3246751
24    39 -0.758235 0.3190296
25    40 -0.784101 0.3134367
26    41 -0.809967 0.3078975
27    42 -0.835833 0.3024131
28    43 -0.861699 0.2969845
29    44 -0.887565 0.2916126
30    45 -0.913431 0.2862983
31    46 -0.939297 0.2810424
32    47 -0.965163 0.2758457
33    48 -0.991029 0.2707089
34    49 -1.016895 0.2656327
35    50 -1.042761 0.2606176
36    51 -1.068627 0.2556643
37    52 -1.094493 0.2507732
38    53 -1.120359 0.2459447
39    54 -1.146225 0.2411793
40    55 -1.172091 0.2364772
41    56 -1.197957 0.2318389
42    57 -1.223823 0.2272644
43    58 -1.249689 0.2227540
44    59 -1.275555 0.2183078
45    60 -1.301421 0.2139260
46    61 -1.327287 0.2096085
47    62 -1.353153 0.2053554
48    63 -1.379019 0.2011666
49    64 -1.404885 0.1970421
50    65 -1.430751 0.1929817
51    66 -1.456617 0.1889853
52    67 -1.482483 0.1850527
53    68 -1.508349 0.1811836
54    69 -1.534215 0.1773778
55    70 -1.560081 0.1736350
56    71 -1.585947 0.1699549
57    72 -1.611813 0.1663371
58    73 -1.637679 0.1627811
59    74 -1.663545 0.1592867
60    75 -1.689411 0.1558533
61    76 -1.715277 0.1524805
62    77 -1.741143 0.1491678
63    78 -1.767009 0.1459147
> 
> b <- -0.025866
> p <- 0.3960438
> derivative <- b*p*(1-p)
> derivative
[1] -0.006186969
> 
```

* Finally, we create a boxplot showing the relationship between age at the time of release (in years) and sex (male/female):

```r
# joint age x gender distribution

boxplot(df$ageyears~df$male,
  xlab="Sex",
  ylab="Age (in years)",
  names=c("Females","Males"))
```

* The resulting 1x3 plotspace is here:

<p align="left">
<img src="/gfiles/age-recid.png" width="800px">
</p>

* Now, we estimate a logistic regresssion analysis with both sex and age as predictor variables. Here is the R code:

```r
# now let's estimate a logistic regression model using age and 
# sex as predictor variables.

m1 <- glm(recid~1+male+ageyears,data=df,family=binomial(link="logit"))
summary(m1)
logLik(m1)
```

and here are the results:

```rout
> # now let's estimate a logistic regression model using age and 
> # sex as predictor variables.
> 
> m1 <- glm(recid~1+male+ageyears,data=df,family=binomial(link="logit"))
> summary(m1)

Call:
glm(formula = recid ~ 1 + male + ageyears, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.1276  -1.0304  -0.8503   1.2970   2.0321  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.555808   0.126760  -4.385 1.16e-05 ***
male         0.860076   0.114624   7.503 6.22e-14 ***
ageyears    -0.026411   0.002188 -12.071  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12370  on 9326  degrees of freedom
Residual deviance: 12156  on 9324  degrees of freedom
AIC: 12162

Number of Fisher Scoring iterations: 4

> logLik(m1)
'log Lik.' -6078.234 (df=3)
> 
```

* We now ask what this model implies about the risk of failure for males and females.
* The answer depends on what the value of age is.
* As an example, let's set the age to the median value of 26.

```r
logit.male <- -0.555808+1*0.860076-0.026411*26
pmale <- exp(logit.male)/(1+exp(logit.male))

logit.female <- -0.555808+0*0.860076-0.026411*26
pfemale <- exp(logit.female)/(1+exp(logit.female))

pmale-pfemale
```

* Here is what we get:

```rout
> logit.male <- -0.555808+1*0.860076-0.026411*26
> pmale <- exp(logit.male)/(1+exp(logit.male))
> 
> logit.female <- -0.555808+0*0.860076-0.026411*26
> pfemale <- exp(logit.female)/(1+exp(logit.female))
> 
> pmale-pfemale
[1] 0.1815417
> 
```

* For further insight, we can evaluate the function at the first and third quartiles of the age distribution:

```r
# first quartile of the age distribution

quantile(df$ageyears,0.25)

logit.male <- -0.555808+1*0.860076-0.026411*21
pmale <- exp(logit.male)/(1+exp(logit.male))

logit.female <- -0.555808+0*0.860076-0.026411*21
pfemale <- exp(logit.female)/(1+exp(logit.female))

pmale-pfemale

# third quartile of the age distribution

quantile(df$ageyears,0.75)

logit.male <- -0.555808+1*0.860076-0.026411*34
pmale <- exp(logit.male)/(1+exp(logit.male))

logit.female <- -0.555808+0*0.860076-0.026411*34
pfemale <- exp(logit.female)/(1+exp(logit.female))

pmale-pfemale
```

* Here are the results:

```rout
> quantile(df$ageyears,0.25)
25% 
 21 
> 
> logit.male <- -0.555808+1*0.860076-0.026411*21
> pmale <- exp(logit.male)/(1+exp(logit.male))
> 
> logit.female <- -0.555808+0*0.860076-0.026411*21
> pfemale <- exp(logit.female)/(1+exp(logit.female))
> 
> pmale-pfemale
[1] 0.1899451
> 
> # third quartile of the age distribution
> 
> quantile(df$ageyears,0.75)
75% 
 34 
> 
> logit.male <- -0.555808+1*0.860076-0.026411*34
> pmale <- exp(logit.male)/(1+exp(logit.male))
> 
> logit.female <- -0.555808+0*0.860076-0.026411*34
> pfemale <- exp(logit.female)/(1+exp(logit.female))
> 
> pmale-pfemale
[1] 0.1663648
> 
```
