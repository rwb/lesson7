### Lesson 7 - Thursday 10/13/22

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

* Now, we ask what this model implies about the risk of failure as a function of age? Of course, the answer depends partially on what we assume about the 
value of sex (male/female). Here is the R code (which also generates a plot displayed below):

```r
# generate the age sequence

age <- seq(from=16,to=78)

# let's start with the females

logit.age.female <- -0.555808+0*0.860076-0.026411*age
p.age.female <- exp(logit.age.female)/(1+exp(logit.age.female))

# now, the males

logit.age.male <- -0.555808+1*0.860076-0.026411*age
p.age.male <- exp(logit.age.male)/(1+exp(logit.age.male))

# here is the plot

plot(x=age,y=p.age.male,type="l",lty=1,lwd=2,col="blue",ylim=c(0,1))
lines(x=age,y=p.age.female,lty=1,lwd=2,col="red")
abline(h=c(0,0.2,0.4,0.6,0.8,1.0),lty=3,lwd=0.5)
abline(v=c(20,30,40,50,60,70,80),lty=3,lwd=0.5)
```

* Here is the plot:

<p align="left">
<img src="/gfiles/age-plot.png" width="800px">
</p>

* Note that this is different from estimating an interaction effect.
* If we want to allow the logits to have different age slopes for males and females, then we need to add another parameter to the model 

```r
m2 <- glm(recid~1+male+ageyears+male*ageyears,data=df,family=binomial(link="logit"))
summary(m2)
logLik(m2)
```

* Here are the results:

```rout
> m2 <- glm(recid~1+male+ageyears+male*ageyears,data=df,family=binomial(link="logit"))
> summary(m2)

Call:
glm(formula = recid ~ 1 + male + ageyears + male * ageyears, 
    family = binomial(link = "logit"), data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.1266  -1.0302  -0.8512   1.2975   2.1093  

Coefficients:
               Estimate Std. Error z value Pr(>|z|)  
(Intercept)   -0.366302   0.383101  -0.956   0.3390  
male           0.665119   0.388916   1.710   0.0872 .
ageyears      -0.033536   0.013853  -2.421   0.0155 *
male:ageyears  0.007317   0.014030   0.522   0.6020  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 12370  on 9326  degrees of freedom
Residual deviance: 12156  on 9323  degrees of freedom
AIC: 12164

Number of Fisher Scoring iterations: 4

> logLik(m2)
'log Lik.' -6078.094 (df=4)
> 
```

* Note that this is a 4-parameter model (compared to the three parameter model we considered earlier. Here is the log-likelihood ratio test:

```r
logLik(m1)
logLik(m2)

ts <- -2*(-6078.234-(-6078.094))
ts
1-pchisq(q=ts,df=1)
```

and the results are:

```rout
> logLik(m1)
'log Lik.' -6078.234 (df=3)
> logLik(m2)
'log Lik.' -6078.094 (df=4)
> 
> ts <- -2*(-6078.234-(-6078.094))
> ts
[1] 0.28
> 1-pchisq(q=ts,df=1)
[1] 0.5967012
> 
```

* Based on this evidence, we conclude that the constrained model (m1) is more consistent with the data. However, we can still interpret the 
interaction effect (between sex and age). Here is some R code:

```r
# let's start with the females

logit.age.female <- -0.366302+0*0.665119-0.033536*age+0.007317*0*age
p.age.female <- exp(logit.age.female)/(1+exp(logit.age.female))

# now, the males

logit.age.male <- -0.366302+1*0.665119-0.033536*age+0.007317*1*age
p.age.male <- exp(logit.age.male)/(1+exp(logit.age.male))

# let's add the expected failure rates from the interaction model to 
# the plot - the lines from model 2 are dashed.

lines(x=age,y=p.age.male,type="l",lty=2,lwd=2,col="blue",ylim=c(0,1))
lines(x=age,y=p.age.female,lty=2,lwd=2,col="red")
```

* The code above generates some additional information to add to the plotspace:

<p align="left">
<img src="/gfiles/age-plot2.png" width="800px">
</p>

* We can also check to see whether the derivatives of the response function / age are equal (comparing males and females) at a particular age point say, age 20.

```r
data.frame(age,p.age.male,p.age.female)

# first, the females

b <- -0.033536
p <- 0.26172501
derivative <- b*p*(1-p)
derivative

# second, the males

b <- -0.033536+0.007317
p <- 0.4438471
derivative <- b*p*(1-p)
derivative
```

* Here are the results:

```rout
> data.frame(age,p.age.male,p.age.female)
   age p.age.male p.age.female
1   16  0.4698648   0.28845943
2   17  0.4633394   0.28162527
3   18  0.4568265   0.27489047
4   19  0.4503284   0.26825658
5   20  0.4438471   0.26172501
6   21  0.4373849   0.25529698
7   22  0.4309439   0.24897359
8   23  0.4245262   0.24275576
9   24  0.4181338   0.23664429
10  25  0.4117688   0.23063981
11  26  0.4054332   0.22474284
12  27  0.3991289   0.21895372
13  28  0.3928579   0.21327271
14  29  0.3866220   0.20769990
15  30  0.3804230   0.20223527
16  31  0.3742629   0.19687869
17  32  0.3681432   0.19162991
18  33  0.3620656   0.18648857
19  34  0.3560319   0.18145421
20  35  0.3500435   0.17652626
21  36  0.3441020   0.17170407
22  37  0.3382089   0.16698690
23  38  0.3323656   0.16237392
24  39  0.3265735   0.15786422
25  40  0.3208337   0.15345682
26  41  0.3151477   0.14915068
27  42  0.3095164   0.14494468
28  43  0.3039412   0.14083766
29  44  0.2984229   0.13682839
30  45  0.2929627   0.13291560
31  46  0.2875615   0.12909796
32  47  0.2822200   0.12537412
33  48  0.2769393   0.12174268
34  49  0.2717199   0.11820221
35  50  0.2665626   0.11475124
36  51  0.2614681   0.11138831
37  52  0.2564369   0.10811189
38  53  0.2514695   0.10492046
39  54  0.2465665   0.10181250
40  55  0.2417282   0.09878643
41  56  0.2369550   0.09584071
42  57  0.2322471   0.09297377
43  58  0.2276049   0.09018403
44  59  0.2230285   0.08746993
45  60  0.2185181   0.08482989
46  61  0.2140738   0.08226235
47  62  0.2096956   0.07976575
48  63  0.2053836   0.07733854
49  64  0.2011377   0.07497916
50  65  0.1969578   0.07268610
51  66  0.1928437   0.07045782
52  67  0.1887955   0.06829282
53  68  0.1848127   0.06618960
54  69  0.1808952   0.06414671
55  70  0.1770428   0.06216266
56  71  0.1732550   0.06023603
57  72  0.1695315   0.05836540
58  73  0.1658721   0.05654937
59  74  0.1622761   0.05478655
60  75  0.1587434   0.05307560
61  76  0.1552732   0.05141517
62  77  0.1518652   0.04980395
63  78  0.1485189   0.04824066
> 
> # first, the females
> 
> b <- -0.033536
> p <- 0.26172501
> derivative <- b*p*(1-p)
> derivative
[1] -0.006479995
> 
> # second, the males
> 
> b <- -0.033536+0.007317
> p <- 0.4438471
> derivative <- b*p*(1-p)
> derivative
[1] -0.006472078
> 
```

* Note: if we want to check on the sampling distribution of the difference between these two derivatives, we can use the bootstrap:

```r
library(boot)

dd <- function(data, indices) 
  {
    d <- data[indices,]
    ms <- glm(recid~1+male+ageyears+male*ageyears,
      data=d,family=binomial(link="logit"))
    pyf.int <- coef(ms)[1]
    pyf.bm <- coef(ms)[2]
    pyf.ba <- coef(ms)[3]
    pyf.pr <- coef(ms)[4]
    logit.f <- pyf.int+pyf.bm*0+pyf.ba*20+pyf.pr*20*0
    p.f <- exp(logit.f)/(1+exp(logit.f))
    dvf <- pyf.ba*p.f*(1-p.f)
    logit.m <- pyf.int+pyf.bm*1+pyf.ba*20+pyf.pr*20*1
    p.m <- exp(logit.m)/(1+exp(logit.m))
    dvm <- (pyf.ba+pyf.pr)*p.m*(1-p.m)
    return(c(dvf,dvm))
  }

dboot <- boot(data=df,statistic=dd,R=3000)
dvf <- dboot$t[,1]
dvm <- dboot$t[,2]
delta <- dvm-dvf
quantile(delta,0.025)
quantile(delta,0.975)

# display a summary of the results
par(mfrow=c(1,2))
hist(delta)
boxplot(dvf,dvm,
  xlab="Sex",
  ylab="d_p(fail)/d_age at Age 20",
  names=c("Females","Males"))
```

* Here are the results:

```rout
> library(boot)
> 
> dd <- function(data, indices) 
+   {
+     d <- data[indices,]
+     ms <- glm(recid~1+male+ageyears+male*ageyears,
+       data=d,family=binomial(link="logit"))
+     pyf.int <- coef(ms)[1]
+     pyf.bm <- coef(ms)[2]
+     pyf.ba <- coef(ms)[3]
+     pyf.pr <- coef(ms)[4]
+     logit.f <- pyf.int+pyf.bm*0+pyf.ba*20+pyf.pr*20*0
+     p.f <- exp(logit.f)/(1+exp(logit.f))
+     dvf <- pyf.ba*p.f*(1-p.f)
+     logit.m <- pyf.int+pyf.bm*1+pyf.ba*20+pyf.pr*20*1
+     p.m <- exp(logit.m)/(1+exp(logit.m))
+     dvm <- (pyf.ba+pyf.pr)*p.m*(1-p.m)
+     return(c(dvf,dvm))
+   }
> 
> dboot <- boot(data=df,statistic=dd,R=3000)
> dvf <- dboot$t[,1]
> dvm <- dboot$t[,2]
> delta <- dvm-dvf
> quantile(delta,0.025)
      2.5% 
-0.0045852 
> quantile(delta,0.975)
      97.5% 
0.006692414 
> 
> # display a summary of the results
> par(mfrow=c(1,2))
> hist(delta)
> boxplot(dvf,dvm,
+   xlab="Sex",
+   ylab="d_p(fail)/d_age at Age 20",
+   names=c("Females","Males"))
> 
```

<p align="left">
<img src="/gfiles/deriv-plot.png" width="800px">
</p>

* One last point about the logistic regression model: we can code the likelihood function and use a general hill-climbing algorithm to maximize that function.

```r
# first we rerun the standard logistic regression analysis

m3 <- glm(recid~1+male+ageyears,data=df,family=binomial(link="logit"))
summary(m3)
logLik(m3)

library(maxLik)

ll4 <- function(parms)
  {
    a <- parms[1]
    b <- parms[2]
    c <- parms[3]
 
    logit <- a+b*df$male+c*df$ageyears

    py0 <- 1/(1+exp(logit))
    py1 <- exp(logit)/(1+exp(logit))

    pmf <- df$recid*py1+(1-df$recid)*py0
    lpmf <- log(pmf)
    return(lpmf)
  }

m4 <- maxLik(ll4,start=c(-0.32937423,0.40384020,-0.0328402402),
  method="BHHH",finalHessian="BHHH")
summary(m4)
```

* Here are the results:

```rout
> # first we rerun the standard logistic regression analysis
> 
> m3 <- glm(recid~1+male+ageyears,data=df,family=binomial(link="logit"))
> summary(m3)

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

> logLik(m3)
'log Lik.' -6078.234 (df=3)
> 
> library(maxLik)
Loading required package: miscTools

Please cite the 'maxLik' package as:
Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.

If you have questions, suggestions, or comments regarding the 'maxLik' package, please use a forum or 'tracker' at maxLik's R-Forge site:
https://r-forge.r-project.org/projects/maxlik/
> 
> ll4 <- function(parms)
+   {
+     a <- parms[1]
+     b <- parms[2]
+     c <- parms[3]
+  
+     logit <- a+b*df$male+c*df$ageyears
+ 
+     py0 <- 1/(1+exp(logit))
+     py1 <- exp(logit)/(1+exp(logit))
+ 
+     pmf <- df$recid*py1+(1-df$recid)*py0
+     lpmf <- log(pmf)
+     return(lpmf)
+   }
> 
> m4 <- maxLik(ll4,start=c(-0.32937423,0.40384020,-0.0328402402),
+   method="BHHH",finalHessian="BHHH")
> summary(m4)
--------------------------------------------
Maximum Likelihood estimation
BHHH maximisation, 4 iterations
Return code 8: successive function values within relative tolerance limit (reltol)
Log-Likelihood: -6078.234 
3  free parameters
Estimates:
      Estimate Std. error t value  Pr(> t)    
[1,] -0.555804   0.126262  -4.402 1.07e-05 ***
[2,]  0.860075   0.114838   7.489 6.92e-14 ***
[3,] -0.026411   0.002169 -12.177  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
--------------------------------------------
> 
```

##### Mid-Term Exam Due Monday 10/24/22

* Use the 1980 North Carolina dataset to: (1) conduct exploratory data analysis looking at marginal distributions and bivariate relationships; (2) estimate a logistic regression model with main effects for age and sex (interpreting your results); (3) check to see whether the age effects vary between sex groups using plots, a likelihood ratio test, and derivative comparison (including an assessment of the sampling distribution of the difference between the male and female derivatives at age 22); and (4) confirm that you can estimate a model writing down your own likelihood function that is comparable to the model estimated by the glm() function.
