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

