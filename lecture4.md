---
title: 'SFB 1036 Course on Data Analysis: Lecture 4'
author: "Simon Anders"
output:
  html_document:
    df_print: paged
---

## Reminder: Fisher test

To connect to the previous lecture, one more example:

Of 20 mice with a genetic predisposition for cancer, 10 get a novel
antidrug in their chow, which supposedly protects against cancer and 10 get
control chow. After 12 months, we check for tumours, and get  this result:


```r
tbl <- 
   rbind( c( 2, 8 ), c( 6, 4 ) )

dimnames(tbl) <- list(
   chow = c( "ctrl", "drug" ),
   tumours = c( "no", "yes" ) )

tbl
```

```
##       tumours
## chow   no yes
##   ctrl  2   8
##   drug  6   4
```
Only half as many mice grew tumours of those who got the prophylactic drug.

Does this mean that the drug has a protective effect? Let's try a FIsher test.


```r
fisher.test( tbl )
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  tbl
## p-value = 0.1698
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.01252647 1.65925396
## sample estimates:
## odds ratio 
##  0.1841181
```

Let's also try the same with a simulation, just to comapre:

Our null hypothesis is that the drug has no effect. Then 12 of the 20 mice 
got cancer just because of their genetics, i.e., the probability to get cancer
is (under the null hypothesis) somewhere close to


```r
p0_cancer <- 12 / 20
p0_cancer
```

```
## [1] 0.6
```

For 10 random mice, we expect 6 to get cancer. In practice, it could be, for example


```r
runif( 10 ) < .6
```

```
##  [1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
```
(`runif(n)` returns n **r**andom numbers, drawn **unif**ormly from the interval [0;1]. They are below 0.6 with probability 0.6.)


To get a row of the contingency table, we use


```r
has_tumour <- runif( 10 ) < 0.6;
has_tumour
```

```
##  [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE  TRUE
```

```r
c( no = sum(!has_tumour), yes = sum(has_tumour) )
```

```
##  no yes 
##   5   5
```
Under the null hypothesis, the drug has no effect, so we can do the same twice to get the two rows:


```r
ctrl_has_tumour <- runif( 10 ) < 0.6
drug_has_tumour <- runif( 10 ) < 0.6
tbl0 <- rbind( 
   ctrl = c( no = sum(ctrl_has_tumour), yes = sum(!ctrl_has_tumour) ),
   drug = c( no = sum(drug_has_tumour), yes = sum(!drug_has_tumour) ) )
tbl0
```

```
##      no yes
## ctrl  9   1
## drug  6   4
```

We need a test statistics. Let's use the odds ratio. For each row, the "odds" of getting cancer are the "yes" counts divided by the "no" counts. The odds ratio is simply the ratio of the odds in the drug row to that in the control row, i.e., we calculate a ratio of ratios, involving all four counts, as follows:


```r
( tbl0["drug","yes"] / tbl0["drug","no"] ) / 
   ( tbl0["ctrl","yes"] / tbl0["ctrl","no"] )
```

```
## [1] 6
```

What was the odds ratio in our original data?


```r
odds_ratio <- 
   ( tbl["drug","yes"] / tbl["drug","no"] ) / 
   ( tbl["ctrl","yes"] / tbl["ctrl","no"] )
odds_ratio
```

```
## [1] 0.1666667
```

Let's get a null distribuition of odds ratios by repeating the above steps 100,000 times


```r
odds_ratio_null <-
   replicate( 100000, {
      ctrl_has_tumour <- runif( 10 ) < 0.6
      drug_has_tumour <- runif( 10 ) < 0.6
      tbl0 <- rbind( 
         ctrl = c( no = sum(ctrl_has_tumour), yes = sum(!ctrl_has_tumour) ),
         drug = c( no = sum(drug_has_tumour), yes = sum(!drug_has_tumour) ) )
      ( tbl0["drug","yes"] / tbl0["drug","no"] ) / 
         ( tbl0["ctrl","yes"] / tbl0["ctrl","no"] )
} )
```

A histogram of these


```r
hist( odds_ratio_null, breaks=100 )
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
That was a bad histogram.

Always use logarithm when visualizing ratios
   

```r
hist( log2( odds_ratio_null ), 100 )
abline( v = log2(odds_ratio), col="green" )
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
 ()
Now it's symmetric. And we've added a green line to mark the actual value of the odds ratio.

What's our p value from this analysis? It's the number of values in the null distribution further out than the actual value.


```r
mean( abs(log2(odds_ratio_null)) > abs(log2(odds_ratio)), na.rm=TRUE )
```

```
## [1] 0.06197124
```


## Continuous data: The t test

Let's load the NHANES data again:


```r
library( tidyverse )
inner_join( 
    haven::read_xpt( "NHANES/DEMO_I.XPT" ), 
    haven::read_xpt( "NHANES/BMX_I.XPT" ), 
    by="SEQN" ) %>%
  select( 
    subjectId = SEQN,
    age = RIDAGEYR,
    sex = RIAGENDR,
    height = BMXHT,
    weight = BMXWT,
    ethnicity = RIDRETH3,
    bornInUS = DMDBORN4,
     ) %>%
  mutate(
    sex = fct_recode( factor(sex), male="1", female="2" ),
    ethnicity = fct_recode( factor(ethnicity), mexican="1", hispanic="2",
       white="3", black="4", asian="6", other="7" ),
    bornInUS = fct_recode( factor(bornInUS), yes="1", no="2" ),
  ) -> 
    nhanes

nhanes
```

```
## # A tibble: 9,544 x 7
##    subjectId   age sex    height weight ethnicity bornInUS
##        <dbl> <dbl> <fct>   <dbl>  <dbl> <fct>     <fct>   
##  1     83732    62 male     184.   94.8 white     yes     
##  2     83733    53 male     171.   90.4 white     no      
##  3     83734    78 male     170.   83.4 white     yes     
##  4     83735    56 female   161.  110.  white     yes     
##  5     83736    42 female   165.   55.2 black     yes     
##  6     83737    72 female   150    64.4 mexican   no      
##  7     83738    11 female   144.   37.2 mexican   yes     
##  8     83739     4 male     102.   16.4 white     yes     
##  9     83740     1 male      NA    10.1 hispanic  yes     
## 10     83741    22 male     165.   76.6 black     yes     
## # ... with 9,534 more rows
```

Last time, we noticed that Mexicans born in the US tend to be taller than Mexicans born not in the US:


```r
nhanes %>%
   filter( age>20, sex=="male", ethnicity=="mexican",  !is.na(height) ) %>%
ggplot + 
   geom_density( aes( x=height, col=bornInUS ) )
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

Let's first visualize this differently, with a beeswarm plot


```r
library(ggbeeswarm)

nhanes %>%
   filter( age>20, sex=="male", ethnicity=="mexican",  !is.na(height) ) %>%
ggplot + 
   geom_beeswarm( aes( y=height, x=bornInUS ) )
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

Now, extract the heights into two variable to have same example data to discuss t tests


```r
nhanes %>%
   filter( age>20, sex=="male", ethnicity=="mexican", bornInUS=="yes", !is.na(height) ) %>%
   pull( height ) ->
      height_A

nhanes %>%
   filter( age>20, sex=="male", ethnicity=="mexican", bornInUS=="no", !is.na(height) ) %>%
   pull( height ) ->
      height_B
```

Is the difference seen above significant?


```r
t.test( height_A, height_B )
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  height_A and height_B
## t = 5.3311, df = 335.76, p-value = 1.795e-07
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  2.459991 5.336814
## sample estimates:
## mean of x mean of y 
##  170.6442  166.7458
```

Would we have seen this also if we had measured only 50 people of each group?


```r
smplA <- sample( height_A, 50 )
smplB <- sample( height_B, 50 )

t.test( smplA, smplB )
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  smplA and smplB
## t = 2.3789, df = 96.15, p-value = 0.01934
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.5763379 6.3836621
## sample estimates:
## mean of x mean of y 
##   170.446   166.966
```

Actually, there are two variants of the t test. The previous one (Welch's version of Student's t test, `var.equal=FALSE`, the default) estimated the standard deviation for each group separately, the next one (Student's original version of the t test, `var.equal=TRUE`) takes an average over both


```r
t.test( smplA, smplB, var.equal=TRUE )
```

```
## 
## 	Two Sample t-test
## 
## data:  smplA and smplB
## t = 2.3789, df = 98, p-value = 0.0193
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.5770363 6.3829637
## sample estimates:
## mean of x mean of y 
##   170.446   166.966
```

But they give nearly the same value, so this distinction does not matter much.

Let's do this "on foot" and try to recreate what R's `t.test` function did.

First, we need a test statistic. We use the difference in means, divided by
the average standard deviation (or more precisely: the square root of the average 
of the variances, but that is nearly the same value).


```r
t <- ( mean(smplB) - mean(smplA) ) / sqrt( ( var(smplA) + var(smplB) )/2 ) 
t
```

```
## [1] -0.475786
```

And we need a null hypothesis. Ours is: There is no difference between the two groups.
All the measured height values come from the same distribution, namely a normal
distribution with mean mu0 (which is just the average over all data, simply mixing
the groups) and standard deviation sd0, which is taken from the average of the variances 
of the groups. (Both our groups have nearly the same standard deviation, so it does not matter 
much how we pick a value which is somehow in the middle between the two.)


```r
mu0 <- ( mean( smplA )  +  mean( smplB ) ) / 2
sd0 <- sqrt( (  var( smplA )  +   var( smplB ) ) / 2 )
mu0
```

```
## [1] 168.706
```

```r
sd0
```

```
## [1] 7.314213
```

Our null hypothesis is that the 50 values of each group are drawn from a normal distribution with this mean and standard deviation. The `rnorm` function gives us **r**andom draws from a **norm*al distribution. So this here
is a set of potential values for our 50 height values under the null hypothesis


```r
rnorm( 50, mu0, sd0 )
```

```
##  [1] 169.6222 169.6083 166.9712 166.5351 184.1486 164.0675 166.9951
##  [8] 177.2269 181.1415 179.3568 163.9210 165.9764 167.3307 171.6149
## [15] 175.9003 163.0423 166.3325 170.8138 159.8358 167.2326 172.5318
## [22] 169.3592 162.7637 167.6245 174.8031 173.8243 163.2623 174.4592
## [29] 180.9711 174.5884 167.1149 163.4124 169.3780 165.6630 168.1042
## [36] 172.0521 161.9493 172.0248 153.1946 165.0124 164.0274 159.2964
## [43] 170.5140 175.0977 182.5072 165.9486 171.0401 157.6310 169.4208
## [50] 171.4729
```

We do this twice to get two null samples of 50 people each and the calculate our null test statistic


```r
smplA0 <- rnorm( 50, mu0, sd0 )
smplB0 <- rnorm( 50, mu0, sd0 )
t0 <- ( mean(smplB0) - mean(smplA0) ) / sqrt( ( var(smplA0) + var(smplB0) )/2 )
t0
```

```
## [1] -0.1295578
```

Do this many times to get a null distribution of our test statistic


```r
t0 <- replicate( 100000, {
   smplA0 <- rnorm( 50, mu0, sd0 )
   smplB0 <- rnorm( 50, mu0, sd0 )
   ( mean(smplB0) - mean(smplA0) ) / sqrt( ( var(smplA0) + var(smplB0) )/2 )
})
```

Here is the histogram of our null distribution


```r
hist( t0, breaks=100 )
abline( v=t, col="green")
abline( v=-t, col="green", lty="dashed")
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)


How many t0 values are more extreme than the t value from our real data, i.e. how many of the 100,000 values are outside the green lines?


```r
table( abs(t0) > abs(t) )
```

```
## 
## FALSE  TRUE 
## 98094  1906
```

```r
mean( abs(t0) > abs(t) )
```

```
## [1] 0.01906
```

That's also what Student's t test gave above as p value. 

What Student's t test does differently is that it doesn't get the t0 histogram from random tries but from an explicite formula, the [t distribution](https://en.wikipedia.org/wiki/Student%27s_t-distribution).

Here is the histogram from before, with the theoretical t value distribution superimposed in blue.


```r
hist( t0 * sqrt(25), breaks=100, freq=FALSE )

xg <- seq( -5, 5, length.out = 1000 )
lines( xg, dt( xg, 98), col="blue" )
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)

A few remarks for those interested in details

- Our test statistic above was the difference in means devided by the standard distribution (SD). In the ususal formulation of the t test, one divides by the standard error (SE) of the difference of the means, not the standard deviation. As the standard error of a mean (SEM) is $SEM = SD / \sqrt{n}$, we have to multiply our test statistic by $\sqrt{50}$. And because the standard error of a difference between two means is $\sqrt{2}$ times the standard error of the means, we have to divide by $\sqrt{2}$. Hence, the rescaling of `t0` by `sqrt(25)`. 
- Student's t distribution has one parameter, the number of degrees of freedom (df). Is is the number of data values (2*50=100) minus the number of estimated parameters (two group mean), hence `df=98`.
- The function `dt` calculated the probability density of the t distribution with `df=`$nu$ degrees of freedom. The formula is $\frac{\Gamma(\frac{\nu+1}{2})}{\sqrt{\nu\pi}\,\Gamma(\frac{\nu}{2})} \left( 1 + \frac{t^2}{\nu_2} \right)^{-\frac{\nu+1}{2}}$. SO, the R function `dt` is a handy abbreviation for this slighltly complicated formula that Gosset figured out.

### Alternative:  A permutation test

ALternatively, we could also have permuted the group labels: We consider each of the 100 height value as labelled with either "A" or "B" to makr the group they are from. Now, instead of drawing new labels from a normal distribution, as we did before, we can also simply permute the labels: we keep the values but shuffle which 50 labels are assigned to group A and which to B, and then calculate a null distribution of our test statistic.

Here is the code for this


```r
t0b <- replicate( 100000, {
   smpl_AB0 <- sample( c( smplA, smplB ) )
   smplA0 <- smpl_AB0[1:50]
   smplB0 <- smpl_AB0[51:100]
   c( mean(smplB0) - mean(smplA0) ) / sqrt( ( var(smplA0) + var(smplB0 ) )/2 )
} )  
mean( abs(t0b) > abs(t) ) 
```

```
## [1] 0.01962
```

The p value is remarkable similar. 

The difference is:
- Student's t test is a *parametric test*: It makes a specific assumption about the distribution that the values are drawn from, namely that they are normally distributed. The null hypothesis is only about the *parameters* of this null distribution, namely mean and standard deviation.
- The permutation test is a *non-parametric test*: it does not need the assumption that the data are normal, because to get the null distribution of the test statistic, it does not need to draw from a specific distribution with estimated parameters. Rather, we only use the data we have and just change its order. This is safer as it makes fewer assumption. But it is computationally intensive, because it needs to try many permutations. There is not closed-expression formula as for the t distribution. Hence, permutation tests have become a real option only with the advent of fast computers.

## Another kind of test

We will now look at a scenario which is usually discussed without mention of any "null hypothesis" or p value.

You are administered a lab test that detects some dangerous disease. The lab test has 
- a *sensitivity* of 98%: If a patient has the disease, the test will give a positive result with 98% probability
- a *specificity* of 95%: If the patient does not have the disease, the test will give a negative result with 95% probability

Your test is positive. How worried should you be?

Very worried, supposedly. After all, the test is 98% specific.

In reality, this depends on the prevalence of the disease! 

Let's say, the test is administered to 10,000 patients, and 1% of them (i.e, 100) have the disease. That latter number is called the prevalence of the disease among the test subjects.

- Of the 100 diseased subjects, 98 are expected to get a positive and 2 a negative result
- Of the 9,900 healthy subject, 95% (i.e., 9405) are expected to get a negative result, but 495 to get a positive result.

So, we have 593 subjects with positive result, of which only 98 (17%) have the disease 

Before taking the test, you believed your risk of having the disease to be 1%, now you have reason to 
believe it to be 17%. Is this helpful to know?

## Bayes theorem

A mathematical way of writing down the calculation we just made is this:

We first define events:
- D: Subject has the disease
- H: Subject is healthy
- P: Test result is positive
- N: Test result is negative

Now we can write the test's sensitivity and specificity as conditional probablities: 
- Sensitivity = Pr( P | D )
- Specificity = Pr( N | H )

Read Pr( P | D ) as: probability of P given that D is the case (i.e., that the test is positive if the patient has the disease)

The prevalence is simply: Prevalence  = Pr( D )

And we have, of course: Pr( H ) = 1 - Pr( D )

We noticed that 593/10000 have a positive result. How did we calculate this probability? We did:

  Pr( P ) = Pr( P | D ) \* Pr( D ) + Pr( P | H ) \* Pr( H )

Then, we used this to calculate:

  Pr( D | P ) = Pr( P | D ) * Pr( D ) / Pr( P )
  
This last formula is Bayes theorem. It allowed us to calculate the probability that the patient has the disease if the test is positive from the probability that the test is positive if the patient has the disease, i.e., to swap the condition and the event in the probability. The crucial information we needed was the prevalence. For a different prevalence, we would have got a very different result!

## Specificity and p value
  
Let's look at the same scenario using Fisher's hypothesis testing scenario. We have a patient and suspect that he might have the disease. Our null hypothesis, however, is that all is normal and that the patient is healthy. The lab test produces a read out and if the read out is above some threshold, we call the test positive. The probability that our read out is above the threshold even though the null hypothesis was true is what Fisher would call the p value (if the observed value were right on the threshold). Above, we called that the specificity.

To write the p value in the conditional probability notation:

  p value = Pr( test statistic has observed value of more extreme | null hypothesis is true )
  
What we would actually like to know is
  
  Pr( null hypothesis is not true | test statistic has the observed value [or more extreme] )
  
As we have just seen, we can transfer one to the other with Bayes theorem if we have the information corresponding to the prevalence. Before, this was Pr( patient has the disease ), now it is Pr( null hypothesis is not true ).

Hence, to know how likely our result is wrong, we would need how common it is for scientists in our line of research to persue a wrong hypothesis. This is obviously impossible to know. If we at least have a guess, we can better judge how much a p value of p=0.05 might mean. But as usually, we don't, we have to deal with the somewhat awkward p value, while in a case as the lab test scenario, things look much easier.
