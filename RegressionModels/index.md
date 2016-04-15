# Patterns for Regression Models
J Faleiro  
April 10, 2016  

# Required libraries


```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(datasets, UsingR, reshape, ggplot2, manipulate, dplyr, GGally, stats, rgl, car)
```

# Why Statisticians Love Confusing Synonyms?

Various names for the same thing: variable independence

Independent Variable | Dependent Variable
---------------------|--------------------
X axis               | Y axis
Predictor            | Outcome
Predictor            | Predicted
Regressor            | Outcome

The function `lm` needs a formula of the form dependent ~ independent (or predicted ~ predictor).

# Basic Least Squares

### Finding Least Squares

Finding the least (i.e. that minimizes) squares (to ensure points below and above the line are treated the same) is to find the minimum value of the following equation:

$\sum_{i=1}^{n} (Y_i - \mu)^2$


```r
y <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
leastSquares <- function(mu) { sum((y - mu)^2) } 
optimize(leastSquares, interval=c(-100, 100), maximum=FALSE)
```

```
## $minimum
## [1] 0.573
## 
## $objective
## [1] 0.25401
```

What in theory should match $\bar{Y}$:


```r
mean(y)
```

```
## [1] 0.573
```

What if we get something a bit more elaborate:

$\sum_{i=1}^{n} w_i(x_i - \mu)^2$

What would be the value of $\mu$ that would minimize the least square equation? 


```r
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
leastSquares <- function(mu) { sum(w*(x - mu)^2) } 
optimize(leastSquares, interval=c(-100, 100), maximum=FALSE)
```

```
## $minimum
## [1] 0.1471429
## 
## $objective
## [1] 3.716543
```

### Heights of Parents and Children


```r
library(UsingR); data(galton); library(reshape)
describe(galton)
```

```
## galton 
## 
##  2  Variables      928  Observations
## ---------------------------------------------------------------------------
## child 
##       n missing  unique    Info    Mean     .05     .10     .25     .50 
##     928       0      14    0.98   68.09    64.2    64.2    66.2    68.2 
##     .75     .90     .95 
##    70.2    71.2    72.2 
## 
##           61.7 62.2 63.2 64.2 65.2 66.2 67.2 68.2 69.2 70.2 71.2 72.2 73.2
## Frequency    5    7   32   59   48  117  138  120  167   99   64   41   17
## %            1    1    3    6    5   13   15   13   18   11    7    4    2
##           73.7
## Frequency   14
## %            2
## ---------------------------------------------------------------------------
## parent 
##       n missing  unique    Info    Mean     .05     .10     .25     .50 
##     928       0      11    0.97   68.31    65.5    65.5    67.5    68.5 
##     .75     .90     .95 
##    69.5    70.5    71.5 
## 
##           64 64.5 65.5 66.5 67.5 68.5 69.5 70.5 71.5 72.5 73
## Frequency 14   23   66   78  211  219  183   68   43   19  4
## %          2    2    7    8   23   24   20    7    5    2  0
## ---------------------------------------------------------------------------
```

```r
head(galton)
```

```
##   child parent
## 1  61.7   70.5
## 2  61.7   68.5
## 3  61.7   65.5
## 4  61.7   64.5
## 5  61.7   64.0
## 6  62.2   67.5
```

Make it wide:


```r
long <- melt(galton) 
describe(long)
```

```
## long 
## 
##  2  Variables      1856  Observations
## ---------------------------------------------------------------------------
## variable 
##       n missing  unique 
##    1856       0       2 
## 
## child (928, 50%), parent (928, 50%) 
## ---------------------------------------------------------------------------
## value 
##       n missing  unique    Info    Mean     .05     .10     .25     .50 
##    1856       0      25    0.99    68.2    64.2    65.2    67.2    68.5 
##     .75     .90     .95 
##    69.5    71.2    72.2 
## 
## lowest : 61.7 62.2 63.2 64.0 64.2, highest: 72.2 72.5 73.0 73.2 73.7 
## ---------------------------------------------------------------------------
```

```r
head(long)
```

```
##   variable value
## 1    child  61.7
## 2    child  61.7
## 3    child  61.7
## 4    child  61.7
## 5    child  61.7
## 6    child  62.2
```

Plot distributions of children and parents, heights in inches.


```r
ggplot(long, aes(x=value, fill=variable)) +
    geom_histogram(colour='black', binwidth=1) +
    facet_grid(. ~ variable)
```

![](index_files/figure-html/unnamed-chunk-7-1.png)

Comparing children and (average over the pair of) parents heights: 


```r
ggplot(galton, aes(x=parent, y=child)) +
    geom_point()
```

![](index_files/figure-html/unnamed-chunk-8-1.png)

A bad plot, no indication of when a point is plotted over it a number of times. We can do a bit better by using `plot` and `jitter`:


```r
plot(jitter(child, 4) ~ parent, galton)
```

![](index_files/figure-html/unnamed-chunk-9-1.png)

The regression line of children ~ parent can be calculated by `lm`. The function `lm` needs a formula of the form dependent ~ independent (or predicted ~ predictor).


```r
regrline <- lm(child ~ parent, galton)
summary(regrline)
```

```
## 
## Call:
## lm(formula = child ~ parent, data = galton)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.8050 -1.3661  0.0487  1.6339  5.9264 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 23.94153    2.81088   8.517   <2e-16 ***
## parent       0.64629    0.04114  15.711   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.239 on 926 degrees of freedom
## Multiple R-squared:  0.2105,	Adjusted R-squared:  0.2096 
## F-statistic: 246.8 on 1 and 926 DF,  p-value: < 2.2e-16
```

The first thing we can notice is that the mean of the residuals of regrline is close to zero:


```r
mean(regrline$residuals)
```

```
## [1] -2.359884e-15
```

And also the correlation between residuals and predictors will be close to zero, i.e. they are uncorrelated.


```r
cov(regrline$residuals, galton$parent)
```

```
## [1] -1.790153e-13
```

The intercept $\beta_0$ is the first element of regrline$coef:


```r
regrline$coef[1]
```

```
## (Intercept) 
##    23.94153
```

The slope $\beta_1$ is the second element of regrline$coef:


```r
regrline$coef[2]
```

```
##    parent 
## 0.6462906
```

A coefficient will be within 2 standard errors of its estimate about 95% of the time. This means the slope of our
regression is significantly different than either 0 or 1 since (.64629) +/- (2*.04114) is near neither 0 nor 1.

We can see that the coeficient (multiplier) of parents to children is given by $0.64629 \pm 2 * 0.04114$ 

And we can add the regression line to the original plot:


```r
plot(jitter(child, 4) ~ parent, galton)
abline(regrline, lwd=3, col='red')
```

![](index_files/figure-html/unnamed-chunk-15-1.png)

We can get a plot with the same information using `ggplot2`:


```r
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
head(freqData)
```

```
##   child parent freq
## 1  61.7     64    1
## 2  62.2     64    0
## 3  63.2     64    2
## 4  64.2     64    4
## 5  65.2     64    1
## 6  66.2     64    2
```


```r
ggplot(filter(freqData, freq > 0), aes(x = parent, y = child)) + 
    scale_size(range = c(2, 20), guide = "none" ) + 
    geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE)) + 
    geom_point(aes(colour=freq, size = freq)) + 
    scale_colour_gradient(low = "lightblue", high="white")
```

![](index_files/figure-html/unnamed-chunk-17-1.png)

We can calculate the optimal linear regression:


```r
lm(I(child - mean(child)) ~ I(parent - mean(parent)) -1, data=galton)
```

```
## 
## Call:
## lm(formula = I(child - mean(child)) ~ I(parent - mean(parent)) - 
##     1, data = galton)
## 
## Coefficients:
## I(parent - mean(parent))  
##                   0.6463
```

Shows the value of $\beta$ through which you minimize the mean square error given by $\sum_{i=1}^{n} {(Y_i - \beta X_i)}^2$, what is 0.646

That's how it looks like graphically:


```r
lm1 <- lm(galton$child ~ galton$parent)
ggplot(filter(freqData, freq > 0), aes(x = parent, y = child)) +
    scale_size(range = c(2, 20), guide = "none" ) + 
    geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE)) +
    geom_point(aes(colour=freq, size = freq)) +
    scale_colour_gradient(low = "lightblue", high="white") + 
    geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], size = 1, colour = grey(.5))
```

![](index_files/figure-html/unnamed-chunk-19-1.png)

### Linear Least Squares


```r
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) * sd(y)/sd(x)
beta0 <- mean(y) - beta1 * mean(x)
c(beta0, beta1)
```

```
## [1] 23.9415302  0.6462906
```

In R, `lm()` does a linear regression and is used in different ways. Here we will regress `y` as the predictor and `x` as the outcome. The function `coef()` gets the output of the linear model and just grabs the coeficient:


```r
coef(lm(y ~ x))
```

```
## (Intercept)           x 
##  23.9415302   0.6462906
```

The coeficients are exactly beta0 and beta1 we calculated above.

If we reverse the outcome/predictor relationships, we switch the model:


```r
beta1 <- cor(y, x) * sd(x)/sd(y)
beta0 <- mean(x) - beta1 * mean(y)
rbind(c(beta0, beta1), coef(lm(x ~ y)))
```

```
##      (Intercept)         y
## [1,]    46.13535 0.3256475
## [2,]    46.13535 0.3256475
```

Same values.

Now, checking $\beta_1$ and $\beta_0$ when a regression is through the origin $(x=0, y=0)$.

If through the origin, $\beta_0 = 0$. Now $\beta_1$:


```r
yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])
```

```
##                   x 
## 0.6462906 0.6462906
```

They both yield the same result. You get the same slope if you use yc and xc, as long as you subtract the intercept (-1):


```r
lm(formula = yc ~ xc -1)
```

```
## 
## Call:
## lm(formula = yc ~ xc - 1)
## 
## Coefficients:
##     xc  
## 0.6463
```

You can also use normalized $X_i$ and $Y_i$ and the slope of $lm(Zy_i, Zx_i)$ will be the same as $cor(Y_i,X_i)$ and the same as $cor(Zy_i, Zx_i)$:


```r
yn <- (y - mean(y))/sd(y) # normalized Y
xn <- (x - mean(x))/sd(x) # normalized X
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])
```

```
##                            xn 
## 0.4587624 0.4587624 0.4587624
```

The best way to add a linear regression line to a plot is by adding a layer `geom_smooth` to a plot, using `lm` as a method and `y ~ x` as the formula (what is the default):


```r
ggplot(filter(freqData, freq > 0), aes(x = parent, y = child)) +
    scale_size(range = c(2, 20), guide = "none" ) + 
    geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE)) +
    geom_point(aes(colour=freq, size = freq)) +
    scale_colour_gradient(low = "lightblue", high="white") + 
    geom_smooth(method='lm', formula=y~x)
```

![](index_files/figure-html/unnamed-chunk-26-1.png)

You can see that a confidence interval (shaded area) is given for free with a `geom_smooth` layer. The statistical uncertainty is added automatically.

### Regression to the mean

Y is related to son's height and X to the father's height, both normalized:


```r
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
```

Since x and y are normalized:


```r
c(sd(x), sd(x))
```

```
## [1] 1 1
```

```r
c(mean(x), mean(y))
```

```
## [1] -1.627998e-15  9.259712e-16
```

The mean is almost zero.

We will plot with a few assumptions:

* we set x and y limits as (-4,4) - since this is in SDs, the chance of something falling beyond that (4 SDs away from the mean) is dim
* the line `geom_abline(intercept=0, slope=1)` is the identity line, what would be the regression line if no noise, i.e. X and Y perfectly correlated.
* the line `geom_abline(intercept=0, slope=rho, size=2)`, in red, is the line showing son's height as the outcome and parent's height as the predictor
* the line `geom_abline(intercept=0, slope=1/rho, size=2)` is the line showing son's height as the predictor and parent's height as the outcome


```r
ggplot(data.frame(x-x, y=y), aes(x = x, y = y)) +
    geom_point(colour="black", alpha=0.2, size=3) +
    geom_point(colour="salmon", alpha=0.2, size=2) +
    xlim(-4,4) + ylim(-4,4) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    geom_abline(intercept=0, slope=1) +
    geom_abline(intercept=0, slope=rho, size=1.5, colour='red') +
    geom_abline(intercept=0, slope=1/rho, size=1.5)
```

![](index_files/figure-html/unnamed-chunk-29-1.png)

The fitted lines are shown with double tickness (size=2). 

Examples of use:

* if we get a parent height as the outcome that is +2 SDs from the mean (on X axis) we can predict that son's height (predictor) should be ~ +1 SD from the mean (regression on the red line, slope = $\rho$) 
* if we get a son height as the outcome that is +2 SDs from the mean (on X axis) we can predict that parent's height (predictor) should be ~ +1 SD from the mean (regression on the black line, slope = $1 / \rho$) 

# Statistical Linear Regression Model

Linear regression of diamonds in singaporean dollars:


```r
data(diamond)
ggplot(diamond, aes(x=carat, y=price)) +
    xlab('mass (carats)') +
    ylab('price (SIN$)') +
    geom_point(size=6, colour='black', alpha=0.2) +
    geom_point(size=5, colour='blue', alpha=0.2) +
    geom_smooth(method='lm', colour='black')
```

![](index_files/figure-html/unnamed-chunk-30-1.png)


```r
fit <- lm(price ~ carat, diamond)
coef(fit)
```

```
## (Intercept)       carat 
##   -259.6259   3721.0249
```

What is says is:

* we estimate a SIN$ 3,721.02 increase in price for every carat in a diamond
* a zero carat diamond should cost SIN$ -259.63 (what doesn't make any sense)

Let's deal with a non-sense intercept (second bullet) my subtracting the mean sized diamond from carat size:


```r
fit2 <- lm(price ~ I(carat - mean(carat)), data=diamond) # adjustments have to be surrounded by I() function
coef(fit2)
```

```
##            (Intercept) I(carat - mean(carat)) 
##               500.0833              3721.0249
```

The slope hasn't changed, but the intercept makes more sense: SIN$ 500.08 is the expected price to be paid for an average size diamond.

We can make the model better, the slope is a factor of a large amount of money - we can scale volume by 10:


```r
fit3 <- lm(price ~ I(carat*10 - mean(carat*10)), data=diamond) # adjustments have to be surrounded by I() function
coef(fit3)
```

```
##                      (Intercept) I(carat * 10 - mean(carat * 10)) 
##                         500.0833                         372.1025
```

So, each tenth of carat increase will represent a difference in price of SIN$ 372.10

How to predict prices?

Given 3 diamonds and their sizes:


```r
newx <- c(0.16, 0.27,0.34)
```

First way, simple calculation $Y_i = \beta_0 + \beta_1 X_i$:


```r
coef(fit)[1] + coef(fit)[2] * newx
```

```
## [1]  335.7381  745.0508 1005.5225
```

Second way, using `predict`


```r
predict(fit, newdata=data.frame(carat=newx))
```

```
##         1         2         3 
##  335.7381  745.0508 1005.5225
```

Both yield the same results.

# Effect of scale conversion in coefficients

What happens if I divide a scale of X by 10?


```r
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y ~ x))$coefficients
```

```
##              Estimate Std. Error   t value   Pr(>|t|)
## (Intercept) 0.1884572  0.2061290 0.9142681 0.39098029
## x           0.7224211  0.3106531 2.3254912 0.05296439
```


```r
summary(lm(y ~ I(x/10)))$coefficients
```

```
##              Estimate Std. Error   t value   Pr(>|t|)
## (Intercept) 0.1884572   0.206129 0.9142681 0.39098029
## I(x/10)     7.2242108   3.106531 2.3254912 0.05296439
```

Gets multiplied by 10.

What happens if I add a constant K to the regressor?


```r
summary(lm(y ~ I(x + 3)))$coefficients
```

```
##               Estimate Std. Error   t value   Pr(>|t|)
## (Intercept) -1.9788061  1.1266738 -1.756326 0.12245739
## I(x + 3)     0.7224211  0.3106531  2.325491 0.05296439
```

The intercept changed, the relationship:


```r
0.1884572 - 3*0.722421
```

```
## [1] -1.978806
```

The new intercept would be $\hat \beta_0 = \beta_0 - K\beta_1$

Another example using `mtcars`. How should a regressor associated to half of weight be interpreted?


```r
data(mtcars)
summary(lm(mpg ~ wt + factor(cyl), data=mtcars))$coef
```

```
##               Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)  33.990794  1.8877934 18.005569 6.257246e-17
## wt           -3.205613  0.7538957 -4.252065 2.130435e-04
## factor(cyl)6 -4.255582  1.3860728 -3.070244 4.717834e-03
## factor(cyl)8 -6.070860  1.6522878 -3.674214 9.991893e-04
```


```r
summary(lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars))$coef
```

```
##               Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)  33.990794   1.887793 18.005569 6.257246e-17
## I(wt * 0.5)  -6.411227   1.507791 -4.252065 2.130435e-04
## factor(cyl)6 -4.255582   1.386073 -3.070244 4.717834e-03
## factor(cyl)8 -6.070860   1.652288 -3.674214 9.991893e-04
```

The estimate `I(wt * 0.5) = -6.411227` gives the estimated change in mpg per half ton increase in weight.

# Residuals

## Residuals Properties

Calculating residuals of price and carat correlation data and checking some of the residuals properties:


```r
data(diamond)
y <- diamond$price
x <- diamond$carat
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
```

Checking residuals, $e_i = Y_i - \hat Y_i$ so $e_i - (Y_i - \hat Y_i) = 0$


```r
max(abs(e - (y - yhat)))
```

```
## [1] 9.485746e-13
```

Almost zero, what should be the same as


```r
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))
```

```
## [1] 9.485746e-13
```

Expected value of residuals is zero, $E[e_i] = 0$


```r
mean(e)
```

```
## [1] -3.885781e-16
```

Sum of residuals is zero, $\sum{i=1}_n e_i = 0$ if intercept is included:


```r
sum(e)
```

```
## [1] -1.865175e-14
```

Sum of regressor times residuals is zero, $\sum{i=1}_n e_i x_i = 0$ if regressor is included:


```r
sum(e * x)
```

```
## [1] 6.959711e-15
```

## Visual Patterns of Residual Plots

Plotting prices, mass, regression and a red line for residuals:


```r
n <- length(y)
plot(x, y, xlab='mass (carats)', ylab='price (SIN$)',
     bg='lightblue', 
     col='black', cex=1.1, pch=21, frame=FALSE)
abline(fit, lwd=2)
for (i in 1:n)
    lines(c(x[i], x[i]), c(y[i], yhat[i]), col='red', lwd=2)
```

![](index_files/figure-html/unnamed-chunk-49-1.png)

This is not the best way to represent residuals (check the amount of unused space above and below the regression line). Instead, let's plot regressors versus residuals:


```r
plot(x, e, xlab='mass (carats)', ylab='residuals (SIN$)',
     bg='lightblue', 
     col='black', cex=1.1, pch=21, frame=FALSE)
abline(h=0, lwd=2)
for (i in 1:n)
    lines(c(x[i], x[i]), c(e[i], 0), col='red', lwd=2)
```

![](index_files/figure-html/unnamed-chunk-50-1.png)

A few things a residual versus regressor plot should show:

* you should see no patterns of disposition, residuals should spread nicely and without any shape above and below the intercept
* since the intercept is included, red lines should add to ~ zero (positive above the intercept, negative below the intercept)

Now a few example of patological patterns a residual plot should help highlight


```r
x <- runif(100, -3, 3)
y <- x + sin(x) + rnorm(100, sd=.2)
```

Basically a uniform distribution on x, and y a senoid oscilation with a bit of random normal noise over it, a regression line defined by `geom_smooth`:


```r
ggplot(data.frame(x=x, y=y), aes(x=x,y=y)) +
    geom_smooth(method='lm', colour='black') +
    geom_point(size=3, colour='black', alpha=0.4) +
    geom_point(size=1, colour='red', alpha=0.4) 
```

![](index_files/figure-html/unnamed-chunk-52-1.png)

It is not apparent that some pattern is hidden in the data. Let's take a look at the residual plot:


```r
e <- resid(lm(y ~ x))
ggplot(data.frame(x=x, y=e), aes(x=x,y=y)) +
    xlab("x") + ylab("residual") +
    geom_hline(yintercept=0, size=2) +
    geom_point(size=3, colour='black', alpha=0.4) +
    geom_point(size=1, colour='red', alpha=0.4) 
```

![](index_files/figure-html/unnamed-chunk-53-1.png)

Now it is apparent that residuals oscilate over and below the reference line of zero.

Another example:


```r
x <- runif(100, 0, 6)
y <- x + rnorm(100, mean=0, sd=.001 * x)
```

X and Y are generated on some pre-determined pattern. X is 100 uniform numbers, Y is X plus 100 random normal numbers - and here is the catch, SD is variable in relation to X - there's the hidden pattern.

A X over Y plot however shows nothing:


```r
ggplot(data.frame(x=x, y=y), aes(x=x,y=y)) +
    geom_smooth(method='lm', colour='black') +
    geom_point(size=3, colour='black', alpha=0.4) +
    geom_point(size=1, colour='red', alpha=0.4) 
```

![](index_files/figure-html/unnamed-chunk-55-1.png)

Shows nothing. Appears as if X and Y fall exactly on top of the identity line. Let's take a look at the residual plot:


```r
e <- resid(lm(y ~ x))
ggplot(data.frame(x=x, y=e), aes(x=x,y=y)) +
    xlab("x") + ylab("residual") +
    geom_hline(yintercept=0, size=2) +
    geom_point(size=3, colour='black', alpha=0.4) +
    geom_point(size=1, colour='red', alpha=0.4) 
```

![](index_files/figure-html/unnamed-chunk-56-1.png)

Shows that residuals spread as the regressor grows to higher values. There is a name for that, heteroscedasticity. 

Heteroscedasticity is a hard word to pronounce, but it doesn't need to be a difficult concept to understand. Put simply, heteroscedasticity (also spelled heteroskedasticity) refers to the circumstance in which the variability of a variable is unequal across the range of values of a second variable that predicts it.

Residual plots are a good tool to diagnose heteroscedasticity.

## Explaining Residuals

Back to our diamond analysis:


```r
diamond$e <- resid(lm(price ~ carat, data=diamond))
head(diamond)
```

```
##   carat price          e
## 1  0.17   355 -17.948318
## 2  0.16   328  -7.738069
## 3  0.17   350 -22.948318
## 4  0.18   325 -85.158566
## 5  0.25   642 -28.630306
## 6  0.16   342   6.261931
```

```r
ggplot(diamond, aes(x=carat,y=e)) +
    xlab('mass (carats)') +
    ylab('residual price (SIN$)') +
    geom_hline(yintercept=0, size=2) +
    geom_point(size=3, colour='black', alpha=0.4) +
    geom_point(size=1, colour='red', alpha=0.4) 
```

![](index_files/figure-html/unnamed-chunk-58-1.png)

No patterns, but variance - where is this variance related to:


```r
e <- c(resid(lm(price ~ 1, data=diamond)),
       resid(lm(price ~ carat, data=diamond)))
fit <- factor(c(rep('intercept', nrow(diamond)),
                rep('intercept, slope', nrow(diamond))))
```

Residuals are given by $e$, in two vectors:

* first residual vector, just a fitted intercept on 1, so they are just variations around an average price


```r
lm(price ~ 1, data=diamond)
```

```
## 
## Call:
## lm(formula = price ~ 1, data = diamond)
## 
## Coefficients:
## (Intercept)  
##       500.1
```


```r
mean(diamond$price)
```

```
## [1] 500.0833
```

* second residual vector, we add carat as a predictor variable, are variations around a regression line of price ~ carat.


```r
lm(price ~ carat, data=diamond)
```

```
## 
## Call:
## lm(formula = price ~ carat, data = diamond)
## 
## Coefficients:
## (Intercept)        carat  
##      -259.6       3721.0
```


The labelling for the plot is given by factor $fit$.


```r
ggplot(data.frame(e=e, fit=fit), aes(x=fit, y=e, fill=fit)) +
    geom_dotplot(binaxis='y', stackdir='center', binwidth=30) +
    xlab('fitting approach') +
    ylab('residual price (SIN$)')
```

![](index_files/figure-html/unnamed-chunk-63-1.png)

On the left side (red), we see the variation of prices around the average diamond prices.

On the right side (blue) we see the variation of prices around the regression line.

## Residual Variance

You can extract the standard variation of a fit using `$sigma`:


```r
fit <- lm(diamond$price ~ diamond$carat)
summary(fit)$sigma
```

```
## [1] 31.84052
```

Residual variance (variance of residuals) should also follow $\hat \sigma^2 = \dfrac{1}{(n - 2)} \sum_{i=1}^n e_i^2$, what when calculated:


```r
n <- length(diamond$price)
sqrt(sum(resid(fit)^2) / (n - 2))
```

```
## [1] 31.84052
```

Gives the exact same results.

This R function should give the exact same result:


```r
sqrt(deviance(fit)/(n-2)) 
```

```
## [1] 31.84052
```

## Comparing models with different combination of slope and intercept


```r
data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
```

Slope and intercept:


```r
sifit <- lm(y ~ x)
```

Only intercept


```r
ifit <- lm(y ~ 1)
```

Only slope


```r
sfit <- lm(y ~ x - 1)
```

What is the ratio of the error variability when comparing intercept only to intercept and slope, $\frac {intercept and slope}{intercept}$?


```r
sum(sifit$residuals^2)/sum(ifit$residuals^2)
```

```
## [1] 0.2471672
```

## R Squared


```r
# mean of all Y
mu <- mean(galton$child)
# Centering data means subtracting the mean from each data point. 
# Sum of the squares of the centered children's heights gives the total variation
sTot <- sum((galton$child - mu)^2) 
fit <- lm(galton$child ~ galton$parent)
# deviance calculates the sum of the squares of the residuals. 
# These are the distances between the children's heights and the regression line. 
sRes <- deviance(fit)
r2 <- 1 - (sRes/sTot)
r2
```

```
## [1] 0.2104629
```

What is the same as


```r
summary(fit)$r.squared
```

```
## [1] 0.2104629
```

And since $R^2 = cor(x,y)^2$:


```r
cor(galton$child, galton$parent)^2
```

```
## [1] 0.2104629
```


```r
data("anscombe"); example("anscombe")
```

```
## 
## anscmb> require(stats); require(graphics)
## 
## anscmb> summary(anscombe)
##        x1             x2             x3             x4    
##  Min.   : 4.0   Min.   : 4.0   Min.   : 4.0   Min.   : 8  
##  1st Qu.: 6.5   1st Qu.: 6.5   1st Qu.: 6.5   1st Qu.: 8  
##  Median : 9.0   Median : 9.0   Median : 9.0   Median : 8  
##  Mean   : 9.0   Mean   : 9.0   Mean   : 9.0   Mean   : 9  
##  3rd Qu.:11.5   3rd Qu.:11.5   3rd Qu.:11.5   3rd Qu.: 8  
##  Max.   :14.0   Max.   :14.0   Max.   :14.0   Max.   :19  
##        y1               y2              y3              y4        
##  Min.   : 4.260   Min.   :3.100   Min.   : 5.39   Min.   : 5.250  
##  1st Qu.: 6.315   1st Qu.:6.695   1st Qu.: 6.25   1st Qu.: 6.170  
##  Median : 7.580   Median :8.140   Median : 7.11   Median : 7.040  
##  Mean   : 7.501   Mean   :7.501   Mean   : 7.50   Mean   : 7.501  
##  3rd Qu.: 8.570   3rd Qu.:8.950   3rd Qu.: 7.98   3rd Qu.: 8.190  
##  Max.   :10.840   Max.   :9.260   Max.   :12.74   Max.   :12.500  
## 
## anscmb> ##-- now some "magic" to do the 4 regressions in a loop:
## anscmb> ff <- y ~ x
## 
## anscmb> mods <- setNames(as.list(1:4), paste0("lm", 1:4))
## 
## anscmb> for(i in 1:4) {
## anscmb+   ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
## anscmb+   ## or   ff[[2]] <- as.name(paste0("y", i))
## anscmb+   ##      ff[[3]] <- as.name(paste0("x", i))
## anscmb+   mods[[i]] <- lmi <- lm(ff, data = anscombe)
## anscmb+   print(anova(lmi))
## anscmb+ }
## Analysis of Variance Table
## 
## Response: y1
##           Df Sum Sq Mean Sq F value  Pr(>F)   
## x1         1 27.510 27.5100   17.99 0.00217 **
## Residuals  9 13.763  1.5292                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## Analysis of Variance Table
## 
## Response: y2
##           Df Sum Sq Mean Sq F value   Pr(>F)   
## x2         1 27.500 27.5000  17.966 0.002179 **
## Residuals  9 13.776  1.5307                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## Analysis of Variance Table
## 
## Response: y3
##           Df Sum Sq Mean Sq F value   Pr(>F)   
## x3         1 27.470 27.4700  17.972 0.002176 **
## Residuals  9 13.756  1.5285                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## Analysis of Variance Table
## 
## Response: y4
##           Df Sum Sq Mean Sq F value   Pr(>F)   
## x4         1 27.490 27.4900  18.003 0.002165 **
## Residuals  9 13.742  1.5269                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## anscmb> ## See how close they are (numerically!)
## anscmb> sapply(mods, coef)
##                   lm1      lm2       lm3       lm4
## (Intercept) 3.0000909 3.000909 3.0024545 3.0017273
## x1          0.5000909 0.500000 0.4997273 0.4999091
## 
## anscmb> lapply(mods, function(fm) coef(summary(fm)))
## $lm1
##              Estimate Std. Error  t value    Pr(>|t|)
## (Intercept) 3.0000909  1.1247468 2.667348 0.025734051
## x1          0.5000909  0.1179055 4.241455 0.002169629
## 
## $lm2
##             Estimate Std. Error  t value    Pr(>|t|)
## (Intercept) 3.000909  1.1253024 2.666758 0.025758941
## x2          0.500000  0.1179637 4.238590 0.002178816
## 
## $lm3
##              Estimate Std. Error  t value    Pr(>|t|)
## (Intercept) 3.0024545  1.1244812 2.670080 0.025619109
## x3          0.4997273  0.1178777 4.239372 0.002176305
## 
## $lm4
##              Estimate Std. Error  t value    Pr(>|t|)
## (Intercept) 3.0017273  1.1239211 2.670763 0.025590425
## x4          0.4999091  0.1178189 4.243028 0.002164602
## 
## 
## anscmb> ## Now, do what you should have done in the first place: PLOTS
## anscmb> op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
## 
## anscmb> for(i in 1:4) {
## anscmb+   ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
## anscmb+   plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
## anscmb+        xlim = c(3, 19), ylim = c(3, 13))
## anscmb+   abline(mods[[i]], col = "blue")
## anscmb+ }
```

![](index_files/figure-html/unnamed-chunk-75-1.png)

```
## 
## anscmb> mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
## 
## anscmb> par(op)
```

On all these examples we have a similar $R_2$, but the disposition of the data shows information is missing.

# Inference in Regression

Let's get back to the diamond data using mass to regress the price of diamonds:


```r
library(UsingR); data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
```

## Coefficients Calculation

The hard way of doing it:


```r
beta1 <- cor(y, x) * sd(y)/sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x # residuals
sigma <- sqrt(sum(e^2) / (n-2)) # estimate of std deviation
ssx <- sum((x - mean(x))^2) # sums of squares of x's
seBeta0 <- (1 / n + mean(x)^2 / ssx) ^.5 * sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df=n-2, lower.tail=FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df=n-2, lower.tail=FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0),
                   c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c('Estimate', 'Std. Error', 't value', 'P(>|t|)')
rownames(coefTable) <- c('(Intercept)', 'x')
coefTable
```

```
##              Estimate Std. Error   t value      P(>|t|)
## (Intercept) -259.6259   17.31886 -14.99094 2.523271e-19
## x           3721.0249   81.78588  45.49715 6.751260e-40
```

The easy way to come up with the exact same values:


```r
fit <- lm(y ~ x)
summary(fit)$coefficients
```

```
##              Estimate Std. Error   t value     Pr(>|t|)
## (Intercept) -259.6259   17.31886 -14.99094 2.523271e-19
## x           3721.0249   81.78588  45.49715 6.751260e-40
```

Both steps produce the exact same results.

## Hypothesis Testing


```r
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
```

What is the p-value for the 2 sided hypothesis test if $\beta_1$ from a linear regression is 0 or not?


```r
fit <- lm(y ~ x)
summary(fit)$coef
```

```
##              Estimate Std. Error   t value   Pr(>|t|)
## (Intercept) 0.1884572  0.2061290 0.9142681 0.39098029
## x           0.7224211  0.3106531 2.3254912 0.05296439
```

The first row is $\beta_0$, second row is $\beta_1$ - so result is ~ 0.052964.

## Confidence Intervals

Now, if we want to produce confidence intervals


```r
library(UsingR); data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
sumCoef <- summary(fit)$coefficients
```

95% confidence interval for $\beta_0$ (intercept):


```r
beta0Estimate <- sumCoef[1,1]
beta0StdError <- sumCoef[1,2]
beta0Estimate + c(-1,1) * qt(.975, df=fit$df) * beta0StdError
```

```
## [1] -294.4870 -224.7649
```

95% confidence interval for $\beta_1$ (slope):


```r
beta1Estimate <- sumCoef[2,1]
beta1StdError <- sumCoef[2,2]
beta1Estimate + c(-1,1) * qt(.975, df=fit$df) * beta1StdError
```

```
## [1] 3556.398 3885.651
```

What this is basically stating is, with a 95% confidence, that a increase of 1 carat in mass will cause a diamond price to increase anything from SIN$ 3556.39 to SIN$ 3885.65

On cars, the mtcars data set, what is a 95% *confidence interval* for the expected mpg at the average weight?


```r
data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
fit <- lm(y ~ x)
predict(fit, data.frame(x=mean(x)), interval='confidence', level=0.95)
```

```
##        fit      lwr      upr
## 1 20.09062 18.99098 21.19027
```

A new 3,000 pounds will be on sale soon. What is a 95% *prediction interval* of it's mpg consumption?


```r
predict(fit, data.frame(x=3.0), interval='prediction', level=0.95)
```

```
##        fit      lwr      upr
## 1 21.25171 14.92987 27.57355
```

What is a 95% confidence interval for the *expected change* in mpg per 2,000 lbs increase in weight?


```r
fit <- lm(y ~ I(x/2)) # x/2 is x/2000 pounds since x is in 1000 pounds
sumCoef <- summary(fit)$coefficients
beta1Estimate <- sumCoef[2,1]
beta1StdError <- sumCoef[2,2]
beta1CI95 <- beta1Estimate + c(-1,1) * qt(.975, df=fit$df) * beta1StdError
beta1CI95
```

```
## [1] -12.97262  -8.40527
```

## Prediction


```r
library(UsingR); data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
newx <- data.frame(x=seq(min(x), max(x), length=100))
p1 <- data.frame(predict(fit, newdata=newx, interval=('confidence')))
p2 <- data.frame(predict(fit, newdata=newx, interval=('prediction')))
p1$interval <- 'confidence'
p2$interval <- 'prediction'
p1$x <- newx$x
p2$x <- newx$x
dat <- rbind(p1, p2)
names(dat)[1] <- 'y'
```


```r
ggplot(dat, aes(x=x, y=y)) +
    geom_ribbon(aes(ymin=lwr, ymax=upr, fill=interval), alpha=0.2) +
    geom_line() +
    geom_point(data=data.frame(x=x,y=y), aes(x=x,y=y), size=1, alpha=0.5)
```

![](index_files/figure-html/unnamed-chunk-88-1.png)

# Multivariate Regression

## Experiment


```r
n <- 100
# predictors
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
# outcome
y <- 1 + x1 + x2 + x3 + rnorm(n, sd=.1)
```

Calculating residuals for $Y$ and $X_1$:


```r
ey <- resid(lm(y ~ x2 + x3))
ex1 <- resid(lm(x1 ~ x2 + x3))
```

Calculating $\hat \beta_1$:


```r
sum(ey * ex1) / sum(ex1 ^ 2)
```

```
## [1] 1.016098
```

Same as:


```r
coef(lm(ey ~ ex1 - 1))
```

```
##      ex1 
## 1.016098
```

And should be the same if you add all regressors, looking under column x1:


```r
coef(lm(y ~ x1 + x2 + x3))
```

```
## (Intercept)          x1          x2          x3 
##   0.9800510   1.0160979   0.9987278   1.0098213
```

## Sample Analysys: Swiss Population Late 1800

We will start by a pairs plot:


```r
data(swiss)
my_fn <- function(data, mapping, method="loess", ...){
      p <- ggplot(data = data, mapping = mapping) + 
      geom_point() + 
      geom_smooth(method=method, ...)
      p
    }
ggpairs(swiss, lower=list(continuous=my_fn))
```

![](index_files/figure-html/unnamed-chunk-94-1.png)

Regress fertility as an outcome of all variables


```r
summary(lm(Fertility ~ ., data=swiss))$coefficients
```

```
##                    Estimate  Std. Error   t value     Pr(>|t|)
## (Intercept)      66.9151817 10.70603759  6.250229 1.906051e-07
## Agriculture      -0.1721140  0.07030392 -2.448142 1.872715e-02
## Examination      -0.2580082  0.25387820 -1.016268 3.154617e-01
## Education        -0.8709401  0.18302860 -4.758492 2.430605e-05
## Catholic          0.1041153  0.03525785  2.952969 5.190079e-03
## Infant.Mortality  1.0770481  0.38171965  2.821568 7.335715e-03
```

Let's go over a few of these numbers one by one:

Agriculture Estimate -0.1721140 : we expect a .17 decrease on standardized fertility rates for every 1% of males involved in agriculture, holding the remanining variables constant.

Agriculture Std.Error 0.07030392: tells us how precise this estimate is. If we were to test $H_0 : \beta_{Agri} = 0$ versus $H_a : \beta{Agri} <> 0$ - we test against $\frac {estimate - \beta_{Agri}}{std error}$

Agriculture t value -2.448142: using $\beta_{Agri} = 0$ estimate divided by std error, -0.1721140/0.07030392

Agriculture Pr(>|t|) 1.872715e-02: probability of getting a t-statistic as extreme as t value


```r
summary(lm(Fertility ~ Agriculture, data=swiss))$coefficients
```

```
##               Estimate Std. Error   t value     Pr(>|t|)
## (Intercept) 60.3043752 4.25125562 14.185074 3.216304e-18
## Agriculture  0.1942017 0.07671176  2.531577 1.491720e-02
```

Coeficients are reversed when we remove all the other regressor variables. A few findings:

* The coeficient sign in Agriculture reverses itself with the inclusion of Examination and Education.
* Agricultural work is negatively correlated to educational attainment (-0.6395), while education and examination (correlation of 0.6984) are measuring equivalent things
* A causality claim between Agriculture and Fertility will be easily negated, all you have to do is to add more variables and the causality would be reversed or gone away.

What happens if you include an replicated/irrelevant regressor?


```r
z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data=swiss)
```

```
## 
## Call:
## lm(formula = Fertility ~ . + z, data = swiss)
## 
## Coefficients:
##      (Intercept)       Agriculture       Examination         Education  
##          66.9152           -0.1721           -0.2580           -0.8709  
##         Catholic  Infant.Mortality                 z  
##           0.1041            1.0770                NA
```

R responds back with a `NA` for z, meaning it is irrelevant to the model already in place.

## Factor Variables for Analysis


```r
data("InsectSprays")
head(InsectSprays)
```

```
##   count spray
## 1    10     A
## 2     7     A
## 3    20     A
## 4    14     A
## 5    14     A
## 6    12     A
```


```r
ggplot(data=InsectSprays, aes(y=count, x=spray, fill=spray)) +
    geom_violin(colour='black', size=2) +
    xlab('type of spray') + ylab('insect count')
```

![](index_files/figure-html/unnamed-chunk-99-1.png)

Let's fit our model, using predictor as spray and count as the outcome


```r
summary(lm(count ~ spray, data=InsectSprays))$coef
```

```
##                Estimate Std. Error    t value     Pr(>|t|)
## (Intercept)  14.5000000   1.132156 12.8074279 1.470512e-19
## sprayB        0.8333333   1.601110  0.5204724 6.044761e-01
## sprayC      -12.4166667   1.601110 -7.7550382 7.266893e-11
## sprayD       -9.5833333   1.601110 -5.9854322 9.816910e-08
## sprayE      -11.0000000   1.601110 -6.8702352 2.753922e-09
## sprayF        2.1666667   1.601110  1.3532281 1.805998e-01
```

You can see that SprayA is missing - that's becasue it hs been picked as the reference predictor. 

A second example: using `mtcars`, fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight (wt) as confounder. Find the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.


```r
mtcars$cylf <- factor(mtcars$cyl)
fit <- lm(mpg ~ cylf + wt, mtcars)
summary(fit)$coef
```

```
##              Estimate Std. Error   t value     Pr(>|t|)
## (Intercept) 33.990794  1.8877934 18.005569 6.257246e-17
## cylf6       -4.255582  1.3860728 -3.070244 4.717834e-03
## cylf8       -6.070860  1.6522878 -3.674214 9.991893e-04
## wt          -3.205613  0.7538957 -4.252065 2.130435e-04
```

The baseline in `(Intercept)` is the regressor factor 4 cylinders. Holding weight constant, the estimate for `cylf8` is relative to 4 cylinders, so the expected change is given by estimate of cylf8L: **-6.070860**

Now, how do estimates of mpg as outcome and cylinders as factor, holding weight constant and without weight? In other words, how does mpg vary in relation to cylinders, adjusted and not adjusted for weight?


```r
fitNotAdjusted <- lm(mpg ~ cylf, mtcars)
summary(fitNotAdjusted)$coef
```

```
##               Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)  26.663636  0.9718008 27.437347 2.688358e-22
## cylf6        -6.920779  1.5583482 -4.441099 1.194696e-04
## cylf8       -11.563636  1.2986235 -8.904534 8.568209e-10
```

If we compare coeficients of `fit` to `fitNotAdjusted` we are basically comparing the mpg rate when we keep weight constant (fit) to when we removed weight (fitNotAdjusted). The difference in mpg from 8 to 4 cylinders changed from **-6.070860** to **-11.563636** from coefficients on `fit` to `fitNotAdjusted`.

So, holding weight constant has less of an impact in mpg consumption than removing weight altogether.

Another test: would adding a regressor of an interaction between number of cylinders (as a factor) and weight have any significant effect in the prediction of the outcome mpg?

Let's first generate the new fit, adding a regressor of the interaction of wt and cylf (remember, interactions are recorded using the `*` operator):


```r
fitInteraction <- lm(mpg ~ wt + cylf + wt*cylf, mtcars)
summary(fitInteraction)$coef
```

```
##               Estimate Std. Error    t value     Pr(>|t|)
## (Intercept)  39.571196   3.193940 12.3894599 2.058359e-12
## wt           -5.647025   1.359498 -4.1537586 3.127578e-04
## cylf6       -11.162351   9.355346 -1.1931522 2.435843e-01
## cylf8       -15.703167   4.839464 -3.2448150 3.223216e-03
## wt:cylf6      2.866919   3.117330  0.9196716 3.661987e-01
## wt:cylf8      3.454587   1.627261  2.1229458 4.344037e-02
```

What's the p-value for the likelihood ratio test comparing the two models, and should we reject the hypothesis using 0.05 as a type I error rate significance benchmark?


```r
anova(fit, fitInteraction)
```

```
## Analysis of Variance Table
## 
## Model 1: mpg ~ cylf + wt
## Model 2: mpg ~ wt + cylf + wt * cylf
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1     28 183.06                           
## 2     26 155.89  2     27.17 2.2658 0.1239
```

Since the p-value is greater that 0.05 (our $\alpha$ for the benchmark), it would fail to reject the hypothesis (i.e. the interaction terms are not necessary)

### Estimates

* The estimate of 14.5 is the mean of sprayA (see the violin plot).
* Other estimates are the difference in means between sprayX and the reference predictor sprayA.
* The probability column is checking the probability of a sprayX being different from the reference sprayA.
* If you want to find a mean of a specific predictor you will have to add that to the reference predictor sprayA located in the intercept. For example for mean for sprayE:


```r
14.5000000 + -11.0000000
```

```
## [1] 3.5
```

* If you have to compare means of other predictors, you will have to subtract one the other, for example - to compare means of sprayC and sprayF, the comparison would be sprayF - sprayC:


```r
2.1666667 -12.4166667
```

```
## [1] -10.25
```

Note that this does not provide an error estimate.

### Fitting a curve by hand

You can fit the curve above


```r
summary(lm(count ~ spray, data=InsectSprays))$coef
```

```
##                Estimate Std. Error    t value     Pr(>|t|)
## (Intercept)  14.5000000   1.132156 12.8074279 1.470512e-19
## sprayB        0.8333333   1.601110  0.5204724 6.044761e-01
## sprayC      -12.4166667   1.601110 -7.7550382 7.266893e-11
## sprayD       -9.5833333   1.601110 -5.9854322 9.816910e-08
## sprayE      -11.0000000   1.601110 -6.8702352 2.753922e-09
## sprayF        2.1666667   1.601110  1.3532281 1.805998e-01
```

With an equivalent, done by hand, set of regressors using the following pattern:


```r
summary(lm(count ~ 
               I(1 * (spray == 'B')) +
               I(1 * (spray == 'C')) +
               I(1 * (spray == 'D')) +
               I(1 * (spray == 'E')) +
               I(1 * (spray == 'F')), 
           data=InsectSprays))$coef
```

```
##                          Estimate Std. Error    t value     Pr(>|t|)
## (Intercept)            14.5000000   1.132156 12.8074279 1.470512e-19
## I(1 * (spray == "B"))   0.8333333   1.601110  0.5204724 6.044761e-01
## I(1 * (spray == "C")) -12.4166667   1.601110 -7.7550382 7.266893e-11
## I(1 * (spray == "D"))  -9.5833333   1.601110 -5.9854322 9.816910e-08
## I(1 * (spray == "E")) -11.0000000   1.601110 -6.8702352 2.753922e-09
## I(1 * (spray == "F"))   2.1666667   1.601110  1.3532281 1.805998e-01
```

You add `1 *` to force the type from boolean to numeric. When you leave spray 'A' out, you force it to be the reference predictor.

What if I add spray A as a predictor to the fitting?


```r
summary(lm(count ~ 
               I(1 * (spray == 'B')) +
               I(1 * (spray == 'C')) +
               I(1 * (spray == 'D')) +
               I(1 * (spray == 'E')) +
               I(1 * (spray == 'F')) +
               I(1 * (spray == 'A')), 
           data=InsectSprays))
```

```
## 
## Call:
## lm(formula = count ~ I(1 * (spray == "B")) + I(1 * (spray == 
##     "C")) + I(1 * (spray == "D")) + I(1 * (spray == "E")) + I(1 * 
##     (spray == "F")) + I(1 * (spray == "A")), data = InsectSprays)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.333 -1.958 -0.500  1.667  9.333 
## 
## Coefficients: (1 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            14.5000     1.1322  12.807  < 2e-16 ***
## I(1 * (spray == "B"))   0.8333     1.6011   0.520    0.604    
## I(1 * (spray == "C")) -12.4167     1.6011  -7.755 7.27e-11 ***
## I(1 * (spray == "D"))  -9.5833     1.6011  -5.985 9.82e-08 ***
## I(1 * (spray == "E")) -11.0000     1.6011  -6.870 2.75e-09 ***
## I(1 * (spray == "F"))   2.1667     1.6011   1.353    0.181    
## I(1 * (spray == "A"))       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.922 on 66 degrees of freedom
## Multiple R-squared:  0.7244,	Adjusted R-squared:  0.7036 
## F-statistic:  34.7 on 5 and 66 DF,  p-value: < 2.2e-16
```

The coeficients remain the same, but note that R marked spray A as `NA`. In other words, since we already have all the regressors to get to the fitting with sprayA as the reference. adding sprayA again will flagged a new regressor sprayA as irrelevant.

If we remove the intercept however, we will tell R to stop working with a reference. We do that with the pattern `- 1` to indicate removal the intercept ($\beta_0$) regressor:


```r
summary(lm(count ~ spray - 1, data=InsectSprays))$coef
```

```
##         Estimate Std. Error   t value     Pr(>|t|)
## sprayA 14.500000   1.132156 12.807428 1.470512e-19
## sprayB 15.333333   1.132156 13.543487 1.001994e-20
## sprayC  2.083333   1.132156  1.840148 7.024334e-02
## sprayD  4.916667   1.132156  4.342749 4.953047e-05
## sprayE  3.500000   1.132156  3.091448 2.916794e-03
## sprayF 16.666667   1.132156 14.721181 1.573471e-22
```

Now all estimates indicate the mean of that spray on the count, all values are absolute to that specific factor.

The probability column now is checking the probability of a sprayX being different than zero.

### Re-Leveling

We can change the reference predictor by using `relevel`:


```r
spray2 <- relevel(InsectSprays$spray, 'C')
summary(lm(count ~ spray2, data=InsectSprays))$coef
```

```
##              Estimate Std. Error  t value     Pr(>|t|)
## (Intercept)  2.083333   1.132156 1.840148 7.024334e-02
## spray2A     12.416667   1.601110 7.755038 7.266893e-11
## spray2B     13.250000   1.601110 8.275511 8.509776e-12
## spray2D      2.833333   1.601110 1.769606 8.141205e-02
## spray2E      1.416667   1.601110 0.884803 3.794750e-01
## spray2F     14.583333   1.601110 9.108266 2.794343e-13
```



## Bi-Modal Regressor and Interaction Terms

Back to our swiss sensus example:


```r
data(swiss)
head(swiss)
```

```
##              Fertility Agriculture Examination Education Catholic
## Courtelary        80.2        17.0          15        12     9.96
## Delemont          83.1        45.1           6         9    84.84
## Franches-Mnt      92.5        39.7           5         5    93.40
## Moutier           85.8        36.5          12         7    33.77
## Neuveville        76.9        43.5          17        15     5.16
## Porrentruy        76.1        35.3           9         7    90.57
##              Infant.Mortality
## Courtelary               22.2
## Delemont                 22.2
## Franches-Mnt             20.2
## Moutier                  20.3
## Neuveville               20.6
## Porrentruy               26.6
```

Let's take a look at Catholic:


```r
hist(swiss$Catholic)
```

![](index_files/figure-html/unnamed-chunk-113-1.png)

We can see that this data is mostly bi-modal, i.e. indicates if a provice is a majority catholic or non-catholic. We should make this a binary indicator of a catholic majority:


```r
swiss <- mutate(swiss, MajorityCatholic = 1 * (Catholic > 50))
```

And here is how a scatter plot of the data looks like


```r
ggplot(swiss, aes(x=Agriculture, y=Fertility)) +
    geom_point(aes(color=factor(MajorityCatholic))) +
    xlab('% agriculture') + ylab('% fertility')
```

![](index_files/figure-html/unnamed-chunk-115-1.png)

Let's fit a line through the entire population, disregarding the majority binary indicator:


```r
fit <- lm(Fertility ~ Agriculture, data=swiss)
summary(fit)$coef
```

```
##               Estimate Std. Error   t value     Pr(>|t|)
## (Intercept) 60.3043752 4.25125562 14.185074 3.216304e-18
## Agriculture  0.1942017 0.07671176  2.531577 1.491720e-02
```

We care about coefficients 1 and 4:


```r
c(coef(fit)[1], coef(fit)[2])
```

```
## (Intercept) Agriculture 
##  60.3043752   0.1942017
```

And adding an `geom_abline` for the regression line.


```r
ggplot(swiss, aes(x=Agriculture, y=Fertility)) +
    geom_point(aes(color=factor(MajorityCatholic))) +
    geom_abline(intercept=coef(fit)[1], slope=coef(fit)[2], size=0.5) +
    xlab('% agriculture') + ylab('% fertility')
```

![](index_files/figure-html/unnamed-chunk-118-1.png)

We want to fit a line in each of the populations, let's try one thing first: adding the factor for the binary predictor:


```r
fit <- lm(Fertility ~ Agriculture + factor(MajorityCatholic), data=swiss)
summary(fit)$coef
```

```
##                             Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)               60.8322366  4.1058630 14.815944 1.032493e-18
## Agriculture                0.1241776  0.0810977  1.531210 1.328763e-01
## factor(MajorityCatholic)1  7.8843292  3.7483622  2.103406 4.118221e-02
```

We care about the following pairs of interceptor/slopes:


```r
c(coef(fit)[1], coef(fit)[2])
```

```
## (Intercept) Agriculture 
##  60.8322366   0.1241776
```

```r
c(coef(fit)[1] + coef(fit)[3], coef(fit)[2])
```

```
## (Intercept) Agriculture 
##  68.7165659   0.1241776
```

For the second, remember that means have to be added for a reference predictor.

And here are they, in the plot:


```r
ggplot(swiss, aes(x=Agriculture, y=Fertility)) +
    geom_point(aes(color=factor(MajorityCatholic))) +
    geom_abline(intercept=coef(fit)[1], slope=coef(fit)[2], size=0.5) +
    geom_abline(intercept=coef(fit)[1] + coef(fit)[3], slope=coef(fit)[2], size=0.5, colour='blue') +
    xlab('% agriculture') + ylab('% fertility')
```

![](index_files/figure-html/unnamed-chunk-121-1.png)

Not what we are looking for. Both lines have the same slope.

Let's fit with a product `*` of factors:


```r
fit <- lm(Fertility ~ Agriculture * factor(MajorityCatholic), data=swiss)
summary(fit)$coef
```

```
##                                          Estimate  Std. Error    t value
## (Intercept)                           62.04993019  4.78915566 12.9563402
## Agriculture                            0.09611572  0.09881204  0.9727127
## factor(MajorityCatholic)1              2.85770359 10.62644275  0.2689238
## Agriculture:factor(MajorityCatholic)1  0.08913512  0.17610660  0.5061430
##                                           Pr(>|t|)
## (Intercept)                           1.919379e-16
## Agriculture                           3.361364e-01
## factor(MajorityCatholic)1             7.892745e-01
## Agriculture:factor(MajorityCatholic)1 6.153416e-01
```

We care about the following pairs of interceptor/slopes:


```r
c(coef(fit)[1], coef(fit)[2])
```

```
## (Intercept) Agriculture 
## 62.04993019  0.09611572
```

```r
c(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4])
```

```
## (Intercept) Agriculture 
##  64.9076338   0.1852508
```

Again, remember that means have to be added for a reference predictor.

And here are they, in the plot:


```r
ggplot(swiss, aes(x=Agriculture, y=Fertility)) +
    geom_point(aes(color=factor(MajorityCatholic))) +
    geom_abline(intercept=coef(fit)[1], slope=coef(fit)[2], size=0.5) +
    geom_abline(intercept=coef(fit)[1] + coef(fit)[3], 
                slope=coef(fit)[2] + coef(fit)[4], size=0.5, colour='blue') +
    xlab('% agriculture') + ylab('% fertility')
```

![](index_files/figure-html/unnamed-chunk-124-1.png)

Blue line is the fitting for mostly catholic provinces.

And this is what we were looking for.


## Gaussian Elimination


```r
ones <- rep(1, nrow(galton))
```

The default intercept can be excluded by using -1 in the formula. Perform a regression which substitutes our regressor, ones, for the default using lm(child ~ ones + parent -1, galton)


```r
lm(child ~ ones + parent - 1, galton)
```

```
## 
## Call:
## lm(formula = child ~ ones + parent - 1, data = galton)
## 
## Coefficients:
##    ones   parent  
## 23.9415   0.6463
```

The regression in one variable given by lm(child ~ parent, galton) really involves two regressors, the variable, parent, and a regressor of all ones (what in the case before was "Intercept").


```r
lm(child ~ parent, galton)
```

```
## 
## Call:
## lm(formula = child ~ parent, data = galton)
## 
## Coefficients:
## (Intercept)       parent  
##     23.9415       0.6463
```

The regression line given by lm(child ~ parent, galton) goes through the point x=mean(parent), y=mean(child). If we subtract the mean from each variable, the regression line goes through the origin, x=0, y=0, hence its intercept is zero. 

Thus, by subtracting the means, we eliminate one of the two regressors, the constant, leaving just one, parent. The coefficient of the remaining regressor is the slope. 

Subtracting the means to eliminate the intercept is a special case of a general technique which is sometimes called _Gaussian Elimination_. As it applies here, the general technique is to pick one regressor and to replace all other variables by the residuals of their regressions against that one.

A residual is the difference between a variable and its predicted value. If, for example, child-mean(child) is a residual, then mean(child) must be its predicted value. But mean(child) is a constant, so the regressor would be a constant (one).

# Adjustment

Adjustment, is the idea of putting regressors into a linear model to investigate the role of a third variable on the relationship between another two. It is often the case that a third variable can distort, or confound, the relationship between two others.

We will look at a few simulations to illustrate the following points:
* Modeling multivariate relationships is difficult.
* Play around with simulations to see how the inclusion or exclusion of another variable can change analyses.
* The results of these analyses deal with the impact of variables on associations.
     + Ascertaining mechanisms or cause are difficult subjects to be added on top of difficulty in understanding multivariate associations.

## First Simulation

Let's look at the first simulation.


```r
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
```

![](index_files/figure-html/unnamed-chunk-128-1.png)

Status is either red or blue, given by the color of the group.

We note a few things:

* X is unrelated to group status
* X is related to Y, but the intercept depends on the group status
* The group variable is related to Y:
    + Relationship between group and Y is constant and depends on X
    + Relationship between group and Y disregarding X is about the same as holding X constant 

## Second Simulation

On the second simulation:


```r
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
```

![](index_files/figure-html/unnamed-chunk-129-1.png)

* X is related to group status
* X is related to Y, the intercept does not depend on the group status
    + Even on different groups, X still depends to Y
* The group variable is marginally related to Y disregarding X
* The model would estimate no adjusted effect due to group
    + Nothing to inform relationship between group and Y 
    
### Third Simulation

This simulation shows an instance of Simpson's Paradox. An outcome can show an entirely different relationship by just adding a regressor to the model.


```r
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), .9 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
```

![](index_files/figure-html/unnamed-chunk-130-1.png)


* Marginal association has red group higher than blue.
* Adjusted relationship has blue group higher than red.
* Group status related to X.
* There is some direct evidence for comparing red and blue holding X fixed.

## Fourth Simulation


```r
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
```

![](index_files/figure-html/unnamed-chunk-131-1.png)


* No marginal association between group status and Y.
* Strong adjusted relationship.
* Group status not related to X.
* There is lots of direct evidence for comparing red and blue holding X fixed.

## Fifth Simulation


```r
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1));
beta0 <- 0; beta1 <- 2; tau <- 0; tau1 <- -4; sigma <- .2
y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t + I(x * t))
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)
```

![](index_files/figure-html/unnamed-chunk-132-1.png)


* There is no such thing as a group effect here.
     + The impact of group reverses itself depending on X.
     + Both intercept and slope depends on group.
* Group status and X unrelated.
     + There's lots of information about group effects holding X fixed.

## Sixth Simulation


```r
p <- 1
n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2
beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)
```

![](index_files/figure-html/unnamed-chunk-133-1.png)

Bivariate Relationships, for use in a X11 enabled display:


```r
library(rgl)
plot3d(x1, x2, y)
```

Residual Relationships


```r
plot(resid(lm(x1 ~ x2)), resid(lm(y ~ x2)), frame = FALSE, col = "black", bg = "lightblue", pch = 21, cex = 2)
abline(lm(I(resid(lm(x1 ~ x2))) ~ I(resid(lm(y ~ x2)))), lwd = 2)
```

![](index_files/figure-html/unnamed-chunk-135-1.png)


* X1 unrelated to X2
* X2 strongly related to Y
* Adjusted relationship between X1 and Y largely unchanged by considering X2.
     + Almost no residual variability after accounting for X2.
     
# Residuals and Diagnostics

## Plot of features of a fit


```r
data(swiss); 
par(mfrow=c(2,2))
fit <- lm(Fertility ~ ., data=swiss)
plot(fit)
```

![](index_files/figure-html/unnamed-chunk-136-1.png)

## Influence, Leverage and Outliers

* Leverage: how far away is the data point from the center of mass of a distribution
* Influence: An observation is influential if an estimate changes substantially after that data point is removed

So, give the following points:


```r
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
```

What is the hat-value (measures of leverage) of the most influencial point?


```r
max(hatvalues(fit))
```

```
## [1] 0.9945734
```

What the slope dfbeta for the point with the highest hat-value?


```r
influence.measures(fit)$infmat
```

```
##        dfb.1_         dfb.x         dffit     cov.r       cook.d       hat
## 1  1.06212391   -0.37811633    1.06794603 0.3412772 2.925794e-01 0.2286650
## 2  0.06748037   -0.02861769    0.06750799 2.9338458 3.394011e-03 0.2438146
## 3 -0.01735756    0.00791512   -0.01735800 3.0073613 2.258743e-04 0.2525027
## 4 -1.24958248    0.67253246   -1.25570867 0.3422019 3.912201e-01 0.2804443
## 5  0.20432010 -133.82261293 -149.72037760 0.1073304 2.704935e+02 0.9945734
```

We look for the column `dfb.x` for the highest `hat` value (**0.9945734**) - so the value is **-133.82261293**

### Case 1

Several data points, one is clearly an outlier (first point, (10,10))


```r
x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))  
```

![](index_files/figure-html/unnamed-chunk-140-1.png)

How can we measure that? First, using `dfbetas`: change in individual coefficients when the $i^{th}$ point is deleted in fitting the model.


```r
fit <- lm(y ~ x)
round(dfbetas(fit)[1:10, 2], 3)
```

```
##      1      2      3      4      5      6      7      8      9     10 
##  7.956  0.003 -0.045 -0.219 -0.018 -0.011 -0.022 -0.047 -0.017 -0.197
```

Measurement of the first point is orders of magnitude higher than other points.

Second, using `hatvalue`: measures of leverage


```r
round(hatvalues(fit)[1:10], 3)
```

```
##     1     2     3     4     5     6     7     8     9    10 
## 0.507 0.010 0.013 0.031 0.012 0.013 0.019 0.018 0.010 0.038
```

Measurement of the first point is higher than other points.

### Case 2

Several data points, one is clearly an outlier but laying on top of the regression line


```r
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)
```

![](index_files/figure-html/unnamed-chunk-143-1.png)


```r
round(dfbetas(fit2)[1 : 10, 2], 3)
```

```
##      1      2      3      4      5      6      7      8      9     10 
## -0.263  0.024 -0.036  0.007 -0.017 -0.018  0.287  0.130 -0.072 -0.013
```

Still large, but not as much as others


```r
round(hatvalues(fit2)[1 : 10], 3)
```

```
##     1     2     3     4     5     6     7     8     9    10 
## 0.216 0.018 0.012 0.011 0.010 0.011 0.053 0.015 0.024 0.017
```

Hat value of the first observation is much larger than before.

### Stefanski Example

Download sample file, cached:


```r
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
```

Pairs plot, quick glimpse.


```r
pairs(dat)
```

![](index_files/figure-html/unnamed-chunk-146-1.png)

Not much, a lot of overplotting clouds over the 5 features.

Let's fit a liner model on all 5 features:


```r
summary(lm(V1 ~ . -1, data=dat))$coef
```

```
##     Estimate Std. Error   t value     Pr(>|t|)
## V2 0.9856157 0.12798121  7.701253 1.989126e-14
## V3 0.9714707 0.12663829  7.671225 2.500259e-14
## V4 0.8606368 0.11958267  7.197003 8.301184e-13
## V5 0.9266981 0.08328434 11.126919 4.778110e-28
```

Let's plot the residuals against predictions, $\hat Y_i$ in $x$ and residuals $e_i$ in $y$:


```r
fit <- lm(V1 ~ . -1, data=dat)
plot(predict(fit), resid(fit), pch='.')
```

![](index_files/figure-html/unnamed-chunk-148-1.png)

# Model Selection

Answering the question: How do we chose what variables to include in a regression model?

## Simulating Impact of New Regressors in $R^2$

Also known as variance inflation, adding variables to the model and increasing actual standard errors for all the other regressors.

Here is a simulation of adding a number of uncorrelated regressors:


```r
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
betas <- sapply(1 : nosim, function(i){
  y <- x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2], 
    coef(lm(y ~ x1 + x2))[2], 
    coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)
```

```
##      x1      x1      x1 
## 0.03140 0.03141 0.03260
```

You can see that standard deviation did not change by a large number.

Now, let's simulate highly correlated regressors:


```r
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
betas <- sapply(1 : nosim, function(i){
  y <- x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2], 
    coef(lm(y ~ x1 + x2))[2], 
    coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)
```

```
##      x1      x1      x1 
## 0.02813 0.04460 0.10029
```

In this case the standard deviation increased by a lot.

On other words: **if we omit regressors we get biased (_underfitting_), if we include unnecessary regressors we increase the standard error (_overfitting_).**

### Variance Inflation Factor (VIF)


```r
data(swiss)
fit <- lm(Fertility ~ . , data = swiss)
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit, Fertility ~ Agriculture + Examination)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
  c(summary(fit2)$cov.unscaled[2,2],
    summary(fit3)$cov.unscaled[2,2]) / a 
```

```
## [1] 1.891576 2.089159
```


```r
library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
```

```
##      Agriculture      Examination        Education         Catholic 
##         2.284129         3.675420         2.774943         1.937160 
## Infant.Mortality 
##         1.107542
```

Means that agriculture for example has 2 times impact in error in comparison if it was a purely random regressor. Infant.Mortality (low, approx 1) is uncorrelated to any of the other regressors.

Also, `vif` is the variation inflation, and the square of standard error inflation:


```r
sqrt(vif(fit)) #I prefer sd 
```

```
##      Agriculture      Examination        Education         Catholic 
##         1.511334         1.917138         1.665816         1.391819 
## Infant.Mortality 
##         1.052398
```

You can remove regressors using a `-` sign:


```r
fit2 <- lm(Fertility ~ . -Examination, data = swiss)
vif(fit2)
```

```
##      Agriculture        Education         Catholic Infant.Mortality 
##         2.147153         1.816361         1.299916         1.107528
```

Omitting Examination has markedly decreased the VIF for Education, effect of how correlated these were.

### Nested Models

As example of a nested models, let's fit three nested models, adding new regressors on each model: fertility and agriculture first, and on subsequent step fit examination and education, and in a last step catholic and infant mortality.


```r
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)
```

```
## Analysis of Variance Table
## 
## Model 1: Fertility ~ Agriculture
## Model 2: Fertility ~ Agriculture + Examination + Education
## Model 3: Fertility ~ Agriculture + Examination + Education + Catholic + 
##     Infant.Mortality
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1     45 6283.1                                  
## 2     43 3180.9  2    3102.2 30.211 8.638e-09 ***
## 3     41 2105.0  2    1075.9 10.477 0.0002111 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

This gives a list of the three models and a number of columns. The F statistic tests the predictive capability of the model as a whole - the larger the F value the better your model is at predicting the dependent variable. Lower F values indicate the model is not as good at predicting the dependent variable.

Three asterisks (***) at the lower right of the printed table indicate that the null hypothesis is rejected at the 0.001 level, so at least one of the two additional regressors is significant. Rejection is based on a right-tailed F test, Pr(>F), applied to an F value.

F values are calculated the following way:


```r
samples <- 47 # samples in swiss
fit1Predictors <- 1 + 1 # 1 predictor + the intercept
fit3Predictors <- 3 + 1 # 3 predictors + the intercept
(deviance(fit3)/(samples - fit3Predictors)) / ((deviance(fit1)-deviance(fit3))/(fit3Predictors-fit1Predictors))
```

```
## [1] 0.0476921
```

# GLM

Generalized Linear Models are defined on three components

* Exponential family model for the response
* Systematic component via linear predictor
* Link function, connecting means of the response to the linear predictor

They are divided in three groups of regressions:

* Linear model
* Logistic regression (Bernoulli)
* Poisson regression

We will look at logistic and poisson regressions only.

## Logistic Regressions

A peak at the logistic curve:

$f(x) = \frac {e^{\beta_0 + \beta_1 x}}{1 + e^{\beta_0 + \beta_1 x}}$:



```r
beta0 <- 0
beta1 <- 2
x <- seq(-10, 10, length=1000)
y <- exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x))
plot(x, y, type='l', lwd=3, frame=FALSE)
```

![](index_files/figure-html/unnamed-chunk-157-1.png)

As an example, we are going to look at to which extent the Baltimore Ravens score defines a win.


```r
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda"
              , destfile="ravensData.rda",method="curl")
load("ravensData.rda")
head(ravensData)
```

```
##   ravenWinNum ravenWin ravenScore opponentScore
## 1           1        W         24             9
## 2           1        W         38            35
## 3           1        W         28            13
## 4           1        W         34            31
## 5           1        W         44            13
## 6           0        L         23            24
```

Should we fit a linear regression model in binary regressors?


```r
lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef
```

```
##                         Estimate  Std. Error  t value   Pr(>|t|)
## (Intercept)           0.28503172 0.256643165 1.110615 0.28135043
## ravensData$ravenScore 0.01589917 0.009058997 1.755069 0.09625261
```

Not usually, you can at a first shot, but you can get to negative probabilities or close to 1 slopes.

Logistic regression on Ravens data:


```r
logLmRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore, family='binomial') # binomial equiv logistic
summary(logLmRavens)
```

```
## 
## Call:
## glm(formula = ravensData$ravenWinNum ~ ravensData$ravenScore, 
##     family = "binomial")
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7575  -1.0999   0.5305   0.8060   1.4947  
## 
## Coefficients:
##                       Estimate Std. Error z value Pr(>|z|)
## (Intercept)           -1.68001    1.55412  -1.081     0.28
## ravensData$ravenScore  0.10658    0.06674   1.597     0.11
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 24.435  on 19  degrees of freedom
## Residual deviance: 20.895  on 18  degrees of freedom
## AIC: 24.895
## 
## Number of Fisher Scoring iterations: 5
```


```r
plot(ravensData$ravenScore, logLmRavens$fitted, pch=19, col='blue', xlab='score', ylab='probability ravens win')
```

![](index_files/figure-html/unnamed-chunk-160-1.png)

If you look closer, this shows a segment of the logistic curve shown earlier.

Odds ratio:


```r
exp(logLmRavens$coeff)
```

```
##           (Intercept) ravensData$ravenScore 
##             0.1863724             1.1124694
```

This basically says we will increase the probablity of win by **11.25%** for every extra point scored.


```r
exp(confint(logLmRavens))
```

```
## Waiting for profiling to be done...
```

```
##                             2.5 %   97.5 %
## (Intercept)           0.005674966 3.106384
## ravensData$ravenScore 0.996229662 1.303304
```

The 2.5% - 97.5% interval includes one, so the coeficients above are not significant.

Applying ANOVA to this logistic regression:


```r
anova(logLmRavens, test='Chisq')
```

```
## Analysis of Deviance Table
## 
## Model: binomial, link: logit
## 
## Response: ravensData$ravenWinNum
## 
## Terms added sequentially (first to last)
## 
## 
##                       Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
## NULL                                     19     24.435           
## ravensData$ravenScore  1   3.5398        18     20.895  0.05991 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Using the shuttle dataset, what are the estimated odds for autlander use comparing headwinds to tailwinds?


```r
data(shuttle)
shuttle$auto <- as.integer(shuttle$use == 'auto')
fit <- glm(auto ~ wind - 1, data=shuttle, family='binomial')
summary(fit)$coef
```

```
##           Estimate Std. Error  z value  Pr(>|z|)
## windhead 0.2513144  0.1781742 1.410499 0.1583925
## windtail 0.2831263  0.1785510 1.585689 0.1128099
```

exp() of coeficients


```r
exp(coef(fit))
```

```
## windhead windtail 
## 1.285714 1.327273
```

So, estimated odds = exp(head) / exp(tail), from table above:


```r
1.285714 / 1.327273
```

```
## [1] 0.9686884
```

Or programatically:


```r
exp(coef(fit))[1]/exp(coef(fit))[2]
```

```
##  windhead 
## 0.9686888
```

Are the estimated odds.

Now, what are the estimated odds if we adjust for wind strenght (`magn`)?


```r
fitWind <- glm(auto ~ wind + magn - 1, data=shuttle, family='binomial')
summary(fitWind)$coef
```

```
##                 Estimate Std. Error       z value  Pr(>|z|)
## windhead    3.635093e-01  0.2840608  1.279688e+00 0.2006547
## windtail    3.955180e-01  0.2843987  1.390717e+00 0.1643114
## magnMedium -1.009525e-15  0.3599481 -2.804642e-15 1.0000000
## magnOut    -3.795136e-01  0.3567709 -1.063746e+00 0.2874438
## magnStrong -6.441258e-02  0.3589560 -1.794442e-01 0.8575889
```


```r
exp(coef(fitWind))[1]/exp(coef(fitWind))[2]
```

```
##  windhead 
## 0.9684981
```

Are the estimated odds, adjusted for wind strength. About the same as before.

What happens if we fit a logistic regression model to a binary variable, then fit a logistic regression model for one minus the outcome what happens to the coefficients?


```r
summary(glm(1 - auto ~ wind - 1, data=shuttle, family=binomial))$coef
```

```
##            Estimate Std. Error   z value  Pr(>|z|)
## windhead -0.2513144  0.1781742 -1.410499 0.1583925
## windtail -0.2831263  0.1785510 -1.585689 0.1128099
```


```r
summary(glm(auto ~ wind - 1, data=shuttle, family=binomial))$coef
```

```
##           Estimate Std. Error  z value  Pr(>|z|)
## windhead 0.2513144  0.1781742 1.410499 0.1583925
## windtail 0.2831263  0.1785510 1.585689 0.1128099
```

The coefficients reverse signs.

## Poisson Regressions

Poisson regressions tend to look like normal distributions as the value of $\lambda$ gets large:


```r
par(mfrow = c(1,3))
plot(0:10, dpois(0:10, lambda=2), type='h', frame=F)
plot(0:20, dpois(0:20, lambda=10), type='h', frame=F)
plot(0:200, dpois(0:200, lambda=100), type='h', frame=F)
```

![](index_files/figure-html/unnamed-chunk-172-1.png)

### Web Traffic

Taking a peek at a web site hit count:


```r
download.file("https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda",destfile="gaData.rda",method="curl")
load("gaData.rda")
```

Let's convert date to julian (an integer count of days from past date)


```r
gaData$julian <- julian(gaData$date)
head(gaData)
```

```
##         date visits simplystats julian
## 1 2011-01-01      0           0  14975
## 2 2011-01-02      0           0  14976
## 3 2011-01-03      0           0  14977
## 4 2011-01-04      0           0  14978
## 5 2011-01-05      0           0  14979
## 6 2011-01-06      0           0  14980
```

Plotting and inserting a linear regression line.


```r
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$julian)
glm1 <- glm(gaData$visits ~ gaData$julian,family="poisson")
abline(lm1,col="red",lwd=1)
lines(gaData$julian,glm1$fitted,col="blue",lwd=3)
```

![](index_files/figure-html/unnamed-chunk-174-1.png)

In red a simple linear regression, in blue a poisson GLM regression.

You can see they almost overlap, for these data points is not much of a difference.



```r
round(exp(coef(lm(I(log(gaData$visits + 1)) ~ gaData$julian))), 5)
```

```
##   (Intercept) gaData$julian 
##       0.00000       1.00231
```

So the model is estimating a 0.2% increase in web traffic per day.

### Insect Spray Efficacy Comparison

Consider the InsectSprays dataset. What is the estimated relative rate comparing spray A to spray B?


```r
data("InsectSprays")
fit <- glm(count ~ factor(spray) -1, data=InsectSprays, family=poisson)
summary(fit)$coef
```

```
##                 Estimate Std. Error   z value      Pr(>|z|)
## factor(spray)A 2.6741486 0.07580980 35.274443 1.448048e-272
## factor(spray)B 2.7300291 0.07372098 37.031917 3.510670e-300
## factor(spray)C 0.7339692 0.19999987  3.669848  2.426946e-04
## factor(spray)D 1.5926308 0.13018891 12.233229  2.065604e-34
## factor(spray)E 1.2527630 0.15430335  8.118832  4.706917e-16
## factor(spray)F 2.8134107 0.07071068 39.787636  0.000000e+00
```


```r
exp(coef(fit))
```

```
## factor(spray)A factor(spray)B factor(spray)C factor(spray)D factor(spray)E 
##      14.500000      15.333333       2.083333       4.916667       3.500000 
## factor(spray)F 
##      16.666667
```


```r
exp(coef(fit))[1]/exp(coef(fit))[2]
```

```
## factor(spray)A 
##      0.9456522
```

# Splines

How to fit pretty complicated curves in linear regressions? We do that through `splines`

Consider the model 

$$ Y_i = \beta_0 + \beta_1 X_i + \sum_{k=1}^d (x_i - \xi_k)_+ \gamma_k + \epsilon{i} $$ where $(a)_+ = a$ if $a > 0$ and $0$ otherwise and $\xi_1 \leq ... \leq \xi_d$ are known knot points



```r
n <- 500; x <- seq(0, 4 * pi, length = n); 
y <- sin(x) + rnorm(n, sd = .3)
knots <- seq(0, 8 * pi, length = 20); 
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
```

![](index_files/figure-html/unnamed-chunk-179-1.png)

One issue, the knot points are too edgy.

The solution is to square the difference in the knot points, so the model becomes

$$ Y_i = \beta_0 + \beta_1 X_i + \beta_2 X_i^2 + \sum_{k=1}^d {(x_i - \xi_k)_+}^2 \gamma_k + \epsilon{i} $$



```r
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)^2) # (x - knot)^2
xMat <- cbind(1, x, x^2, splineTerms) # x^2
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
```

![](index_files/figure-html/unnamed-chunk-180-1.png)

Better.

# Detecting Sound Harmonics

How to detect sound harmonics in a number of data points representing sound? First generating notes and chords: 


```r
##Chord finder, playing the white keys on a piano from octave c4 - c5
notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25)
t <- seq(0, 2, by = .001); n <- length(t)
# enconded chords
c4 <- sin(2 * pi * notes4[1] * t); 
e4 <- sin(2 * pi * notes4[3] * t); 
g4 <- sin(2 * pi * notes4[5] * t)
chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3)
```

Let's generate a linear fit with an outcome `chord` and sound `x` as the regressor. 


```r
x <- sapply(notes4, function(freq) sin(2 * pi * freq * t))
fit <- lm(chord ~ x - 1)
```

Now plotting, just the 

```r
plot(c(0, 9), c(0, 1.5), xlab = "Note", ylab = "Coef^2", axes = FALSE, frame = TRUE, type = "n")
axis(2)
axis(1, at = 1 : 8, labels = c("c4", "d4", "e4", "f4", "g4", "a4", "b4", "c5"))
for (i in 1 : 8) 
    abline(v = i, lwd = 3, col = grey(.8))
# x is the note, y os coef(fit)^2
lines(c(0, 1 : 8, 9), c(0, coef(fit)^2, 0), type = "l", lwd = 3, col = "red")
```

![](index_files/figure-html/unnamed-chunk-183-1.png)

We can see c4, e4 and g4 are detected as the encoded chords:

Now, how do we really do that: using a fast fourier transform.


```r
a <- fft(chord); 
plot(Re(a)^2, type = "l")
```

![](index_files/figure-html/unnamed-chunk-184-1.png)

The frequency is the representation of c4, e4 and g4 originally encoded.
