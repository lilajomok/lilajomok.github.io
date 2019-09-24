---
layout: post
title: "Time Series Analysis of RTA Boardings, Route 16 (2008-2015)"
categories: Projects
tags: [R, time series]
img: /images/blog-img/11-20-2016-rta-routes_files/figure-markdown_github
---

_This is a condensed version of an undergrad project for my Time Series analysis class at UCR. Files can be found at the [rta-routes](https://github.com/lilajomok/rta-routes) repository on GitHub._

Table of Contents
=================
{:.no_toc}
* TOC
{:toc}

Data
====

{% highlight r linenos %}
  # Open TSA library
  library(TSA)

  # Set to directory where data files are
  # setwd("~/Documents/rta-routes/data/")

  # Import data files for routes 1 and 16; each file contains monthly numbers of riders
  route.16 <- ts(read.table(file="data/route16.txt"), start=c(2008,7), frequency = 12)
{% endhighlight %}

Although there are many bus routes, we are interested in Route 16 mainly due to the fact that it runs through UCR (University of California - Riverside). We create a time series data frame `route.16` from the given `.txt` files. It contains *monthly* counts of how many riders take this bus route.

{% highlight r linenos %}
  # Plot time series
  plot(route.16, ylab="Number of Riders", xlab="Year", main="Riders Per Month - Route 16",type="o")
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-2-1.png)

The time series plot suggest we can apply a seasonal model since there is a pattern. It also shows that there is an increasing trend over time, especially between years 2008-2013.

------------------------------------------------------------------------

Model Specification
===================

Data suggests an ARIMA(p,q,d) model. In order to reach stationarity, we look at possible transformations through a Box-Cox transformation with a 95% confidence interval and *λ*= 0.5.

Transformation
--------------

The output suggests a **square root transformation**.

{% highlight r linenos %}
  # Create subset of route 16 from Jan 2008 to Feb 2015
  route.16.window <- window(route.16, end=c(2015,2))

  # Use Box-Cox transformation to determine which power transf to use
  # BoxCox.ar(route.16.window)
{% endhighlight %}

Below is the plot of the transformed data:

{% highlight r linenos %}
  plot(sqrt(route.16.window),ylab="Passengers",xlab="Years",main="Squareroot transformation of Riders per month line 16")
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-4-1.png)

Differentiation
---------------

The plot of the transformed data still shows stationarity, so we plot the **first degree difference** of the transformation. In addition to the stationarity, the plot still shows an increasing trend.

{% highlight r linenos %}
  # Differentiation
  plot(diff(sqrt(route.16.window),xlab="Years",ylab="First difference", main="First degree difference of the sqrt transformation"))
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-5-1.png)

The **Augmented Dickey-Fuller Test** verifies the stationarity of the differentiated data with a p-value of 0.01. This value is sufficient to sustain the hypothesis that our data is stationary with a signficance level of *α* = 0.05.

{% highlight r linenos %}
  # Testing Stationarity
  adf.test(diff(sqrt(route.16.window)))

    ##
    ##  Augmented Dickey-Fuller Test
    ##
    ## data:  diff(sqrt(route.16.window))
    ## Dickey-Fuller = -6.8511, Lag order = 4, p-value = 0.01
    ## alternative hypothesis: stationary
{% endhighlight %}

We can now proceed to find the appropriate ARMA(p,q) model by using the sample ACF and PACF with a lag of 12:

{% highlight r linenos %}
  # Sample ACF
  acf(as.vector(diff(sqrt(route.16.window),lag=12)))
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-7-1.png)

The **Sample ACF** resembles one of a seasonal MA(1) - it spikes at lag *k* = 1 and then drops below the margin of error. The **Sample PACF** also has a similar output.

{% highlight r linenos %}
  # Sample PACF
  pacf(as.vector(diff(sqrt(route.16.window),lag=12)))
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-8-1.png)

Both figures suggest that an ARIMA(1,1,1) model is most suitable for our data. To confirm this, we can also look at the **EACF**:

{% highlight r linenos %}
  # Sample EACF
  eacf(as.vector(diff(sqrt(route.16.window),lag=12)))

    ## AR/MA
    ##   0 1 2 3 4 5 6 7 8 9 10 11 12 13
    ## 0 x x o x x o o o o o o  o  o  o
    ## 1 x o o o o o o o o o o  o  o  o
    ## 2 x o o o o o o o o o o  o  o  o
    ## 3 x x x o o o o o x o o  o  o  o
    ## 4 x o o x o o o o o o o  o  o  o
    ## 5 x o o x o o o o o o o  o  o  o
    ## 6 o x o o o o o o o o o  o  o  o
    ## 7 o x o o x o o o o o o  o  o  o
{% endhighlight %}

The tip of the EACF is reached at the point (1,1), creating a wedge from there. In addition to the ARIMA(1,1,1) model, another suggested model for the data is an ARI(1,1) model since the sample ACF does not immediately drop below the margin of error. Different results can be obtained if the MA(1) of the ARIMA model is left out.

------------------------------------------------------------------------

Model Fitting
=============

Since we want to estimate how many people ride the bus each month, we will test two different approaches of our model, one with seasonality and one without it.

ARIMA(1,1,1) non-seasonal
-------------------------

{% highlight r linenos %}
  # ARIMA(1,1,1) non-seasonal
  arima111.ns.fit <- arima(sqrt(route.16.window), order=c(1,1,1), method='ML')
  arima111.ns.fit

    ##
    ## Call:
    ## arima(x = sqrt(route.16.window), order = c(1, 1, 1), method = "ML")
    ##
    ## Coefficients:
    ##          ar1      ma1
    ##       0.3046  -0.8660
    ## s.e.  0.1218   0.0487
    ##
    ## sigma^2 estimated as 315.9:  log likelihood = -339.87,  aic = 683.75
{% endhighlight %}

ARIMA(1,1,1) seasonal
---------------------

{% highlight r linenos %}
  # ARIMA(1,1,1) seasonal
  arima111.fit <- arima(sqrt(route.16.window), order=c(1,1,1), seasonal=list(order=c(1,1,1), period=12), method="ML")
  arima111.fit

    ##
    ## Call:
    ## arima(x = sqrt(route.16.window), order = c(1, 1, 1), seasonal = list(order = c(1,
    ##     1, 1), period = 12), method = "ML")
    ##
    ## Coefficients:
    ##          ar1      ma1     sar1    sma1
    ##       0.2056  -0.8089  -0.8462  0.6708
    ## s.e.  0.1734   0.1168   0.3205  0.4487
    ##
    ## sigma^2 estimated as 36.52:  log likelihood = -217.05,  aic = 442.09
{% endhighlight %}
The following are the parameters:

$$
\sqrt{Y_t} = (1+0.3046)\sqrt{Y_{t-1}}-0.3046\sqrt{Y_{t-2}} + e_t - 0.8660e_{t-1}
$$

which is equivalent to:

$$
\sqrt{Y_t} - \sqrt{Y_{t-1}} = 0.3046(\sqrt{Y_{t-1}} -\sqrt{Y_{t-2}}) + e_t - 0.8660e_{t-1}
$$

With ML estimates as \\( \sigma^2_{e} \\)≈ 315.9, \\( \hat\phi \\) = 0.3146, \\( \hat\theta \\) = -0.8660. The approximate 95% confidence intervals for \\( \hat\phi \\) and \\( \hat\theta \\) is (0.065872, 0.543328) and (-0.961452,-0.770548), respectively. We found that \\( \hat\phi \\) and \\( \hat\theta \\)are significantly different from 0.


ARI(1,1) non-seasonal
---------------------

{% highlight r linenos %}
  # ARI(1,1) non-seasonal
  ari11.ns.fit <- arima(sqrt(route.16.window), order=c(1,1,0), method='ML')
  ari11.ns.fit

    ##
    ## Call:
    ## arima(x = sqrt(route.16.window), order = c(1, 1, 0), method = "ML")
    ##
    ## Coefficients:
    ##           ar1
    ##       -0.1733
    ## s.e.   0.1101
    ##
    ## sigma^2 estimated as 413.4:  log likelihood = -350.07,  aic = 702.15
{% endhighlight %}
ARI(1,1) seasonal
-----------------

{% highlight r linenos %}
  ari11.fit <- arima(sqrt(route.16.window), order=c(1,1,0), seasonal=list(order=c(1,1,0), period=12), method="ML")
  ari11.fit

    ##
    ## Call:
    ## arima(x = sqrt(route.16.window), order = c(1, 1, 0), seasonal = list(order = c(1,
    ##     1, 0), period = 12), method = "ML")
    ##
    ## Coefficients:
    ##           ar1     sar1
    ##       -0.4081  -0.2667
    ## s.e.   0.1104   0.1282
    ##
    ## sigma^2 estimated as 44.52:  log likelihood = -222.77,  aic = 449.54
{% endhighlight %}
------------------------------------------------------------------------

Model Diagnosis
===============

Non-Seasonal Models
-------------------

{% highlight r linenos %}
  #ARI(1,1)
  par(mfrow=c(2,2))
  plot(route.16, ylab="Number of Riders", xlab="Year", main="Riders Per Month -Route 16",type="o")
  plot(rstandard(ari11.ns.fit), ylab="Std. Residuals", xlab="Time", type="o")
  abline(h=0)
  hist(rstandard(ari11.ns.fit), xlab="Std. Residuals", main="")
  qqnorm(rstandard(ari11.ns.fit), main="")
  qqline(rstandard(ari11.ns.fit), main="")
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-14-1.png)

### Normality and Independence

{% highlight r linenos %}
  # Run Shapiro-Wilks test for normality
  shapiro.test(rstandard(ari11.ns.fit))  

    ##
    ##  Shapiro-Wilk normality test
    ##
    ## data:  rstandard(ari11.ns.fit)
    ## W = 0.95787, p-value = 0.009997

  shapiro.test(rstandard(arima111.ns.fit))

    ##
    ##  Shapiro-Wilk normality test
    ##
    ## data:  rstandard(arima111.ns.fit)
    ## W = 0.97308, p-value = 0.08925

  # Runs test for Independence
  runs(rstandard(ari11.ns.fit))  


    ## $pvalue
    ## [1] 0.916
    ##
    ## $observed.runs
    ## [1] 41
    ##
    ## $expected.runs
    ## [1] 40.975
    ##
    ## $n1
    ## [1] 39
    ##
    ## $n2
    ## [1] 41
    ##
    ## $k
    ## [1] 0

  runs(rstandard(arima111.ns.fit))

    ## $pvalue
    ## [1] 0.322
    ##
    ## $observed.runs
    ## [1] 35
    ##
    ## $expected.runs
    ## [1] 39.775
    ##
    ## $n1
    ## [1] 33
    ##
    ## $n2
    ## [1] 47
    ##
    ## $k
    ## [1] 0
{% endhighlight %}

### Residual PACF

{% highlight r linenos %}
  # Residual PACF
  pacf(rstandard(ari11.ns.fit), main="Sample PACF")  
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-16-1.png)

### Overfitting

{% highlight r linenos %}
  #ARIMA(1,1,1)
  par(mfrow=c(2,2))
  plot(route.16, ylab="Number of Riders", xlab="Year", main="Riders Per Month - Route 16",type="o")
  plot(rstandard(arima111.ns.fit), ylab="Std. Residuals", xlab="Time", type="o")
  abline(h=0)
  hist(rstandard(arima111.ns.fit), xlab="Std. Residuals", main="")
  qqnorm(rstandard(arima111.ns.fit), main="")
  qqline(rstandard(arima111.ns.fit), main="")
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-17-1.png)

{% highlight r linenos %}
  # Residual ACF / PACF
  par(mfrow=c(2,2))
  acf(rstandard(arima111.ns.fit), main="Sample ACF")
  pacf(rstandard(arima111.ns.fit), main="Sample PACF")
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-17-2.png)

------------------------------------------------------------------------

Seasonal Models
---------------

{% highlight r linenos %}
  #ARI(1,1)
  par(mfrow=c(2,2))
  plot(route.16, ylab="Number of Riders", xlab="Year", main="Riders Per Month -Route 16",type="o")
  plot(rstandard(ari11.fit), ylab="Std. Residuals", xlab="Time", type="o")
  abline(h=0)
  hist(rstandard(ari11.fit), xlab="Std. Residuals", main="")
  qqnorm(rstandard(ari11.fit), main="")
  qqline(rstandard(ari11.fit), main="")
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-18-1.png)

### Normality and Independence

{% highlight r linenos %}
  # Run Shapiro-Wilks test for normality
  shapiro.test(rstandard(ari11.fit))  

    ##
    ##  Shapiro-Wilk normality test
    ##
    ## data:  rstandard(ari11.fit)
    ## W = 0.90862, p-value = 2.871e-05

  shapiro.test(rstandard(arima111.fit))

    ##
    ##  Shapiro-Wilk normality test
    ##
    ## data:  rstandard(arima111.fit)
    ## W = 0.91728, p-value = 7.093e-05

  # Runs test for Independence
  runs(rstandard(ari11.fit))  

    ## $pvalue
    ## [1] 0.145
    ##
    ## $observed.runs
    ## [1] 34
    ##
    ## $expected.runs
    ## [1] 40.975
    ##
    ## $n1
    ## [1] 39
    ##
    ## $n2
    ## [1] 41
    ##
    ## $k
    ## [1] 0

  runs(rstandard(arima111.fit))

    ## $pvalue
    ## [1] 0.000994
    ##
    ## $observed.runs
    ## [1] 26
    ##
    ## $expected.runs
    ## [1] 40.975
    ##
    ## $n1
    ## [1] 41
    ##
    ## $n2
    ## [1] 39
    ##
    ## $k
    ## [1] 0
{% endhighlight %}

### Residual PACF

{% highlight r linenos %}
  # Residual PACF
  pacf(rstandard(ari11.fit), main="Sample PACF")  
{% endhighlight %}

![]({{ page.img }}/unnamed-chunk-20-1.png)

------------------------------------------------------------------------

Forecasting
===========

To test our model, we withold observations from February 2015 to February 2016. The table below summarizes the actual values in March 2015 and January 2016 and the predictions estimated by the four ARIMA and ARI models.

<table class="table">
  <thead>
    <tr>
      <th scope="col">Time</th>
      <th scope="col" align="center">Observation</th>
      <th scope="col" align="center">ARIMA (1,1,1) Seasonal</th>
      <th scope="col" align="center">ARI (1,1) Seasonal</th>
      <th scope="col" align="center">ARIMA (1,1,1) Non-Seasonal</th>
      <th scope="col" align="center">ARI (1,1) Non-Seasonal</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th scope="row">March 2015</th>
      <td align="center">119.5</td>
      <td align="center">122.3</td>
      <td align="center">119.43</td>
      <td align="center">125.42</td>
      <td align="center">131.9</td>
    </tr>
    <tr>
    <th scope="row">January 2016</th>
    <td align="center">122.21</td>
    <td align="center">135.11</td>
    <td align="center">133.87</td>
    <td align="center">122.78</td>
    <td align="center">131.83</td>
    </tr>
  </tbody>
</table>
