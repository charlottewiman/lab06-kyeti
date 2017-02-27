### Earth and Environmental Data Analysis
#============
##Lab 06 - Covariation, correlation and significance

#1a. Write a function that uses matrix algebra to calculate correlation coefficients. If there's a for loop in it, you're doing something wrong

correlation = function(x, y){
  x.anom = x - mean(x)
  y.anom = y - mean(y)
  covXY = t(x.anom)%*%y.anom / (length(y.anom) - 1)
  corXY = covXY/(sqrt((t(x.anom)%*%x.anom)/(length(x.anom) - 1)*(t(y.anom)%*%y.anom)/(length(y.anom) - 1)))
  print(corXY)
}

#1b. Use the eruption waittimes dataset to show that your correlation function works the same as cor()

correlation(faithful$eruptions, faithful$waiting)

cor(faithful$eruptions, faithful$waiting)


#1c. Using the matrix algebra you used and our discussion from class, explain how to interpret the correlation coefficient squared.(R^2)

library(expm)

  # The amount of covariance within the total variation of both sets of data. It is the fraction of total variance. 

(correlation(faithful$eruptions, faithful$waiting))^2

  # so there is 81.1% of covariance within the total variation of the "faithful eruptions" data and "faithful waiting" data. 


#OK. Let's explore the sensitivity of correlation coefficients to various conditions.
#2a. We're going to compare the body temperatures of two different beavers, measured at 10-minute increments, over the course of many hours. These datasets are beaver1, and beaver2.
?beavers


#First off, you realize that the measurements in the two datasets don't start and end at the same time, and to do a meaningful correlation, you need to subset the datasets such that the observations match. Subset the two datasets to get the longest possible datasets with matching times.


beaver1new = beaver1[6:104,]
beaver2new = beaver2[-78,]

correlation(beaver1new$time, beaver2new$time)


#2b. Use your function to calculate their correlation coefficient?

correlation(beaver1new$temp, beaver2new$temp)

#2c. Oh no! Your grad students just told you that there were some errors during data collection. The temperatures for the first beaver were all 3.4 degrees to high, and somehow, all of the temperatures for the second beaver were divided by 1.3. Correct the datasets based on these revelations and calculate the new correlation coefficient. How did it change?

correlation(beaver1new$temp-3.4, beaver2new$temp*1.3)

#2d. What does this result tell you about correlation coefficients?

  # This tells us that the relationship doesn't change if you change each of your data sets by a specific amount. They are a ratio. 

#2e. Lets test the impact of the number of observations.
#Pick two observations out of your 100-observation random datasets at random (you can do this with R, or you can just make up a couple numbers). What's the correlation coefficient of those two observations?


correlation(sample(beaver1new$temp, 2, replace = F), sample(beaver2new$temp, 2, replace = F))
  
#2f. Let's do this systematically. Write a for loop that goes through your observations and calculates r-values for the first 2, then 3, then 4.... all the way to 100.

r_values = c()
for(i in 2:99){
  r_values[i-1] = correlation(beaver1new$temp[1:i], beaver2new$temp[1:i])
}
r_values

#2f - OPTIONAL (BUT AWESOMER) CHALLENGE VERSION. Loop through the same way, and add an observation each time, but go in random order (e.g., first loop you use observations 7 and 54, then add 36, then 15, and so on, randomly)


#2g. Now plot the relationship between number of observations and the absolute value of the correlation coefficients with ggplot. 

library(ggplot2)
ggplot()+
  geom_point(aes(x = c(2:99), y = abs(r_values)))+
  labs(x = "Number of observations", y = "Absolute value of R-values")
  

#2h. What's the general relationship between r-value an number of observations?

  # The general relationship between r-values and the number of observations decreases rapidly from 1, to almost zero, and then approaches the calculated correlation coefficient for the whole data set. 

 
#3. Significance testing correlation coefficients. 
#In number 2 we observed that the correlation coefficients are sensitive to the number of observations, and they often have an undesirable feature, in that the r-value decreases with increasing samples. We'd like to build this into a significance test. We use a Student's T distribution instead of a normal distribution for significance testing correlation coefficients.

#3a. create a sequence from -5 to 5 by 0.1 and use it with dt() to calculate a student's t-distribution with 10 degrees of freedom

td = dt(seq(-5, 5, by = .1), df = 10)

#now create a second with 1000 df.

td_2 = dt(seq(-5, 5, by = .1), df = 1000)

#third, calculate a regular normal distribution over the same interval. 

nd = dnorm(seq(-5, 5, by = .1))

#plot all three distributions using ggplot (I'd use geom_area with some sweet colors and transparency (alpha))

ggplot()+
  geom_area(aes(x = seq(-5, 5, by = .1),y = nd, fill = "Normal Distribution"), alpha = .5)+
  geom_area(aes(x = seq(-5, 5, by = .1), y = td_2, fill = "t-distribution, df = 1000"), alpha = .5)+
  geom_area(aes(x = seq(-5, 5, by = .1), y = td, fill = "t-distribution, df = 10"), alpha = .5)
  

#3b. How does the t-distribution change with increasing sample sizes (degrees of freedom?). What happens to tails of the distribution? What does this mean about the likilihood of outliers with increasing observations?

  # With increasing sample size, the t-distribution tails shrink, and it approaches the normal distribution. This means that  the likilihood of outliers will increase, but their effect on the mean will decrease with increasing observations. 

#3c. The r-values of most datasets are well represented by T distributions. In fact we can convert r-values to "t-values", allowing us to think about where they fall on the distribution, quite easily using this equation: T = r * sqrt(  (number of Observations-2)/ (1 - r^2))
#NOTE: with correlations, the degrees of freedom are the number of observations - 2. 


#Add this equation to your correlation function, so that when you calculate the correlation it also returns the t-value version of the correlation coefficient.
#refer to class notes about functions that return multiple values



correlation_t = function(x, y){
  out = list()
  x.anom = x - mean(x)
  y.anom = y - mean(y)
  covXY = t(x.anom)%*%y.anom / (length(y.anom) - 1)
  out$corXY = covXY/(sqrt((t(x.anom)%*%x.anom)/(length(x.anom) - 1)*(t(y.anom)%*%y.anom)/(length(y.anom) - 1)))
  out$t_value = out$corXY * sqrt(  (length(x.anom)-2)/ (1 - out$corXY^2))
  
  return(out)
}

out = correlation_t(faithful$eruptions, faithful$waiting)
print(out)

cor.test(faithful$eruptions, faithful$waiting)

#3d. write a for loop that uses your function to calculate r and t values for 1000 random datasets

nits = 1000 # number of iterations of loop

#matrix full of NAs to fill
randomDataset = matrix(NA, nits, 2)

for (i in 1:nits){
  random1 = rnorm(60)
  random2 = rnorm(60)
  cor_t_values = correlation_t(random1, random2)
  randomDataset[i,1] = cor_t_values$corXY
  randomDataset[i,2] = cor_t_values$t_value
}

#Create a histogram of the t-value results (vs density, not count), and compare them to a comparable t-distribution

ggplot()+
  geom_histogram(aes(x = randomDataset[,2], y = ..density.., fill = "Random t-values"))+
  geom_area(aes(x = seq(-5, 5, by = .1), y = td_2, fill = "t-distribution, df = 1000"), alpha = .5)
  


#3e. Does the t-distribution seem appropriate to characterize the results of random correlations you simulated?
  #yes

#3f. Use your new function to calculate the correlation and t-value (or T-statisctic) of the beaver data from above. 
#Create a new plot that shows an appropriate t-distribution (given the df you have) as an area plot, and your t-value as a vertical line on that plot. To do this, check out geom_vline.

beaver_cor_t = correlation_t(beaver1new$temp, beaver2new$temp)

#t-value
beaver_cor_t$t_value

# t-distribution
td = dt(seq(-5,5,by=.1),df=length(beaver1new$temp)-2)


#plot 
ggplot()+
  geom_area(aes(x = seq(-5, 5, by = .1), y = td, fill = "t-distribution"), alpha = .5)+
  geom_vline(aes(xintercept = beaver_cor_t$t_value, color = "t-value"))

#3g. Use pt() to determine the cumulative probality that corresponds to your t-value

tcumuProb = pt(beaver_cor_t$t_value, df = length(beaver1new$temp) -2)
tcumuProb

#is your t-value positive or negative?
  #it is positive

#switch the sign and reevaluate. Why are they different?
pt(-(beaver_cor_t$t_value), df = length(beaver1new$temp) -2)*2

  # the negative t-value gives 1 minus the positive t-value cumulative probability

#3h. add another feature to your function, that uses pt() to calculate the probability a correlation (either positive or negative) at least as high as the one you observed could be found in random data.

correlation_t_p= function(x, y){
  out = list()
  x.anom = x - mean(x)
  y.anom = y - mean(y)
  covXY = t(x.anom)%*%y.anom / (length(y.anom) - 1)
  out$corXY = covXY/(sqrt((t(x.anom)%*%x.anom)/(length(x.anom) - 1)*(t(y.anom)%*%y.anom)/(length(y.anom) - 1)))
  out$t_value = out$corXY * sqrt(  (length(x.anom)-2)/ (1 - out$corXY^2))
  out$prob_corr = pt(-abs(out$t_value), df = length(x.anom)-2)*2
  return(out)
}


#3i. Compare this result to the "p-value" calculated by cor.test() for the same data.

correlation_t_p(beaver1new$temp, beaver2new$temp)
cor.test(beaver1new$temp, beaver2new$temp)

  # they are the same



#4. Autocorrelation.
#Autocorrelation measures the extent to which values in a dataset are correlated to adjacent to values (1 step forward or back in time, for example)

#4a. Write a new function called AR1 (stands for Auto-regressive, 1 timestep lag). The input for this function is just one vector, since were correlating something against itself. A simple way (there are better, more complicated ways) of estimating AR1 is to simply offset the values in a vector by one step and correlating just as we correlate everything else. This means that the vectors you correlate should be 1 observation shorter than the original vector.

#input is one vector (x)
AR1 = function(x){
  newVector = head(x, -1)
  AR = correlation_t_p(x[-1], newVector)
  
  return(AR$corXY)
}



#Load in the dataset "AutoCorrelatedData.csv" using read.csv(), and use ggplot2 to create a scatter plot of these datasets. 
getwd()

AutoCorrData = read.csv("AutoCorrelatedData.csv")

#4b. Use your AR1 function to estimate AR1 for the first vector in the dataset. Compare that value to the output of acf() at lag-step 1. They should be similar (but maybe not identical, because acf() works slightly differently)

AR1(AutoCorrData[,1])

acf(AutoCorrData[,1])

#4c. There are numerous effects that can cause autocorrelation in a dataset; a common one is a trend. Let's create a new vector by adding our data to a trend. We can generate a trendline to add to our data very easily using seq
#e.g., Xnew=X+seq(-1,1,length.out=100)

Xnew=AutoCorrData[,1]+seq(-1, 1,length.out=100)

plot(Xnew, type = "l")


#4d. Calculate the AR1 on the new, trended dataset. Tinker with steeper and shallower slopes. How does this effect the autocorrelation?


Xnew2=AutoCorrData[,1]+seq(-5,5,length.out=100)
AR1(Xnew2)

plot(Xnew2, type = 'l')

Xnew3=AutoCorrData[,1]+seq(-20,20,length.out=100)
AR1(Xnew3)

plot(Xnew3, type = 'l')

# steeper slope means a stronger correlation, or larger R value. 


#4e. Now, going back to the original data, use your funcions from above to calculate the correlation coefficient and p-value between the vectors in AutoCorrelatedData. Does this appear to have a significant correlation?


correlation_t_p(AutoCorrData[,1], AutoCorrData[,2])
  # Yes, the p-value is less than 0.05, so it is a significant correlation. 

#4f. Let's generate 1000 random, autocorrelated datasets, and calculate their correlation coefficients. The R function arima.sim() does this beautifully. The following command generates a random dataset with 200 values and an AR1 coefficient of 0.7
arima.sim(list(order = c(1,0,0), ar = 0.7), n = 200)
#Generate 8 random series with AR1 values of -.5, -.1, 0.01, .1, .3, .5, .7, .9, and plot them, to get a sense of what autocorrelated data look like. 

RandAutoCorr1 = arima.sim(list(order = c(1,0,0), ar = -0.5), n = 1000)

RandAutoCorr2 = arima.sim(list(order = c(1,0,0), ar = -0.1), n = 1000)

RandAutoCorr3 = arima.sim(list(order = c(1,0,0), ar = 0.01), n = 1000)

RandAutoCorr4 = arima.sim(list(order = c(1,0,0), ar = 0.1), n = 1000)

RandAutoCorr5 = arima.sim(list(order = c(1,0,0), ar = 0.3), n = 1000)

RandAutoCorr6 = arima.sim(list(order = c(1,0,0), ar = 0.5), n = 1000)

RandAutoCorr7 = arima.sim(list(order = c(1,0,0), ar = 0.7), n = 1000)

RandAutoCorr8 = arima.sim(list(order = c(1,0,0), ar = 0.9), n = 1000)


RandomData = data.frame(RandAutoCorr1, RandAutoCorr2, RandAutoCorr3, RandAutoCorr4, RandAutoCorr5, RandAutoCorr6, RandAutoCorr7, RandAutoCorr8)

#x-axis for ggplot:
x = c(1:1000)

ggplot(RandomData)+
  geom_line(aes(x, RandAutoCorr1, color = "ar -0.5"))+
  geom_line(aes(x, RandAutoCorr2, color = "ar -0.1"))+
  geom_line(aes(x, RandAutoCorr3, color = "ar 0.01"))+
  geom_line(aes(x, RandAutoCorr4, color = "ar 0.1"))+
  geom_line(aes(x, RandAutoCorr5, color = "ar 0.3"))+
  geom_line(aes(x, RandAutoCorr6, color = "ar 0.5"))+
  geom_line(aes(x, RandAutoCorr7, color = "ar 0.7"))+
  geom_line(aes(x, RandAutoCorr8, color = "ar 0.9"))+
  labs(x = "number", y = "autocorrelation coefficent")

#What does a negative AR1 coefficient mean?

ggplot(RandomData)+
  geom_line(aes(x, RandAutoCorr1, color = "ar -0.5"))+
  geom_line(aes(x, RandAutoCorr8, color = "ar 0.9"))

  # A negative AR1 coefficient means that the predicted subsequent timestep will be the opposite (negative) of the previous timestep The negative AR1 values have a higher frequency and quickly go back and forth around zero, while the positive AR1 values have longer paths that fluctuate around zero with more extreme peaks, to show long-term changes.  

#OK, back to our project, 1000, random r-values for autocorrelated data. Write a for loop, that will calculate 1000 correlations between two random datasets that both have AR1 values of 0.7, and that have 300 data points. Also calculate the T-statistic for each of those values as above. Make sure to store those correlation coefficients and T-stats each time through the loop so we can plot them later.


random_R_T = matrix(NA, 1000, 2)
for (i in 1:1000){
  randomAR1 = arima.sim(list(order = c(1,0,0), ar = 0.7), n = 300)
  randomAR2 = arima.sim(list(order = c(1,0,0), ar = 0.7), n = 300)
  values = correlation_t_p(randomAR1, randomAR2)
  random_R_T[i,1]= values$corXY
  random_R_T[i,2]= values$t_value
}

head(random_R_T)


#4g. Use ggplot to plot a histogram (using the ..density.. option) of your T-values, and then overlay a semi-transparent Student's T-distribution (using dt() and  sequence from -6 to 6) with appropriate degrees of freedom

xx1 = seq(-6, 6, length.out = 298)
stud_td1 = dt(xx1, df = 298)


ggplot()+
  geom_histogram(aes(x = random_R_T[,2], y = ..density.., fill = "Random t-values"))+
  geom_area(aes(x = xx1, y = stud_td1, fill = "Student's t-distribution"), alpha = .5)+
  labs(x = "T values")


#Does the t distribution with the appropriate degrees of freedom seem like an appropriate null hypothesis for our autocorrelated correlations? If not, would your autocorrelated p-values be too high or too low?

  #No, the t-distribution with the appropriate degrees of freedom is underestimating the uncertainty. The autocorrelated p-values would be too low. 


#Autocorrelation makes high t-values more likely. One way to think about is that because in autocorrelated series, each value has some predictive capacity for the subsequent value, their not really independent. Consequently, our degrees of freedom is not really the number of observations - 2, since the observations aren't independent (or free) from each other. So one solution to getting more realistic p-values on correlations between autocorrelated series, is to adjust the degrees of freedom in our p-value calculation to reflect that our "effective" number of observations is less than the number of observations

#Here's an equation to make that estimation (from Bretherton et al., 1999)
#EffN = N * (1 - AR1x*AR1y)/(1+AR1x*AR1y)


#4h. Let's use this equation to account for autocorrelation in your p-value calculations. Go back to your correlation function, and have it determine the effective degrees of freedom for use in the pt() calculation. 
#Important note!!! If AR1x*AR1y <0 this equation will increase your effective sample size. This is bad. If this tries to happen, use the original df calculation instead of the effective df. 
#Remember! EffDF = EffN-2

#After you do all of this, is the correlation between the two datasets in AutoCorrelatedData.csv still signficant at the 0.05 level?
AR1 = function(x){
  newVector = head(x, -1)
  cor(x[-1], newVector)
}

correlation_t_p_effn= function(x, y){
  out = list()
  x.anom = x - mean(x)
  y.anom = y - mean(y)
  EffN = length(x) * (1 - AR1(x)*AR1(y))/(1+AR1(x)*AR1(y))
  EffDF = EffN - 2
  covXY = t(x.anom)%*%y.anom / (length(y.anom) - 1)
  out$corXY = covXY/(sqrt((t(x.anom)%*%x.anom)/(length(x.anom) - 1)*(t(y.anom)%*%y.anom)/(length(y.anom) - 1)))
  
  if(AR1(x) > 0 & AR1(y) > 0){
    df = EffDF
  }else{
    df = (length(x)-2)
  }
  
  out$t_value = out$corXY * sqrt((df)/ (1 - (out$corXY)^2))
  out$prob_corr = pt(-abs(out$t_value), df)*2
  
  return(out)
} 


#test
correlation_t_p(AutoCorrData[,1],AutoCorrData[,2])
correlation_t_p_effn(AutoCorrData[,1],AutoCorrData[,2])

cor.test(AutoCorrData[,1],AutoCorrData[,2])

#They are not significant now because the p-value is now 0.095. 


#5. Monte Carlo significance testing
#5a.Go back to where you made a plot comparing you random correlations and the t-distribution. Repeat that plot here, but this time adjusting the degrees of freedom on the t-distibution following the Bretherton et al. 1999 scheme.

#


random_R_T = matrix(NA, 1000, 2)
for (i in 1:1000){
  randomAR1 = arima.sim(list(order = c(1,0,0), ar = 0.7), n = 300)
  randomAR2 = arima.sim(list(order = c(1,0,0), ar = 0.7), n = 300)
  values = correlation_t_p(randomAR1, randomAR2)
  random_R_T[i,1]= values$corXY
  random_R_T[i,2]= values$t_value
}

head(random_R_T)


xx = seq(-6, 6, length.out = (300 * (1 - 0.7*0.7)/(1 + 0.7*0.7))-2)
stud_td = dt(xx, df = (300 * (1 - 0.7*0.7)/(1 + 0.7*0.7))-2)

ggplot()+
  geom_histogram(aes(x = random_R_T[,2], y = ..density.., fill = "Random t-values"))+
  geom_area(aes(x = xx, y = stud_td, fill = "t-distribution"), alpha = .5)+
  labs(x = "T values")



#Does the distribution match better now? Does it match well in general?

  #The distribution matches a tiny bit better now, but still does not match well in general. 


#An alternative method of significance testing, of determining the likelihood of finding a result with random data, is to simply do that test many, many times with random data. Using randomly generated data to test your data is part of a very broad suite of methods called Monte Carlo techniques. In problems where your data or analysis doesn't fit cleanly into the assumptions required by tradiional statistics, monte-carlo methods often give more interpretable results. 

#5b. In this case, our monte carlo approach is pretty straightforward. We want to know how extreme (high positive or negative) a correlation coefficient must be to be more extreme than 95% of random data. In this case lets look at datasets with an AR1 coefficent of 0.5, and 200 values.

#First, generate 10,000 random correlations as above, but with AR1 = 0.5, and length=50

MonteCarlo_R = matrix(NA, 10000, 1)
for (i in 1:10000){
  randomAR3 = arima.sim(list(order = c(1,0,0), ar = 0.5), n = 50)
  randomAR4 = arima.sim(list(order = c(1,0,0), ar = 0.5), n = 50)
  values = correlation_t_p(randomAR3, randomAR4)
  MonteCarlo_R[i,1]= values$corXY
  
}

head(MonteCarlo_R)

#5c. Now, we need to find the 2.5% most extreme values on both tails (positive and negative), and figure out which correlation coefficients correspond to these. There are many ways to do this. Personally, I'd use either sort() or quantile()

RExtremes = quantile(MonteCarlo_R,c(0.025,0.975))


#5d. Lastly, use ggplot to plot the histogram of your random values, and put vertical lines that mark where significance (as defined above)

ggplot()+
  geom_histogram(aes(x = MonteCarlo_R, y = ..density..))+
  geom_vline(xintercept = RExtremes, color = "red")+
  labs(x = "Random correlation coefficients", title = "Monte Carlo plot")


