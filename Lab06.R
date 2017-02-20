### Earth and Environmental Data Analysis
#============
##Lab 06 - Covariation, correlation and significance

#1a. Write a function that uses matrix algebra to calculate correlation coefficients. If there's a for loop in it, you're doing something wrong

#1b. Use the eruption waittimes dataset to show that your correlation function works the same as cor()

#1c. Using the matrix algebra you used and our discussion from class, explain how to interpret the correlation coefficient squared.(R^2)

#OK. Let's explore the sensitivity of correlation coefficients to various conditions.
#2a. We're going to compare the body temperatures of two different beavers, measured at 10-minute increments, over the course of many hours. These datasets are beaver1, and beaver2.
?beavers

#First off, you realize that the measurements in the two datasets don't start and end at the same time, and to do a meaningful correlation, you need to subset the datasets such that the observations match. Subset the two datasets to get the longest possible datasets with matching times.

#2b. Use your function to calculate their correlation coefficient?

#2c. Oh no! Your grad students just told you that there were some errors during data collection. The temperatures for the first beaver were all 3.4 degrees to high, and somehow, all of the temperatures for the second beaver were divided by 1.3. Correct the datasets based on these revelations and calculate the new correlation coefficient. How did it change?

#2d. What does this result tell you about correlation coefficients?

#2e. Lets test the impact of the number of observations.
#Pick two observations out of your 100-observation random datasets at random (you can do this with R, or you can just make up a couple numbers). What's the correlation coefficient of those two observations?


#2f. Let's do this systematically. Write a for loop that goes through your observations and calculates r-values for the first 2, then 3, then 4.... all the way to 100.

#2f - OPTIONAL (BUT AWESOMER) CHALLENGE VERSION. Loop through the same way, and add an observation each time, but go in random order (e.g., first loop you use observations 7 and 54, then add 36, then 15, and so on, randomly)


#2g. Now plot the relationship between number of observations and the absolute value of the correlation coefficients with ggplot. 

#2h. What's the general relationship between r-value an number of observations?



#3. Significance testing correlation coefficients. 
#In number 2 we observed that the correlation coefficients are sensitive to the number of observations, and they often have an undesirable feature, in that the r-value decreases with increasing samples. We'd like to build this into a significance test. We use a Student's T distribution instead of a normal distribution for significance testing correlation coefficients.

#3a. create a sequence from -5 to 5 by 0.1 and use it with dt() to calculate a student's t-distribution with 10 degrees of freedom

#now create a second with 1000 df.

#third, calculate a regular normal distribution over the same interval. 

#plot all three distributions using ggplot (I'd use geom_area with some sweet colors and transparency (alpha))

#3b. How does the t-distribution change with increasing sample sizes (degrees of freedom?). What happens to tails of the distribution? What does this mean about the likilihood of outliers with increasing observations?


#3c. The r-values of most datasets are well represented by T distributions. In fact we can convert r-values to "t-values", allowing us to think about where they fall on the distribution, quite easily using this equation: T = r * sqrt(  (number of Observations-2)/ (1 - r^2))
#NOTE: with correlations, the degrees of freedom are the number of observations - 2. 


#Add this equation to your correlation function, so that when you calculate the correlation it also returns the t-value version of the correlation coefficient.
#refer to class notes about functions that return multiple values

#3d. write a for loop that uses your function to calculate r and t values for 1000 random datasets
#Create a histogram of the t-value results (vs density, not count), and compare them to a comparable t-distribution

#3e. Does the t-distribution seem appropriate to characterize the results of random correlations you simulated?


#3f. Use your new function to calculate the correlation and t-value (or T-statisctic) of the beaver data from above. 
#Create a new plot that shows an appropriate t-distribution (given the df you have) as an area plot, and your t-value as a vertical line on that plot. To do this, check out geom_vline.

#3g. Use pt() to determine the cumulative probality that corresponds to your t-value



#is your t-value positive or negative?
#switch the sign and reevaluate. Why are they different?


#3h. add another feature to your function, that uses pt() to calculate the probability a correlation (either positive or negative) at least as high as the one you observed could be found in random data.


#3i. Compare this result to the "p-value" calculated by cor.test() for the same data.

#4. Autocorrelation.
#Autocorrelation measures the extent to which values in a dataset are correlated to adjacent to values (1 step forward or back in time, for example)

#4a. Write a new function called AR1 (stands for Auto-regressive, 1 timestep lag). The input for this function is just one vector, since were correlating something against itself. A simple way (there are better, more complicated ways) of estimating AR1 is to simply offset the values in a vector by one step and correlating just as we correlate everything else. This means that the vectors you correlate should be 1 observation shorter than the original vector.

#Load in the dataset "AutoCorrelatedData.csv" using read.csv(), and use ggplot2 to create a scatter plot of these datasets. 

#4b. Use your AR1 function to estimate AR1 for the first vector in the dataset. Compare that value to the output of acf() at lag-step 1. They should be similar (but maybe not identical, because acf() works slightly differently)

#4c. There are numerous effects that can cause autocorrelation in a dataset; a common one is a trend. Let's create a new vector by adding our data to a trend. We can generate a trendline to add to our data very easily using seq
#e.g., Xnew=X+seq(-1,1,length.out=100)

#4d. Calculate the AR1 on the new, trended dataset. Tinker with steeper and shallower slopes. How does this effect the autocorrelation?

#4e. Now, going back to the original data, use your funcions from above to calculate the correlation coefficient and p-value between the vectors in AutoCorrelatedData. Does this appear to have a significant correlation?


#4f. Let's generate 1000 random, autocorrelated datasets, and calculate their correlation coefficients. The R function arima.sim() does this beautifully. The following command generates a random dataset with 200 values and an AR1 coefficient of 0.7
arima.sim(list(order = c(1,0,0), ar = 0.7), n = 200)
#Generate 8 random series with AR1 values of -.5, -.1, 0.01, .1, .3, .5, .7, .9, and plot them, to get a sense of what autocorrelated data look like. 

#What does a negative AR1 coefficient mean?


#OK, back to our project, 1000, random r-values for autocorrelated data. Write a for loop, that will calculate 1000 correlations between two random datasets that both have AR1 values of 0.7, and that have 300 data points. Also calculate the T-statistic for each of those values as above. Make sure to store those correlation coefficients and T-stats each time through the loop so we can plot them later.

#4g. Use ggplot to plot a histogram (using the ..density.. option) of your T-values, and then overlay a semi-transparent Student's T-distribution (using dt() and  sequence from -6 to 6) with appropriate degrees of freedom



#Does the t distribution with the appropriate degrees of freedom seem like an appropriate null hypothesis for our autocorrelated correlations? If not, would your autocorrelated p-values be too high or too low?




#Autocorrelation makes high t-values more likely. One way to think about is that because in autocorrelated series, each value has some predictive capacity for the subsequent value, their not really independent. Consequently, our degrees of freedom is not really the number of observations - 2, since the observations aren't independent (or free) from each other. So one solution to getting more realistic p-values on correlations between autocorrelated series, is to adjust the degrees of freedom in our p-value calculation to reflect that our "effective" number of observations is less than the number of observations

#Here's an equation to make that estimation (from Bretherton et al., 1999)
#EffN = N * (1 - AR1x*AR1y)/(1+AR1x*AR1y)


#4g. Let's use this equation to account for autocorrelation in your p-value calculations. Go back to your correlation function, and have it determine the effective degrees of freedom for use in the pt() calculation. 
#Important note!!! If AR1x*AR1y <0 this equation will increase your effective sample size. This is bad. If this tries to happen, use the original df calculation instead of the effective df. 
#Remember! EffDF = EffN-2
#After you do all of this, is the correlation between the two datasets in AutoCorrelatedData.csv still signficant at the 0.05 level?


#5. Monte Carlo significance testing
#5a.Go back to where you made a plot comparing you random correlations and the t-distribution. Repeat that plot hear, but this time adjusting the degrees of freedom on the t-distibution following the Bretherton et al. 1999 scheme.




#Does the distribution match better now? Does it match well in general?

#An alternative method of significance testing, of determining the likelihood of finding a result with random data, is to simply do that test many, many times with random data. Using randomly generated data to test your data is part of a very broad suite of methods called Monte Carlo techniques. In problems where your data or analysis doesn't fit cleanly into the assumptions required by tradiional statistics, monte-carlo methods often give more interpretable results. 

#5b. In this case, our monte carlo approach is pretty straightforward. We want to know how extreme (high positive or negative) a correlation coefficient must be to be more extreme than 95% of random data. In this case lets look at datasets with an AR1 coefficent of 0.5, and 200 values.

#First, generate 10,000 random correlations as above, but with AR1 = 0.5, and length=50


#5c. Now, we need to find the 2.5% most extreme values on both tails (positive and negative), and figure out which correlation coefficients correspond to these. There are many ways to do this. Personally, I'd use either sort() or quantile()

#5d. Lastly, use ggplot to plot the histogram of your random values, and put vertical lines that mark where significance (as defined above)








