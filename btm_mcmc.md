---
title: "Bayesian Bradley Terry With MCMC"
author: "Andrew Mack | @gingfacekillah"
date: "28/12/2020"
output: html_document
---
*The author would like to thank [Rasmus Bååth](https://twitter.com/rabaath) for his assistance with the MCMC portion of this R script.*

In my last [RPubs Publication](https://rpubs.com/gingfacekillah/btm), we looked at a relatively simple Bradley Terry model in R that estimated 
NHL team strength ratings using maximum likelihood estimation **[MLE]**. In this RMarkdown document, I'd like to show you how to do the same thing 
but with a Bayesian twist - approximating team strength using markov chain monte carlo methods **[MCMC]**.

*Why might we want to do this?*

**Main reason:** quantifying uncertainty.

If take a you look at the estimates of team strength produced by the MLE version of the model, you'll notice that they are single point estimates - 
meaning we don't have any indication of the uncertainty (or distribution) surrounding the individual team ratings. It's true that we could probably use 
a bootstrap method to estimate the parameter uncertainty, but ultimately we'd probably prefer a Bayesian 
[credible interval](https://en.wikipedia.org/wiki/Credible_interval) rather than a Frequentist [confidence interval](https://en.wikipedia.org/wiki/Confidence_interval) here. 
Let's pop the hood and take a look at how it works:

### 1. Load The Raw Data
To make a fair comparison, we'll load the same hockey data from the 2018-2019 NHL season that we used before:
```{r warning=FALSE, message=FALSE}
library(knitr)
data <- read.csv('hockeydata.csv')
kable(head(data), caption = "2018-2019 NHL Game Result Data")
```

### 2. Clean & Wrangle The Data
Now, just like last time, we'll make some new variables and clean the variable names up a bit. First we'll create a margin of victory **[h_mov]** 
variable from the home team's perspective, and then create a binary result variable **[h_win]** where the home team records a win [1] or a loss [0] 
based on the positive or negative value of the margin of victory. Then we'll rename the variables using lowercase letters and select the data that we 
want for our new cleaned up dataframe.

```{r warning=FALSE, message=FALSE}
library(tidyverse)
bt.games <- data %>%
mutate(h_mov = HG - AG, h_win = ifelse(h_mov > 0,1,0)) %>%
  select(date = Date,
          home = Home,
          away = Visitor,
          h_score = HG,
          a_score = AG,
          h_mov,
          h_win)
  
kable(head(bt.games))
```

### 3. Create A Model Formula Function

Next we need to create a model formula. For a Bradley Terry model we'll want to use a logistic function that combines the home rating, away rating, and 
home ice advantage. There are other ways to build this, but this method closely follows the Excel version of this model from my book Statistical Sports 
Models in Excel Volume 1, so I think it will be approachable for those trying to make the leap from Excel to R.
```{r warning=FALSE, message=FALSE}
bt.home_forecast <- function(home_rating,
                          visitor_rating,
                          homeice_adv){
  1/(1+exp(-(homeice_adv + home_rating - visitor_rating))) #Logistic Function
}
```

### 4. Assign Dummy Team Ratings
Next we create a vector of dummy team ratings and start all teams off with a rating of 1.000. We'll later optimize the ratings using MCMC, but this gives 
us a starting point to work from.

```{r warning=FALSE, message=FALSE}
teams <- as.vector(sort(unique(bt.games$home)))
bt.ratings <- rep(1.000,32) #Starting Rating is 1.000, 31 teams + hia
names(bt.ratings) <- c(teams, 'hia') #'hia' is our home ice advantage
```

### 5. Create a Likelihood Function
Now we need to create a log likelihood function which we can use to optimize the team ratings in a way that makes the observed game results most probable. 
This function compares the estimated win probability to the binary result variable **[h_win]** and returns the estimated probability if [h_win] is 1, and 1 
minus the estimated probability if [h_win] is 0. The function then takes the natural logarithm of the resulting probabilities and sums them together to return 
a log likelihood as **[ll.final]**.

```{r warning=FALSE, message=FALSE}

# Specify the Log Likelihood function
bll <- function(bt.ratings) {
  bt.games %>%
    mutate(forecast = bt.home_forecast(bt.ratings[home],
                                    bt.ratings[away],
                                    bt.ratings[32])) %>% #32 is the home ice advantage
    mutate(result.f = ifelse(h_win==1,forecast, (1-forecast))) %>%  # The result function
    summarise(ll.final = sum(log(result.f))) %>% #sum of the log likelihood%>%
    pull(ll.final)
}
```

### 6. Setting Up The MCMC
[Markov Chain Monte Carlo](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) sampling will be used to replace our use of the **Optim** package in R from 
last time. Let's load the **BayesianTools** package and establish our settings. We'll use our **[bll]** likelihood function, our team names, and a flat prior 
distribution bounded between 0 and 2.5. This will make the rating outputs comparable to our results with MLE.
```{r warning=FALSE, message=FALSE}
library(BayesianTools) # Our R package for implementing MCMC
set.seed(123)
bayesianSetup = createBayesianSetup(likelihood = bll, 
                                    names = names(bt.ratings),
                                    lower = rep(0,32),
                                    upper =  rep(2.5,32)) # A flat prior bounded between 0 and 2.5
```

### 7. Fire Up The MCMC Simulation
Time to release the Kraken! We'll set the number of iterations to 100,000 and the number of chains to 1. These values can be experimented with - and adding a few 
more chains can be helpful, though it does make the process more computationally intensive. Finally, we need to select the specific MCMC algorithm we want to use. 
In this example we're using a differential evolution MCMC variant **["DEzs"]**, but more vanilla algorithms like the Metropolis-Hastings **["metropolis"]** are 
available in this R packakge as well.
```{r, warning=FALSE, message=FALSE, eval=T, echo=T, results='hide'}
settings = list(iterations = 100000, nrChains = 1, burnin= 5000) # Number of samples, chains, burn-in
bt.mcmc <- runMCMC(bayesianSetup = bayesianSetup,
                   sampler = "DEzs", # Differential Evolution MCMC, also try 'metropolis'
                   settings = settings)
# Warning: computationally intensive depending on number of chains, samples & algorithm selected!
#Also note, "DEzs" algorithm runs 3 chains for every 1 chain specified. nrChains = 2 runs 6 chains, etc.
```


### 8. Extract Maximum A Posteriori Rating Estimates
The MCMC sampling process will now begin to run. This will take a few minutes - grab a coffee! When it's complete, we can extract the
[Maximum A Posteriori](https://en.wikipedia.org/wiki/Maximum_a_posteriori_estimation)  **[MAP]** team strength estimates. These are comparable 
to the MLE team strength estimates from our first Bradley Terry Model, as they represent the mode parameter value from each team's posterior distribution.
```{r warning=FALSE, message=FALSE}
#Extract team rating parameters MAP
ratings.mcmc <- MAP(bt.mcmc)
ratings.mcmc.final <- ratings.mcmc$parametersMAP
ratings.mcmc.final
```
### 9. Plot The Posterior Distribution & Credible Interval
So far, the results look more or less the same as with our MLE model - so you might be wondering when our Bayesian approach gets around to helping us quantify 
the uncertainty in the team ratings. Let's get on that. We can take a look at a given team's posterior distribution as well as the Bayesian credible interval 
surrounding their strength rating. This will show us how certain or uncertain we might want to be about each team's true strength rating.
```{r warning=FALSE, message=FALSE}
# Extract the posterior distributions and plot them
post.dist <- data.frame(bt.mcmc$Z)
names(post.dist) <- c(teams, 'hia')

hist(post.dist$`Anaheim Ducks`) # Posterior distribution histogram for Anaheim
# 95% Credible Interval
getCredibleIntervals(as.matrix(post.dist$`Anaheim Ducks`), quantiles = c(0.025, 0.975))
# You can also run summary(bt.mcmc) for a full list of MAP values, median values and credible intervals
```
As you can see, we have a range of values for Anaheim's estimated strength. While our MAP value indicates a single point estimate, our posterior plot shows that 
there is considerable uncertainty in that rating. Our credible interval values supplement this assessment, by showing a 95% probability that Anaheim's true strength 
parameter, given this specific data and model assumptions, covers a huge range of possibilities. We see a substantial amount of underlying variation - and our Bayesian 
approach allows us to recognize this and temper our confidence in the model ratings accordingly.

### 10. Predicting Games with MCMC
Just like before, we can make a forecast for a hypothetical game:
```{r warning=FALSE, message=FALSE}
mcmc.forecast <- function(home, away){
  1/(1+exp(-(ratings.mcmc.final['hia'] + ratings.mcmc.final[home] - ratings.mcmc.final[away])))
}
```
Plugging in our two previous teams, Tampa Bay and Anaheim, produces a comparable forecast to our MLE estimate:
```{r warning=FALSE, message=FALSE}
mcf <- mcmc.forecast("Tampa Bay Lightning", "Anaheim Ducks")
names(mcf) <- NULL #Remove names from the returned probability
round(mcf,4)#Round output probability to 4 decimal points
```
**Sanity Check for Home Ice Advantage**  
Once again, as a quick sanity check, we can also forecast the same team playing itself to determine if home ice advantage appears reasonable. Typical home ice 
advantage is ~3-4%. Let's try that now with Tampa Bay:
```{r warning=FALSE, message=FALSE}
mcf <- mcmc.forecast("Tampa Bay Lightning", "Tampa Bay Lightning")
names(mcf) <- NULL #Remove names from the returned probability
round(mcf,4)#Round output probability to 4 decimal points
```

