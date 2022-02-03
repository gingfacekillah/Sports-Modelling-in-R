---
title: "Bradley Terry Sport Models in R"
author: "Andrew Mack | @gingfacekillah"
date: "21/12/2020"
output:
  html_document: default
  word_document: default
---
Bradley Terry models in R are nothing new - in fact there are some very good packages already in existence (like **BradleyTerry2**)
that can be used to run these models with a minimal amount of elbow grease. That being said, sometimes it's fun (and instructive)
to take a shot at building a simple model from the ground up. Let's give it a shot together using simple game score data from the NHL. 
We'll use our model to estimate team strength and then forecast a hypothetical matchup. Let's dive in.

### 1. Load The Raw Data
First let's load some game result data from the 2018-2019 NHL season as a .csv file:
```{r warning=FALSE, message=FALSE}
library(knitr)
data <- read.csv('hockeydata.csv')
kable(head(data), caption = "2018-2019 NHL Game Result Data")
```

### 2. Clean & Wrangle The Data
Now let's make some new variables and clean the variable names up a bit. First we'll create a margin of victory **[h_mov]** variable 
from the home team's perspective, and then create a binary result variable **[h_win]** where the home team records a win [1] or a loss 
[0] based on the positive or negative value of the margin of victory. Then we'll rename the variables using lowercase letters and select 
the data that we want for our new cleaned up dataframe.

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

Next we need to create a model formula. For a Bradley Terry model we'll want to use a logistic function that combines the home rating,
away rating, and home ice advantage. There are other ways to build this, but this method closely follows the Excel version of this model
from my book Statistical Sports Models in Excel Volume 1, so I think it will be approachable for those trying to make the leap from Excel to R.
```{r warning=FALSE, message=FALSE}
bt.home_forecast <- function(home_rating,
                          visitor_rating,
                          homeice_adv){
  1/(1+exp(-(homeice_adv + home_rating - visitor_rating))) #Logistic Function
}
```


### 4. Assign Dummy Team Ratings
Next we create a vector of dummy team ratings and start all teams off with a rating of 1.000. We'll later optimize the ratings using
maximum likelihood estimation, but this gives us a starting point to work from.

```{r warning=FALSE, message=FALSE}
teams <- as.vector(sort(unique(bt.games$home)))
bt.ratings <- rep(1.000,32) #Starting Rating is 1.000, 31 teams + hia
names(bt.ratings) <- c(teams, 'hia') #'hia' is our home ice advantage
```

### 5. Create a Likelihood Function
Now we need to create a log likelihood function which we can use to optimize the team ratings in a way that makes the observed game results
most probable. This function compares the estimated win probability to the binary result variable **[h_win]** and returns the estimated
probability if [h_win] is 1, and 1 minus the estimated probability if [h_win] is 0. The function then takes the natural logarithm of the
resulting probabilities and sums them together to return a log likelihood as **[ll.final]**.

```{r warning=FALSE, message=FALSE}
ll <- function(bt.ratings) {
  bt.games %>%
    mutate(forecast = bt.home_forecast(bt.ratings[home],
                                    bt.ratings[away],
                                    bt.ratings[32])) %>% #32 is the home ice advantage
    mutate(result.f = ifelse(h_win==1,forecast, (1-forecast))) %>%  # The result function
    summarise(ll.final = sum(log(result.f))) %>% #sum of the log likelihood
    pull(ll.final)
}
```

### 6. Optimize Team Ratings With Maximum Likelihood Estimation
Using **Optim** in R, we can replicate the non-linear Solver add-on from Excel. We input the team ratings **[bt.ratings]** as the parameters,
the log likelihood function **[ll]** as the function to optimize, and then make the **fnscale** equal to -1. This is important to remember, as
**Optim** will attempt to minimize a given function otherwise.

```{r warning=FALSE, message=FALSE}
bt.optim <- optim(bt.ratings, ll,
                  method = "BFGS", #Broyden–Fletcher–Goldfarb–Shanno algorithm - a non-linear optimization
                  control = list(fnscale=-1)) #[-1] in fnscale necessary for maximization of function
bt.optim.ratings <- bt.optim$par #Isolate team rating estimates
bt.optim.ratings
```
We can also use ggplot to make a quick visual of the estimated team strengths:
```{r warning=FALSE, message=FALSE, fig.align='center', fig.asp = 0.8, fig.width = 8}
library(ggplot2)

plot.ratings <- stack(bt.optim.ratings) %>% #Convert named vector into dataframe with labels
  select(team = ind,
         rating = values)

plot.ratings$team <- as.character(plot.ratings$team)

plot.ratings %>%
  arrange(rating) %>%    #Sorting plot by logistic rating strength using dplyr
  mutate(team=factor(team, levels=team)) %>%  
  ggplot( aes(x=rating, y=team)) +
  geom_point(aes(size=2, colour=rating))+
  labs(y="Team",
       x="Logistic Strength Rating")
```

### 7. Forecasting Future Games
To forecast a future game we can create a function that combines our freshly derived team strength ratings with the model structure we used
earlier. We'll code the function so that teams are entered as **["home team", "away team"]**.
```{r warning=FALSE, message=FALSE}
xbt.forecast <- function(home, away){
  1/(1+exp(-(bt.optim.ratings['hia'] + bt.optim.ratings[home] - bt.optim.ratings[away])))
}
```

Entering two teams into our newly created function produces the desired forecast:
```{r warning=FALSE, message=FALSE}
xbt <- xbt.forecast("Tampa Bay Lightning", "Anaheim Ducks")
names(xbt) <- NULL #Remove names from the returned probability
round(xbt,4)#Round output probability to 4 decimal points
```
**Sanity Check for Home Ice Advantage**  
As a quick sanity check, we can also forecast the same team playing itself to determine if home ice advantage appears reasonable. Typical home
ice advantage is ~3-4%. Let's try that now with Tampa Bay:
```{r warning=FALSE, message=FALSE}
xbt <- xbt.forecast("Tampa Bay Lightning", "Tampa Bay Lightning")
names(xbt) <- NULL #Remove names from the returned probability
round(xbt,4) #Round output probability to 4 decimal points
```
