# Solver Approximation in R | Andrew Mack | @gingfacekillah
# Load Libraries
library(tidyverse)
library(dplyr)
library(GA)
require(utils)
# Load the data as a .csv file
ev.data <- read.csv('plusEV.csv')
# Create dataframe
#ev.data <- data.frame(player, salary, rookie, last.year.gp, last.year.gf, current.year.gp, current.year.gf)
head(ev.data)
# Define Parameters to Optimize
parameter.names <- (c('p.intercept', 'p.log.salary', 'p.min.salary', 'p.rookie', 'p.ballast'))
#prior.params <- as.vector(rep(1,5))
prior.params <- c(1,1,1,1,1)
names(prior.params) <- (parameter.names)
# Prior Function
prior.function <- function(p.intercept, p.log.salary, p.min.salary, p.rookie, salary, rookie) {
  prior.output <- exp(p.intercept + p.log.salary * (log(pmax(salary, p.min.salary))) + p.rookie * rookie)
}
# Posterior Function    
posterior.function <- function(p.ballast, last.year.gp, last.year.gf, ll.calc) {
  posterior.output <- ((last.year.gf + p.ballast * ll.calc)/(last.year.gp + p.ballast))
}
# Log Likelihood Function
post.ll <- function(prior.params) {ev.data %>%
    mutate(ll.calc = prior.function(prior.params['p.intercept'],
                                    prior.params['p.log.salary'],
                                    prior.params['p.min.salary'],
                                    prior.params['p.rookie'],
                                    salary, rookie)) %>%
    mutate(ll.calc2 = posterior.function(prior.params['p.ballast'],
                                         last.year.gp, last.year.gf, ll.calc)) %>%
    summarize(sum.loglik = sum(dpois(current.year.gf,(current.year.gp * ll.calc2), log = TRUE)))
}
#Genetic Algorithm for Global Search
post_ga <- function(x1, x2, x3, x4,x5)
{
  x <- c(x1,x2,x3,x4,x5)
  names(x) <- (parameter.names)
  output <- post.ll(x)
  output$sum.loglik
}

lower_bound = c(-10, 0.01, 1, 0, 0.01)
upper_bound = c(-5,0.5,1e7,3,300)
bounds <- as.data.frame(matrix(c(lower_bound,upper_bound), nrow=2, byrow=TRUE, dimnames = list(c("lower","upper"),c(parameter.names))))

p.intercept.grid = seq(bounds$p.intercept[1],bounds$p.intercept[2],length.out=3)
p.log.salary.grid = seq(bounds$p.log.salary[1],bounds$p.log.salary[2],length.out=3)
p.rookie.grid = seq(bounds$p.rookie[1],bounds$p.rookie[2],length.out=3)
p.ballast.grid = seq(bounds$p.ballast[1],bounds$p.ballast[2],length.out=3)
p.min.salary.grid = sort(ev.data$salary)

initial_guess = expand.grid(p.intercept = p.intercept.grid, p.log.salary=p.log.salary.grid, p.min.salary=p.min.salary.grid,
                            p.rookie = p.rookie.grid, p.ballast = p.ballast.grid)

# Combine Global & Local Search Using Genetic Algorithm & BFGS with Box Constraints
start_time <- Sys.time()
GA <- ga(type = "real-valued", 
         fitness =  function(x) post_ga(x[1], x[2], x[3], x[4],x[5]),
         lower = as.numeric(bounds["lower",]),
         upper = as.numeric(bounds["upper",]), 
         popSize = nrow(initial_guess)*2,
         maxiter = 1000,
         run = 50,
         pmutation=0.8,
         pcrossover=0.9,
         optim = TRUE,
         seed = 1001,
         monitor = gaMonitor,
         suggestions= initial_guess,
         optimArgs = list(method = "L-BFGS-B", 
                          poptim = 0.5,
                          pressel = 0.5,
                          control = list(fnscale = -1, maxit = 1000)))

# Print Optimized Results
summary(GA)
# Plot Genetic Algorithm Search
plot(GA)
# Print Time Elapsed - Should be about ~8 minutes
end_time <- Sys.time()
end_time - start_time

# This is L-BFGS-B with Box Constraints, but without the Genetic Algorithm. Faster, but liable to stop at a local solution.
BFGSB <- optim(prior.params, post.ll,
                  method = "L-BFGS-B", # BFGS Algorithm that allows constraints (upper and lower bounds)
                  lower = c(-Inf, -Inf, 1, -Inf, 0.01),# Constraints: P.Min.Salary >=1, P.Ballast >=0.01, 
                  upper = c(Inf,Inf,Inf,Inf,Inf),
                  control = list(fnscale=-1, # fnscale =-1 specifies maximization of function
                                 maxit = 1000, # Number of iterations
                                 REPORT = 1, # Show every iteration
                                 trace = 1)) # 1= Verbose - print the optimization process
# Solver Summary: Optimized Parameters
BFGSB

