# rstan NBA player points MCMC simulation tutorial | @gingfacekillah
# using player points per minute and estimated minutes played distributions

#Load necessary packages
library(rstan)

#Define prior distribution mean & stdev
prior_minutes_mean <- 34.68
prior_minutes_sd <- 12.122
prior_ppm_mean <- 1.03
prior_ppm_sd <- 0.25

#Define Stan model
model_code <- "
data {
real minutes_mean;
real minutes_sd;
real ppm_mean;
real ppm_sd;
}
parameters {
real minutes;
real ppm;
}
model {
// Priors
minutes ~ normal(minutes_mean, minutes_sd);
ppm ~ normal(ppm_mean, ppm_sd);
}
generated quantities {
real points = minutes * ppm;
}
"

#Compile model
model <- stan_model(model_code = model_code)

#Generate posterior samples
posterior_samples <- sampling(model, data = list(minutes_mean = prior_minutes_mean,
                                                 minutes_sd = prior_minutes_sd,
                                                 ppm_mean = prior_ppm_mean,
                                                 ppm_sd = prior_ppm_sd),
                              chains = 4, iter = 1000, warmup = 500, thin = 2)

#Extract posterior samples for expected points
posterior_points <- extract(posterior_samples)$points

#Plot posterior distribution of expected points
hist(posterior_points, breaks = 50, xlab = "Expected Points", ylab = "Frequency")

median(posterior_points) # Median of distribution
mean(posterior_points > 26.5) # probability of observing more than 26.5
mean(posterior_points < 26.5)
