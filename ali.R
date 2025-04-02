# loading libraries
library(tidyverse)
library(patchwork)

R <- 10000
sample.size <- 1004
satisfaction <- 0.39
samples <- tibble(sat = numeric(R))

for(i in 1:R){
  samples$sat[i] <- (rbinom(1, sample.size, satisfaction)/sample.size)
}

(polls.hist <- ggplot(samples) +
    geom_histogram(aes(x=sat, y=after_stat(density)),
                   breaks = seq(0.325, 0.475, 0.005),
                   color="grey")+
    stat_density(aes(x = sat), geom="line") +
    geom_hline(yintercept=0)+
    theme_bw()+
    xlab("Satisfaction")+
    ylab("Frequency")
)


lower_bound <- quantile(samples$sat, 0.025)
upper_bound <- quantile(samples$sat, 0.975)
range <- upper_bound[[1]] - lower_bound[[1]]
moe <- range/2
# moe is less than the estimated 4%, is in range

larger.size <- sample.size *2
larger.samples <- tibble(sat = numeric(R))
for(i in 1:R){
  larger.samples$sat[i] <- (rbinom(1, larger.size, satisfaction)/larger.size)
}

(larger.polls.hist <- ggplot(larger.samples) +
    geom_histogram(aes(x=sat, y=after_stat(density)),
                   breaks = seq(0.325, 0.475, 0.005),
                   color="grey")+
    stat_density(aes(x = sat), geom="line") +
    geom_hline(yintercept=0)+
    theme_bw()+
    xlab("Larger Satisfaction")+
    ylab("Frequency")
)
lower_bound2 <- quantile(larger.samples$sat, 0.025)
upper_bound2 <- quantile(larger.samples$sat, 0.975)
range2 <- upper_bound[[1]] - lower_bound[[1]]
moe2 <- range/2
# moe didn't quite half and is slightly over the 2% range

larger.polls.hist +polls.hist


gallup.survey <- data.frame(values = sample(c(rep(1, 391), rep(0, 613))))

resamples <- tibble(value = numeric(R))
for(i in 1:R){
  # Take a resample
  curr.resample <- sample(x = gallup.survey$values,
                          size = nrow(gallup.survey),
                          replace = T)
  # compute the stat on the resample
  resamples$value[i] <- mean(curr.resample)
}

(resamples.hist <- ggplot(resamples) +
    geom_histogram(aes(x=value, y=after_stat(density)),
                   breaks = seq(0.325, 0.475, 0.005),
                   color="grey")+
    stat_density(aes(x = value), geom="line") +
    geom_hline(yintercept=0)+
    theme_bw()+
    xlab("Resamples Satisfaction")+
    ylab("Density")
)
lower_bound3 <- quantile(resamples$value, 0.025)
upper_bound3 <- quantile(resamples$value, 0.975)
range.resample <- upper_bound3[[1]] - lower_bound3[[1]]
moe.resample <- range.resample/2
# moe is lower than the stimulation

n <- seq(100,3000,10)
p <- seq(0.01, 0.99, 0.01)
simulations <- tibble()
counter <- 1

find_moe <- function(size,prob){
  currents <- tibble(percent = numeric(10000))
  for(i in 1:10000){
    currents$percent[i] <- (rbinom(1, size, prob)/size)
  }
  lower_bound <- quantile(currents$percent, 0.025)
  upper_bound <- quantile(currents$percent, 0.975)
  moe<- (upper_bound[[1]] - lower_bound[[1]])/2
  return(moe)
}

for(j in 1:length(p)){
  new <- tibble(n) %>%
    mutate(
      p= p[j],
      moe = 0
    )
  simulations <- bind_rows(simulations, new)
}
for(j in 1:length(p)){
  for(i in 1:length(n)){
    v1 <- n[i]
    v2 <- p[j]
    simulations$moe[counter] <- find_moe(v1,v2)
    counter <- counter +1
  }
}
