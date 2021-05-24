## Globe tossing model

# probability of 5 waters, 3 lands if p=0.7
dbinom(x=5, size=8, prob=0.7)

# what does binomial look like when you draw lots of data??
w_draw <- rbinom(n=5000, size=30, prob=0.75)
hist(w_draw)

## Grid Approximation ---------------------

# define grid
p_grid <- seq( from=0, to=1, length.out = 20 )
p_grid

# define (uninformative) prior
prior <- rep(1, 20)
prior

# prior with all the mass > 0.5
prior <- c(rep(0,10), rep(1, 10))

# compute the likelihood
likelihood <- dbinom(x=2, size=5, prob=p_grid)

plot(p_grid, likelihood)

# do bayes rule
unstd.posterior <- prior * likelihood

# standardized posterior
posterior <- unstd.posterior / sum(unstd.posterior)

# plot the posterior!
plot(p_grid, posterior)

## A new experimenter comes along --------------

# NEW prior based on the earlier experiment
prior <- posterior

# NEW likelihood
likelihood <- dbinom(x=3, size=6, prob=p_grid )

# NEW posterior
unstd.posterior <- prior * likelihood
posterior <- unstd.posterior / sum(unstd.posterior)
plot(p_grid, posterior)


## Here's why grid approximation breaks in higher dimensions -------

grid_size <- 20

grid_size^2

grid_size ^ 3

grid_size ^ 10
