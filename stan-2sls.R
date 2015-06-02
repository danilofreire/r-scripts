# How to estimate an IV model (two-stage least squares) using R and Stan.

# Load necessary packages
library(rstan)
library(AER)

# Generate data
n <- 1000
z <- rnorm(n)
x <- 1 + 2*z + rnorm(n)
y <- 3 + 1.5*2*z + rnorm(n)

d <- data.frame(z, x, y)

# Baseline results
iv <- ivreg(y ~ x | z, data = d)
summary(iv)

# Stan model

model <- "
    data {
 int<lower=0> n;
 matrix[n,2] yt;
 vector[n] z;
}

    parameters {
 real b;
 real d;
 real a;
 real g;
 real<lower=0,upper=100> sigma_t;
 real<lower=0,upper=100> sigma_y;
 real<lower=-1,upper=1> rho_yt;
}

    transformed parameters {
 cov_matrix[2] Sigma_yt;
 matrix[n,2] yt_hat;

 Sigma_yt[1,1] <- pow(sigma_y,2);
 Sigma_yt[2,2] <- pow(sigma_t,2);
 Sigma_yt[1,2] <- rho_yt*sigma_y*sigma_t;
 Sigma_yt[2,1] <- Sigma_yt[1,2];


 for (i in 1:n) {
 yt_hat[i,2] <- g + d*z[i];
 yt_hat[i,1] <- a + b*yt[i,2];
}
}

    model {
 sigma_y ~ uniform(0,100);
 sigma_t ~ uniform(0,100);
 rho_yt ~ uniform(-1,1);
 d ~ normal (0,100);
 b ~ normal (0,100);
 a ~ normal (0,100);
 g ~ normal (0,100);

 for (i in 1:n)
 transpose(yt[i]) ~ multi_normal(transpose(yt_hat[i]), Sigma_yt);
}
"

d_list <- list(y1=y, y2=x)
d_matrix <- as.matrix(data.frame(d_list))
data <- list(yt=d_matrix, z=z, n=n)

fit <- stan(model_code = model, data=data, iter = 1000, chains = 3, pars=c("a", "b", "d", "g", "sigma_t", "sigma_y", "rho_yt"))

print(fit)

d <- as.data.frame(fit)
