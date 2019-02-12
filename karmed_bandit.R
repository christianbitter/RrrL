# author: christian bitter at gmail dot com
# description:
# the k-armed bandit problem ... following Reinforcement Learning an Introduction 
# not optimized
#

rm(list = ls());

library(dplyr);
library(ggplot2);
library(gridExtra);
library(ggthemes);

set.seed(1234);


Q_t <- function(R_t, a, t){
  numerator <- sum(!is.na(R_t[a, 1:t]));
  if (numerator == 0) return(0);
  return(
    sum(R_t[a, 1:t], na.rm = T) / numerator 
  );
}


bandit <- function(k){
  base <- runif(k, min = 0, max = k);
  sample <- function(b){
    if (b < 1 | b > k) stop("sample - bandit arm [1, k]");
    
    return(rnorm(n = 1, mean = base[b], sd = 1.));
  }
  
  return(sample);
}

run_session <- function(rl_env, action_space, session, n = 10, tmax = 10, ...){
  s <- rep(NA, n * tmax);
  s <- matrix(s, nrow = n, ncol = tmax)
  for (i in 1:n) {
    s_result <- session(rl_env, action_space, tmax, ...); 
    s[i, ] <- s_result$session;
  }
  s_mean <- apply(s, 2, mean);
  s_sd   <- apply(s, 2, sd);
  return(list(
    "data" = s,
    "s_mean" = s_mean,
    "s_sd" = s_sd
  ))
}

plot_session <- function(xs){
  df <- data.frame(s_mean = xs$s_mean, s_sd = xs$s_sd, s = 1:length(xs$s_mean));
  p1 <- 
    df %>% 
      ggplot(aes(x = s, y = s_mean)) + 
      geom_line(linetype = 2) + 
      geom_hline(aes(yintercept = mean(s_mean)), colour="blue", alpha = .7) +
      # geom_point() +
      # geom_errorbar(aes(ymin = s_mean - s_sd, ymax = s_mean + s_sd), alpha = .3) + 
      labs(x = "session", y = "average reward",
           title = "N-Armed Bandit Problem - Rewards",
           subtitle = "Performance/ Reward of Agent at each time step averaged over sessions (average shown in blue)")
      theme_light();
  # cumulative reward
  df$s_cum <- cumsum(df$s_mean);
  p2 <- 
    df %>% 
    ggplot(aes(x = s, y = s_cum)) + 
    geom_line(alpha = .7, linetype = 2) + 
    geom_point() +
    geom_errorbar(aes(ymin = s_cum - s_sd, ymax = s_cum + s_sd)) + 
    labs(x = "session", y = "average reward",
         title = "N-Armed Bandit Problem Cumulative Rewards",
         subtitle = "Cumulative Averaged Rewards per time step")
    theme_light();
    
  gridExtra::grid.arrange(p1, p2);
}

# let's test random selection
random_seletion <- function(rl_env, action_space, tmax, ...) {
    a_session <- rep(NA, tmax);
    A_t <- rep(NA, tmax);
    
    for (j in 1:tmax) {
      action <- sample(x = action_space, size = 1, replace = T);
      A_t[j] <- action;
      s <- rl_env(action);
      a_session[j] <- s;
    }
    
    return(list(
      "session" = a_session,
      "A"  = A_t,
      "R" = NULL,
      "Q" = NULL)
    );
}

# let's test greedy selection
greedy_selection <- function(rl_env, action_space, tmax, ...){
  a_session <- rep(NA, tmax);

  # in the greedy way we do not explore, we keep track of the best bandit seen so far
  # and always push that lever
  A_t <- rep(NA, tmax);
  r_t <- matrix(NA, nrow = length(action_space), ncol = tmax);
  for (j in 1:tmax) {
    Q_t_a <- sapply(X = action_space, 
                    FUN = function(i)Q_t(R_t = r_t, a = i, t = j),
                    simplify = T);
    action <- which.max(x = Q_t_a);
    A_t[j] <- action;
    s <- rl_env(action);
    r_t[action, j] <- s;
    a_session[j] <- s;
  }
    
  return(list(
    "session" = a_session,
    "A"  = A_t,
    "Q" = NULL,
    "R" = r_t)
  );
}

e_greedy_selection <- function(rl_env, action_space, tmax, ...){
  a_session <- rep(NA, tmax);
  args <- list(...);
  epsilon <- 0.;
  if ("epsilon" %in% names(args)) epsilon <- args$epsilon;
  # in the epsilon-greedy way we leave some room for exploration
  A_t <- rep(NA, tmax);
  r_t <- matrix(NA, nrow = length(action_space), ncol = tmax);
  for (j in 1:tmax) {
    Q_t_a <- sapply(X = action_space, 
                    FUN = function(i)Q_t(R_t = r_t, a = i, t = j),
                    simplify = T);
    action <- which.max(x = Q_t_a);
    if (runif(1) < epsilon) action <- sample(x = action_space, size = 1, replace = T);
    A_t[j] <- action;
    s <- rl_env(action);
    r_t[action, j] <- s;
    a_session[j] <- s;
  }
  
  return(list(
    "session" = a_session,
    "A"  = A_t,
    "Q" = NULL,
    "R" = r_t)
  );
}

nBandits <- 6;
action_space <- 1:nBandits;
rl_env <- bandit(k = nBandits);
TMAX   <- 1e3;

xs_random <- run_session(rl_env, action_space, session = random_seletion, n = 5, tmax = TMAX);
xs_greedy <- run_session(rl_env, action_space, greedy_selection, n = 5, tmax = TMAX);
xs_egreedy_1 <- run_session(rl_env, action_space, e_greedy_selection, n = 5, tmax = TMAX, epsilon = .1);
xs_egreedy_2 <- run_session(rl_env, action_space, e_greedy_selection, n = 5, tmax = TMAX, epsilon = .2);
xs_egreedy_3 <- run_session(rl_env, action_space, e_greedy_selection, n = 5, tmax = TMAX, epsilon = .3);
xs_egreedy_5 <- run_session(rl_env, action_space, e_greedy_selection, n = 5, tmax = TMAX, epsilon = .5);

# let's test and compare e-greedy selection
plot_session(xs_random); # average reward per session of about 2.5, cum reward ~ 245
plot_session(xs_greedy); # average reward per session of about 1.9, cum reward ~ 260
plot_session(xs_egreedy_1); # average reward per session of about 5, cum reward ~ 360
plot_session(xs_egreedy_2); # average reward per session of about 4.9, cum reward ~ 360
plot_session(xs_egreedy_3); # average reward per session of about 4.7, cum reward ~ 360
plot_session(xs_egreedy_5); # average reward per session of about 4.4, cum reward ~ 360
