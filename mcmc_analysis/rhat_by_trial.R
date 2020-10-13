#library(coda)

trial_mpsrf <- function(df){
  # 'df' is a data frame of a subject_KO data
  #  returns a data frame of mpsrf values per fruit per trial

# create list objects of class mcmc  
df_apple_mcmc <- mcmc.list(as.mcmc(subset(df, df$chain == 0)[,7:12]), 
                                      as.mcmc(subset(df, df$chain == 3)[,7:12]), 
                                      as.mcmc(subset(df, df$chain == 6)[,7:12]))
df_orange_mcmc <- mcmc.list(as.mcmc(subset(df, df$chain == 1)[,7:12]), 
                                      as.mcmc(subset(df, df$chain == 4)[,7:12]), 
                                      as.mcmc(subset(df, df$chain == 7)[,7:12]))
df_grape_mcmc <- mcmc.list(as.mcmc(subset(df, df$chain == 2)[,7:12]), 
                                       as.mcmc(subset(df, df$chain == 5)[,7:12]), 
                                       as.mcmc(subset(df, df$chain == 8)[,7:12]))

# create empty vectors for adding in mpsrf values
apple_mpsrf <- c()
grape_mpsrf <-c()
orange_mpsrf <- c()


# loop through apple list and create/append mpsrf values
i <- 10
while (i <= nrow(as.data.frame(df_apple_mcmc[[1]]))){
  
  current_1 <- as.mcmc(df_apple_mcmc[[1]][1:i,])
  current_2 <- as.mcmc(df_apple_mcmc[[2]][1:i,])
  current_3 <- as.mcmc(df_apple_mcmc[[3]][1:i,])
  current <- mcmc.list(current_1, current_2, current_3)
  current_mpsrf <- gelman.diag(current, autoburnin = FALSE)[[2]]
  apple_mpsrf[(i-10) + 1] <- current_mpsrf
  i <- i + 1
}

# loop through grape list and create/append mpsrf values
j <- 10
while (j <= nrow(as.data.frame(df_orange_mcmc[[1]]))){
  
  current_1 <- as.mcmc(df_orange_mcmc[[1]][1:j,])
  current_2 <- as.mcmc(df_orange_mcmc[[2]][1:j,])
  current_3 <- as.mcmc(df_orange_mcmc[[3]][1:j,])
  current <- mcmc.list(current_1, current_2, current_3)
  
  current_mpsrf <- gelman.diag(current, autoburnin = FALSE)[[2]]
  grape_mpsrf[(j-10) + 1] <- current_mpsrf
  j <- j + 1
}

# loop through grape list and create/append mpsrf values
k <- 10
while (k <= nrow(as.data.frame(df_grape_mcmc[[1]]))){
  
  current_1 <- as.mcmc(df_grape_mcmc[[1]][1:k,])
  current_2 <- as.mcmc(df_grape_mcmc[[2]][1:k,])
  current_3 <- as.mcmc(df_grape_mcmc[[3]][1:k,])
  current <- mcmc.list(current_1, current_2, current_3)
  
  current_mpsrf <- gelman.diag(current, autoburnin = FALSE)[[2]]
  orange_mpsrf[(k-10) + 1] <- current_mpsrf
  k <- k + 1
}

mpsrf_df <- as.data.frame(cbind(apple_mpsrf, grape_mpsrf, orange_mpsrf))

return(mpsrf_df)
}
