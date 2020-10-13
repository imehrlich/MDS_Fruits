acceptance <- function(list_subject){
  # 'list_subject' is a list of subject data
  # returns data frame with acceptance rates
  
  # empty data frame
  accepted_df <- data.frame()
  
  # loop through subjects
  for (df in list_subject){
    
    # subset subject data frame by fruit
    apple <- subset(df, df$current.fruit == "APPLE")
    orange <- subset(df, df$current.fruit == "ORANGE")
    grape <- subset(df, df$current.fruit == "GRAPE")
  
    # find proportions by fruit
    apple_prop <- table(apple$accept)[2] / nrow(apple)
    orange_prop <- table(orange$accept)[2] / nrow(orange)
    grape_prop <- table(grape$accept)[2] / nrow(grape)
    df_prop <- table(df$accept)[2] / nrow(df)
    
    # create vector and add to data frame
    accepted_prop <- c(apple_prop, grape_prop, orange_prop, df_prop)
    accepted_df <- rbind(accepted_df, accepted_prop)
  }
  
  colnames(accepted_df) <- c("apple", "grape", "orange", "total")

  return(accepted_df)
  
}
