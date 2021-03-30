#################################################
#######                                   #######
#######    Example of Gradient Booting    #######
#######                                   #######
#################################################

library(tibble)
library(ggplot2)
library(dplyr)
library(cowplot)
library(wesanderson)
library(rpart)
library(rpart.plot)
#library(xgboost)      


# Step 1 - Create quadratic dataset ---------------------------------------

df <- tibble(hours=c(6,8, 9, 12, 14, 20,28, 30, 35, 40, 47, 51, 55, 60,75),
             happiness=c(14,19, 28,35, 50, 63,70, 89, 94, 90, 75, 59, 44, 27,13))

# Plot
ggplot(df, aes(x=hours, y = happiness))+
  geom_point(size=4)+
  theme_minimal_grid()+
  labs(y = "Happines score",
       x= "Hours worked")


# Step 2 - Example of decision tree and boosting --------------------------


# First tree fit
dec1 <- rpart(happiness ~ hours, data = df, method= "anova", control = list(cp = 0.001,
                                                                            minsplit = 5, 
                                                                            maxdepth  = 1, 
                                                                            minbucket = 3))
df$pred1 <- predict(dec1) 

# Plot decision tree
rpart.plot(dec1, 
           type = 5,
           under = T,
           tweak = 1.2,
           box.palette = wes_palette("Zissou1", 5, type = c("continuous")))

# Second fit
df$y2 <- residuals(dec1) 
dec2 <- rpart(y2 ~ hours,  data = df, method= "anova", control = list(cp = 0.001,
                                                                      minsplit = 5, 
                                                                      maxdepth  = 1,
                                                                      minbucket = 3))
df$pred2 <- predict(dec2)

# Third fit
df$y3 <- residuals(dec2) 
dec3 <- rpart(y3 ~ hours,  data = df, method= "anova", control = list(cp = 0.001,
                                                                      minsplit = 5, 
                                                                      maxdepth  = 1,
                                                                      minbucket = 3))
df$pred3 <- predict(dec3)


wes_pal <- wes_palette("FantasticFox1", 5, type = c("discrete"))

# Plot
ggplot(df, aes(x=hours, y = happiness))+
  geom_point(size=4)+
  theme_minimal_grid()+
  geom_line(aes(x = hours, y = pred1), col = wes_pal[5], size = 2.5, alpha=0.8) +
  annotate("text", x = 68, y = 64, label = "1 regressor", col = wes_pal[5] )+
  geom_line(aes(x = hours, y = pred2+pred1), col = wes_pal[2], size = 2.5, alpha=0.8) +
  annotate("text", x = 68, y = 24, label = "2 regressors", col = wes_pal[2] )+
  geom_line(aes(x = hours, y = pred3+pred2+pred1), col = wes_pal[3], size = 2.5, alpha=0.8)+
  annotate("text", x = 68, y = 37, label = "3 regressors", col = wes_pal[3] )+
  labs(title = "Gradient boosting with three decision trees",
       y = "Happines score",
       x= "Hours worked")



# Step 3 - Boosting with multiple trees -----------------------------------
df <- tibble(hours=c(6,8, 9, 12, 14, 20,28, 30, 35, 40, 47, 51, 55, 60,75,20),
             happiness=c(14,19, 28,35, 50, 63,70, 89, 94, 90, 75, 59, 44, 27,13,120))

# Add noise
df$happiness <- df$happiness + rnorm(length(df$happiness),mean = 2, sd = 15)


# Gradient boosting function
gb <- function(n_trees, x, y , data){
  reg <- list()
  
  for (tree in seq(1, n_trees, by=1)){
    
    if (tree == 1) {
      
      # Fit first tree
      reg[[1]] <- rpart(eval(parse(text=y)) ~ eval(parse(text=x)),
                    data = data, 
                    method= "anova", control = list(cp = 0.00001,
                                                    minsplit = 2, 
                                                    maxdepth  = 3,
                                                    minbucket = 3))
      data$pred1 <- predict(reg[[1]]) 
      
      next
    }
    
    # Loop tree fits
    data[paste("y", tree, sep="")] <- residuals(reg[[tree-1]])
    
    reg[[tree]] <- rpart(eval(parse(text=paste("y", tree, sep=""))) ~ hours,
                                                            data = data,
                                                            method= "anova",
                                                            control = list(cp = 0.00001,
                                                                           minsplit = 2, 
                                                                           maxdepth  = 3,
                                                                           minbucket = 3))
    data[paste("pred", tree, sep="")] <- predict(reg[[tree]])
    
    
  }
  return (data)
}


# Fit gb function for choosen number of trees
gb_preds <- gb(n_trees = 12, x = "hours", y = "happiness", data =  df)

# Create annotation function
annot_f <- function(n_trees, data){
  annot <- list()
  for (tree in seq(1, n_trees)){
    
    to_sum <- data %>% 
               select(contains("pred") & !contains(c(as.character(seq(tree+1, n_trees))))) %>%
      rowSums()
    
    annot[[tree]] <- to_sum
  }
    
  return(annot)
}

# Apply function
a <- annot_f(12, gb_preds)

# Plot
ggplot(gb_preds, aes(x=hours, y = happiness))+
  geom_point(size=4)+
  theme_minimal_grid()+
  geom_line(aes(x = hours, y = a[[2]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[3]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[4]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[5]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[6]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[7]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[8]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[9]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[10]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[11]]), col = "grey", size = 2.5, alpha=0.6) +
  geom_line(aes(x = hours, y = a[[1]]), col = wes_pal[5], size = 2.5, alpha=0.6) +
    geom_line(aes(x = hours, y = a[[12]]), col = wes_pal[3], size = 2.5, alpha=0.8) +
  annotate("text", x = 68, y = 29, label = "Final model", col = wes_pal[3] )+
  labs(title = "Gradient boosted prediction of happines",
       y = "Happines score",
       x= "Hours worked")






