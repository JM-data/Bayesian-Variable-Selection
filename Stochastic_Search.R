library(mlbench)
library(BayesFactor)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(dplyr)
library(wle)
library(xtable)
library(dummies)



########################OZONE###########################################
#We unfortunately have to use a different syntax in R because the as.formula
#doesn't understand the symbol '*' or '^2'. So we used 'µ' for multiply
#And "µµ2' for taking the square value or the variable.


# OZONE -------------------------------------------------------------------
data("Ozone")

#Deleting NaN & Renaming Columns
Ozone <- na.omit(Ozone)
colnames(Ozone) <- c("x1", "x2","x3","y","x4",
                     "x5","x6",'x7','x11','x8',
                     'x9','x12','x10')
#Select and rearrange variables: x1 to x10, and y 
Ozone <- Ozone[,c(1,2,3,5,6,7,8,10,11,13,9,12,4)]

#Correlation Plot for numeric variables
nums <- sapply(Ozone, is.numeric)
M <- cor(Ozone[,nums])
corrplot(M, method = "number")

#We notice Variables X11 and X12 are strongly correlated between each other, and
#with variables x4,X7,x8. Delete in order to avoid multicolinearity
Ozone <- Ozone[,-c(11,12)]

### Box plots (Replace with Normal Values after, Jan, Mon...)

par(mfrow=c(1,3))
for (i in names(Ozone[,1:3])){
  boxplot( as.formula(paste0("y~", i)), data = Ozone, main = paste("Response Variable by", names(Ozone[i])),
           xlab = names(Ozone[i]), ylab="Response Variable")
}
par(mfrow=c(1,1))

#####Creating the Whole databases with dummies etc

Ozone_reg <- Ozone[,c(-11)]
dummy <- dummy.data.frame(Ozone[,c(1,2,3)], sep = "_")
Ozone_reg[,c(1,2,3)] <- NULL
dummy[,c('x1_12','x2_31','x3_5')] <- NULL # To Avoid Multicolinearity
Ozone_reg <- cbind(dummy,Ozone_reg)

#Getting the Squared Values, 
Ozone_sq <- Ozone[,4:10]
for(col in colnames(Ozone)[4:10]){
  col2 <- paste0(col,"µµ2")
  Ozone_sq[[col2]] <- Ozone_reg[[col]]^2
  Ozone_sq[[col]] <- NULL
}

#Getting the interaction terms
Ozone_inter <- Ozone_reg
N <- length(Ozone_reg)
COLnames <- colnames(Ozone_reg)
for(i in seq(from = 1, to = N-1, by = 1)){
  for(j in seq(from = i+1, to = N,by = 1)){
    new_col <- paste(COLnames[i],COLnames[j],sep="µ")
    Ozone_inter[[new_col]] <- Ozone_reg[,COLnames[i]]*Ozone_reg[,COLnames[j]]
  }
  Ozone_inter[,COLnames[i]] <- 0
  Ozone_inter[[col]] <- NULL  
}
Ozone_inter[,'x10'] <- NULL
Ozone_inter <- Ozone_inter[, !duplicated(colnames(Ozone_inter))]
Ozone_inter <- Ozone_inter[, colSums(Ozone_inter != 0) > 0]

Ozone_all <- cbind(Ozone_reg,Ozone_sq,Ozone_inter)

y <- Ozone[,11]

Ozone_all <- cbind(Ozone_all,y)

write.csv(Ozone_all, file = "Ozone_all_variables.csv",row.names=FALSE)


#Separe Training data (178 observations) from test data (25 observations) as in the article
set.seed(3)
train_ind <- sample(nrow(Ozone_all), size = 178)
train <- Ozone_all[train_ind, ]
test <- Ozone_all[-train_ind, ]

y <- train[,721]
X <- train[,-721]
########################
#######FUCTIONS#########
########################


#Function that creates gamma (cf article) with only i number of 1's in it
gamma_i = function(X,i){
  if(i>length(X)){
    return("Not going to work")
  }
  gamma <- rep(0, length(X))
  index <- sample(1:length(X),i)
  gamma[index] <- rep(1,i)
  return(gamma)
}

#Function that selects the dataset with the variables chosen by
#having 1's in the gamma vector
X_gamma = function(X,gamma){
  indices <- gamma == 1
  return(X[,indices, drop = FALSE])
}

#Function that gives Bayes Factor for a given subset of X and gamma
BayesF = function(X,y,gamma){
  X_gamma <- X_gamma(X,gamma)
  reg <- paste(colnames(X_gamma), collapse = "+")
  BF <- data.frame(lmBF(as.formula(paste("y~", reg)), data = cbind(X_gamma,y)))
  #BF <- data.frame(lmBF(as.formula(paste("V1~", reg)), data = cbind(X_gamma,y)))
  result <- BF$bf
  model <- rownames(BF)
  return(list("result" = result,"model" = model))
}



#For example, here we chose to calculate the Bayes Factor for gamma in B_4
BayesF(X,y,gamma_i(X,8))
#"x3_2"       "x1_2µx2_25" "x1_8µx2_30" "x1_9µx4"    "x1_11µx9"   "x2_2µx3_1"  "x2_17µx3_4" "x2_21µx5"
#0.08542492

############################
##########################################################
#Finding the Distribution P_hat

prob = rep(1,length(y)-2)
N <- 100000 


bfFull <-data.frame(matrix(ncol = 0, nrow = 5))

for(j in seq(from=1,to=N,by=1)){
  if(j%%1000 == 0){
    print(j)
  }
  i = sample(1:length(prob), size=1, prob=prob)
  X_gam <- X_gamma(X,gamma_i(X,i))
  reg <- paste(colnames(X_gam), collapse = "+")
  bf0 <- as.data.frame(lmBF(as.formula(paste("y~", reg)), data = cbind(X_gam,y)))
  bf0$Group <- i
  
  bfFull <- rbind(bfFull,bf0)
  
}


bfFull[c("error","time","code")] <- NULL

bfFull_bygroup <- aggregate(cbind(posterior_prob = bf) ~ Group, 
                            data = bfFull, 
                            FUN = function(x){sum(x)})

bfFull_bygroup$posterior_prob <- bfFull_bygroup$posterior_prob/sum(bfFull_bygroup$posterior_prob)

bfFull_bygroup_sorted <- bfFull_bygroup[order(-bfFull_bygroup$posterior_prob),]

head_bfFull_sorted <- head(bfFull_bygroup_sorted)

View(head_bfFull_sorted)


bfFull_sbygroup <- bfFull_bygroup_sorted [order(-bfFull_bygroup_sorted$Group, decreasing = T),]

prob_G <- bfFull_sbygroup$posterior_prob

#prob_G is the vector containing the estimate of the posterior distributions 
##########################

####################################################################################################
#####################
#METROPOLIS-HASTINGS#
#####################
####################################################################################################

MH = function(N,n,X,y,prob_G,models){
  #N Simulations
  #n maximum number of variables
  #X, y are the regressors and response variables
  #prob is the vector of probability to chose how many variables we chose among n possibilities
  alphas <-c()
  j <- sample(1:length(prob_G),1)
  gamma_t1 <- gamma_i(X,j)
  BF_t1 <- BayesF(X,y,gamma_t1)
  
  for(t in seq(from=1,to=N,by=1)){
    if(t%%1000 == 0){
      print(t)
    }
    
    
    #Adding model1 (old model) to the datafram
    model1 <- data.frame(BF_t1$model,BF_t1$result)
    models <- rbind(models, model1)
    
    #Choosing B_i, the number of variables
    i = sample(1:length(prob_G),1,prob=prob_G)
    
    #Choosing gamma_t', another model among the B_i chosen
    gamma_t2 = gamma_i(X,i)
    
    #Calculating BayesFactor
    BF_t2 <- BayesF(X,y,gamma_t2)
    model2 <- data.frame(BF_t2$model,BF_t2$result)
    
    #Calculating new prob_G because of
    #iteration
    P_t1 <- 1/(n+1)*1/log(t+1)+prob_G[j]
    P_t2 <- 1/(n+1)*1/log(t+1)+prob_G[i]
    
    #Calculating MH Constant :
    alpha <- min(1,(BF_t2$result/BF_t1$result)*(P_t1/P_t2) )
    
    #Changing models with probability alpha
    if(rbinom(T,1,alpha)){
      gamma_t1 <- gamma_t2
      j <- i
      BF_t1 <- BF_t2
      model_1 <- model2
    }
  }
  names(models)<-c("Model","Bayes Factor")
  return(models)
}



####################################
###RUNNING CODE ON THE OZONE DATA###
####################################


n <- length(y)-2
models <-data.frame(matrix(ncol = 2, nrow = 0))
names(models)<-c("Model","Bayes Factor") 
proba = prob_G
N <- 1000000

MH_models <- MH(N,n,X,y,proba,models)

total <- aggregate(cbind(count = `Bayes Factor`) ~ Model, 
                   data = MH_models, 
                   FUN = function(x){NROW(x)})

total$`Proportion of visits` <- total$count/N
View(head(total))



#Best 6 models
table_best <- total[order(-total$`Proportion of visits`),][1:9,] 
best_models <- lapply(paste0("y~", table_best$Model),
                      function(x) lm(as.formula(x), data = cbind(X,y)))

#Add R squared
table_best$rsquared <- sapply(best_models, function(x) summary(x)$r.squared)
table_best$adjrsquared <- sapply(best_models, function(x) summary(x)$adj.r.squared)

View(head(table_best[1:9,]))

data <- head(total[order(-total$"Proportion of visits"),])

bfFull_bygroup[order(-bfFull_bygroup$posterior_prob),]


xtable(head(table_best[1:9,]))
