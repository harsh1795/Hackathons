## setting directory
setwd("C:/Users/Harshit/Desktop/Analytics Competitions/AMEX")

library(h2o)
## starting cluster
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "4G")

h2o.removeAll()
df1 <- h2o.importFile(path = "tra1.csv")
df2 <- h2o.importFile(path = "tra2.csv")
#df3 <- h2o.importFile(path = "trainsplit1.csv")
#df4 <- h2o.importFile(path = "train22.csv")
lead1 = h2o.importFile(path = "vali1.csv")
lead2 = h2o.importFile(path = "vali2.csv")
#lead3 = h2o.importFile(path = "validsplit1.csv")
#lead4 = h2o.importFile(path = "test22.csv")
df1$C1=NULL
df2$C1=NULL
df3$C1 = NULL
df4$C1 = NULL
lead1$C1=NULL
lead2$C1=NULL
lead3$C1 = NULL
lead4$C1 = NULL
df1$X = NULL
df2$X = NULL
df3$X = NULL
df4$X = NULL
lead1$X = NULL
lead2$X = NULL
lead3$X = NULL
lead4$X = NULL
df1$ind1=NULL
df2$ind1=NULL
df3$ind1=NULL
lead1$ind1=NULL
lead2$ind1=NULL
lead3$ind=NULL
df1$ind = df1$lev_pref1-df1$lev_pref2
df2$ind = df2$lev_pref1-df2$lev_pref2
lead1$ind = lead1$lev_pref1 - lead1$lev_pref2
lead2$ind = lead2$lev_pref1 - lead2$lev_pref2


View(df1)
###############################################################################
new_features_test= c('Cit_pref1','Cit_pref2','city_change','ZIP_pref1','ZIP_pref2','mvar34','mvar35','mvar29','lev_pref1','lev_pref2','ZIP_lev_pref1','ZIP_lev_pref2','mvar27','mvar28','mvar31')
gg = df[,new_features_test]

###############################################################################
splits <- h2o.splitFrame(
  df2,           ##  splitting the H2O frame we read above
  c(0.8),   ##  create splits of 60% and 20%; 
  ##  H2O will create one more split of 1-(sum of these parameters)
  ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0.2
  seed=1234) 

train <- h2o.assign(splits[[1]], "train.hex") 
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[2]], "test.hex")
train[1:5,]




finalRf_predictions<-h2o.predict(
  object = m1
  ,newdata = lead2)
y = as.data.frame(finalRf_predictions$predict)
write.csv(x,"lead11.csv")
mean(finalRf_predictions$predict==test$actual_vote) ## test set accuracy

dat1 = h2o.cbind(lead1$citizen_id,finalRf_predictions$predict)
dat2 = h2o.cbind(lead2$citizen_id,finalRf_predictions$predict)
dat3 = h2o.cbind(lead3$citizen_id,finalRf_predictions$predict)
dat4 = h2o.cbind(lead4$citizen_id,finalRf_predictions$predict)

dat = h2o.rbind(dat1,dat2)
dat = h2o.rbind(dat,dat3)
dat = h2o.rbind(dat,dat4)

dat = as.data.frame(dat)
write.csv(dat,"fina.csv")

dat2 = h2o.cbind(dat2,lead2$party_voted_past)
data2 = as.data.frame(dat2)
View(data2)
length(which(data2$predict==data2$Cit_pref1))
#    1     #####################################################################################
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = df2,        ## the H2O frame for training
  #validation_frame = valid,      ## the H2O frame for validation (not required)
  x=c(2:33,37:52),                        ## the predictor columns, by column index
  y=36,                          ## the target index (what we are predicting)
  model_id = "rf_v1",            ## name the model in H2O
  ntrees = 250,
  mtries = 7,
  nbins = 100,                    ## use a maximum of 200 trees to create the
  #stopping_rounds = 3,           ## Stop fitting new trees when the 2-tree
  score_each_iteration = T)
#    2    #################################################################################
rf2 <- h2o.randomForest(        ##
  training_frame = train,       ##
  #validation_frame = valid,     ##
  x=c(1:32,34:68),                       ##
  y=33,                         ##
  model_id = "rf_covType2",     ## 
  ntrees = 500,                 ##
  max_depth = 30,               ## Increase depth, from 20
  stopping_rounds = 2, 
  mtries = 15,                  ##
  stopping_tolerance = 1e-2,    ##
  score_each_iteration = T)
#    2    #################################################################################
gbm1 <- h2o.gbm(
  training_frame = df,        ## the H2O frame for training
  #validation_frame = valid,      ## the H2O frame for validation (not required)
  x=1:32,                        ## the predictor columns, by column index
  y=35,                          ## the target index (what we are predicting)
  model_id = "gbm_1",            ## name the model in H2O
  seed = 2000000) 
summary(gbm1)
#    3     ################################################################################
gbm2 <- h2o.gbm(
  training_frame = df2,        ##
  #validation_frame = valid,      ##
  x=c(3:33,35,36,50:69),                        ##
  y=34,                          ## 
  ntrees = 200,                  ## decrease the trees, mostly to allow for
                                 ##run time (from 50)
  learn_rate = 0.05,              ## increase the learning rate (from 0.1)
  max_depth = 20,                ## increase the depth (from 5)
  stopping_rounds = 2,           ## 
  stopping_tolerance = 0.01,     ##
  score_each_iteration = T,      ##
  model_id = "gbm_2")
#    4    ############################################################################
## deep learning
response <- "actual_vote"
n = c("mvar1","mvar2","mvar3","mvar4","mvar5","mvar6","mvar7","mvar8","mvar9",
    "mvar10","mvar11","mvar12","mvar13","mvar14","mvar15","mvar16","mvar17",
    "mvar18","mvar19","mvar20","mvar21","mvar22","mvar23","mvar24","mvar25",
    "mvar27","mvar28","mvar29","mvar30","mvar31", "mvar34","mvar35",
    "lev_pref2","Cit_pref2","city_change","ZIP_pref1","ZIP_pref2",
    "Ebony","Odyssey","Tokugawa","Cosmos","Centaur","ZIP_lev_pref1","ZIP_lev_pref2")
predictors <- setdiff(n, response)
predictors
m1 <- h2o.deeplearning(
  model_id="dl_model_first", 
  training_frame=train, 
  validation_frame=valid,   ## validation dataset: used for scoring and early stopping
  x=predictors,
  y=response,
  #activation="Rectifier",  ## default
  #hidden=c(200,200),       ## default: 2 hidden layers with 200 neurons each
  epochs=1,
  variable_importances=T    ## not enabled by default
)
summary(m1)
head(as.data.frame(h2o.varimp(m1)))
#    5     #################################################################################
m2 <- h2o.deeplearning(
  #validation_frame=valid,
  model_id="dl_model_faster", 
  training_frame=df, 
  x=predictors,
  y=response,
  hidden=c(100,120,100),                  ## small network, runs faster
  epochs=100000,                      ## hopefully converges earlier...
  score_validation_samples=10000,      ## sample the validation dataset (faster)
  stopping_rounds=2,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.01
)
summary(m2)
plot(m2)
#    6    #################################################################################
m3 <- h2o.deeplearning(
  model_id="dl_model_tuned", 
  training_frame=train, 
  validation_frame=valid, 
  x=predictors, 
  y=response, 
  overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
  hidden=c(128,128,128),          ## more hidden layers -> more complex interactions
  epochs=30,                      ## to keep it short enough
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  rate=0.01, 
  rate_annealing=2e-6,            
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7, 
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  max_w2=10                       ## helps stability for Rectifier
) 
summary(m3)
plot(m3)
#    7    #################################################################################

#    8    ############################################################################
hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=list(c(50,20,20),c(50,50,100)),
  input_dropout_ratio=c(0,0.05),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6)
)
hyper_params

## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_random",
  training_frame=train,
  #validation_frame=valid, 
  x=predictors, 
  y=response,
  epochs=1000,
  stopping_metric="logloss",
  stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria
)                                
grid <- h2o.getGrid("dl_grid_random",sort_by="logloss",decreasing=FALSE)
grid

grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
best_model
##################################################################################
x = c(2:33,37,38:45)
y = 36
m1 = h2o.glm(training_frame = df2,x = x,y = y,family='multinomial',solver='L_BFGS')
m2 = h2o.glm(training_frame = df1,x = x, y = y,family='multinomial',solver='L_BFGS',lambda=1e-2)
##

#####################################################################################################
df
pc <- princomp(model.matrix(~.+0, data=df))
plot(pc)

