##### READING DATA & PACKAGES ####
rm(list=ls())

# Packages
library(RColorBrewer)
library(kohonen)
library(readxl)
library(caTools)
library(xts)
library(plotly)
library(lubridate)
library(Rtsne)
library(ROCR)
library(corrplot)
library(readr)
library(stringr)
library(bit64)
library(xlsx)
library(ggplot2)
library(readr)
library(tibble)
library(data.table)
library(dplyr)
library(compare)
library(reshape2)
library(caret)
library(Boruta)
library(plotly)
# Load data properly
setwd("C:/Users/orteg/Dropbox/Arch Org/1 Almacen/EMDS/ML & AI/3 Cursos/KUL Master Thesis")
df = fread("telco_churn.csv") # so that it interprets blank spaces as NA's

# Own functions
multi.fun <- function(x) {
  c(mean = mean(x),median=median(x), sd=sd(x), mad=mad(x),  
    var.coef=(sd(x)/mean(x)), var.coef2=(mad(x)/median(x)),
    max = max(x), min= min(x),
    skewness= mean((x- mean(x))^3)/(sd(x)^3)
  )      
}
vif = function(df){
  diag(solve(cor(df)))
}
# Evaluation Functions
Eval_Fun_boruta = function(pred_vector, df = scaled_train_df_boruta_10){
  df_test = data.frame(obs = df$CHURN, YES = pred_vector)
  df_test$obs = relevel(df_test$obs,'Churner')
  levels(df_test$obs) = c('YES','NO')
  df_test$pred = factor(ifelse(df_test$YES > 0.5, "YES", "NO"))
  df_test$obs = relevel(df_test$obs,'YES')
  df_test$pred = relevel(df_test$pred,'YES')
  PR = prSummary(df_test, lev = levels(df_test$obs))
  ROC = twoClassSummary(df_test, lev = levels(df_test$obs))
  return(c(PR,ROC))
}
Eval_Fun_boruta_tsne = function(pred_vector, df = scaled_train_df_boruta_10){
  df_test = data.frame(obs = df$CHURN, YES = pred_vector)
  df_test$obs = relevel(df_test$obs,'Churner')
  levels(df_test$obs) = c('YES','NO')
  df_test$pred = factor(ifelse(df_test$YES > 0.50, "YES", "NO"))
  df_test$obs = relevel(df_test$obs,'YES')
  df_test$pred = relevel(df_test$pred,'YES')
  PR = prSummary(df_test, lev = levels(df_test$obs))
  ROC = twoClassSummary(df_test, lev = levels(df_test$obs))
  return(c(PR,ROC))
}

#### PRE-EDA PREPROCESSING ####

# Elimination of redundant variables
df$ACTIVE_WEEKS = NULL
df$ACTIVE_MONTHS = NULL
df$CHURN = as.factor(df$CHURN)
df$CHURN = relevel(df$CHURN,'1')
df$AVG_DATA_1MONTH = as.numeric(df$AVG_DATA_1MONTH/1048576)
df$AVG_DATA_3MONTH = as.numeric(df$AVG_DATA_3MONTH/1048576)
df$ID = NULL

#### DATA PARTITION & SCALING ####

# Training Set
set.seed(123); index = createDataPartition(df$CHURN, p=.70, list = F)
df_tr = df[index,]
df_ts = df[-index,]

# Scale training set and save the scale and center for test set
glimpse(df_tr[,-1])
sapply(df_tr[,-1],multi.fun)
scl_tr = scale(df_tr[,-1]) %>% as_data_frame() # Create new dataframe for scaling
scl_tr_center = attributes(scale(df_tr[,-1]))[['scaled:center']] 
scl_tr_scale = attributes(scale(df_tr[,-1]))[['scaled:scale']]
scl_tr = bind_cols(CHURN = df_tr$CHURN,scl_tr)

# # Auxiliary variables
# x1 = matrix(scl_tr_center,1,length(scl_tr_center)) %>% data.frame(); colnames(x1) = names(scl_tr_center)
# x2 = matrix(scl_tr_scale,1,length(scl_tr_scale)) %>% data.frame(); colnames(x2) = names(scl_tr_scale)

# Transactions
scl_ts = scale(df_ts[,-1], center = scl_tr_center, scale = scl_tr_scale) %>% as_data_frame()
scl_ts = bind_cols(CHURN = df_ts$CHURN, scl_ts)

# Export for Python pt-SNE
# write.xlsx(scl_tr, 'scaled_train_df.xlsx', sheetName="Sheet1")
# write.xlsx(scl_ts, 'scaled_test_df.xlsx', sheetName="Sheet1")

#### EDA ####

## Plots ##

ggplot(df_tr, aes(x = CHURN, y = ACTIVE_DAYS, fill = CHURN )) + 
  geom_boxplot()

ggplot(df_tr, aes(x = CHURN, y = COMPLAINT_2WEEKS, fill = CHURN )) + 
  geom_boxplot()

ggplot(scl_tr, aes(x = CHURN, y = COUNT_SMS_INC_ONNET_6MONTH, fill = CHURN )) + 
  geom_boxplot() 

ggplot(df_tr, aes(x = CHURN, y = AVG_DATA_1MONTH, fill = CHURN )) + 
  geom_boxplot() +
  scale_y_sqrt()

## Skewness and Coef.V ##
apply(df_tr[,3:39],2,multi.fun) %>% data.frame()
barchart(summary[9,], main = 'Graph 2: Skewness', xlab ='')
barchart(summary[5,], main = 'Graph 3: Coefficient of Variation', xlab ='')

# Corr Plot
corplot = corrplot(cor(df_tr[,5:41]),title = "Graph 1: Correlation Plot", method = "square", outline = T, 
                   addgrid.col = "darkgray", mar = c(0,0,1,0),
                   rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", 
                   tl.cex = 0.5, cl.cex = 1)

# VIF plot 
barchart(vif(df_tr[,c(3:29)]))
barchart(vif(df_tr[,c(19:39)]))

## t-SNE BH ##
set.seed(123); tsne2_scl_tr = Rtsne(scl_tr[,-1],
                                    dims = 2, perplexity = 30, verbose = TRUE,
                                    check_duplicates = F, max_iter = 5000)

set.seed(123); tsne3D_scl_tr = Rtsne(scl_tr[,-1],
                                    dims = 3, perplexity = 30, verbose = TRUE,
                                    check_duplicates = F, max_iter = 5000)

tsne2_scl_tr = as_data_frame(tsne2_scl_tr$Y)
tsne3_scld_tr = as_data_frame(tsne3D_scl_tr$Y)

# Plots t-SNE 2D
ggplot(bind_cols(CHURN=scl_tr$CHURN,tsne2_scl_tr), aes(x=V1, y=V2)) +
  geom_point(size=1, aes(col = CHURN)) +
  xlab("") + ylab("") +
  ggtitle("Graph 4: t-SNE") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(bind_cols(CHURN=scl_tr$CHURN,tsne2_scl_tr), aes(x=V1, y=V2)) +
  geom_point(size=2, aes(shape = CHURN, col = MINUTES_INC_OFFNET_3MONTH)) +
  scale_colour_gradient(name  ="Minutes of incoming ofnet calls \n in last 3 months", low = 'white', high = 'red') + xlab("") + ylab("") +
  ggtitle("Graph 6: t-SNE with Minutes of incoming ofnet calls in last 3 months") + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'dark gray', colour = 'black')) +
  scale_shape_manual(values = c(4, 1))

ggplot(bind_cols(CHURN=scl_tr$CHURN,tsne2_scl_tr), aes(x=V1, y=V2)) +
  geom_point(size=2, aes(shape = CHURN, col = AVG_MINUTES_OUT_OFFNET_1MONTH)) +
  scale_colour_gradient2() + xlab("") + ylab("") +
  ggtitle("t-SNE") + 
  theme(axis.text.x=element_blank(),axis.text.y=element_blank()) +
  scale_shape_manual(values = c(67, 4))

# Plots t-SNE 3D

plot_ly(data.table(tsne3_scld_tr), x = ~V1, y = ~V2, z = ~V3, 
        color = ~df_tr$CHURN, colors = c('#FFE1A1', '#683531')) %>%
        add_markers() %>% layout(scene = list(xaxis = list(title = 'V1'),
                                yaxis = list(title = 'V2'),
                                zaxis = list(title = 'V3')))

#### FEATURE SELECTION: BORUTA ####

# Estimating Boruta and its plot
set.seed(123); bor = Boruta(CHURN~., data = scl_tr, doTrace = 2)
plot(boruta, xlab = "", xaxt = "n")
lz=lapply(1:ncol(boruta$ImpHistory),function(i)
  boruta$ImpHistory[is.finite(boruta$ImpHistory[,i]),i])
names(lz) = colnames(boruta$ImpHistory)
Labels = sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta$ImpHistory), cex.axis = 0.7)
rm(list = c('lz','Labels'))

# The ranked list of most relevant features
featlist = attStats(boruta) %>% rownames_to_column(var = 'Features') %>% 
          select(Features, medianImp) %>% mutate(rank = rank(-medianImp)) %>% arrange(rank)

# Checking multicollinearity between the top boruta ranking
  # the 7th and 9th have cor = 1 with the 6th and 8th.
df_tr %>% select(featlist$Features[c(1:6,8,10:12)]) %>% vif()
df_tr %>% select(featlist$Features[c(1:6,8,10:12)]) %>% cor() %>% corrplot()

# Create the best set of predictors with no multicollinearity
scl_tr_bor_10 = scl_tr %>% select(CHURN, featlist$Features[c(1:6,8,10:12)])
scl_tr_bor_5 = scl_tr %>% select(CHURN, featlist$Features[1:5])


#### DIMENSIONALITY REDUCTION: t-SNE & PCA & SOM ####

## PCA ##
pca_tr = prcomp(scl_tr[,-1], center = F)
autoplot(pca_tr, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) + 
          geom_point(aes(colour = scl_tr_bor_10$CHURN))

#proportion of variance explained
prop_varex = (pca_tr$sdev)^2/sum((pca_tr$sdev)^2)
sum(prop_varex[1:2])

# Predict PCA on test set
pca_ts = predict(pca_tr,newdata = scl_ts[,-1])
scl_ts = bind_cols(scl_ts, as_data_frame(pca_ts[,1:5]))

## PARAMETRIC t-SNE ##
ptsne = read_excel("ptsne_tr.xlsx")
ptsne_5 = read_excel("ptsne_tr_5.xlsx")
## SOM ##

# Training SOM
som_dim = floor(sqrt(5*sqrt(nrow(scl_tr)))) # Vesanto's Rule for number of celss
somgrid_tr = somgrid(xdim = som_dim*0.9, ydim= som_dim, topo="hexagonal") # non-symmetrical
som_tr = som(as.matrix(scl_tr[,-1]), grid = somgrid_tr)
plot(som_tr, type="count")

# Training Boruta 10 with SOM
som_vars_tr = data_frame(Xsom = som_tr$grid$pts[som_tr$unit.classif,'x'], 
           Ysom = som_tr$grid$pts[som_tr$unit.classif,'y'])

# Predict SOM on test set
som_ts = predict(som_tr,newdata = as.matrix(scl_ts[,-1]))
som_vars_ts = data_frame(Xsom = som_tr$grid$pts[som_ts$unit.classif,'x'], 
                      Ysom = som_tr$grid$pts[som_ts$unit.classif,'y'])
som_scl_ts = bind_cols(scl_ts, som_vars_ts)
rm(som_vars_ts)

#### FEATURE SELECTION WITH NEW MAPPINGS ####

# Boruta
scl_tr2 = bind_cols(scl_tr, ptsne, som_vars_tr,  as_data_frame(pca_tr$x[,1:5]))
set.seed(123); bor2 = Boruta(CHURN~., data = scl_tr2, doTrace = 1)

# Plot
plot(bor2, xlab = "", xaxt = "n", main = 'Graph 7: Relevant variables ranked by Boruta')
lz=lapply(1:ncol(bor2$ImpHistory),function(i)
  bor2$ImpHistory[is.finite(bor2$ImpHistory[,i]),i])
names(lz) = colnames(bor2$ImpHistory)
Labels = sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(bor2$ImpHistory), cex.axis = 0.6)
rm(list = c('lz','Labels'))

# The ranked list of most relevant features
featlist2 = attStats(bor2) %>%
  rownames_to_column(var = 'Features')
featlist2 = subset(featlist2, select = c('Features','medianImp')) %>%
  mutate(rank = rank(-featlist2$medianImp)) %>% arrange(rank)

# Correlation Plot between the dimensionality reducers and original variables
corrplot(cor(scl_tr2 %>% select(featlist2[1:30,1])),
        title = "Graph 2: Correlation Plot", method = "square", outline = T, 
        addgrid.col = "darkgray", mar = c(0,0,1,0),
        rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", 
        tl.cex = 0.5, cl.cex = 1)

scl_tr2_bor_10 = scl_tr2 %>% select(CHURN,featlist2$Features[c(1:6,8,10,11,13)],X,Y,PC1,PC2,Xsom,Ysom)

rm(list = c('ptsne'))

#### CLASSIFICATION PREDICTION ####

## 10D ##

levels(scl_tr2_bor_10$CHURN) = c("Churner","No_Churner")
train_control = trainControl(method="cv",number=10,savePredictions=TRUE,summaryFunction=twoClassSummary,
                              classProbs=TRUE)
# Original Variables
set.seed(123); lg_10 = train(CHURN ~ ., data = scl_tr2_bor_10[,-c(12:17)], method = "glm", family="binomial",
                             trControl=train_control,metric='ROC')

set.seed(123); dt_10 = train(CHURN ~ ., data = scl_tr2_bor_10[,-c(12:17)], 
                                method='rpart',trControl=train_control,tuneLength = 10,
                                metric='ROC')

set.seed(123); rf_10 = train(CHURN ~ ., data = scl_tr2_bor_10[,-c(12:17)], 
                          method='ranger', trControl=train_control,
                          tuneGrid= expand.grid(mtry=c(2:length(scl_tr2_bor_10[,-c(11:17)])),
                                                min.node.size = c(5,10), splitrule = 'gini'), 
                          metric = 'ROC')

knn_10 = train(CHURN ~ ., data = scl_tr2_bor_10[,-c(12:17)], method = "knn", trControl = train_control,
               metric = 'ROC',tuneLength = 20)

# Evaluate performance in training set
pred_10 = data_frame(predict(lg_10, type="prob")[,1], predict(dt_10, type="prob")[,1], 
                     predict(rf_10, type="prob")[,1], predict(knn_10, type="prob")[,1])
colnames(pred_10) = c('logistic','d_tree','random_forest','KNN')
boxplot(pred_10)
colAUC(pred_10,scl_tr$CHURN, plotROC = T)

## 5D ##

train_control = trainControl(method="cv",number=10,savePredictions=TRUE,summaryFunction=twoClassSummary,
                             classProbs=TRUE)
# Original Variables
set.seed(123); lg_5 = train(CHURN ~ ., data = scl_tr2_bor_10[,-c(7:17)], method = "glm", family="binomial",
                             trControl=train_control,metric='ROC')

set.seed(123); dt_5 = train(CHURN ~ ., data = scl_tr2_bor_10[,-c(7:17)], 
                             method='rpart',trControl=train_control,tuneLength = 10,
                             metric='ROC')

set.seed(123); rf_5 = train(CHURN ~ ., data = scl_tr2_bor_10[,-c(7:17)], 
                             method='ranger', trControl=train_control,
                             tuneGrid= expand.grid(mtry=c(2:length(scl_tr2_bor_10[,-c(1,7:17)])),
                                                   min.node.size = c(5,10), splitrule = 'gini'), 
                             metric = 'ROC')

set.seed(123); knn_5 = train(CHURN ~ ., data =  scl_tr2_bor_10[,-c(7:17)], method = "knn", trControl = train_control,
               metric = 'ROC',tuneLength = 20)


# Evaluate performance in training set
pred_5 = data_frame(predict(lg_5, type="prob")[,1], predict(dt_5, type="prob")[,1], 
                     predict(rf_5, type="prob")[,1], predict(knn_5, type="prob")[,1])
colnames(pred_5) = c('logistic','d_tree','random_forest','KNN')
boxplot(pred_5)
colAUC(pred_5,scl_tr$CHURN, plotROC = T)

## Dimension Reducer: ptsne 2D ##

set.seed(123); lg_ptsne_2 = train(CHURN ~ ., data = scl_tr2_bor_10[,c(1,12:13)], method = "glm", family="binomial",
                            trControl=train_control,metric='ROC')

set.seed(123); dt_ptsne_2 = train(CHURN ~ ., data = scl_tr2_bor_10[,c(1,12:13)], 
                            method='rpart',trControl=train_control,tuneLength = 10,
                            metric='ROC')

set.seed(123); rf_ptsne_2 = train(CHURN ~ ., data = scl_tr2_bor_10[,c(1,12:13)], 
                            method='ranger', trControl=train_control,
                            tuneGrid= expand.grid(mtry=c(2:length(scl_tr2_bor_10[,c(12:13)])),
                                                  min.node.size = c(5,10), splitrule = 'gini'), 
                            metric = 'ROC')

set.seed(123); knn_ptsne_2 = train(CHURN ~ ., data =  scl_tr2_bor_10[,c(1,12:13)],
                                   method = "knn", trControl = train_control, metric = 'ROC',tuneLength = 20)


# Evaluate performance in training set
pred_ptsne_2 = data_frame(predict(lg_ptsne_2, type="prob")[,1], predict(dt_ptsne_2, type="prob")[,1], 
                    predict(rf_ptsne_2, type="prob")[,1], predict(knn_ptsne_2, type="prob")[,1])
colnames(pred_ptsne_2) = c('logistic','d_tree','random_forest','KNN')
boxplot(pred_ptsne_2)
colAUC(pred_ptsne,scl_tr$CHURN, plotROC = T)

## Dimension Reducer: pca 2D ##

set.seed(123); lg_pca_2 = train(CHURN ~ ., data = scl_tr2_bor_10[,c(1,14:15)], method = "glm", family="binomial",
                                trControl=train_control,metric='ROC')

set.seed(123); dt_pca_2 = train(CHURN ~ ., data = scl_tr2_bor_10[,c(1,14:15)], 
                                method='rpart',trControl=train_control,tuneLength = 10,
                                metric='ROC')

set.seed(123); rf_pca_2 = train(CHURN ~ ., data = scl_tr2_bor_10[,c(1,14:15)], 
                                method='ranger', trControl=train_control,
                                tuneGrid= expand.grid(mtry=c(2:length(scl_tr2_bor_10[,c(14:15)])),
                                                      min.node.size = c(5,10), splitrule = 'gini'), 
                                metric = 'ROC')

set.seed(123); knn_pca_2 = train(CHURN ~ ., data =  scl_tr2_bor_10[,c(1,14:15)], method = "knn", trControl = train_control,
                                 metric = 'ROC',tuneLength = 20)


# Evaluate performance in training set
pred_pca_2 = data_frame(predict(lg_pca_2, type="prob")[,1], predict(dt_pca_2, type="prob")[,1], 
                        predict(rf_pca_2, type="prob")[,1], predict(knn_pca_2, type="prob")[,1])
colnames(pred_pca_2) = c('logistic','d_tree','random_forest','KNN')
boxplot(pred_pca_2)
colAUC(pred_pca_2,scl_tr$CHURN, plotROC = T)

## Dimension Reducer: som 2D ##

set.seed(123); lg_som_2 = train(CHURN ~ ., data = scl_tr2_bor_10[,c(1,16:17)], method = "glm", family="binomial",
                              trControl=train_control,metric='ROC')

set.seed(123); dt_som_2 = train(CHURN ~ ., data = scl_tr2_bor_10[,c(1,16:17)], 
                              method='rpart',trControl=train_control,tuneLength = 10,
                              metric='ROC')

set.seed(123); rf_som_2 = train(CHURN ~ ., data = scl_tr2_bor_10[,c(1,16:17)], 
                              method='ranger', trControl=train_control,
                              tuneGrid= expand.grid(mtry=c(2:length(scl_tr2_bor_10[,c(16:17)])),
                                                    min.node.size = c(5,10), splitrule = 'gini'), 
                              metric = 'ROC')

set.seed(123); knn_som_2 = train(CHURN ~ ., data =  scl_tr2_bor_10[,c(1,16:17)], method = "knn", 
                                 trControl = train_control, metric = 'ROC',tuneLength = 20)


# Evaluate performance in training set
pred_som_2 = data_frame(predict(lg_som_2, type="prob")[,1], predict(dt_som_2, type="prob")[,1], 
                      predict(rf_som_2, type="prob")[,1], predict(knn_som_2, type="prob")[,1])
colnames(pred_som_2) = c('logistic','d_tree','random_forest','KNN')
boxplot(pred_som_2)
colAUC(pred_som_2,scl_tr$CHURN, plotROC = T)

## Dimension Reducer: pca 5D ##
set.seed(123); lg_pca_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, as_data_frame(pca_tr$x[,1:5])),
                                  method = "glm", family="binomial",
                                  trControl=train_control,metric='ROC')

set.seed(123); dt_pca_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, as_data_frame(pca_tr$x[,1:5])), 
                                  method='rpart',trControl=train_control,tuneLength = 10,
                                  metric='ROC')

set.seed(123); rf_pca_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, as_data_frame(pca_tr$x[,1:5])), 
                                  method='ranger', trControl=train_control,
                                  tuneGrid= expand.grid(mtry=c(2:5),
                                                        min.node.size = c(5,10), splitrule = 'gini'), 
                                  metric = 'ROC')

set.seed(123); knn_pca_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, as_data_frame(pca_tr$x[,1:5])),
                                   method = "knn", trControl = train_control, metric = 'ROC',tuneLength = 20)

# Evaluate performance in training set
pred_pca_5 = data_frame(predict(lg_pca_5, type="prob")[,1], predict(dt_pca_5, type="prob")[,1], 
                          predict(rf_pca_5, type="prob")[,1], predict(knn_pca_5, type="prob")[,1])
colnames(pred_pca_5) = c('logistic','d_tree','random_forest','KNN')
boxplot(pred_pca_5)
colAUC(pred_pca_5,scl_tr$CHURN, plotROC = T)

## Dimension Reducer: ptsne 5D ##
set.seed(123); lg_ptsne_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, ptsne_5),
                                  method = "glm", family="binomial",
                                  trControl=train_control,metric='ROC')

set.seed(123); dt_ptsne_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, ptsne_5), 
                                  method='rpart',trControl=train_control,tuneLength = 10,
                                  metric='ROC')

set.seed(123); rf_ptsne_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, ptsne_5), 
                                method='ranger', trControl=train_control,
                                tuneGrid= expand.grid(mtry=c(2:5),
                                                      min.node.size = c(5,10), splitrule = 'gini'), 
                                metric = 'ROC')

set.seed(123); knn_ptsne_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, ptsne_5),
                                   method = "knn", trControl = train_control, metric = 'ROC',tuneLength = 20)

# Evaluate performance in training set
pred_ptsne_5 = data_frame(predict(lg_ptsne_5, type="prob")[,1], predict(dt_ptsne_5, type="prob")[,1], 
                        predict(rf_ptsne_5, type="prob")[,1], predict(knn_ptsne_5, type="prob")[,1])
colnames(pred_ptsne_5) = c('logistic','d_tree','random_forest','KNN')
boxplot(pred_ptsne_5)
colAUC(pred_ptsne_5,scl_tr$CHURN, plotROC = T)

## Dimension Reducer: ptsne 5D ##
set.seed(123); lg_ptsne_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, ptsne_5),
                                  method = "glm", family="binomial",
                                  trControl=train_control,metric='ROC')

set.seed(123); dt_ptsne_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, ptsne_5), 
                                  method='rpart',trControl=train_control,tuneLength = 10,
                                  metric='ROC')

set.seed(123); rf_ptsne_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, ptsne_5), 
                                  method='ranger', trControl=train_control,
                                  tuneGrid= expand.grid(mtry=c(2:5),
                                                        min.node.size = c(5,10), splitrule = 'gini'), 
                                  metric = 'ROC')

set.seed(123); knn_ptsne_5 = train(CHURN ~ ., data = bind_cols(CHURN = scl_tr2_bor_10$CHURN, ptsne_5),
                                   method = "knn", trControl = train_control, metric = 'ROC',tuneLength = 20)

# Evaluate performance in training set
pred_ptsne_5 = data_frame(predict(lg_ptsne_5, type="prob")[,1], predict(dt_ptsne_5, type="prob")[,1], 
                          predict(rf_ptsne_5, type="prob")[,1], predict(knn_ptsne_5, type="prob")[,1])
colnames(pred_ptsne_5) = c('logistic','d_tree','random_forest','KNN')
boxplot(pred_ptsne_5)
colAUC(pred_ptsne_5,scl_tr$CHURN, plotROC = T)

#### TEST SET EVALUATION ####

# Evaluate performance in training set
pred_10_ts = data_frame(predict(lg_10, newdata = scl_ts, type="prob")[,1],
                        predict(dt_10, newdata = scl_ts, type="prob")[,1], 
                        predict(rf_10, newdata = scl_ts, type="prob")[,1],
                        predict(knn_10, newdata = scl_ts, type="prob")[,1])
colnames(pred_10_ts) = c('logistic','d_tree','random_forest','KNN')
boxplot(pred_10_ts)
colAUC(pred_10_ts,scl_ts$CHURN, plotROC = T)

Eval_Fun_10 = function(pred_vector, df = scl_tr2_bor_10){
  df_test = data.frame(obs = df$CHURN, YES = pred_vector)
  df_test$obs = relevel(df_test$obs,'Churner')
  levels(df_test$obs) = c('YES','NO')
  df_test$pred = factor(ifelse(df_test$YES > median(pred_vector), "YES", "NO"))
  df_test$obs = relevel(df_test$obs,'YES')
  df_test$pred = relevel(df_test$pred,'YES')
  PR = prSummary(df_test, lev = levels(df_test$obs))
  ROC = twoClassSummary(df_test, lev = levels(df_test$obs))
  return(c(PR,ROC))
}
apply(pred_10,2,Eval_Fun_10)

# Loop for creating the list of df 
list_lift_10 = list()
for (i in names(list_lift_10)){
  Class_bin = ifelse(df_tr$CHURN == 'Churner', 1, 0)
  list_lift_10[[i]] = data.frame(Class = Class_bin, 
                                 data.frame(pred_5D), cum = 1, perpop = 1)
  list_lift_10[[i]] = list_lift_10[[i]][order(list_lift_10[[i]][[i]], decreasing = T),]
  list_lift_10[[i]][['cum']] = 100*cumsum(list_lift_10[[i]][['Class']])/sum(list_lift_10[[i]][['Class']])
  list_lift_10[[i]][['perpop']] = (seq(nrow(list_lift_10[[i]]))/nrow(list_lift_10[[i]]))*100
} 
rm(Class_bin)
# Ploting Skimming Plot
skimming_10 = ggplot(data = NULL, aes(cum, perpop, color = Models)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = list_lift_5D[['logistic']],  aes(color = 'Logistic')) +
  geom_line(data = list_lift_5D[['LMT']], aes(color = 'LMT')) +
  geom_line(data = list_lift_5D[['random_forest']], aes( color = 'RF')) +
  geom_line(data = list_lift_5D[['XGB']], aes(color = 'XGB')) +
  geom_line(data = list_lift_5D[['d_tree']], aes(color = 'decision_tree')) +
  geom_line(data = list_lift_5D[['stump_lgboost']], aes(color = 'logitboost')) +
  geom_line(data = list_lift_5D[['KNN']], aes(color = 'KNN')) +
  labs(title = "Skimming-the-cream Plot", x ="% of Churners", y="% of Customers") +
  theme(legend.position = c(0.2,0.65),legend.direction = 'vertical') +
  scale_y_continuous(breaks=seq(0,80,20), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(0,100,20)) +
  geom_vline(xintercept = 95, linetype = 'dotdash' ) +
  annotate("text", x = 98.5, y = 59, label = "95%", size = 4) +
  geom_vline(xintercept = 85, linetype = 'dotdash' ) +
  annotate("text", x = 88.5, y = 59, label = "85%", size = 4) +
  coord_cartesian(ylim = c(0, 80))
