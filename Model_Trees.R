#### LOADING FUNCTIONS & DATA ####
rm(list = ls())
library(forcats)
library(FNN) # used
library(varhandle) # used
library(rlang)
library(Rtsne)
library(ROCR)
library(VIM)
library(PresenceAbsence) # used
library(stringr)
library(caret) # used
library(bit64) # used
library(caTools) #used
library(fmsb)
library(RSKC) #used
library(sparcl) #used alternatively to RSKC
library(factoextra)
library(cluster)
library(NbClust)
library(Fdplyr::selector) # used
library(data.table) # used
library(compare)
library(devtools) # used
library(mice) # used
library(Boruta) # used
library(smotefamily) # used
library(readxl)
library(tidyverse) # used
environment(arrange)


# Function that identifies if the variable can be used as factor
is.whole = function(x){
  ifelse(all(is.character(x)),FALSE,all(ifelse(as.integer(x)%%1==0,TRUE,FALSE),na.rm = TRUE))
}
can.factor = function(x){
  ifelse(is.character(x)|is.logical(x)|(length(unique(x))<15&is.whole(x)),TRUE,FALSE)
}

# VIF: Calculates the VIF
vif = function(df){
  diag(solve(cor(df,use = 'complete.obs')))
}

num = function(col){
  as.numeric(gsub(",", "", col))
}
# Extraction tool
extr = function(scenario){
  df=data_scenario[data_scenario$scenario==scenario,] # filtering the main Scenario DF on dplyr::selected scenario
  df=df[, colSums(df != "") != 0] # Eliminating empty columns
  df$scenario=NULL # Just droping this which is already redundant
  df=apply(df,2,num)%>%as_data_frame() # making all characters / factors into numeric
  colnames(df)[-1] = paste(colnames(df)[-1], deparse(scenario), sep = "_") # Adding suffix depending on the scenario
  data2 = left_join(data3,df,by = 'index')
  assign("data3", data2, envir = .GlobalEnv)
}

multi.fun  =  function(x) {
  c(mean = mean(x),median=median(x), sd=sd(x), mad=mad(x),
    IQR = IQR(x),  
    var.coef=(sd(x)/mean(x)), 
    var.coef2=(IQR(x)/median(x)),
    max = max(x), min= min(x),
    skewness= mean((x- mean(x))^3)/(sd(x)^3)
  )      
}

# Mode Function
Mode = function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux = unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

Mode2 = function(x) {
  ux = unique(x)
  if(!anyDuplicated(x)){
    NA_character_ } else { 
      tbl =  tabulate(match(x, ux))
      toString(ux[tbl==max(tbl)])
    }
}

median_mode = function(x){
  ifelse(is.numeric(x),median(x),Mode2(x))
}

# Load data properly
setwd("C:/Users/orteg/Documents/GitHub/datathon/Credit")
df = read_excel("Credit_DataSet.xlsx", sheet = 2,
                na=c(""," ",".","NA","#N/A",'#N/A N/A')) # so that it interprets blank spaces as NA's

#### DATA CLEANING ####

glimpse(df)
df$ID = NULL
df$COUNTRY_NAME = NULL

# Currency

dfaux = df[is.na(df$CURRENCY),]      

# Manual Lookup [Daniel]
dfaux[dfaux$COUNTRY=="AT",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="BE",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="BM",]$CURRENCY = "BMD"
dfaux[dfaux$COUNTRY=="CA",]$CURRENCY = "CAD"
dfaux[dfaux$COUNTRY=="CH",]$CURRENCY = "CHF"
dfaux[dfaux$COUNTRY=="CN",]$CURRENCY = "CNY"
dfaux[dfaux$COUNTRY=="DE",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="DK",]$CURRENCY = "DKK"
dfaux[dfaux$COUNTRY=="ES",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="FI",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="FR",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="GB",]$CURRENCY = "GBP"
dfaux[dfaux$COUNTRY=="GR",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="IE",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="IL",]$CURRENCY = "ILS"
dfaux[dfaux$COUNTRY=="IN",]$CURRENCY = "INR"
dfaux[dfaux$COUNTRY=="IT",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="JE",]$CURRENCY = "GBP"
dfaux[dfaux$COUNTRY=="JP",]$CURRENCY = "JPY"
dfaux[dfaux$COUNTRY=="KY",]$CURRENCY = "KYD"
dfaux[dfaux$COUNTRY=="LU",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="MC",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="NL",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="NO",]$CURRENCY = "NOK"
dfaux[dfaux$COUNTRY=="PR",]$CURRENCY = "USD"
dfaux[dfaux$COUNTRY=="PT",]$CURRENCY = "EUR"
dfaux[dfaux$COUNTRY=="SE",]$CURRENCY = "SEK"
dfaux[dfaux$COUNTRY=="TH",]$CURRENCY = "THB"
dfaux[dfaux$COUNTRY=="US",]$CURRENCY = "USD"
dfaux[dfaux$COUNTRY=="ZA",]$CURRENCY = "ZAR"

# Add again on main df
df  =  rbind(dfaux, df[!is.na(df$CURRENCY),])
df$CURRENCY = as.factor(df$CURRENCY)

#Change strange names to euros
df$CURRENCY[df$CURRENCY %in% c('EDM','EES','EIL','EFR','EIP','EFM','EDG','EPE','EAS','EBF','EGD','EUR')] = 'EUR'

df.stand = df
df.stand$MARKETCAP = df.stand$MARKETCAP/1000000
df.stand[,c(22,24:30)] = df.stand[,c(22,24:30)]/df.stand$TOTAL_ASSETS
df.stand$TOTAL_ASSETS = NULL
df = df.stand
rm(df.stand)


# Missing Values
df$PCT_WOMEN_EMPLOYEES = NULL
df$PCT_WOMEN_MGT = NULL
df$ETHICS_POLICY[is.na(df$ETHICS_POLICY)] = 'N'
df$WHISTLE_BLOWER_POLICY[is.na(df$WHISTLE_BLOWER_POLICY)] = 'N'
df$BRIBERY_POLICY[is.na(df$BRIBERY_POLICY)] = 'N'


# Missing Plot on DF [Warning: Data Leaking]
missingness = aggr(df, col=c('navyblue','yellow'),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(df_tr), cex.axis=.7,
                   gap=3, ylab=c("Missing data","Pattern"))

# Missing Values
df$EMPL_GROWTH[is.na(df$EMPL_GROWTH)] = median(df$EMPL_GROWTH,na.rm = TRUE)
df$PCT_CHG_1_YEAR[is.na(df$PCT_CHG_1_YEAR)] = median(df$PCT_CHG_1_YEAR,na.rm = TRUE)
df$VOLATILITY_180D[is.na(df$VOLATILITY_180D)] = median(df$VOLATILITY_180D,na.rm = TRUE)
df$PCT_CHG_6_M[is.na(df$PCT_CHG_6_M)] = median(df$PCT_CHG_6_M,na.rm = TRUE)


#### DATA WRANGLING ####

## DATA TRANSFORMATION: Making seemingly numeric variables as such, others can be factors
vector_attr = attributes(which(apply(apply(df,2,check.numeric),2,all)))[['names']] # Identify numeric-able
df = df %>% mutate_if(names(df) %in% vector_attr,as.numeric) # Make it numeric if numeric-able
df = df %>% mutate_if(can.factor,as.factor) # Make factor if factor or character
glimpse(df)

# index
set.seed(123); index = sample(1:nrow(df), size = floor(nrow(df)*0.70))

# training sets
df_tr = df[index,]
df_ts = df[-index,]

#### EDA ####

# Employment Growth Outliers
multi.fun(df$EMPL_GROWTH[complete.cases(df$EMPL_GROWTH)])
boxplot2(df$EMPL_GROWTH)


# It seems that Market Cap (Equity) + Liabilities ~ Assets (Accounting Equation)
#   So it seems that there's multicollinearity, maybe PCA can help
df_tr %>% dplyr::select_if(is.numeric) %>% cor(use='complete.obs') %>% corrplot::corrplot()

# Checking if there are redundant variables with only one factor
sapply(df_tr[names(dplyr::select_if(df_tr[,-c(1:2)], is.factor))],levels)
glimpse(df_tr)

# t-SNE: Need to take only numeric data
tsne_df = df_tr %>% select_if(is.numeric) %>% scale() %>% as_data_frame()

set.seed(123); tsne = Rtsne(tsne_df,dims = 2, 
                            perplexity = 30, verbose = TRUE,
                            check_duplicates = F, max_iter = 5000)
set.seed(123); tsne_3D = Rtsne(tsne_df,dims = 3, 
                               perplexity = 30, verbose = TRUE,
                               check_duplicates = F, max_iter = 5000)

tsne_3D_tr = as_data_frame(tsne_3D$Y)
tsne_2D_tr = as_data_frame(tsne$Y)

df_tr %>% dplyr::select_if(is.numeric) %>% bind_cols(tsne_3D_tr) %>% 
            cor(use='complete.obs') %>% corrplot::corrplot()

df_tr %>% dplyr::select_if(is.numeric) %>% bind_cols(tsne_2D_tr) %>% 
  cor(use='complete.obs') %>% corrplot::corrplot()

# Plots t-SNE 2D
ggplot(bind_cols(PROB=df_tr$DEFAULT_PROB,tsne_2D_tr), aes(x=V1, y=V2)) +
  geom_point(size=1, aes(col = PROB)) +
  xlab("") + ylab("") +
  ggtitle("Graph 4: t-SNE") + 
  theme(plot.title = element_text(hjust = 0.5))


# 3D Plot
plot_ly(data.table(tsne_3D_tr), x = ~V1, y = ~V2, z = ~V3, 
        color = ~tsne_df$DEFAULT_PROB, colors = c('#FFE1A1', '#683531')) %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'V1'),
                                        yaxis = list(title = 'V2'),
                                        zaxis = list(title = 'V3')))

rm(tsne_df)
#### POST-EDA PREPROCESSING ####
df_tr = df_tr %>% dplyr::select(DEFAULT_PROB, everything())

scl_tr_center = attributes(scale(df_tr[,-1] %>% select_if(is.numeric)))[['scaled:center']] 
scl_tr_scale = attributes(scale(df_tr[,-1] %>% select_if(is.numeric)))[['scaled:scale']]

df_tr = df_tr[,-1] %>% select_if(is.numeric) %>% scale() %>% 
         cbind(DEFAULT_PROB=df_tr$DEFAULT_PROB, select_if(df_tr,is.factor)) %>%
          as_data_frame() # Create new dataframe for scaling

#### CLUSTER ANALYSIS ####

# Decide the number of clusters
df_tr_num = df_tr %>% select_if(is.numeric) %>% as.matrix()
# set.seed(123); clest = Clest(df_tr_num, maxK = 3, alpha = 0.1, B = 15, B0 = 5, nstart = 1000)

# Calculate the Regularization Parameter
set.seed(123); kperm_4 = KMeansSparseCluster.permute(df_tr_num, K=4, nperms = 5)

# Cluster
set.seed(123); rskm_4 = RSKC(d = df_tr_num, ncl = 4, alpha = 0.1, L1 = kperm_4$bestw)

# Visualization of Clusters
fviz_cluster(list(data = df_tr_num, cluster = rskm_4$labels),
             stand = FALSE, geom = "point",frame.type = "norm")

fviz_cluster(list(data = tsne_2D_tr, cluster = rskm_4$labels),
             stand = FALSE, geom = "point",frame.type = "norm")


# Important Variables for Clustering
cluster_vars = data.frame('Variables' = unlist(attributes(rskm_4$weights)),'Weights' = rskm_4$weights) %>% 
              arrange(desc(Weights)) %>% filter(Weights > 0.01)

# Interprete
cl_interprete = df_tr %>% dplyr::select(as.character(cluster_vars$Variables))
cl_interprete_all = df_tr
cl_interprete_all$cluster = as.factor(rskm_4$labels)
cl_interprete$cluster = as.factor(rskm_4$labels)
cl_interprete = dplyr::select(cl_interprete, cluster, everything())

profile = apply(cl_interprete[,-1], 2, function(x) tapply(x, cl_interprete$cluster, median_mode))

table(rskm_4$labels)


# Add cluster into df
df_tr$labels = as.factor(rskm_4$labels)


#### OUTLIER DETECTION ####
df_tr_1 = df_tr %>% filter(labels == 1)
df_tr_2 = df_tr %>% filter(labels == 2)
df_tr_3 = df_tr %>% filter(labels == 3)
df_tr_4 = df_tr %>% filter(labels == 4)

isotree_1 = IsolationForest::IsolationTrees(x = df_tr_1 %>% dplyr::select(-labels))
isotree_2 = IsolationForest::IsolationTrees(x = df_tr_2 %>% dplyr::select(-labels))
isotree_3 = IsolationForest::IsolationTrees(x = df_tr_3 %>% dplyr::select(-labels))
isotree_4 = IsolationForest::IsolationTrees(x = df_tr_4 %>% dplyr::select(-labels))

df_tr_1$isof = IsolationForest::AnomalyScore(df_tr_1 %>% dplyr::select(-labels),isotree_1)[['outF']]
df_tr_2$isof = IsolationForest::AnomalyScore(df_tr_2 %>% dplyr::select(-labels),isotree_2)[['outF']]
df_tr_3$isof = IsolationForest::AnomalyScore(df_tr_3 %>% dplyr::select(-labels),isotree_3)[['outF']]
df_tr_4$isof = IsolationForest::AnomalyScore(df_tr_4 %>% dplyr::select(-labels),isotree_4)[['outF']]

df_tr2 = bind_rows(df_tr_1,df_tr_2,df_tr_3,df_tr_4)
df_tr2$labels = NULL
#### FEATURE SELECTION ####
set.seed(123); bor = Boruta(DEFAULT_PROB~., data = df_tr2, doTrace = 2)
plot(bor,cex.axis=.6, las=2, xlab="", main="Variable Importance")
featlist = attStats(bor) %>% rownames_to_column(var = 'Features') %>% 
            dplyr::select(Features, medianImp) %>% mutate(rank = rank(-medianImp)) %>% arrange(rank)

df_tr_bor = df_tr2 %>% dplyr::select(DEFAULT_PROB, featlist$Features[c(1:10)])


vif(df_tr_bor)

df_tr$isof



#### MODEL BUILDING ####
train_control = trainControl(method="cv",number=10,savePredictions=TRUE)

lm = train(DEFAULT_PROB ~ ., data = df_tr_bor, method = "lm", trControl=train_control,metric='RMSE')

set.seed(123); rf = train(DEFAULT_PROB ~ ., data = df_tr_bor, method='ranger',
                          trControl=train_control, tuneGrid = expand.grid(mtry=c(2:5),
                          min.node.size = 5, splitrule = 'variance'), metric = 'RMSE')

glm_fit = glm(DEFAULT_PROB ~ ., data = df_tr_bor, family =  binomial(link = "logit"))
predict(glm_fit)


df_tr_bor$DEFAULT_PROB = (df_tr_bor$DEFAULT_PROB)/100

fit = betareg(DEFAULT_PROB ~ .,data = df_tr_bor)




summary(df_tr_bor$DEFAULT_PROB)


ggplot()

summary(glm_fit)
logit(1.152)

#### OUTLIER DETECTION ####
IsolationForest::IsolationTrees()
IsolationForest::AnomalyScore()

