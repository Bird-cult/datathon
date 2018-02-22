#### LOADING FUNCTIONS & DATA ####
rm(list = ls())
library(forcats)
library(FNN) # used
library(tidyverse) # used
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

# Ridit Function
ridit_scores=function(y){
  x = vector('numeric') # creating an vector type numeric
  for (i in 1:length(y)) {
    x[i]=(sum(y[1:i])-sum(y[length(y):(i)]))
  } # Uses the formula according to Baesens et al. Check the notes for further details
  return(x)
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

# Density GG Function - Creating a GGplot object for density graph
density_gg = function (df,a){
  enq_col_a = enquo(a)
  vector_a = unlist(dplyr::select(df,UQ(enq_col_a)))
  ggplot(df,aes(x = vector_a)) + geom_density(alpha = 0.3) +
    labs(x = deparse(substitute(a)))   
}

#Stack Bar Chart Create a GGplot object for density graph
stacked_gg = function (df,a){
  enq_col_a = enquo(a)
  vector_a = unlist(dply::dplyr::select(df,UQ(enq_col_a)))
  ggplot(df, aes(x = vector_a)) + geom_bar(position = "fill") + 
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x 
         = deparse(substitute(a)))   
}
# Ridit_Modifier needs that SAR levels includes 'SAR' and Ridits Score function already in WS
Ridit_Modifier = function(df,x){
  name_df = deparse(substitute(df)) # takes the df into proper format
  enq_col = enquo(x) # takes the col into proper format
  vector_x = unlist(dply::dplyr::select(df,UQ(enq_col))) # Vectorize the column x from df dataframe
  vector_y = unlist(dplyr::select(df,SAR)) # Vectorize the column SAR from df dataframe
  table_xy = table(vector_y,vector_x) # Make a table out of two previous vectors
  prop_table = prop.table(table_xy,2) %>% data.frame() %>% filter(!is.nan(Freq),vector_y == 'SAR')  %>% 
    arrange(desc(Freq))
  # Make a table with Relative Frequency of SAR that filters only the case when SAR is TRUE and orders decreasingly
  colnames(prop_table) = c("Var1", deparse(substitute(x)), "Freq_SAR")  
  table_freqx = table(vector_x) %>% prop.table() %>% data.frame() # Make a table with Relative Frequency of each category
  colnames(table_freqx) = c(deparse(substitute(x)), 'Freq') 
  join = left_join(prop_table,table_freqx, by = deparse(substitute(x))) # Adding to prop_table the Rel.Freq. of table_freqx
  ridit = ridit_scores(join$Freq) %>% data.frame() # Use basic function of Ridit Scoring
  colnames(ridit) = 'Ridit_Scores'
  ridit = data.frame(join,ridit) %>% dplyr::select(UQ(enq_col),'Ridit_Scores')
  colnames(ridit)=c(deparse(substitute(x)),paste0('RS_',deparse(substitute(x))))
  df2 = left_join(df,ridit,by = deparse(substitute(x)))
  df2 = dplyr::select(df2,-c(UQ(enq_col))) # remove original
  assign(name_df, df2, .GlobalEnv) # Updating the Dataframe
  assign(paste0('Ridit_Table_',deparse(substitute(df)),deparse(substitute(x))), ridit, .GlobalEnv) #Create a Ridit Table - for each category , a Ridit Table
}

# Load data properly
setwd("C:/Users/orteg/Documents/GitHub/datathon")
df = read_excel("credit.xlsx",na=c(""," ",".","NA","#N/A",'#N/A N/A')) # so that it interprets blank spaces as NA's

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

#### DATA WRANGLING ####

## DATA TRANSFORMATION: Making seemingly numeric variables as such, others can be factors
vector_attr = attributes(which(apply(apply(df,2,check.numeric),2,all)))[['names']] # Identify numeric-able
df = df %>% mutate_if(names(df) %in% vector_attr,as.numeric) # Make it numeric if numeric-able
df = df %>% mutate_if(can.factor,as.factor) # Make factor if factor or character
glimpse(df)

# index
set.seed(123); index = sample(1:nrow(df), size = floor(nrow(df)*0.70))
df_tr = df[index,]
df_ts = df[-index,]


# training sets
df_tr = df[index,]
df_ts = df[-index,]

#### EDA ####
multi.fun(df$EMPL_GROWTH[complete.cases(df$EMPL_GROWTH)])
boxplot2(df$EMPL_GROWTH)

# Missing Plot on DF [Warning: Data Leaking]
missingness = aggr(df_tr, col=c('navyblue','yellow'),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(df_tr), cex.axis=.7,
                   gap=3, ylab=c("Missing data","Pattern"))


init = mice(df, meth = "rf", ntree = 10)
plot(init)
densityplot(init)
df2 = complete(init)


df_tr2 = df2[index,]

# It seems that Market Cap (Equity) + Liabilities ~ Assets (Accounting Equation)
#   So it seems that there's multicollinearity, maybe PCA can 
df_tr2 %>% dplyr::select_if(is.numeric) %>% cor(use='complete.obs') %>% corrplot::corrplot()


glimpse(df_tr2[names(dplyr::select_if(df_tr2[,-c(1:2)], is.factor))])
# Checking if the 
sapply(df_tr2[names(dplyr::select_if(df_tr2[,-c(1:2)], is.factor))],levels)

glimpse(df_tr2)
tsne_df = df_tr2 %>% dplyr::select(-ID) %>% dplyr::select_if(is.numeric) %>% scale() %>% as_data_frame()
tsne_df = tsne_df[complete.cases(tsne_df),]

set.seed(123); tsne = Rtsne(tsne_df,dims = 2, 
                            perplexity = 30, verbose = TRUE,
                            check_duplicates = F, max_iter = 5000)
set.seed(123); tsne_3D = Rtsne(tsne_df,dims = 3, 
                               perplexity = 30, verbose = TRUE,
                               check_duplicates = F, max_iter = 5000)

tsne_3D_tr = as_data_frame(tsne_3D$Y)
# Plots t-SNE 2D
ggplot(bind_cols(CHURN=scl_tr$CHURN,tsne2_scl_tr), aes(x=V1, y=V2)) +
  geom_point(size=1, aes(col = CHURN)) +
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
#### FEATURE SELECTION ####
set.seed(123); bor = Boruta(DEFAULT_PROB~., data = df_tr2, doTrace = 2)
plot(bor,cex.axis=.6, las=2, xlab="", main="Variable Importance")
featlist = attStats(bor) %>% rownames_to_column(var = 'Features') %>% 
            dplyr::select(Features, medianImp) %>% mutate(rank = rank(-medianImp)) %>% arrange(rank)


