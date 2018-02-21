library(dplyr)
library(readxl)
library(MCMCpack)
library(ggplot2)
library("quantmod")
library("reshape2") 
#TODO: ONE OF THESE PACKAGES OVERWRITES DPLYR::SELECT DANIEL?

credit <- read_excel("./Credit/Credit_DataSet.xlsx", sheet=2,
                     col_types=c("skip", "text", "text",
                                 "text", "text", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "text", "text",
                                 "text", "text", "skip",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric")) %>%
  mutate_if(is.character, as.factor) %>% # Convert all character columns to factor
  mutate(DEFAULT_PROB = DEFAULT_PROB / 100)

# print NA percentage per column
credit %>% summarize_all(function(x) sum(is.na(x))/length(x)) %>% select_if(function(x) x>.001)

# drop na, and columns with sufficiently many NAs
credit_smaller_non_na <- credit %>% dplyr::select(-c(CURRENCY,PCT_WOMEN_EMPLOYEES, PCT_WOMEN_MGT, WHISTLE_BLOWER_POLICY, ETHICS_POLICY, BRIBERY_POLICY)) %>% drop_na
# Complete case GLM
glm_fit <- glm(DEFAULT_PROB ~ ., data=credit_smaller_non_na, na.action = na.omit)
summary(glm_fit)

# Don't fit on INDUSTRY GROUP and INDUSTRY, as multicolinearity perhaps?
glm_bayesian_fit <- MCMCregress(DEFAULT_PROB ~ . -INDUSTRY_GROUP -INDUSTRY, data=credit_smaller_non_na, na.action = na.omit)



############# Normalize country currency ###################

#take subset where currency is missing and fill the NAs with country currency
bdNA<-credit[is.na(credit$CURRENCY),]
bdNA$CURRENCY<-as.character(bdNA$CURRENCY)

bdNA[bdNA$COUNTRY=="AT",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="BE",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="BM",]$CURRENCY<-"BMD"
bdNA[bdNA$COUNTRY=="CA",]$CURRENCY<-"CAD"
bdNA[bdNA$COUNTRY=="CH",]$CURRENCY<-"CHF"
bdNA[bdNA$COUNTRY=="CN",]$CURRENCY<-"CNY"
bdNA[bdNA$COUNTRY=="DE",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="DK",]$CURRENCY<-"DKK"
bdNA[bdNA$COUNTRY=="ES",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="FI",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="FR",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="GB",]$CURRENCY<-"GBP"
bdNA[bdNA$COUNTRY=="GR",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="IE",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="IL",]$CURRENCY<-"ILS"
bdNA[bdNA$COUNTRY=="IN",]$CURRENCY<-"INR"
bdNA[bdNA$COUNTRY=="IT",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="JE",]$CURRENCY<-"GBP"
bdNA[bdNA$COUNTRY=="JP",]$CURRENCY<-"JPY"
bdNA[bdNA$COUNTRY=="KY",]$CURRENCY<-"KYD"
bdNA[bdNA$COUNTRY=="LU",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="MC",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="NL",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="NO",]$CURRENCY<-"NOK"
bdNA[bdNA$COUNTRY=="PR",]$CURRENCY<-"USD"
bdNA[bdNA$COUNTRY=="PT",]$CURRENCY<-"EUR"
bdNA[bdNA$COUNTRY=="SE",]$CURRENCY<-"SEK"
bdNA[bdNA$COUNTRY=="TH",]$CURRENCY<-"THB"
bdNA[bdNA$COUNTRY=="US",]$CURRENCY<-"USD"
bdNA[bdNA$COUNTRY=="ZA",]$CURRENCY<-"ZAR"

bdNA$CURRENCY<-as.factor(bdNA$CURRENCY)

credit <- rbind(bdNA, credit[!is.na(credit$CURRENCY),])
summary(credit$CURRENCY)

#Change strange names to euros
credit[credit$CURRENCY=="EDM" | credit$CURRENCY=="EES" | credit$CURRENCY=="EIL"
       | credit$CURRENCY=="EFR" | credit$CURRENCY=="EIP"| credit$CURRENCY=="EFM"
       | credit$CURRENCY=="EDG"| credit$CURRENCY=="EPE"| credit$CURRENCY=="EAS"
       | credit$CURRENCY=="EBF"| credit$CURRENCY=="EGD",]$CURRENCY<-"EUR"

summary(credit$CURRENCY)


#Set start and end dates
startDt = as.Date("2017-12-29")
endDt = as.Date("2017-12-29")

#create currency pair combinations i.e. EUR/GBP, USDGBP 
currCombinations = paste(setdiff(unique(credit$CURRENCY),"EUR"),"EUR",sep="/")
currCombinations

#get FX data for each currency pair and merge them into a single xts dataset
#see ?lapply, ?do.call and ?merge.xts
#note auto.assign needs to be FALSE

fxData = do.call(merge.xts,lapply(currCombinations,function(x) 
  getFX(x,from=startDt,to=endDt,auto.assign=FALSE))) 

fxData

#remove .EUR from all columns above
colnames(fxData) = gsub("[.]EUR","",colnames(fxData))
#set conversion factor for EUR = 1
fxData$EUR = 1


#create data.frame from xts data
fxData_DF = data.frame(date=index(fxData),coredata(fxData),stringsAsFactors=FALSE)

#To make fx dataset(wide format) amenable for merging with currDF
#we convert it to long format by using melt from reshape2 package see,?melt

fxMolten = melt(fxData_DF,id="date",variable.name="currency",value.name="conversionFactor")

#For each currency and date, we need a conversion factor hence
#we merge both datasets by columns date and currency 

names(fxMolten)[names(fxMolten) == "currency"] <- "CURRENCY"
fxMerged = merge(credit,fxMolten,by="CURRENCY")

#calculate FX translated amount for the columns of interest (overwrite originals)
fxMerged[,22:30] = fxMerged[,22:30] * fxMerged$conversionFactor
#To compare with the originals
#fxMerged[,33:41] = fxMerged[,22:30] * fxMerged$conversionFactor

#Reshape as the original 
fxMerged<-fxMerged[,c(2,3,4,1,5:30)]

credit<-fxMerged
