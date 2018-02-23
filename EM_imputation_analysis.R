other_credit <- read.csv("EM_data_fuller.csv")
par(mfrow=c(2,3))
hist(other_credit$WHISTLE_BLOWER_POLICY , xlab = "whistle blower policy") 
hist(other_credit$ETHICS_POLICY , xlab="ethics policy")
hist(other_credit$BRIBERY_POLICY, xlab="bribery policy")

no_na_credit <- credit %>% drop_na

hist(as.integer(no_na_credit$WHISTLE_BLOWER_POLICY) - 1, xlab = "whistle blower policy")
hist(as.integer(no_na_credit$ETHICS_POLICY) - 1,  xlab="ethics policy")
hist(as.integer(no_na_credit$BRIBERY_POLICY) - 1, xlab="bribery policy")

# These indices are most deviant
# Int64Index([3132, 3068, 2937, 227, 331], dtype='int64')