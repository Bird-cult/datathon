This is the code of our team for the first KU Leuven Datathon, where we were the 3rd best Master's team.

Our approach was mainly to directly model the default probability of companies.

# Jointly predicting probability of default and finding missing values:
  We do not use the categorical country columns that do not seem needed given the continuous ones.

  We derived and implemented an EM-kind of algorithm:
  REPEAT:
	1) In one step, we train our neural network given all data.  
	2) In the other step, we fill in our data given our model. The columns we fill are the POLICY ones, as well as the women percentages.

  This gave us a nice insight on the missing policy columns:
  In comparison to the other missing data procedures, it filled most missing rows as 'not having a policy';
     => This is more in line with our intuition that if the company doesn't specifically have a policy, that they don't publish it.
     
  Our best neural network is a two-layer model, that has around half the error of logistic regression on the test set.

  The predictions seem sensible, with the residuals not showing any specific pattern.


## cluster analysis
