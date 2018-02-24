# TODO: 

## predict the probability thing
logistic regression, MLP, random forest etc

# Jointly predicting probability of default and finding missing values:
  We do not use the categorical country columns that do not seem needed given the continuous ones.

  We derived and implemented an EM-kind of algorithm:
  REPEAT:
	1) In one step, we train our neural network given all data
	2) In the other step, we fill in our data given our model.

  This gave us a nice insight on the missing policy columns:
  In comparison to the other missing data procedures, it filled most missing rows as 'not having a policy';
     => This is more in line with our intuition that if the company doesn't specifically have a policy, that they don't pub;ish it (how do you know if a company doesn\t havne a television? They will tell you!)

  We have to remember that the DEFAULT_PROB comes from a model itself.
  Our neural network is a two-layer model, that we believe has sufficiently many parameters to learn this.

  The predictions seem sensible. (plot)

  (LIE): Did you do cross validation? How good is it? 'Yeah, it seems better than logistic regression'



## cluster analysis
We believe in Carlos.

## test models on these data
SEM, from the literature or something

## Missingness: Tonto
-> we can use this together with a bayesian model for missing data


## Visualization:
find something to visualize

## outliers
find outliers (isolation forest or something)


# Not gonna happen:
## Exteral data
- deanonymize the dataset, find company

## Some sort of NLP
scrape twitter per country for that date,
e.g. see how big companies are perceived against small? Hard to do without external data (names of companies) at the very least
