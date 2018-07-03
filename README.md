# Bayesian-Variable-Selection
Using Bayesian Statistics for variable selection in Machine Learning modelling 


In this project, the goal was to find a way to rank models within a subset of a database's variables. Sometimes, we have too many variables, so we are interested in finding a subset which could be give better results than using the whole dataset, in addition to lowering the number of calculations by having less variables. The number of models being <a href="http://www.codecogs.com/eqnedit.php?latex=2^{number&space;of&space;variables}" target="_blank"><img src="http://latex.codecogs.com/gif.latex?2^{number&space;of&space;variables}" title="2^{number of variables}" /></a>, it becomes quite impossible to calculate a criterion for every one of them, especially with a lot of variables.

We explain how to use a MCMC algorithm that searches for these good models. We get very satisfying results despite having <a href="http://www.codecogs.com/eqnedit.php?latex=2^{720}" target="_blank"><img src="http://latex.codecogs.com/gif.latex?2^{720}" title="2^{720}" /></a> like in our paper's example. That corresponds to 5.5e216 models possible, and yet in a reasonable amount of time our algorithm finds very satisfying possible models.
