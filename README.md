# Insurance pricing model with a Bayesian Approach
This git includes the code used for my master's degree thesis. The goal is to develop a statistic model for non-life insurance pricing.
## Requirements
### Insurance Theory
To understand the code's function, it is important to have base level knowledge of:
- Fundamental pricing equations;
- Frequency-Severity pricing models;
- Pricing with Generalised Linear Models (GLM).
### Basics of Probability
Aside from insurance knowledge, we also need base level knowledge of:

- Generalised Linear Models and their function;
- Bayes Theorem and therefore, Bayesian inference;
- Markov Chain Monte Carlo.

### Programming with R
To develop the code, we need a series of packages. From our library, we need to recall the following packages:

- brms;
- ggplot2;
- gdata;
- dplyr;
- parallel;
- cowplot.
## The dataset
Insurances develop their pricing models using data from insurance providers, or data deriving from their own experience. Since we cannot have access to this kind of information, the code includes a method to simulate the values used to develop the Frequency and Severity model, which are both analysed separately.
