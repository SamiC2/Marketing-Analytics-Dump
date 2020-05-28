rm(list=ls())
library(caret)
library(e1071)

options(scipen=999) # turn off scientific notation
set.seed(999) # set seed
data <- read.csv('telco_churn_data.csv', header=TRUE) # read data
my_data <- data[1:5000, -5]

my_data$Churn[my_data$Churn == 'No'] = 0
my_data$Churn[my_data$Churn == 'Yes'] = 1

my_data$Churn <- as.factor(my_data$Churn)
#my_data <- within(my_data, Churn <- relevel(Churn, ref = 'No')) # making sure Churn == 'Yes' is coded as 1 and Churn == 'No' as zero, in binary logistic regression
head(my_data,5) # take a look at the data

# Each row in the data corresponds to a different customer. The dependent variable here is Churn (0/1) -- whether customer churned during last month, and we are trying to evaluate how different features in the data affect churn probability, including demographic information (whether the customer has a partner, is he a senior citizen) and other account information (contract type, paperless billing, monthly charges). Full variable list is as follows

colnames(my_data)

# Logistic Regression
# We now perform a logistic regression to summarize the data, regressing the churn event (0/1) on all other variables.


set.seed(999) # set seed
model <- glm(Churn ~ .,data=my_data, family=binomial(link = "logit"))
summary(model)

 
# From these results we observe that, for example, increase in monthly charges is significantly positively correlated with increased probability of churn, whereas being on a contract rather than on month-to-month renewal significantly reduces churn probability.

# Predictions

# We can now predict the probability of churn for the data set. 


my_data$churn_prob <- predict.glm(model, newdata=my_data, type="response")
hist(my_data$churn_prob)
confusionMatrix(table(1*(my_data$churn_prob>0.5),1*(my_data$Churn==1)))
# 
# 1. Sum up all the coefficients and find this value, let this = x
# 
x = sum(model$coefficients[2:9])
# 2. Sum up all the coefficients except PaperlessBillingYes, let this = y
# 
y = x - 0.4115122
# 3. Find the value of a = exp(x)/(1 + exp(x)) and the value of b = exp(y)/(1 + exp(y)).
# 
a = exp(x)/(1+exp(x))
b = exp(y)/(1+exp(y))
a/b
# 4. Divide a/b to get the answer.
#prob likely someone churns with agree to paperlessbilling
exp(model$coefficients["PaperlessBillingYes"]) # the number after 1. is what you need, so 50.9%

#proportion of customers with  churn < 20
low_churn = sum(my_data$churn_prob<0.20)
prob_lowchurn = low_churn / 5000
#somwehere around 44%

#monthyl charge get with hgiest and lowest churn probs
mon_charge_hi = my_data[which.max(my_data$churn_prob), 7]
mon_charge_lo = my_data[which.min(my_data$churn_prob), 7]

#visualize answer
table(mon_charge_hi, mon_charge_lo)



# Targeting
# One thing we can do with this data is target individuals at high risk of churn in an email blast. This is how we would identify the individuals.


sum(data$churn_prob>0.6) # number of customers with probability of churn strictly greater than 60%

# Monetary value of a contract
# 
# Being on a 1-year contract significantly deters a person from churning, compared to month-to-month renewal schedule. Can we express this pressure not to churn in dollars? That is, how much extra would we need to charge a person on a 1-year contract to make him/her as likely to churn as a person on month-to-month renewal process?

#how much a comapny increases monthly charge to offset shift by customer from month2month to 1yr contract and keep churn prob unchanged
-model$coefficients[6]/model$coefficients[9]

# We would need to charge that person extra $\$94.7$ per month. This amount gives us some idea about how much more valuable a customer on a contract is compared to a customer without a contract.
# Increase in monthly charges and churn
# 
# After some conversations with the CEO, you feel there is a space for price increase. You decide on an immediate one-time hike in monthly charges. You want it to be the same for every customer in percentage terms relative to their current monthly charge.
# 
# In order to determine the profit-maximizing increase, you decide to perform analysis of future profits that incorporates effect of churn change due to increase in monthly charges. Note that in this type of analysis we require the critical assumption that the coefficient on the monthly charge is accurate for charge *changes*, rather than just charge *differences* between individuals. Assume that this is the case in this data set.
# 
# We first make some simplifying assumptions. We assume that customers make a decision to churn or not at the end of each discrete period. We notice that each customer in the data set that has not churned will have to pay fees for the first month with probability 1. At the end of the period the cutomer will again decide whether to churn or not, based on the experienced fee. We also assume that upon a possible one-time change in monthly charges, all other customer and account characteristics will remain fixed for lifetime for the purposes of this analysis.
# 
# Let $c$ be the probability that customer churns, so that $p = 1-c$ is the retention rate, i.e., the probability that customer will transact during subsequent period. Let $\gamma$ be a discount factor (by how much money tomorrow is worth less than money today; if $\gamma = 0.99$, then a dollar tomorrow is worth $\$0.99$ today). Let $m$ be customer's monthly charge at time $t$. Then the expected discounted profit from a customer (i.e., customer lifetime value, or CLV), based on formula for the sum of terms of a geometric progression https://en.wikipedia.org/wiki/Geometric_progression, is as follows
# 
# $$CLV = m + \gamma(1-c) m + \gamma^2(1-c)^2 m + \cdots = m + \gamma p m + \gamma^2 p^2 m + \cdots = \frac{m}{1-\gamma p}$$
# (Note, slight variations are possible here in when we start discounting, and whether retention rate matters for the first payment, but we will go with this formula).
# 
# Using this formula, we can calculate customer lifetime value -- expected discounted profit up to the infinite time horizon -- conditional on specific $m$ (monthly charge) and customer-specific probability of retention $p=1-c$, where we get churn probability $c$ as a prediction from the logistic regression. Notice that $p$ implicitly depends on $m$ through logistic equation (because $m$ is an input in the equation), and for $m$ fixed, we assume $p$ will remain constant throghout consumer lifetime.

pf<-function(incr, my_data)
{
  d <- my_data[my_data$Churn==0,] # only keeping customers that have not churned yet
  d$MonthlyCharges <- d$MonthlyCharges*incr # possible increase in monthly charges (no increase is incr==1.0)
  g <- 0.97 # discount factor (money in the next period is worth 0.99 money in the period before that)
  p <- 1-predict.glm(model, newdata=d, type="response") # retention probability based on logistic regresion (we assume retention probability will remain constant for each consumer, conditional on fixed d$MonthlyCharges)
  clv <- d$MonthlyCharges/(1-p*g) # CLV formula
  return(sum(clv)) # sum of discounted profits across all individual consumers
}
# 
# Let us compute this discounted profit across the dataset under different scenarios. 

pf(1.0, my_data) # no increase in price: 1 * charge_amount
pf(1.5, my_data) # 50% increase: (1 + 0.5) * charge_amount
pf(2, my_data) # 100% increase / doubling: 2 * charge_amount
pf(3, my_data) # 200% increase / trippling: 3 * charge_amount

pf(1.2, my_data) - pf(0.8, my_data)
# Discounted profit optimization
# 
# Using the built-in optim function in R, we can directly optimize for discounted profit from this set of customers. We use a starting value of 1.0 (no change) for the optimal monthly charge increase, and store the optimization results in opt. 

set.seed(999)
opt<-optim(par = 1.0, fn = pf, method="L-BFGS-B", control=list(fnscale=-1), my_data=my_data)

# 
holder = my_data[4,]
holder$MonthlyCharges = holder$MonthlyCharges * 1.2
1-predict.glm(model, newdata = holder, type="response") #prob customer 4 doesnt churn after a 20% hike in monthly charges
(1-predict.glm(model, newdata = holder, type="response"))^4 #cumulative after 4 months of no churn
# The optimal increase in monthly charges and the optimal profit can be extracted from the optimization result as below.

c(opt$par, opt$value)

# We find that the optimal hike in monthly charges is $18.1\%=1.180934-1$ . 

pf(opt$par, my_data) - pf(1.0, my_data)
 
# We also see that the hike in charges yields extra $\$36752$ in customer lifetime value.

