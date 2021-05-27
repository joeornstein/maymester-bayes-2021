## Draw DAGs and get conditional independencies using dagitty

library(dagitty)

# dagitty() function creates a DAG
dag1 <- dagitty('dag{ GDP -> CPI
                Pop -> GDP
                Pop -> Regime
                Regime -> GDP
                Regime -> CPI
                Resources -> Regime}')

# you can plot the DAG
plot(dag1)

# which variables are independent of one another?
impliedConditionalIndependencies(dag1)

# if you want to find the causal effect of X on Y, what must we condition on?
adjustmentSets( dag1 , exposure="GDP" , outcome="CPI" )
