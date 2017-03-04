# monterey_pine project log

## 16 February

MJ consulted on reformulating the mp growth model as a simple AR1 process; simulated data and basic stan model recovers parameters consistently.

Features to add to the model:
+ Within data simulation, add additional trees, how to set initial values so that tree start sizes are representative of actual data?
+ Modify stan code to accomodate more than one individual
+ Go back to tidy file and reformat data to accomodate new model specification
+ Figure out how to add wx component to the model
  + additive
  + embed in beta term?
  + max talked about using a log link to ensure positive
+ Add survivorship to model

## 24 February

Next steps:
+ Figure out how to deal wacky estimates for first col of epsilon
+ Add wx component to model; MJ suggests using lognormal likelihood on the model block to ensure positive behavior; I can think of how this is different than using a parameter constraint in the parameter block
+ Could make beta lognormal and introduce wx in the location parameter with some alpha  

# 2 March

+ Linearize the model form for data simulation
