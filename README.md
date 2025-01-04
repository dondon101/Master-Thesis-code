# Master-Thesis-code
The repository is for publishing the master thesis program code written in R language.

The main code used to produce the results presented in the Master Thesis is in the `main_code.R` file.

The `supplementary_functions.R` script provides all the supplementary functions, such as plots and visuals for results, 
model creation, and date and time manipulation.

In the main code, everything is written in terms of functions that are defined in another script -- `process_workflow.R`.
It contains functions for preparing the initial data, summarising area-level information from unit-level data, 
selecting a bootstrap sample, obtaining predictions of $k$-NN model, estimating domain totals with the MC estimator, 
preparing data for the FH model, and obtaining final EBLUPs of areas.
The purpose of decomposing the code is to modularize the whole process.

Due to data sensitivity and confidentiality concerns, the datasets are not provided.
