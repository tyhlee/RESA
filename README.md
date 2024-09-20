# RESA (Under Development)
R package for the the Risk calculator for severe asthma Exacerbations in patients with Severe Asthma.

## Installation

```
install.packages("remotes")
remotes::install_github("tyhlee/RESA")
```

# Web App for RESA 

RESA is available as a web app, accessible at [https://resplab.shinyapps.io/RESA/](https://resplab.shinyapps.io/RESA/).

# RESA in R

### Sample Patient Data

There are four mandatory inputs for RESA: setting, age, sex, and the number of severe asthma exacerbations in the past 12 months. The other inputs are optional and should be left as NA if unknown.

A sample patient data matrix is provided as part of the package.

```
library(accept)
samplePatients <- RESA::samplePatients
```

The same column names must be used, and you can go to the help page for details on the inputs.

```
?RESA::samplePatients
```

### Predicting the risk of having any or frequent exacerbations in the next 365 days.

The **resa()** function returns a data frame with the patient data used for prediction, along with the predictions for the risk of having 0, at least 1, and at least 2 severe asthma exacerbations in the next 365 days. 

```
results <- resa(samplePatients)
```

## Cloud-based API Access 

The [Peer Models Network](https://www.peermodelsnetwork.com) allows users to access RESA through the cloud. A MACRO-enabled Excel-file can be used to interact with the model and see the results. To download the PRISM Excel template file for ACCEPT, please refer to the [Peer Models Network model repository](https://models.peermodelsnetwork.com).

## Citation

TODO:

