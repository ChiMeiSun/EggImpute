# EggImpute: Egg Laying Data Analysis and Imputation
author:
  - Chi Mei Sun

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A Bayesian imputation approach that uses behavioral information captured by an automated nest system and hand-counted egg numbers to estimate individual egg numbers under uncertain identification.
An R package for analyzing chicken egg laying patterns, imputing missing laying dates using Hidden Markov Models (HMM), and assigning eggs to animals using probabilistic methods.

## Features

- **Data Preparation**: Clean and format raw autonest metadata
- **Quality Control**: Flagging of suspicious autonest records
- **Bayesian Imputation**: A probabilistic model that iteratively updates prior probabilities for egg-hen pairs using behavioral likelihoods
- **Egg Assignment**: Generate Probabilistic or deterministic assignment of eggs to animals
- **Validation**: Cross-validation tools for model evaluation


## Behavioral Scenarios
Egg laying behavior is modeled through three complementary behavioral scenarios:

### 🥚 **Individual Laying Pattern**
- Fitted using Hidden Markov Models (HMM) to identify laying states
- Implementation: [![depmixS4](https://img.shields.io/badge/depmixS4-CRAN-blue)](https://github.com/cran/depmixS4) package

### 🏠 **Short-term Nest Visitation Habits**
- Recent nest selection patterns (± ?days)
- Penalizes frequent nest switching

### 📊 **Long-term Nest Preference**
- Historical nest usage patterns


## Installation

### From GitHub (Development Version)
```r
# Install devtools if needed
install.packages("devtools")

# Install EggImpute
devtools::install_github("YourUsername/EggImpute")

# Load the package
library(EggImpute)
