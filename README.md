# Assessing Numerical Consistency between wHole sample and subgROups (ANCHOR)

## A method to check for inconsistencies in reported summary statistics for the full sample vs. subgroups

Note that this method of error checking is well established and I make no claim to having invented it. I came across it in Wilkinson et al.'s draft of INSPECT-SR (see [here](https://doi.org/10.1136/bmjopen-2024-084164) for development protocol). My contribution here is to provide an accessible web app to allow users to check for these consistencies, not only for means but also Ns and SDs. 



## Methods

- Recalculated N uses the sum of the subsample Ns. 
- Recalculated M uses the weighted average of the subsample means, weighted by N.
- Recalculated SD uses the pooled SD, i.e., by pooling the subgroup standard deviations, weighting each by its subgroup's degrees of freedom (n − 1), and then taking the square root of the weighted average variance.
- 

## Cite as

Hussey, I. (2024). Assessing Numerical Consistency between wHole sample and subgROups (ANCHOR): A method to check for inconsistencies in reported summary statistics for the full sample vs. subgroups. https://github.com/ianhussey/ANCHOR



## TODO

- Get Lukas to work his rounding magic to improve the current rounding approach (janitor::round_half_up)



