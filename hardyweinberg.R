hardyweinberg <- function(AA, Aa, aa) {
    
# Given the allele frequencies of a SNP in a sample, this function returns 
# the 1 df Pearson's chi-square test statistic and p value for a
# test of deviation from Hardy-Weinberg equilibrium. Heterozygotes must
# be the middle argument (Aa).

n <- AA+Aa+aa

p <- ((2*AA)+Aa)/(2*n)
q<- 1-p

ExpAA <- (p^2)*n
ExpAa <- 2*p*q*n
Expaa <- q^2*n

#chi-square stat
ChiSq <- ((AA-ExpAA)^2/ExpAA)+ ((Aa-ExpAa)^2/ExpAa) +((aa-Expaa)^2/Expaa)

#p value
pVal <- pchisq(ChiSq, 1, lower.tail=FALSE)

return(list("ChiSq" = ChiSq, "pVal" = pVal))
}
