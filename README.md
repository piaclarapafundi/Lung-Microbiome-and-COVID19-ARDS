# Lung-Microbiome-and-COVID19-ARDS
Lung Microbiome is associated with COVID-19 ARDS respiratory mechanics and mortality

# Background
Few data are available on lung microbiome in COVID-19 patients with acute respiratory distress syndrome (ARDS). We hypothesized that lung microbiome composition may affect respiratory mechanics and mortality in this population.

# Methods
We collected bronchoalveolar lavages (BALs) from intubated patients with COVID-19 ARDS, sequencing V3-V4 regions of S16 ribosomal bacterial RNA. Respiratory mechanics were assessed the day of microbiological sampling.
To visualize community microbial composition profiles across BAL samples, we created heatmaps with a hierarchical clustering of samples and top ten taxa agglomerated at genus-level (eFigure1). To quantify richness, diversity and equitability within each BAL sample (alpha diversity), we used the ultimate dataset to compute observed species index, a measure of bacterial burden, Shannon diversity index and Pielou’s evenness, respectively. Beta diversity metrics were evaluated using Bray-Curtis distance and weighted UniFrac distance. Differences according to alpha diversity metrics were assessed using the Kruskal-Wallis test, whereas those according to Bray-Curtis or weighted UniFrac beta diversity metrics were assessed using the permutational multivariate analysis of variance (PERMANOVA). To minimize the effect of differences in sequencing depth, we computed rarefaction curves and normalized at the median sequencing depth before measuring alpha and beta diversities. Relative abundances were computed at phylum- and genus- level and statistical significance assessed by Mann-Whitney test. 
The whole data were preliminary summarized by descriptive statistics, both on over-all population sample and according to cluster stratification (A and B). To evaluate potential predictors of 28-days mortality, ordinary Proportional hazard Cox regression models were fitted, and Hazard Ratios (HRs) and 95% confidence intervals (CIs) reported. Proportionality of the hazard functions was assessed by visual inspection of hazard plots and Schoenfeld residuals. When proportionality was doubtful, weighted Cox regression models were fitted. Kaplan-Meier survival analysis was applied to test for potential differences between clusters A and B in terms of 28- and 90-days mortality and weaning from invasive mechanical ventilation. Log-rank-test was computed and Kaplan-Meier curves further depicted each comparison.  
Statistical significance was set at P value < 0.05. P-values between 0.05 and 0.10 were also reported as suggestive. All analyses were performed by using R software (v. 4.2.0, R Core Team, 2022), and its packages “coin”, “survival”, “survminer”, and “coxphw”.

# Interpretation
In COVID-19 ARDS, less lung microbial diversity, loss of Firmicutes and abundance in Proteobacteria identified patients with lower Crs, higher VR, longer mechanical ventilation and increased mortality. 

