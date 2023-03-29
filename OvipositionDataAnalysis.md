Oviposition Prefernce by Genotype Data Analaysis
================

Preference is the number of eggs laid on rice to the number laid on
Leersia, and Genotype refers to the parent race on rice (“rice”), the
parent race on Leersia (“leer”), their F1 and F2 hybrids (“f1”, “f2”),
and the backcrosses between the F1 hybrid and each parent race (“br” for
rice and “bl” for Leersia).

``` r
ovipos <- read_csv("egglayingpreferencedata.csv")
```

    ## Rows: 524 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): genotype
    ## dbl (1): preference
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
attach(ovipos)
head(ovipos)
```

    ## # A tibble: 6 × 2
    ##   genotype preference
    ##   <chr>         <dbl>
    ## 1 rice           1.67
    ## 2 rice           1.73
    ## 3 rice           2.66
    ## 4 rice           1.08
    ## 5 rice           0.96
    ## 6 rice           1.13

AIC = - 2 \* ln L(Model \| Data) + 2k

BIC = - 2 \* ln L(Model \| Data) + ln(n) \* k

Where L(Model \| Data) is the log-likelihood of the model given the
dataset, k is the number of parameters in the model, and n is the number
of observations in the dataset.

The equations for both AIC and BIC are based on the same criteria except
for the penalty imposed by the number of variables included in the
model. This penalty is important because, without it, the more complex
model with the highest number of parameters would always be calculated
as the most likely. Therefore both AIC and BIC impose this penalty as a
way to account for the trade-off between variance and bias. BIC imposes
a larger penalty than AIC in most cases. If we compare the penalty
portions of the two equations we find that if the sample size is greater
than 7, BIC imposes a higher penalty than AIC as shown below.

BIC \> AIC

ln(n)k \> 2k

ln(n) \> 2

n \> exp(2)

Visualize the oviposition preference of the different genotypes.

``` r
plot(ovipos$preference ~ as.factor(ovipos$genotype), xlab = "Genotype", ylab = "Oviposition Prefernce Ratio (Rice/Leersia)")
```

![**Figure 1:** Boxplots showing preference ratios for egg-laying in
Rice and Leersia plants by genotype. The ‘rice’ genotype refers to the
parent race that oviposits on rice, ‘Leer’ is the parent race that
oviposits on Leerisa plants. F1 and F2 refer to hybrid genotypes. The
‘br’ genotype is the backcross of F1 hybrids with rice parent genotype.
The ‘bl’ genotype is the backcross of the F1 hybrid with the Leerisa
parent genotype. (n rice = 55, n bl = 53, n br = 191, n f1 = 91, n f2 =
113, n leer =
21))](OvipositionDataAnalysis_files/figure-gfm/unnamed-chunk-3-1.png)
The data seems to indicate that prefence may depend on the genotype of
the insects. The rice genotype shows the strongest preference for
oviposition on rice plants, while the leer phenotype shows the lowest
preference for oviposition on rice plants. The hybrids have intermediate
preference. Since there is overlap of all treatments further analysis is
needed to confirm the significance of these trends.

Table of means and standard deviations of the genotypes.

``` r
favstats(preference ~ genotype) -> ovipos.stats
ovipos.stats <- ovipos.stats[,c("genotype", "mean", "sd")]
names(ovipos.stats) <- c("Genotype", "Mean", "Standard Deviation")

ovipos.stats$Mean<- round(ovipos.stats$Mean, 2)
ovipos.stats$`Standard Deviation` <- round(ovipos.stats$`Standard Deviation`, 3)

kable(ovipos.stats,  
      caption = "**Table 1:** Descriptive statistics for the Oviposition data",
      digits = c(0, 2, 3), align = "ccrr") %>%
  kable_styling(full_width = FALSE, position = "left") %>%
  add_header_above(c("Preference ratio of of eggs laid on rice plants to the number laid on Leersia plants" = 3))
```

<table class="table" style="width: auto !important; ">
<caption>
**Table 1:** Descriptive statistics for the Oviposition data
</caption>
<thead>
<tr>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

Preference ratio of of eggs laid on rice plants to the number laid on
Leersia plants

</div>

</th>
</tr>
<tr>
<th style="text-align:center;">
Genotype
</th>
<th style="text-align:center;">
Mean
</th>
<th style="text-align:right;">
Standard Deviation
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
bl
</td>
<td style="text-align:center;">
0.46
</td>
<td style="text-align:right;">
1.073
</td>
</tr>
<tr>
<td style="text-align:center;">
br
</td>
<td style="text-align:center;">
1.20
</td>
<td style="text-align:right;">
1.077
</td>
</tr>
<tr>
<td style="text-align:center;">
f1
</td>
<td style="text-align:center;">
0.79
</td>
<td style="text-align:right;">
0.908
</td>
</tr>
<tr>
<td style="text-align:center;">
f2
</td>
<td style="text-align:center;">
1.33
</td>
<td style="text-align:right;">
1.053
</td>
</tr>
<tr>
<td style="text-align:center;">
leer
</td>
<td style="text-align:center;">
0.09
</td>
<td style="text-align:right;">
0.815
</td>
</tr>
<tr>
<td style="text-align:center;">
rice
</td>
<td style="text-align:center;">
1.60
</td>
<td style="text-align:right;">
0.975
</td>
</tr>
</tbody>
</table>

Adding a numeric variable in the data set to represent the proportion of
the genome inherited from the rice parent:

1 for the rice parent genotype

0 for the Leersia parent genotype (leer)

0.5 for the F1 and F2 hybrids (f1, f2)

0.25 for the backcross to the Leersia population (bl)

0.75 for the backcross to the rice backcross (br)

``` r
ovipos$prop.genome <- ovipos$genotype
ovipos$prop.genome<-gsub("rice", 1, ovipos$prop.genome)
ovipos$prop.genome<-gsub("leer", 0, ovipos$prop.genome)
ovipos$prop.genome<-gsub("f1", 0.5, ovipos$prop.genome)
ovipos$prop.genome<-gsub("f2", 0.5, ovipos$prop.genome)
ovipos$prop.genome<-gsub("bl", 0.25, ovipos$prop.genome)
ovipos$prop.genome<-gsub("br", 0.75, ovipos$prop.genome)
#unique(ovipos$genotype)
#unique(ovipos$prop.genome)
ovipos$prop.genome <- as.numeric(ovipos$prop.genome)

head(ovipos)
```

    ## # A tibble: 6 × 3
    ##   genotype preference prop.genome
    ##   <chr>         <dbl>       <dbl>
    ## 1 rice           1.67           1
    ## 2 rice           1.73           1
    ## 3 rice           2.66           1
    ## 4 rice           1.08           1
    ## 5 rice           0.96           1
    ## 6 rice           1.13           1

Fit the numeric variable you to the preference data using a linear
model.

This is called the additive model (model 1), whereby mean preference for
rice increases linearly with the proportion of the genome inherited from
the rice parent. Include your code.

``` r
attach(ovipos)
```

    ## The following objects are masked from ovipos (pos = 3):
    ## 
    ##     genotype, preference

``` r
ovipos.lm.additive <- lm(preference ~ prop.genome)
summary(ovipos.lm.additive)
```

    ## 
    ## Call:
    ## lm(formula = preference ~ prop.genome)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2230 -0.7199  0.0295  0.7195  3.0995 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.2855     0.1239   2.304   0.0216 *  
    ## prop.genome   1.3300     0.1927   6.904 1.47e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.041 on 522 degrees of freedom
    ## Multiple R-squared:  0.08367,    Adjusted R-squared:  0.08191 
    ## F-statistic: 47.66 on 1 and 522 DF,  p-value: 1.475e-11

Checking the assumptions of the linear model and evaluate the fit of the
model.

``` r
par(mfrow=c(2,2)) 
plot(ovipos.lm.additive)
```

![**Figure 2:** Resiual and normal quantile plots for the additive
model.](OvipositionDataAnalysis_files/figure-gfm/unnamed-chunk-7-1.png)

The data look reasonably normally distributed. However the distribution
of the residuals do not look equally distributed. I believe this is
because there are different numbers of individuals in each group.

``` r
visreg(ovipos.lm.additive, points.par = list(pch = 16, cex = 1.2, col = "purple"), ylab = "Oviposition Preference", xlab="Proportion of genome from rice oviposition preference genotype")
```

![**Figure 3:** Preference of oviposition of different genotypes of
Planthopper insects on Rice vs Leersia plants. The blue solid line is
the least square regression line, 95% confidence bands are shown in gray
shading.](OvipositionDataAnalysis_files/figure-gfm/unnamed-chunk-8-1.png)

``` r
anova(ovipos.lm.additive)
```

    ## Analysis of Variance Table
    ## 
    ## Response: preference
    ##              Df Sum Sq Mean Sq F value    Pr(>F)    
    ## prop.genome   1  51.64  51.635  47.663 1.475e-11 ***
    ## Residuals   522 565.50   1.083                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Comparing the additive model to the null intercept model. The high
F-value indicates that more variation is explained by the additive model
than the null intercept model.

Evaluate the model fit by calculating and comparing the AIC weight of
the previous model with the one of the null model where preference is
assumed to be independent from the proportion inherited from rice
parents.

``` r
#make a null model 

null <- lm(ovipos$preference ~ 1)

#AIC comparing the null to the additive model 

AICdelta2 <-  c(AIC(ovipos.lm.additive), AIC(null)) - min(AIC(ovipos.lm.additive), AIC(null))
AICdelta2
```

    ## [1]  0.00000 43.78574

``` r
Like.null.add <- exp(-0.5 * AICdelta2) # relative likelihoods of models
akweight.null.add <- Like.null.add/sum(Like.null.add) # Akaike weights

akweight.null.add
```

    ## [1] 1.000000e+00 3.104894e-10

``` r
cbind(round(akweight.null.add, 10), c("Additive Model", "Null Model")) %>% data.frame() -> weights_0
names(weights_0) <- c("AIC Weights","Model")
weights_0
```

    ##    AIC Weights          Model
    ## 1 0.9999999997 Additive Model
    ## 2        3e-10     Null Model

Weights represent relative liklihood of the model, in the weighted
values, a higher values indicates the better model.

Add another numeric variable to the data set to represent dominance
effects that might be present in the hybrids: - 0 for both parental
genotypes - 1 for the F1 hybrid genotype - 0.5 for the remaining three
hybrid genotypes

``` r
ovipos$dom.effect <- ovipos$genotype
ovipos$dom.effect<-gsub("rice", 0, ovipos$dom.effect)
ovipos$dom.effect<-gsub("leer", 0, ovipos$dom.effect)
ovipos$dom.effect<-gsub("f1", 1, ovipos$dom.effect)
ovipos$dom.effect<-gsub("f2", 0.5, ovipos$dom.effect)
ovipos$dom.effect<-gsub("bl", 0.5, ovipos$dom.effect)
ovipos$dom.effect<-gsub("br", 0.5, ovipos$dom.effect)

ovipos$dom.effect <- as.numeric(ovipos$dom.effect)
str(ovipos)
```

    ## spc_tbl_ [524 × 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ genotype   : chr [1:524] "rice" "rice" "rice" "rice" ...
    ##  $ preference : num [1:524] 1.67 1.73 2.66 1.08 0.96 1.13 2.72 1.11 0.62 0.17 ...
    ##  $ prop.genome: num [1:524] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ dom.effect : num [1:524] 0 0 0 0 0 0 0 0 0 0 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   genotype = col_character(),
    ##   ..   preference = col_double()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

Fit a second model to the same preference data that includes both of the
numeric variables.

``` r
ovipos.lm.additive.dom <- lm(ovipos$preference ~ ovipos$prop.genome + ovipos$dom.effect)
summary(ovipos.lm.additive.dom)
```

    ## 
    ## Call:
    ## lm(formula = ovipos$preference ~ ovipos$prop.genome + ovipos$dom.effect)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2186 -0.7120  0.0335  0.7114  3.0935 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.3780     0.1710   2.211   0.0275 *  
    ## ovipos$prop.genome   1.2884     0.1999   6.446 2.61e-10 ***
    ## ovipos$dom.effect   -0.1315     0.1673  -0.786   0.4321    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.041 on 521 degrees of freedom
    ## Multiple R-squared:  0.08475,    Adjusted R-squared:  0.08124 
    ## F-statistic: 24.12 on 2 and 521 DF,  p-value: 9.562e-11

Evaluate the model fit by calculating and comparing the AIC weights of
the model set composed of (1) the null, (2) the additive and the
additive plus dominance models.

``` r
#comparing null, additive, and dominance 

AICdelta3 <- c(AIC(ovipos.lm.additive), AIC(ovipos.lm.additive.dom), AIC(null)) -
min(AIC(ovipos.lm.additive), AIC(ovipos.lm.additive.dom), AIC(null))
AICdelta3
```

    ## [1]  0.000000  1.378805 43.785742

``` r
Like.null.add.dom <- exp(-0.5 * AICdelta3)           
akweight.null.add.dom <- Like.null.add.dom/sum(Like.null.add.dom) 
akweight.null.add.dom
```

    ## [1] 6.658340e-01 3.341660e-01 2.067344e-10

``` r
weight_table <- cbind(round(akweight.null.add.dom,4), c("Additive","Additive_Dom","Null")) %>% data.frame()
names(weight_table) <- c("AIC Weight", "Model")
weight_table
```

    ##   AIC Weight        Model
    ## 1     0.6658     Additive
    ## 2     0.3342 Additive_Dom
    ## 3          0         Null

The additive model has the highest weighted value, indicating it is the
best fit relative to the additive + dominance model and the null model.
However the additive + dominance model is a better fit than null model
based on the weighted values.

fit a third model that has the original genotype variable as the only
explanatory variable.

``` r
ovipos.og.lm <- lm(ovipos$preference ~ ovipos$genotype)
summary(ovipos.lm.additive)
```

    ## 
    ## Call:
    ## lm(formula = preference ~ prop.genome)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2230 -0.7199  0.0295  0.7195  3.0995 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.2855     0.1239   2.304   0.0216 *  
    ## prop.genome   1.3300     0.1927   6.904 1.47e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.041 on 522 degrees of freedom
    ## Multiple R-squared:  0.08367,    Adjusted R-squared:  0.08191 
    ## F-statistic: 47.66 on 1 and 522 DF,  p-value: 1.475e-11

Compare model fits by calculating the AIC weight of the four models.

``` r
#comparing all 4 models 

AICdelta <- c(AIC(ovipos.og.lm), AIC(ovipos.lm.additive), AIC(ovipos.lm.additive.dom), AIC(null)) -
  min(AIC(ovipos.og.lm), AIC(ovipos.lm.additive), AIC(ovipos.lm.additive.dom), AIC(null))

#AICdelta

Likihood <- exp(-0.5 * AICdelta)            # relative likelihoods of models
akweight <- Likihood/sum(Likihood)                     # Akaike weights

akweight
```

    ## [1] 9.978759e-01 1.414311e-03 7.098082e-04 4.391285e-13

``` r
weight_table2 <- cbind(round(akweight, 4), c("Original","Additive","Additive_Dom","Null")) %>% data.frame()

names(weight_table2) <- c("AIC Weight", "Model")

weight_table2
```

    ##   AIC Weight        Model
    ## 1     0.9979     Original
    ## 2     0.0014     Additive
    ## 3      7e-04 Additive_Dom
    ## 4          0         Null

The original model has the largest weight and therefore represents the
most likely hypothesis. This is the model that only included the
genotype as an explanatory varible. Biologically, this follows the
trends observed in figure 1 that prefrence was directly related to
genotype.

redo model comparison but using BIC

``` r
BICdelta <- c(BIC(ovipos.og.lm), BIC(ovipos.lm.additive), BIC(ovipos.lm.additive.dom), BIC(null)) - min(BIC(ovipos.og.lm), BIC(ovipos.lm.additive), BIC(ovipos.lm.additive.dom), BIC(null))
BICdelta
```

    ## [1]  3.927993  0.000000  5.640297 39.524251

``` r
BICLikihood <- exp(-0.5 * BICdelta)           
BICakweight <- BICLikihood/sum(BICLikihood) 
BICakweight
```

    ## [1] 1.169242e-01 8.334072e-01 4.966864e-02 2.179090e-09

``` r
weight_table3 <- cbind(round(BICakweight, 4), c("Original","Additive","Additive_Dom","Null")) %>% data.frame()

names(weight_table3) <- c("BIC Weight", "Model")

weight_table3
```

    ##   BIC Weight        Model
    ## 1     0.1169     Original
    ## 2     0.8334     Additive
    ## 3     0.0497 Additive_Dom
    ## 4          0         Null

Doing BIC analysis results in the additive model being the best fit
model to the data, opposed to the original model when AIC was
calculated. The difference in results could be due to the large sample
size because BIC penalizes more than AIC when the sample size is higher
than 7. In this case, the genotype preference was penalized more than
the proportion of the genome inherited by the rice parent, making the
additive model a better fit when using BIC.
