# shinyTC.CohortProgression
An R package encapsulating a Shiny app to explore Tanner crab cohort progressions

## Introduction
This R package encapsulates a Shiny app that allows the user to explore the effects of changes in various life history and population dynamics processes on the progression of a cohort of Tanner crab (*Chionoecetes bairdi*) in the eastern Bering Sea. 

The packaging of this app is based on the article https://www.r-bloggers.com/packaging-shiny-applications-a-deep-dive/.

## Installation
```r
devtools::install_github("wStockhausen/shinyTC.CohortProgression")
```
You will also need the following R packages installed to run it locally:

* shiny
* shinyjs
* ggplot2
* reshape2

as well as the following packages available from https://github.com/wStockhausen:

* rCompTCMs
* rTCSAM02
* wtsUtilities

(not sure if the above list is complete)

## Run the app
To launch the app, run 
```r
  shiny::runApp(shinyTC.CohortProgression::launchApp())
```
  
from the commandline in R (or RStudio). Default values are for males from the 2018 assessment model.

## Tanner crab cohort progression

This app allows the user to visualize, based on population processes for an unfished stock as incorporated in the Tanner crab stock assessment model, the progression of a single-sex cohort of Tanner crab through a succession of years. The progression takes the initial size distribution of immature, new shell crab at recruitment to the assessment model and projects it forward in time on an annual basis, applying size- and life stage-specific model processes for natural mortality, annual molting, growth, terminal molt, and changes in shell condition to the relative abundance of crab by maturity state (*m*), shell condition (*s*), and size (*z*). Plots describing all model processes and the resulting cohort progression are included in the associated tabbed sections of this app. 

The user may also download the cohort progression model results as an RData file or create a pdf report that includes all the plots shown in the app (using buttons located on the *Cohort Prgression* tab).

The cohort progression model consists of:

* setting a general configuration specifying the model size range, the bin size for size classes, the maturity state names, the shell condition names, and the molt timing
* setting natural mortality rates
* setting annual molt probabilities
* characterizing growth (molt increments)
* defining probabilties of terminal molt
* defining the size distribution at recruitment

### Recruitment
Annual recruitment to the model may be spread across several size bins and may reflect several age classes. All recruitment occurs as immature, new shell crab. Here, recruits to the model in a given year are regarded as a "cohort". A truncated gamma probability distribution, $\gamma_N(z|\alpha,\beta)$, is used to describe the relative abundance of recruiting crab. $\gamma_N(z_i|\alpha,\beta)$ is typically truncated after a few size bins and the resulting distribution is normalized to sum to 1: 

  $$\gamma_N(z_i|\alpha,\beta) = \frac{\gamma(z_i|\alpha,\beta)} {\sum_{i}{\gamma(z_i|\alpha,\beta)}}$$
  
where $z_i$ is the mid-point of the *i*th size bin, $\alpha$ is the location parameter for the gamma distribution, $\beta$ is the scale parameter, and the sum in the denominator is over the non-truncated size bins.

### Natural mortality rates
Natural mortality (*M*)in the cohort progression model is assumed to be a function of maturity state, such that immature and mature crab may experience different rates of natural mortality (but these rates do not depend on shell condition). These are parameterized using the folllowing multiplicative approach:

$$M_{m,s} = \delta M_m * M_0$$

where $M_0$ is a baseline value for *M* and $\delta M_m$ is the maturity state-specific multiplier.

### Molting
Immature crab in the assessment model are (currently) assumed to molt annually until their terminal molt to maturity. In order to explore the implications of skip molting on cohort progression, the cohort progression model incorporates the ability to specify a size-dependent probbility of molting for immature crab. Crab that don't molt are classified as "old shell" during the following year based on the appearance of their carapace, while crab that do molt are classified as "new shell". The probability that immature crab will undergo a molt, $p^M_{m,s}(z)$, is allowed to be a decreasing logistic function of size (but independent of age) given by:

$$p^M_{i,n}(z_j) = 1 \quad \text{where} \quad z_j < z_{min}$$

$$p^M_{i,n}(z_j) = 1 - \frac {1-p_{min}} {1+e^{(z_j-z_{50})/b_{50}}} \quad \text{where} \quad z_j \ge z_{min}$$

$$p^M_{i,o}(z_j) = 1$$

where *i* indicates "immature", *n* indicates "new shell", *o* indicates "old shell", $z_j$ is the midpoint of the *j*th size bin, $p_{min}$ is the minimum probability of large, immature new shell crab molting, $z_{50}$ is the inflection point of the logistic curve, $b_{50}$ is the scale of the logistic curve, and $z_min$ is the minimum size that immature crab potentially undergo skip molting.

### Growth
Mean post-molt size $\overline{z}_{pst}$ is modeled as a power function of pre-molt size $z_{pre}$, paramterized as:

$$\overline{z}_{pst} = z^A_{pst}\cdot exp\{\frac{log(\overline{z}^B_{pst}/\overline{z}^A_{pst})} {log(z^B_{pre}/z^A_{pre})} \cdot log(z_{pre}/z^A_{pre})\}$$

where $z^A_{pst}$ is the estimated mean post-molt size at pre-molt size $z^A_{pre}$ and $z^B_{pst}$ is the estimated mean post-molt size at pre-molt size $z^B_{pre}$. The actual post-molt size $z_{pst}$ for a crab, given that it was in size bin $z_i$ prior to molting, is described using a $\gamma$ distribution, with the probability that the post-molt crab falls into the *j*th size bin $z_j$ given by:

$$p(z_j|z_i)=\int_{\alpha_i(z_j)-\frac{\delta \alpha}{2}}^{\alpha_i(z_j)+\frac{\delta \alpha}{2}}\gamma(\alpha-\overline{\alpha_i}) \cdot d\alpha$$

where $\alpha_i(z)=\frac{z-z_i}{\beta}$ represents the scaled molt increment, $\overline{\alpha_i}=\frac{\overline{z}_{pst}-z_i}{\beta}$ is the scaled mean molt increment for pre-molt size bin $z_i$, $\delta \alpha = \frac{\delta z}{\beta}$ is the scaled size bin width, and $\beta$ is the scale factor. The largest model size bin, $z_{max}$, functions as an accumulator bin, so it is handled slightly differently: the probability of a post-molt crab ending up in the largest size bin is simply the probability of it ending up at any larger size than its lower cutpoint:

$$p(z_{max}|z_i)=\int_{\alpha_i(z_{max})-\frac{\delta \alpha}{2}}^{\inf}\gamma(\alpha-\overline{\alpha_i}) \cdot d\alpha = 1-\int_{0}^{\alpha_i(z_{max})-\frac{\delta \alpha}{2}}\gamma(\alpha-\overline{\alpha_i}) \cdot d\alpha$$

The model also allows one to limit potential growth to a maximum number of size bins, $n_{max}$, in which case $p(z_j|z_i)$ is set to 0 for $j-i > n_{max}$ and normalized to sum to 1 for $j-i \le n_{max}$.

### Terminal molt

The probability that a molt is the terminal molt to maturity, $p^T(z)$ is parameterized in the assessment model on the logit scale as a nonparameteric, smooth function of pre-molt size. Inidividual parameters are estimated on the logit scale for each size bin, with likelihood penalties applied to the second order differences to impose a smoothness constraint on the resulting shape of the function. In addition, the first size bin at which terminal molt *can* occur ($p^T(z_0)>0$)and the first size bin at which it *must* occur ($p^T(z_1) \equiv 1$) can be set to reduce the number of logit-scale parameters that must be estimated. In the interest of simplicity, it is also possible here to use a logistic function parameterized by size at the inflection point ($z_{50}$) and scale ($b_{50}$).

