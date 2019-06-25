# 🌲🌉 Species distribution models with BART 🌉 🌲

__What's BART?__ 

Bayesian additive regression trees (BARTs) are an exciting alternative to other popular classification tree methods being used in ecology, like random forests or boosted regression trees. Whereas boosted regression trees fit an ensemble of trees each explaining smaller fractions of variance, BART starts by fitting a sum-of-trees model and then uses Bayesian "backfitting" with an MCMC algorithm to create a posterior draw. 

__Why BART?__ 

BART does well with model-free variable selection and handles irrelevant predictors well; the Bayesian aspect also comes with perks, like posterior distributions on predictions (without having to bootstrap, like you do with BRTs) and automated confidence intervals on partial dependency plots. So far, it also seems to avoid the problem BRTs have for niche modeling: with no automated selection of tree depth or tree complexity, BRTs tend to overfit especially with randomly-generated pseudoabsences and ensembling over subsets. In previous "bake-offs", BART commonly outperforms other methods.

__Are BARTs new to ecology?__

No, there is nothing new under the sun. [Yen et al.](https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1600-0587.2011.06651.x) used BARTs to examine habitat selection of birds back in 2000. But so far, no, no one is using them for species distribution modeling (as far as I can tell).

__What does embarcadero do?__

This package is basically a wrapper around 'dbarts'  with a few tools
- spatial prediction 
- variable importance 
- stepwise variable elimination
- AUC calculation 

It's aggressively incomplete, with more to come hopefully. That would ideally include:
- prediction quantiles from the posteriors (easy, next to be added)
- automatic Nice Plots for partials
- parallelized prediction to Make More Faster

__Can I help__?

Please! I'm hoping to write this up for _Methods in Ecology & Evolution_ sometime this year.

__Why is it called embarcadero?__

Because of Embarcadero BART station, and because I'm homesick for Humphry Slocombe. 
