## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
suppressMessages(library(tidyverse))
options(digits=2)


## ----setup, echo=TRUE----------------------------------------------------
#devtools::install_github('cjcarlson/embarcadero')
library(embarcadero, quietly = T)
library(dismo, quietly=T)
library(NLMR, quietly = T)
library(raster, quietly = T)
library(virtualspecies, quietly = T)
set.seed(42)

## ------------------------------------------------------------------------
onelandscape <- function(x) {NLMR::nlm_gaussianfield(nrow = 150,
                                                     ncol = 150,
                                                     rescale = FALSE)}
climate <- stack(lapply(c(1:8), onelandscape))
xnames <- c('x1','x2','x3','x4','x5','x6','x7','x8')
names(climate) <- xnames

plot(climate[[1]],main='An imaginary variable')

## ------------------------------------------------------------------------

# Generate the species' climatic niche

random.sp <- generateRandomSp(climate[[1:4]], 
                              # ^ These are the informative predictors
                              approach="pca",
                              relations='gaussian',
                              species.prevalence=0.5,
                              realistic.sp = TRUE,
                              PA.method='threshold')

# Generate some presences, and some absences, with imperfect detection

sp.points <- sampleOccurrences(random.sp,
                               n=250,
                               type = 'presence-absence',
                               detection.probability = 0.9)

# Extract the associated climate values

occ <- SpatialPoints(sp.points$sample.points[,c('x','y')])
occ.df <- cbind(sp.points$sample.points,
                raster::extract(climate, occ))

# Finally, let's drop the long-lats and the "Real" ground truthed presence-absence values, and just leave behind an "Observed" and the climate data

occ.df <- occ.df[,-c(1:3)]


## ------------------------------------------------------------------------
# Check out the data structure
head(occ.df)

# Train the model
sdm <- bart(y.train=occ.df[,'Observed'],
            x.train=occ.df[,xnames],
            keeptrees = TRUE) # It's very important this is set to TRUE

# Model diagnostics
summary(sdm)

# Predict the species distribution!
map <- predict(sdm, climate, quiet=TRUE)

# How's it look?
par(mfrow=c(1,2))
plot(random.sp$pa.raster, main='True distribution')
plot(map, main='Predicted probability')

## ------------------------------------------------------------------------
# A variable importance diagnostic. What's behaving well?
# This takes a while to run normally! Drop the iter if you want a plot faster with more variance.
varimp.diag(occ.df[,xnames], occ.df[,'Observed'], iter=50, quiet=TRUE)

## ------------------------------------------------------------------------
# Stepwise variable set reduction
step.model <- variable.step(x.data=occ.df[,xnames], 
                            y.data=occ.df[,'Observed'],
                            quiet=TRUE)
step.model

# Retrain the model
sdm <- bart(x.train=occ.df[, step.model], y.train=occ.df[,'Observed'],
            keeptrees = TRUE)

# Predict the species distribution!
map <- predict(sdm, climate, quiet=TRUE)

# How's it look?
par(mfrow=c(1,2))
plot(random.sp$pa.raster, main='True distribution')
plot(map, main='Predicted probability')

## ------------------------------------------------------------------------
# How good is it?
summary(sdm) 

map <- predict(sdm, climate, quantiles=c(0.025, 0.975), quiet=TRUE)

# Check 
par(mfrow=c(1,2))
plot(map[[1]], main='Predicted')
plot(random.sp$pa.raster, main='Truth')
#plot(values(map) ~ values(random.sp$pa.raster))

## ------------------------------------------------------------------------
partial(sdm, x.vars=c('x4'),
        smooth=5,
        equal=TRUE,
        trace=FALSE)

## ------------------------------------------------------------------------
gbm1 <- gbm.step(data=occ.df, gbm.x = 2:5, gbm.y = 1, 
                 family = "bernoulli",
                 tree.complexity = 5, 
                 learning.rate = 0.01, 
                 bag.fraction = 0.5)
par(mfrow=c(1,1))
gbm.plot(gbm1, variable.no=4, rug=TRUE,
         #main="BRT partial",
         plot.layout=c(1,1))


