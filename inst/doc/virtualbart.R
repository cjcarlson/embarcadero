## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
suppressMessages(library(tidyverse))

## ----setup, echo=FALSE---------------------------------------------------
#devtools::install_github('cjcarlson/embarcadero')
library(embarcadero)
library(NLMR)
library(raster, quietly = T)
library(virtualspecies)
set.seed(12345)

## ------------------------------------------------------------------------
onelandscape <- function(x) {NLMR::nlm_gaussianfield(nrow = 150,
                                                     ncol = 150,
                                                     rescale = FALSE)}
climate <- stack(lapply(c(1:8), onelandscape))
names(climate) <- c('x1','x2','x3','x4','x5','x6','x7','x8')

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
                               type = 'presence-absence')

# Extract the associated climate values

occ <- SpatialPoints(sp.points$sample.points[,c('x','y')])
occ.df <- cbind(sp.points$sample.points,
                raster::extract(climate, occ))

# Finally, let's drop the long-lats and the "Real" ground truthed presence-absence values, and just leave behind an "Observed" and the climate data

occ.df <- occ.df[,-c(1:3)]


## ------------------------------------------------------------------------
# Train the model
sdm <- bart(y.train=occ.df[,'Observed'],
            x.train=occ.df[,c(2:9)],
            keeptrees = TRUE) # It's very important this is set to TRUE

# Predict the species distribution!
map <- predict(sdm, climate, splitby=5)

# How's it look?
par(mfrow=c(1,2))
plot(random.sp$pa.raster, main='True distribution')
plot(map, main='Predicted probability')

## ------------------------------------------------------------------------
# A variable importance diagnostic. What's behaving well?
# This takes a while to run normally! Drop the iter if you want a plot faster with more variance.
varimp.diag(occ.df[,2:9], occ.df[,'Observed'], iter=50)

## ------------------------------------------------------------------------
# Stepwise variable set reduction
step.model <- variable.step(x.data=occ.df[,2:9], 
                            y.data=occ.df[,'Observed'])
step.model

# Retrain the model
sdm <- bart(x.train=occ.df[, step.model], y.train=occ.df[,'Observed'],
            keeptrees = TRUE)

# Predict the species distribution!
map <- predict(sdm, climate)

# How's it look?
par(mfrow=c(1,2))
plot(random.sp$pa.raster, main='True distribution')
plot(map, main='Predicted probability')

## ------------------------------------------------------------------------
# How good is it?
summary(sdm) 

map <- predict(sdm, climate, splitby=50, quantiles=c(0.01))

# Check 
par(mfrow=c(1,2))
plot(map[[1]], main='Predicted')
plot(random.sp$pa.raster, main='Truth')
#plot(values(map) ~ values(random.sp$pa.raster))

