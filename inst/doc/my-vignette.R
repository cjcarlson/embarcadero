## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

suppressMessages(library(tidyverse))

## ----setup, echo=FALSE---------------------------------------------------
library(embarcadero)
library(velox)

set.seed(12345)

## ------------------------------------------------------------------------
data(covs)

cov.big <- bigstack(covs, 10)

## ------------------------------------------------------------------------
data(ticks)
head(ticks)
nrow(ticks)

## ------------------------------------------------------------------------

mod <- SpatialPointsDataFrame(ticks[,3:4],data.frame(ticks[,1]))
names(mod@data) <- 'Presence'
# Rasterizing makes unique points to the grid cell
tmp=rasterize(mod, covs[[1]], field="Presence", fun="min")
pts.sp1=rasterToPoints(tmp, fun=function(x){x>0})
nrow(pts.sp1)

pres.cov <- raster::extract(covs, pts.sp1[,1:2])
head(pres.cov)


## ------------------------------------------------------------------------
#Generate the data
absence <- randomPoints(covs,nrow(ticks)) 
abs.cov <- raster::extract(covs, absence)

#Code the response
pres.cov <- data.frame(pres.cov); pres.cov$tick <- 1
abs.cov <- data.frame(abs.cov); abs.cov$tick <- 0

# And one to bind them
all.cov <- rbind(pres.cov, abs.cov)
head(all.cov)

# Let's just clean it up a little bit
all.cov <- all.cov[complete.cases(all.cov),]

## ------------------------------------------------------------------------
first.model <- bart(all.cov[,1:11], all.cov[,'tick'], keeptrees=TRUE)

## ------------------------------------------------------------------------
bart.auc(first.model)

## ---- fig.fullwidth=TRUE-------------------------------------------------

pred.prelim <- bart.map(model = first.model,
                        inputstack = cov.big)
plot(pred.prelim, main='Hyalomma truncatum')

points(SpatialPoints(ticks[,c('Longitude.X','Latitude.Y')]), 
        pch=16, cex=0.2)


## ------------------------------------------------------------------------
varimp(model=first.model, 
       plots = TRUE)

## ------------------------------------------------------------------------
varimp.plot(x.data=all.cov[,1:11], 
            y.data=all.cov[,'tick'],
            iter=100)

## ------------------------------------------------------------------------

varlist <- variable.step(x.data=all.cov[,1:11],
              y.data=all.cov[,'tick'],
              n.trees=10)

## ------------------------------------------------------------------------

# Rerun the model
good.model <- bart(all.cov[,varlist], all.cov[,'tick'], keeptrees=TRUE)
# Check the AUC
bart.auc(good.model)
# Do the spatial prediction
hytr.layer <- bart.map(model = good.model,
                       inputstack = covs)
# How's it look?
plot(hytr.layer, main='Hyalomma truncatum')


## ------------------------------------------------------------------------

# Update those pesky covariates
covs <- stack(covs, hytr.layer)
names(covs)[12]='hytr'

# Read in the data
data(cchf)
head(cchf)
nrow(cchf)

# Spatial thinning checks; this also limits it to African points
cchf <- cchf[,c('LONGITUDE','LATITUDE')]; cchf$Presence = 1
cchf <- SpatialPointsDataFrame(cchf[,1:2],data.frame(Presence=cchf[,3]))
tmp=rasterize(cchf, covs[[1]], field="Presence", fun="min")
pts.sp1=rasterToPoints(tmp, fun=function(x){x>0})
nrow(pts.sp1)

# Extract presence values
pres.cov <- raster::extract(covs, pts.sp1[,1:2])
pres.cov <- na.omit(pres.cov)
head(pres.cov)

#Generate pseudoabsences
absence <- randomPoints(covs,nrow(pres.cov)) 
abs.cov <- raster::extract(covs, absence)

#Code the response
pres.cov <- data.frame(pres.cov); pres.cov$cchf <- 1
abs.cov <- data.frame(abs.cov); abs.cov$cchf <- 0

# And one to bind them
all.cov <- rbind(pres.cov, abs.cov)
all.cov <- all.cov[complete.cases(all.cov),]; nrow(all.cov)
head(all.cov)

# This part automates the variable selection and returns the model
cchf.model <- bart.var(xdata=all.cov[,1:12], 
                       ydata=all.cov[,'cchf'], 
                       iter.step = 100, 
                       tree.step = 10,
                       iter.plot = 100)

# Do the spatial prediction
cchf.map <- bart.map(model = cchf.model,
                     inputstack = covs)
# How's it look?
plot(cchf.map, main='CCHF')

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(hytr.layer, main='H. truncatum')
plot(cchf.map, main='CCHF')

## ------------------------------------------------------------------------

cchf.map <- bart.map(model = cchf.model,
                     inputstack = covs,
                     ci=TRUE)

## ------------------------------------------------------------------------
varimp(cchf.model, plots=TRUE)

## ------------------------------------------------------------------------
# Let's do one variable
p <- pdbart(cchf.model, xind=hytr, pl=TRUE)

# That's not very pretty. Let's use embarcadero's version, which allows multiple ways of visualizing the posterior
partial(cchf.model, 'hytr', trace=FALSE, ci=TRUE, equal=FALSE)

# Hm.... that's a little better. Let's up the smooth (this ISN'T a smoother - it just takes more points to fit the curve) and make the spacing equal.
partial(cchf.model, 'hytr', trace=TRUE, ci=FALSE, equal=TRUE, smooth=10)

# Let's do a couple at once
partial(cchf.model, x.vars=c('bio12','bio13'), trace=FALSE, ci=TRUE, panel=TRUE, smooth=5)

## ------------------------------------------------------------------------
p <- pd2bart(cchf.model, xind=c('ndvi.mean', 'ndvi.amp'), pl=TRUE)

