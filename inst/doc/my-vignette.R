## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

suppressMessages(library(tidyverse))

## ----setup---------------------------------------------------------------
library(embarcadero, quietly = TRUE)
library(velox)

set.seed(12345)

## ------------------------------------------------------------------------
files <- list.files('~/Github/embarcadero/vignettes/covariates', full.names=TRUE)
covs <- raster::stack(lapply(files, raster))

cov.big <- covs

  for(i in 1:nlayers(covs)) {
    vx <- velox(covs[[i]])
    vx$aggregate(factor=c(5,5), aggtype='mean')
    if (i == 1) { cov.big <- stack(vx$as.RasterLayer())
    } else { cov.big <- stack(cov.big,vx$as.RasterLayer())
    }
    print(i)
  }

names(cov.big) <- names(covs)

## ------------------------------------------------------------------------
ticks <- read.csv('~/Github/embarcadero/vignettes/Hytr.csv')
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
bart.auc(first.model, all.cov[,'tick'])

## ---- fig.fullwidth=TRUE-------------------------------------------------

pred.prelim <- predict.dbart.raster(model = first.model,
                                    inputstack = cov.big)
plot(pred.prelim)

points(SpatialPoints(ticks[,c('Longitude.X','Latitude.Y')]), 
        pch=16, cex=0.2)


## ------------------------------------------------------------------------
varimp(model=first.model, 
       names=names(covs), 
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
bart.auc(good.model, all.cov[,'tick'])
# Do the spatial prediction
hytr.layer <- predict.dbart.raster(model = good.model,
                                    inputstack = covs[[varlist]])
# How's it look?
plot(hytr.layer)


## ------------------------------------------------------------------------

# Update those pesky covariates
covs <- stack(covs, hytr.layer)
names(covs)[12]='hytr'

# Read in the data
cchf <- read.csv('~/Github/embarcadero/vignettes/CCHF_1953_2012_Messina.csv')
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
cchf.map <- predict.dbart.raster(model = cchf.model$Model.object,
                                    inputstack = covs[[cchf.model$Variables]])
# How's it look?
plot(hytr.layer)
plot(cchf.map)

## ------------------------------------------------------------------------
varimp(cchf.model$Model.object, cchf.model$Variables, plots=TRUE)

## ------------------------------------------------------------------------
# Let's do one variable
p <- pdbart(cchf.model$Model.object, xind=hytr, pl=TRUE)
# Let's do em all
p <- pdbart(cchf.model$Model.object, pl=TRUE)

