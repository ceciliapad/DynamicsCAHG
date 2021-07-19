Code to reproduce SDM

# Population inter-connectivity over the past 120,000 years explains distribution and diversity of Central African hunter-gatherers.

This document contains the R code to reproduce the analyses reported in Padilla-Iglesias et al. "Population inter-connectivity over the past 120,000 years explains distribution and diversity of Central African hunter-gatherers".

# 1. Modelling density and distribution of contemporary CAHG

## Read in data and basic plots 

```
camps <- read.csv("congo-camps.csv")
pr <- subset(camps, camps$P_A=="1")

coord<- data.frame(long=camps$Longitud, lat=camps$Latitud)
coord_pr<- data.frame(long=pr$Longitud, lat=pr$Latitud)

my_extent <- extent( min(camps$Longitud), max(camps$Longitud), min(camps$Latitud), max(camps$Latitud))

# Load packages
library (maps)
library(raster)
library(Jodorowsky)

# Load natural earth raster 
g <- list.files(pattern="NE1")
g <- raster(g)

congo_land <- crop(g, my_extent)

# Plot camps
png ("my_map.png", units="in", width=6, height=4, res=500)
par (mar=c(3,4,3,0))
#par (mar=c(0,4,0,0))
plot(congo_land, col=rev(hcl.colors(100, palette="Earth")), legend=F, main="Central
     African Hunter-Gatherers", box=F)
points(coord_pr, pch=16, col=holy_mountain(2)[4], cex=0.3)
map(add=T)
dev.off()

```

## Extract bioclimatic data from .nc files by Beyer et al.2020 


```
file <- "LateQuaternary_Environment.nc";

# get variables you think could be useful
# combination of significant predictors in Olivero et al. 2016 as well
# as variables that have been hypothesised to affect HG presence (Kelly, 2013)
require("ncdf4")
require("lattice")
require("ggplot2")
library(raster)
library(maptools)

# get variables you think could be useful
env_nc      <- ncdf4::nc_open(file)
longitude   <- ncdf4::ncvar_get(env_nc, "longitude")
latitude    <- ncdf4::ncvar_get(env_nc, "latitude")
years       <- ncdf4::ncvar_get(env_nc, "time")
months      <- ncdf4::ncvar_get(env_nc, "month")
temperature <- ncdf4::ncvar_get(env_nc, "temperature")
biome       <- ncdf4::ncvar_get(env_nc, "biome")
bio1        <- ncdf4::ncvar_get(env_nc, "BIO1")
bio5        <- ncdf4::ncvar_get(env_nc, "BIO5")
bio6        <- ncdf4::ncvar_get(env_nc, "BIO6")
bio7        <- ncdf4::ncvar_get(env_nc, "BIO7")
bio12       <- ncdf4::ncvar_get(env_nc, "BIO12")
bio15       <- ncdf4::ncvar_get(env_nc, "BIO15")
lai         <- ncdf4::ncvar_get(env_nc, "lai")
npp         <- ncdf4::ncvar_get(env_nc, "npp")

ncdf4::nc_close(env_nc)

# extract variables for the present
my_year      <- 0;   # Choose a time period from present to -120000 (120000BP)
my_longitude <- coord$long;
my_latitude  <- coord$lat;  

biome_slice <- biome[,,years == my_year]
# Convert biomes into megabiomes according to BIOME 4.2 MODEL and original paper

biome_slice[biome_slice == "11"] <- "B" # Boreal forest
biome_slice[biome_slice == "10"] <- "B"
biome_slice[biome_slice == "18"] <- "B" # This is normally boreal parkland
biome_slice[biome_slice == "21"] <- "D" # Desert
biome_slice[biome_slice == "27"] <- "D"
biome_slice[biome_slice == "14"] <- "G" # Grassland and dry shrubland
biome_slice[biome_slice == "13"] <- "G"
biome_slice[biome_slice == "19"] <- "G"
biome_slice[biome_slice == "20"] <- "G"
biome_slice[biome_slice == "12"] <- "S" # Savanna and dry woodland
biome_slice[biome_slice == "15"] <- "S"
biome_slice[biome_slice == "4"] <- "T" # Temperate forest
biome_slice[biome_slice == "5"] <- "T"
biome_slice[biome_slice == "7"] <- "T"
biome_slice[biome_slice == "8"] <- "T"
biome_slice[biome_slice == "9"] <- "T"
biome_slice[biome_slice == "16"] <- "T"
biome_slice[biome_slice == "17"] <- "T"
biome_slice[biome_slice == "1"] <- "F" # Tropical forest
biome_slice[biome_slice == "2"] <- "F"
biome_slice[biome_slice == "3"] <- "F"
biome_slice[biome_slice == "22"] <- "Tu" # Tundra
biome_slice[biome_slice == "23"] <- "Tu"
biome_slice[biome_slice == "24"] <- "Tu"
biome_slice[biome_slice == "25"] <- "Tu"
biome_slice[biome_slice == "26"] <- "Tu"
biome_slice[biome_slice == "6"] <- "W" # Warm, temperate forest
biome_slice[biome_slice == "28"] <- "I" # Ice - keep in mind 0 is water


biome_slice[biome_slice == "B"] <- 1 # Boreal forest
biome_slice[biome_slice == "D"] <- 2 # Desert
biome_slice[biome_slice == "G"] <- 3 # Grassland and dry shrubland
biome_slice[biome_slice == "S"] <- 4 # Savanna and dry woodland
biome_slice[biome_slice == "T"] <- 5 # Temperate forest
biome_slice[biome_slice == "F"] <- 6 # Tropical forest
biome_slice[biome_slice == "Tu"] <- 7 # Tundra
biome_slice[biome_slice == "W"] <- 8 # Warm, temperate forest
biome_slice[biome_slice == "I"] <- 9 # Ice

bio1_slice <- bio1[,,years==my_year]
bio5_slice <- bio5[,,years==my_year]
bio6_slice <- bio6[,,years==my_year]
bio7_slice <- bio7[,,years==my_year]
bio12_slice <- bio12[,,years==my_year]
bio15_slice <- bio15[,,years==my_year]
lai_slice <- lai[,,years==my_year]
npp_slice <- npp[,,years==my_year]

# extent of nc files from Beyer et al.2020
world_limits <- c(-179.75, 179.75, -59.75, 89.75)

# make rasters
biome_r <- raster(biome_slice) 
bio1_r <- raster(bio1_slice) 
bio5_r <-raster(bio5_slice) 
bio6_r <-raster(bio6_slice) 
bio7_r <-raster(bio7_slice) 
bio12_r <-raster(bio12_slice) 
bio15_r <-raster(bio15_slice) 
lai_r <-raster(lai_slice) 
npp_r <-raster(npp_slice) 

var_0BP<- stack (biome_r, bio1_r, bio5_r, bio6_r,bio7_r,bio12_r, bio15_r,
                 lai_r, npp_r)

# transpose x to y and double flip the map
var_0BP <- flip(t(var_0BP), direction="y")

# then rotate from 0:360 to -180:180
#var_0BP <- raster::rotate(var_0BP)

crs(var_0BP) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
extent(var_0BP) <- world_limits 
names(var_0BP) <- c("Biome", "Bio1", "Bio5", "Bio6", "Bio7", "Bio12", "Bio15",
                    "LAI", "NPP")


congo_0BP <- crop(var_0BP, my_extent)


DF_ENV <- raster::extract(congo_0BP, camps[,c("Longitud","Latitud")])
#DF_ENV_original <- extract(congo_0BP_original, camps[,c("Longitud","Latitud")])
DF_ALL <- cbind(camps, DF_ENV)

```

## Inspect  correlations between predictors
```
library(corrplot)

DF_USE <- DF_ALL[complete.cases(DF_ALL),]

# Correlations between predictors (all need to be numeric)
DF_USE$Biome <- as.numeric(DF_USE$Biome)
cor_mat <- cor(DF_USE[,-c(1:12)], method='spearman')

# we plot the correlation coefficients as percentages.
corrplot.mixed(cor_mat, tl.pos='lt', tl.cex=0.6, number.cex=0.5, addCoefasPercent=T)

AIC(m1)

# Use our function to identify all pairs of variables with correlation |r| > 0.7
# and remove the least important variable
# Adapted from Dormann et al. 2013

select07 <- function(predictor_dat, response_dat, cor_mat=NULL, threshold=0.7){
  # Function for calculating AIC - we use univariate GLMs with linear and quadratic term
  var.imp <- function (predictor, response) {
    AIC(glm(response ~ predictor + I(predictor^2), binomial)) }
  # Calculate AIC for all predictor variables
  aic_imp <- apply(predictor_dat, 2, var.imp, response= response_dat) # Names of sorted variables
  sort_imp <- names(sort(aic_imp))
  # Calculate correlation matrix if not provided in function call
  if (is.null(cor_mat)) {
    cor_mat <- cor(predictor_dat, method='spearman')
  }
  # Identifies correlated variable pairs:
  diag(cor_mat)=NA
  pairs <- which(abs(cor_mat)>= threshold, arr.ind=T)
  # Identify which variables should be excluded
  exclude <- NULL
  for (i in 1:length(sort_imp)) 
  {
    if ((sort_imp[i] %in% row.names(pairs))& 
        ((sort_imp[i] %in% exclude)==F)) {
      cv <- cor_mat[setdiff(row.names(cor_mat),exclude),sort_imp[i]] 
      cv <- cv[setdiff(names(cv),sort_imp[1:i])]
      exclude <- c(exclude,names(which((abs(cv)>=threshold))))
    } }
  # Select set of weakly correlated predictors:
  pred_sel <- sort_imp[!(sort_imp %in% exclude)]
  # Return list with AIC, correlation matrix, and final predictors:
  return(list(AIC=sort(aic_imp), cor_mat=cor_mat, pred_sel=pred_sel)) 
}

# Try out the function

var_sel <- select07(predictor_dat=DF_USE[,-c(1:11)], response_dat=DF_USE$P_A,
                    cor_mat=cor_mat, threshold=0.7) 
pred_sel <- var_sel$pred_sel
pred_sel

# Make sure biome is taken as categorical variable
DF_USE$Biome <- as.factor(DF_USE$Biome)

```

## Build GLMs 


```

# Rural population density
m_full_rural <- glm(P_A ~ elevation + slope + dist_water_mass +
                      dist_min_river + rur_pop_dens +
                      Biome + Bio5 + Bio7 + Bio15 +
                      LAI, family="binomial", data= DF_USE)

# Distance to populated places
m_full_distance <- glm(P_A ~ elevation + slope + dist_water_mass +
                         dist_min_river + dist_pop_places +
                         Biome + Bio5 + Bio7 + Bio15 +
                         LAI, family="binomial", data= DF_USE)

# Only bioclimatic variables
m_abiotic <- glm(P_A ~ elevation + slope + dist_water_mass +
                   dist_min_river + 
                   Biome + Bio5 + Bio7 + Bio15 +
                   LAI, family="binomial", data= DF_USE)

# Only major water bodies (no rivers)
m_abiotic_norivers <- glm(P_A ~ elevation + slope + dist_water_mass +
                            Biome + Bio5 + Bio7 + Bio15 +
                            LAI, family="binomial", data= DF_USE)

#Explained deviance:
library(ecospat) 

ecospat.adj.D2.glm(m_abiotic)

# Build predictor lists

my_preds <- c("elevation", "slope", "dist_water_mass",
              "dist_min_river", "Biome", "Bio5", "Bio7",
              "Bio15", "LAI")

my_preds_norivers <- c("elevation", "slope", "dist_water_mass",
                      "Biome", "Bio5", "Bio7",
                      "Bio15", "LAI")

my_preds_dist <- c("elevation", "slope", "dist_water_mass",
                   "dist_min_river",  "Biome", "Bio5", "Bio7",
                   "Bio15", "LAI", "dist_pop_places")

my_preds_rur <- c("elevation", "slope", "dist_water_mass",
                  "dist_min_river",  "Biome", "Bio5", "Bio7",
                  "Bio15", "LAI", "rur_pop_dens")
```


## Validate model and test performance

To move beyod the data - use training and testing sets (80% of the data for model building, 20% NEW data only for validation)

```
library(dismo)

# Build a function for cross-validation
cross.val <- function(model, k=5, data = model$data){
  # Ensure the dismo package is loaded for "kfold" function: require(dismo)
  # This function returns a vector with group assignments,
  # meaning that each data point is assigned to a specific fold/group
  ks <- dismo::kfold(model$data, k = k, by = model$data[,as.character(formula(model)[2])])
  # We prepare an empty data frame for storing the predictions
  crossval_preds <- data.frame(row = row.names(data), crossval_preds = numeric(length = nrow(data)))
  # We loop through the k folds
  for(i in seq_len(k)){
    # all data points belonging to fold i will be used as test data
    test <- data[ks==i,]
    # all other data points (from the other folds) will serve as training data
    train <- data[ks!=i,]
    # Now we re-estimate/update the model using the new training data
    modtmp <- update(model, data = train)
    # We make predictions to the hold-out test data and store these
    crossval_preds[which(ks==i),2] <-
      predict(modtmp, newdata = test, type = 'response')
  }
  # The function outputs a vector of cross-validated predictions
  return(crossval_preds) }

# now we split data into 5-folds and re-calibrate model using 4/5ths of the data
preds_fav <- preds_cv <- cross.val(m_abiotic)
preds_fav_norivers <- preds_cv_norivers <- cross.val(m_abiotic_norivers)
preds_fav_rural <- preds_cv_rural <- cross.val(m_full_rural)
preds_fav_distance <- preds_cv_distance <- cross.val(m_full_distance)

# convert probability into  favourability to evaluate models (compensate uneven Presences and absences)
# divide preds_cv by presences/absences
preds_fav[,2] <- (preds_cv[,2]/(1- preds_cv[,2]))/((4577/31186)+(preds_cv[,2]/(1- preds_cv[,2])))
fitted_fav <- (m_abiotic$fitted.values/(1-m_abiotic$fitted.values))/((4577/31186)+(m_abiotic$fitted.values/(1- m_abiotic$fitted.values)))
fitted_cv <- (m_abiotic$fitted.values)

preds_fav_norivers[,2] <- (preds_cv_norivers[,2]/(1- preds_cv_norivers[,2]))/((4577/31186)+(preds_cv_norivers[,2]/(1- preds_cv_norivers[,2])))
fitted_fav_norivers <- (m_abiotic_norivers$fitted.values/(1-m_abiotic_norivers$fitted.values))/((4577/31186)+(m_abiotic_norivers$fitted.values/(1- m_abiotic_norivers$fitted.values)))
fitted_cv_norivers <- (m_abiotic_norivers$fitted.values)

preds_fav_rural[,2] <- (preds_cv_rural[,2]/(1- preds_cv_rural[,2]))/((4577/31186)+(preds_cv_rural[,2]/(1- preds_cv_rural[,2])))
fitted_fav_rural <- (m_full_rural$fitted.values/(1-m_full_rural$fitted.values))/((4577/31186)+(m_full_rural$fitted.values/(1- m_full_rural$fitted.values)))
fitted_cv_rural <- (m_full_rural$fitted.values)

preds_fav_distance[,2] <- (preds_cv_distance[,2]/(1- preds_cv_distance[,2]))/((4577/31186)+(preds_cv_distance[,2]/(1- preds_cv_distance[,2])))
fitted_fav_distance <- (m_full_distance$fitted.values/(1-m_full_distance$fitted.values))/((4577/31186)+(m_full_distance$fitted.values/(1- m_full_distance$fitted.values)))
fitted_cv_distance <- (m_full_distance$fitted.values)

# Plot fitted values on training data against predictions on cross-validation data
# using favourability

plot(fitted_cv, preds_cv[,2], xlab='Fitted values', ylab='Predicted values from CV')
abline(0,1,col='red',lwd=2)

plot(fitted_cv_norivers, preds_cv_norivers[,2], xlab='Fitted values', ylab='Predicted values from CV')
abline(0,1,col='red',lwd=2)

# Threshold-dependent performance measures (Do these all with fav)
library(PresenceAbsence)

# We first prepare our data:
# Prepare predictions on training data & favourability: 
thresh_dat_train_fav <- data.frame( ID = seq_len(nrow(DF_USE)),
                                    obs = DF_USE$P_A, pred = fitted_fav)

thresh_dat_train_fav_norivers <- data.frame( ID = seq_len(nrow(DF_USE)),
                                       obs = DF_USE$P_A, pred = fitted_fav_norivers)

thresh_dat_train_fav_rural <- data.frame( ID = seq_len(nrow(DF_USE)),
                                          obs = DF_USE$P_A, pred = fitted_fav_rural)

thresh_dat_train_fav_distance <- data.frame( ID = seq_len(nrow(DF_USE)),
                                             obs = DF_USE$P_A, pred = fitted_fav_distance)

# Prepare cross-validated predictions & favourability:
thresh_dat_cv_fav <- data.frame( ID = seq_len(nrow(DF_USE)),
                                 obs = DF_USE$P_A, pred = preds_fav[,2])

thresh_dat_cv_fav_norivers <- data.frame( ID = seq_len(nrow(DF_USE)),
                                    obs = DF_USE$P_A, pred = preds_fav_norivers[,2])

thresh_dat_cv_fav_rural <- data.frame( ID = seq_len(nrow(DF_USE)),
                                       obs = DF_USE$P_A, pred = preds_fav_rural[,2])

thresh_dat_cv_fav_distance <- data.frame( ID = seq_len(nrow(DF_USE)),
                                          obs = DF_USE$P_A, pred = preds_fav_distance[,2])


thresh_cv_fav <- PresenceAbsence::optimal.thresholds(DATA= thresh_dat_cv_fav)
thresh_cv_fav_norivers <- PresenceAbsence::optimal.thresholds(DATA= thresh_dat_cv_fav_norivers)
thresh_cv_fav_rural <- PresenceAbsence::optimal.thresholds(DATA= thresh_dat_cv_fav_rural)
thresh_cv_fav_distance <- PresenceAbsence::optimal.thresholds(DATA= thresh_dat_cv_fav_distance)

# We use threshold of F=0.5 as it is biologically meaningful (see Olivero et al. 2016)

(cmx_maxSSS_fav <- PresenceAbsence::cmx(DATA= thresh_dat_cv_fav, threshold=0.5))
(cmx_maxSSS_fav_norivers <- PresenceAbsence::cmx(DATA= thresh_dat_cv_fav_norivers, threshold=0.5))
(cmx_maxSSS_fav_rural<- PresenceAbsence::cmx(DATA= thresh_dat_cv_fav_rural, threshold=0.5))
(cmx_maxSSS_fav_distance <- PresenceAbsence::cmx(DATA= thresh_dat_cv_fav_distance, threshold=0.5))

# Proportion of correctly classified test observations
PresenceAbsence::pcc(cmx_maxSSS_fav, st.dev=F)
PresenceAbsence::pcc(cmx_maxSSS_fav_norivers, st.dev=F)
PresenceAbsence::pcc(cmx_maxSSS_fav_rural, st.dev=F)
PresenceAbsence::pcc(cmx_maxSSS_fav_distance, st.dev=F)

# True positive rate (sensitivity)
PresenceAbsence::sensitivity(cmx_maxSSS_fav, st.dev=F)
PresenceAbsence::sensitivity(cmx_maxSSS_fav_norivers, st.dev=F)
PresenceAbsence::sensitivity(cmx_maxSSS_fav_rural, st.dev=F)
PresenceAbsence::sensitivity(cmx_maxSSS_fav_distance, st.dev=F)

# True negative rate (specificity)
PresenceAbsence::specificity(cmx_maxSSS_fav, st.dev=F)
PresenceAbsence::specificity(cmx_maxSSS_fav_norivers, st.dev=F)
PresenceAbsence::specificity(cmx_maxSSS_fav_rural, st.dev=F)
PresenceAbsence::specificity(cmx_maxSSS_fav_distance, st.dev=F)

# Kappa
PresenceAbsence::Kappa(cmx_maxSSS_fav, st.dev=F)
PresenceAbsence::Kappa(cmx_maxSSS_fav_norivers, st.dev=F)
PresenceAbsence::Kappa(cmx_maxSSS_fav_rural, st.dev=F)
PresenceAbsence::Kappa(cmx_maxSSS_fav_distance, st.dev=F)

# True Skill Statistic (after Allouche et al. 2006)
# above >0.5 is good predictions
TSS = function(cmx){
  require(PresenceAbsence)
  PresenceAbsence::sensitivity(cmx, st.dev=F)+PresenceAbsence::specificity(cmx, st.dev=F)-1 }

# TSS(cmx_maxSSS)
TSS(cmx_maxSSS_fav)
TSS(cmx_maxSSS_fav_norivers)
TSS(cmx_maxSSS_fav_rural)
TSS(cmx_maxSSS_fav_distance)

# Threshold independent measures

library(AUC) # This package clashes with PresenceAbsence

# Let's have a look a the ROC curve:
roc_cv <- roc(preds_cv[,2], as.factor(DF_USE$P_A)) 
plot(roc_cv, col = "grey70", lwd = 2)

# Compute the AUC
# above 0.7 = good predictions
auc(roc_cv)

# Store result tables *WITH FAVOURABILITY* not probability (Muñoz et al, 2015)
# 

(perf_glm_fav<- data.frame(
  pcc = pcc(cmx_maxSSS_fav, st.dev=F),
  sens = PresenceAbsence::sensitivity(cmx_maxSSS_fav, st.dev=F),
  spec = PresenceAbsence::specificity(cmx_maxSSS_fav, st.dev=F),
  TSS = TSS(cmx_maxSSS_fav),
  AUC = PresenceAbsence::auc(thresh_dat_cv_fav, st.dev=F),
  D2 = d.square(thresh_dat_cv_fav$obs, thresh_dat_cv_fav$pred),
  Kappa = PresenceAbsence::Kappa(cmx_maxSSS_fav, st.dev=F),
  thres = 0.5))

write.csv(perf_glm_fav, "abiotic_glm_fav_jan.csv")

(perf_glm_fav_norivers<- data.frame(
  pcc = pcc(cmx_maxSSS_fav_norivers, st.dev=F),
  sens = PresenceAbsence::sensitivity(cmx_maxSSS_fav_norivers, st.dev=F),
  spec = PresenceAbsence::specificity(cmx_maxSSS_fav_norivers, st.dev=F),
  TSS = TSS(cmx_maxSSS_fav_norivers),
  AUC = PresenceAbsence::auc(thresh_dat_cv_fav_norivers, st.dev=F),
  D2 = d.square(thresh_dat_cv_fav_norivers$obs, thresh_dat_cv_fav_norivers$pred),
  Kappa = PresenceAbsence::Kappa(cmx_maxSSS_fav_norivers, st.dev=F),
  thres = 0.5))

write.csv(perf_glm_fav, "abiotic_glm_fav_jan_nowater.csv")

(perf_glm_fav_rural<- data.frame(
  pcc = pcc(cmx_maxSSS_fav_rural, st.dev=F),
  sens = PresenceAbsence::sensitivity(cmx_maxSSS_fav_rural, st.dev=F),
  spec = PresenceAbsence::specificity(cmx_maxSSS_fav_rural, st.dev=F),
  TSS = TSS(cmx_maxSSS_fav_rural),
  AUC = PresenceAbsence::auc(thresh_dat_cv_fav_rural, st.dev=F),
  D2 = d.square(thresh_dat_cv_fav_rural$obs, thresh_dat_cv_fav_rural$pred),
  Kappa = PresenceAbsence::Kappa(cmx_maxSSS_fav_rural, st.dev=F),
  thres = 0.5))

write.csv(perf_glm_fav_rural, "rurpop_glm_fav.csv")

(perf_glm_fav_distance<- data.frame(
  pcc = pcc(cmx_maxSSS_fav_distance, st.dev=F),
  sens = PresenceAbsence::sensitivity(cmx_maxSSS_fav_distance, st.dev=F),
  spec = PresenceAbsence::specificity(cmx_maxSSS_fav_distance, st.dev=F),
  TSS = TSS(cmx_maxSSS_fav_distance),
  AUC = PresenceAbsence::auc(thresh_dat_cv_fav_distance, st.dev=F),
  D2 = d.square(thresh_dat_cv_fav_distance$obs, thresh_dat_cv_fav_distance$pred),
  Kappa = PresenceAbsence::Kappa(cmx_maxSSS_fav_distance, st.dev=F),
  thres = 0.5))

write.csv(perf_glm_fav_distance, "distance_glm_fav.csv")

```

## Use favourability to estimate population density 


```
# Make predictions from model in order to plot them 
bio_curr_df <- DF_USE

bio_curr_df$pred_glm <- predict(m_abiotic, newdata= bio_curr_df, type="response")
bio_curr_df$pred_glm_norivers <- predict(m_abiotic_norivers, newdata= bio_curr_df, type="response")

# with favourability (the equation is cells with presences/cells with absences in DF_USE)
bio_curr_df$fav_glm <- (bio_curr_df$pred_glm/(1- bio_curr_df$pred_glm))/((4577/31186)+(bio_curr_df$pred_glm/(1- bio_curr_df$pred_glm)))
bio_curr_df$fav_glm_norivers <- (bio_curr_df$pred_glm_norivers/(1- bio_curr_df$pred_glm_norivers))/((4577/31186)+(bio_curr_df$pred_glm_norivers/(1- bio_curr_df$pred_glm_norivers)))

# Make raster stack of predictions to current environment:

r_pred_fav_bin <- r_pred_fav <- rasterFromXYZ(bio_curr_df[,c('Longitud','Latitud','fav_glm')]) 
r_pred_fav_bin_norivers <- r_pred_fav_norivers <- rasterFromXYZ(bio_curr_df[,c('Longitud','Latitud','fav_glm_norivers')]) 

png ("glm_present_prob.png", units="in", width=7, height=5, res=500) 
plot(r_pred_fav, main="Favourability GLM", col=hcl.colors(50, "Geyser")[7:50])
dev.off()

png ("glm_present_prob_norivers.png", units="in", width=7, height=5, res=500) 
plot(r_pred_fav_norivers, main="Favourability GLM", col=hcl.colors(50, "Geyser")[7:50])
dev.off()

# png ("glm_present_probability.png", units="in", width=7, height=5, res=500) 
# plot(r_pred, main="Probability GLM")
# dev.off()

# binarize with 0.5
values(r_pred_fav_bin) <- ifelse(values(r_pred_fav)>=0.5, 1, 0)
values(r_pred_fav_bin_norivers) <- ifelse(values(r_pred_fav_norivers)>=0.5, 1, 0)

png ("glm_present_probbin.png", units="in", width=7, height=5, res=500) 
plot(stack(r_pred_fav, r_pred_fav_bin), main=c("Fav. GLM", "Fav. GLM, threshold = 0.5"), col=hcl.colors(50, "Geyser")[7:50])
dev.off()

png ("glm_present_probbin_norivers.png", units="in", width=7, height=5, res=500) 
plot(stack(r_pred_fav_norivers, r_pred_fav_bin_norivers), main=c("Fav. GLM", "Fav. GLM, threshold = 0.5"), col=hcl.colors(50, "Geyser")[7:50])
dev.off()

# Read in populated camp data
pop_camps <- read.csv("populated_camps.csv")
pop_camps_df <- pop_camps

# Get outliers 
boxplot(pop_camps$Population)
outlier_values <- boxplot.stats(pop_camps$Population)$out  # outlier values.
pop_out <- pop_camps$Population %in% outlier_values
pop_numbers <- pop_camps$Population
pop_numbers <- setdiff(pop_numbers, outlier_values)

pop_num <- pop_camps[!pop_out,]
pop_num_df <- pop_num

# Calculate mean population size
mean(pop_numbers)

# convert to spatial points dataframe
coordinates(pop_camps)= ~ Longitude + Latitude
coordinates(pop_num)= ~ Longitude+ Latitude

# Plot
png ("glm_present_camps_norivers.png", units="in", width=7, height=5, res=500) 
plot(r_pred_fav_norivers, main="Favourability GLM", col = hcl.colors(50, "Geyser")[7:50])
points(pop_num, pch=17, col= hcl.colors(1, "Reds3"))
dev.off()

# convert predicted favorability raster into a spatial polygon df
fav_grid <- rasterToPolygons(r_pred_fav)
fav_grid_df <- as.data.frame(fav_grid)

fav_grid_norivers <- rasterToPolygons(r_pred_fav_norivers)
fav_grid_df_norivers <- as.data.frame(fav_grid_norivers)

# track which cell numbers correspond to each polygon
fav_grid$cell_no <- rownames(fav_grid_df)
fav_grid_norivers$cell_no <- rownames(fav_grid_df_norivers)

# get grid cells with camps
cell_camps_fav <- over(pop_camps,fav_grid)
cell_camps_fav_norivers <- over(pop_camps,fav_grid_norivers)


cell_num_fav <- over(pop_num,fav_grid)
cell_num_fav_norivers <- over(pop_num,fav_grid_norivers)

cc <- data.frame(pop_camps, cell_camps_fav)
write.csv(cc, "cell_camps_fav.csv")

cc_norivers <- data.frame(pop_camps, cell_camps_fav_norivers)
write.csv(cc, "cell_camps_fav_norivers.csv")

ee <- data.frame(pop_num, cell_num_fav)
write.csv(ee, "cell_num_fav.csv")

ee_norivers <- data.frame(pop_num, cell_num_fav_norivers)
write.csv(ee, "cell_num_fav_norivers.csv")

# extract favourability from camps with values
fav_value <- raster::extract(r_pred_fav, pop_camps)
# plot(y=pop_camps$Population, x=fav_value, xlim=c(0,1))
fav_value_norivers <- raster::extract(r_pred_fav_norivers, pop_camps)
# plot(y=pop_camps$Population, x=fav_value_norivers, xlim=c(0,1))

fav_cell_raster_norivers <- cbind(pop_camps_df, fav_value_norivers)

# add population of those cells with the same camps
# added total population of cells with > 1 camp
cc$cell_no <- as.numeric(cc$cell_no)
ag_cc <- cc
ag_cc$Country <- NULL
ag_cc$Name <- NULL
ag_cc$Culture <- NULL
ag_cc$optional <- NULL

cc_norivers$cell_no <- as.numeric(cc_norivers$cell_no)
ag_cc_norivers <- cc_norivers
ag_cc_norivers$Country <- NULL
ag_cc_norivers$Name <- NULL
ag_cc_norivers$Culture <- NULL
ag_cc_norivers$optional <- NULL

pop_sum <- aggregate( ag_cc$Population, by=list(cell = ag_cc$cell_no), FUN= sum)
colnames(pop_sum)[2] <- "Population"
fav_av <- aggregate( ag_cc$fav_glm, by=list(cell = ag_cc$cell_no), FUN= mean)
colnames(fav_av)[2] <- "Favourability"
fav_av_norivers <- aggregate( ag_cc_norivers$fav_glm_norivers, by=list(cell = ag_cc_norivers$cell_no), FUN= mean)
colnames(fav_av_norivers)[2] <- "Favourability"

pop_cell <- merge(pop_sum, fav_av, by="cell")
pop_cell_norivers <- merge(pop_sum, fav_av_norivers, by="cell")


# Test linear relationship between favourability and grid cell population size
fit <- lm(pop_cell_no$Population ~ pop_cell_no$Favourability)
summary(fit)
plot(pop_cell_no$Favourability, pop_cell_no$Population,ylab="Population", xlab="Favourability", xlim=c(0,1), main="Population size and favourability")
abline(fit, col="blue")

fit <- lm(pop_cell_no_02$Population ~ pop_cell_no_02$Favourability)
summary(fit)
plot(pop_cell_no_02$Favourability, pop_cell_no_02$Population,ylab="Population", xlab="Favourability", xlim=c(0,1), main="Population size and favourability")
abline(fit, col="blue")

fit <- lm(pop_cell_no_norivers$Population ~ pop_cell_no_norivers$Favourability)
summary(fit)
plot(pop_cell_no_norivers$Favourability, pop_cell_no_norivers$Population,ylab="Population", xlab="Favourability", xlim=c(0,1), main="Population size and favourability")
abline(fit, col="blue")

# Now fit quantile regressions

library(quantreg)
pop_cell_no_02 <- subset(pop_cell_no, pop_cell_no$Favourability>=0.2)
cor.test(pop_cell_no_02$Population, pop_cell_no_02$Favourability, method="spearman" )
pop_cell_no_02$pop_std <- scale(pop_cell_no_02$Population)

pop_cell_no_02_norivers <- subset(pop_cell_no_norivers, pop_cell_no_norivers$Favourability>=0.2)
cor.test(pop_cell_no_02_norivers$Population, pop_cell_no_02_norivers$Favourability, method="spearman" )
pop_cell_no_02_norivers$pop_std <- scale(pop_cell_no_02_norivers$Population)

rqfit <- rq(pop_cell_no_02$Population  ~ pop_cell_no_02$Favourability, tau=0.90)
summary(rqfit)
rqfit1 <- rq(pop_cell_no_02$Population ~ pop_cell_no_02$Favourability, tau=0.95)
summary(rqfit1)
rqfit2 <- rq(pop_cell_no_02$Population  ~ pop_cell_no_02$Favourability, tau=0.99)
summary(rqfit2)

# Calculate goodness of fit using R1 (Koenker and Machado, 1999; Muñoz et al 2015)

rqfit_0 <- rq(pop_cell_no_02$Population~1,tau=0.95) # only intercept
rqfit_1 <- rq(pop_cell_no_02$Population~1,tau=0.99)

rho <- function(u,tau=.5)u*(tau - (u < 0))
R1 <- 1 - rqfit1$rho/rqfit_0$rho # fit of 99th percentile model
R2 <- 1 - rqfit2$rho/rqfit_1$rho # fit of 95th percentile model

# Make plot between favourability and population density

png ("fav_density.png", units="in", width=6, height=6, res=500) 
plot(pop_cell_no$Favourability, pop_cell_no$Population, ylab="Population per grid cell", xlab="Favourability", xlim=c(0.2,0.8), main="Population density and favourability", pch=16, col="#FFCC33")
abline(rqfit1, col="#FF6633", lwd=2)
abline(rqfit2, col="#CC3300", lwd=2)
# segments = mean population per favourability category
segments(x0 = 0.2, y0 = mean(int1$Population), x1 = 0.5, y1 = mean(int1$Population),
         col = "#33CCCC", lwd=2)
segments(x0 = 0.5, y0 = mean(int2$Population), x1 = 0.8, y1 = mean(int2$Population),
         col = "#33CCCC", lwd=2)
segments(x0 = 0.5, y0 = 0, x1 = 0.5, y1 = 155,
         col = "black", lwd=1, lty=2)
legend("topleft", legend = c("95th Percentile", "99th Percentile"), 
       col = c("#FF6633", "#CC3300"), 
       lty=c(1,1), 
       lwd=c(1,1),
       bty = "n", 
       pt.cex = 2, 
       cex = 0.7, 
       text.col = "black")
dev.off()

plot(pop_cell_no$Favourability, pop_cell_no$Population,ylab="Population", xlab="Favourability", xlim=c(0,1), main="Population size and favourability")
abline(rqfit, col="blue")

# Calculate mean population density per favourability interval
int1 <- subset(pop_cell_no, pop_cell_no$Favourability <= 0.5)
mean(int1$Population)
sd(int1$Population)
int2 <- subset(pop_cell_no, pop_cell_no$Favourability > 0.5)
mean(int2$Population)
sd(int2$Population)

cor.test(pop_cell_no$Population, pop_cell_no$Favourability)
# multi_rqfit <- rq(pop_cell_no_02$pop_std  ~ pop_cell_no_02$Favourability, tau = seq(0, 1, by = 0.1))
# summary(multi_rqfit)

# # Here I do see the triangle shape
# fav_value_glm <- as.data.frame(cbind(pop_camps, fav_value))
# colnames(fav_value_glm)[6] <- "Favourability"
hist(fav_table_glm$fav_value_glm, xlim=c(0,1), xlab="Favourability", main="Camp presence and favourability")

fit2 <- lm(pop_cell_no$Population ~ log(pop_cell_no$Probability))
summary(fit2)
plot(pop_cell_no$Probability, pop_cell_no$Population,ylab="Population", xlab="Probability", xlim=c(0,1), main="Population size and probability")
abline(fit, col="orange")


# So for presence predictions - probability is better but for population density/camp density is better to have favourability
hist(pop_cell_no$Probability, xlim=c(0,1), xlab="Probability", main="Camp presence and probability")
hist(pop_cell_no$Favourability, xlim=c(0,1), xlab="Favourability", main="Camp presence and favourability")

# No relationship!

df_pr <- subset(camps, camps$P_A==1)
coordinates(df_pr) <- c("Longitud", "Latitud")

# Test for overfitting
test <- raster::extract(r_pred_fav, df_pr)
png ("fav_camps.png", units="in", width=6, height=5, res=500) 
hist(test, main="Favourability where camps present", xlab="Favourability", xlim=0:1)
dev.off()

test2 <- raster::extract(r_pred_fav, pop_data_glm)
png ("fav_camps_pop.png", units="in", width=6, height=5, res=500) 
hist(test2, main="Favourability where camps with pop data", xlab="Favourability", xlim=0:1)
dev.off()

# Use equation in Olivero et al to calculate PPS
# list of cells with fav 0.2-0.5
fav_1 <- subset(fav_grid_df, fav_grid_df$fav_glm <= 0.5)
fav_1 <- subset(fav_grid_df, fav_grid_df$fav_glm >= 0.2) # remove cells with F < 0.2 (assume no pop)

# list of cells with fav > 0.5
fav_2 <- subset(fav_grid_df, fav_grid_df$fav_glm > 0.5)

# gross potential pop size
gpps_present <- (nrow(fav_1)*mean(int1$Population))+(nrow(fav_2)*mean(int2$Population))

# metapop (gpps * grid cell size / avg subsistence area)
metapop_present <- gpps_present*123/1079
```

## Fit MAXENT models

```
# First, we randomly select 80% of the rows that will be used as training data
# Standard - withold 20% of the sample for testing (cross-validation)
train_i <- sample(seq_len(nrow(DF_USE)), size=round(0.8*nrow(DF_USE)))

# Then, we can subset the training and testing data
DF_USE$Biome <- as.factor(DF_USE$Biome)
DF_USE_pr <- subset(DF_USE, DF_USE$P_A==1)
sp_train <- DF_USE[train_i,] 
sp_test <- DF_USE[-train_i,]

# We store the split information for later:
write(train_i, file='../indices_traindata.txt')

library(maxnet)

# Fit Maxent without correlated predictors for abiotic, 
# dist to pop places and rur pop dens

m_maxent <- maxnet(p=sp_train$P_A, data=sp_train[,my_preds], 
                   maxnet.formula(p=sp_train$P_A, data=sp_train[,my_preds], classes="lqpht"))

m_maxent_noriver <- maxnet(p=sp_train$P_A, data=sp_train[,my_preds[-4]], 
                           maxnet.formula(p=sp_train$P_A, data=sp_train[,my_preds[-4]], classes="lqpht"))

# m_maxent2 <- m_maxent3 # this is the other model. Make sure you store it

m_maxent_dist <- maxnet(p=sp_train$P_A, data=sp_train[,my_preds_dist], 
                        maxnet.formula(p=sp_train$P_A, data=sp_train[,my_preds_dist], classes="lqpht"))

m_maxent_rur <- maxnet(p=sp_train$P_A, data=sp_train[,my_preds_rur], 
                       maxnet.formula(p=sp_train$P_A, data=sp_train[,my_preds_rur], classes="lqpht"))


```

## Validation and plotting model predictions

```
thresh_dat <- data.frame(ID=seq_len(nrow(sp_test)), obs= sp_test$P_A,
                         pred=predict(m_maxent, sp_test[,my_preds], type="logistic"))

thresh_dat_noriv <- data.frame(ID=seq_len(nrow(sp_test)), obs= sp_test$P_A,
                               pred=predict(m_maxent_noriver, sp_test[,my_preds[-4]], type="logistic"))

thresh_dat_dist <- data.frame(ID=seq_len(nrow(sp_test)), obs= sp_test$P_A,
                              pred=predict(m_maxent_dist, sp_test[,my_preds_dist], type="logistic"))

thresh_dat_rur <- data.frame(ID=seq_len(nrow(sp_test)), obs= sp_test$P_A,
                             pred=predict(m_maxent_rur, sp_test[,my_preds_rur], type="logistic"))


# optimal threshold maximising specificity and sensitivity
thresh <- optimal.thresholds(DATA= thresh_dat)
thresh_nr <- optimal.thresholds(DATA= thresh_dat_noriv)
thresh_dist <- optimal.thresholds(DATA= thresh_dat_dist)
thresh_rur <- optimal.thresholds(DATA= thresh_dat_rur)

cmx_maxSSS <- cmx(DATA= thresh_dat, threshold=thresh[3,2])
cmx_maxSSS_nr <- cmx(DATA= thresh_dat_noriv, threshold=thresh_nr[3,2])
cmx_maxSSS_dist <- cmx(DATA= thresh_dat_dist, threshold=thresh_dist[3,2])
cmx_maxSSS_rur <- cmx(DATA= thresh_dat_rur, threshold=thresh_rur[3,2])

# performance measures
(perf_maxent <- data.frame(
  pcc = pcc(cmx_maxSSS, st.dev=F),
  sens = PresenceAbsence::sensitivity(cmx_maxSSS, st.dev=F),
  spec = PresenceAbsence::specificity(cmx_maxSSS, st.dev=F),
  TSS = TSS(cmx_maxSSS),
  AUC = PresenceAbsence::auc(thresh_dat, st.dev=F),
  D2 = d.square(thresh_dat$obs, thresh_dat$pred),
  Kappa = PresenceAbsence::Kappa(cmx_maxSSS, st.dev=F),
  maxTSS = thresh[3,2]))

write.csv(perf_maxent, "maxent_output_present.csv")

(perf_maxent_noriv <- data.frame(
  pcc = pcc(cmx_maxSSS_nr, st.dev=F),
  sens = PresenceAbsence::sensitivity(cmx_maxSSS_nr, st.dev=F),
  spec = PresenceAbsence::specificity(cmx_maxSSS_nr, st.dev=F),
  TSS = TSS(cmx_maxSSS_nr),
  AUC = PresenceAbsence::auc(thresh_dat_noriv, st.dev=F),
  D2 = d.square(thresh_dat_noriv$obs, thresh_dat_noriv$pred),
  Kappa = PresenceAbsence::Kappa(cmx_maxSSS_nr, st.dev=F),
  maxTSS = thresh_nr[3,2]))

write.csv(perf_maxent, "maxent_output_present_norivers.csv")

(perf_maxent_dist<- data.frame(
  pcc = pcc(cmx_maxSSS_dist, st.dev=F),
  sens = PresenceAbsence::sensitivity(cmx_maxSSS_dist, st.dev=F),
  spec = PresenceAbsence::specificity(cmx_maxSSS_dist, st.dev=F),
  TSS = TSS(cmx_maxSSS_dist),
  AUC = PresenceAbsence::auc(thresh_dat_dist, st.dev=F),
  D2 = d.square(thresh_dat_dist$obs, thresh_dat_dist$pred),
  Kappa = PresenceAbsence::Kappa(cmx_maxSSS_dist, st.dev=F),
  maxTSS = thresh_dist[3,2]))

write.csv(perf_maxent_dist, "maxent_output_present_distpop.csv")

(perf_maxent_rur<- data.frame(
  pcc = pcc(cmx_maxSSS_rur, st.dev=F),
  sens = PresenceAbsence::sensitivity(cmx_maxSSS_rur, st.dev=F),
  spec = PresenceAbsence::specificity(cmx_maxSSS_rur, st.dev=F),
  TSS = TSS(cmx_maxSSS_dist),
  AUC = PresenceAbsence::auc(thresh_dat_rur, st.dev=F),
  D2 = d.square(thresh_dat_rur$obs, thresh_dat_rur$pred),
  Kappa = PresenceAbsence::Kappa(cmx_maxSSS_rur, st.dev=F),
  maxTSS = thresh_rur[3,2]))

write.csv(perf_maxent_rur, "maxent_output_present_rurdens.csv")


# Map predictions:
bio_curr_df <- DF_USE
r_maxent_bin <- r_maxent_pred <- rasterFromXYZ(cbind(bio_curr_df[,c("Longitud", "Latitud")], predict(m_maxent, bio_curr_df, type="logistic")))
values(r_maxent_bin) <- ifelse(values(r_maxent_pred)>=thresh[3,2], 1, 0) 
png ("maxent_present.png", units="in", width=7, height=4, res=500) 
plot(stack(r_maxent_pred, r_maxent_bin),main=c('Maxent prob.','Maxent bin.'), axes=F, col=hcl.colors(50, "Geyser")[7:50])
dev.off()

bio_curr_df <- DF_USE
r_maxent_pred <- rasterFromXYZ(cbind(bio_curr_df[,c("Longitud", "Latitud")], predict(m_maxent, bio_curr_df, type="logistic")))
png ("maxent_present_prob.png", units="in", width=7, height=5, res=500) 
plot(r_maxent_pred, main='Maxent prob.', axes=F, col=hcl.colors(50, "Geyser")[7:50])
dev.off()

# no rivers

r_maxent_bin_noriv <- r_maxent_pred_noriv <- rasterFromXYZ(cbind(bio_curr_df[,c("Longitud", "Latitud")], predict(m_maxent_noriver, bio_curr_df, type="logistic")))
values(r_maxent_bin_noriv) <- ifelse(values(r_maxent_pred_noriv)>=thresh_nr[3,2], 1, 0) 
png ("maxent_present.png", units="in", width=7, height=4, res=500) 
plot(stack(r_maxent_pred_noriv, r_maxent_bin_noriv),main=c('Maxent prob.','Maxent bin.'), axes=F, col=hcl.colors(50, "Geyser")[7:50])
dev.off()

# extract values
# get predictors of presences and absences
DF_USE_abs <- subset(DF_USE, DF_USE$P_A==0)
DF_USE_pr <- subset(DF_USE, DF_USE$P_A==1)

e2 <- evaluate(m_maxent, p=DF_USE_pr[,my_preds], a=DF_USE_abs[,my_preds])

# Get importance of each variable
# More or less based on the random forest algorithm: shuffle a single variable of the given data. 
# Make model prediction with this 'shuffled' data.set. 
# Then computes a simple correlation (Pearson's) between references predictions and the 'shuffled' one. 
# The return score is 1-cor(pred_ref,pred_shuffled).
var_imp <- ecospat.maxentvarimport(m_maxent, bio_curr_df[,my_preds], nperm=5)
write.csv(var_imp, "variable_importance_maxent_present_campsjan.csv")

var_imp2 <- ecospat.maxentvarimport(m_maxent_rur, bio_curr_df[,my_preds_rur], nperm=5)
write.csv(var_imp2, "variable_importance_maxent_present_ruralpop.csv")

var_imp3 <- ecospat.maxentvarimport(m_maxent_dist, bio_curr_df[,my_preds_dist], nperm=5)
write.csv(var_imp3, "variable_importance_maxent_present_distance.csv")
```

## Determine relationship between suitability and grid cell population size

```
suit_value <- raster::extract(r_maxent_pred, pop_camps)
plot(y=pop_camps$Population, suit_value, xlim=c(0,1))

# convert predicted suitability raster into a spatial polygon df
suit_grid <- rasterToPolygons(r_maxent_pred)
suit_grid_df <- as.data.frame(suit_grid)

# track which cell numbers correspond to each polygon
suit_grid$cell_no <- rownames(suit_grid_df )

# get grid cells with camps
cell_camps_suit <- over(pop_camps,suit_grid)

ee <- data.frame(pop_camps, cell_camps_suit)
ee$Country <- NULL
ee$Name <- NULL
ee$Culture <- NULL
ee$optional <- NULL

pop_sum <- aggregate( ee$Population, by=list(cell = ee$cell_no), FUN= sum)
colnames(pop_sum)[2] <- "Population"
suit_av <- aggregate( ee$predict.m_maxent..bio_curr_df..type....logistic.., by=list(cell = ee$cell_no), FUN= mean)
colnames(suit_av)[2] <- "Suitability"
pop_cell_suit <- merge(pop_sum, suit_av, by="cell")

# Boxplot camp sizes and detect outliers
outlier_values <- boxplot.stats(pop_cell_suit$Population)$out  # outlier values.
boxplot(pop_cell_suit$Population, main="Camp size", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

# Remove outliers 
pop_cell_suit_no <- pop_cell_suit
# from large df the outliers are c(2,7,10,14,15,44,60,63,65)
# from small df outliers c(9,13,33,48)
pop_cell_suit_no <- pop_cell_suit_no[-c(9,13,33,48),]
fit <- lm(pop_cell_suit_no$Population ~ pop_cell_suit_no$Suitability)
suit_summ_pop <- summary(fit)
write.csv(suit_summ_pop$coefficients, "suitability_population_summary.csv")

png ("suit_density.png", units="in", width=6, height=5, res=500) 
plot(pop_cell_suit_no$Suitability, pop_cell_suit_no$Population, ylab="Population", xlab="Suitability", xlim=c(0,1), main="Population size and suitability")
abline(fit, col="orange2")
dev.off()

summary(fit)
hist(suit_table_maxent$suit_value_maxent, xlim=c(0,1), xlab="Suitability", main="Camp presence and suitability")

# Now quantile regressions
rqfit_maxent <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.50)
summary(rqfit_maxent,se = "nid")
rqfit_maxent <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.55)
summary(rqfit_maxent,se = "nid")
rqfit_maxent <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.60)
summary(rqfit_maxent,se = "nid")
rqfit_maxent <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.65)
summary(rqfit_maxent,se = "nid")
rqfit_maxent <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.75)
summary(rqfit_maxent,se = "nid")
rqfit_maxent <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.80)
summary(rqfit_maxent,se = "nid")
rqfit_maxent <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.85)
summary(rqfit_maxent,se = "nid")
rqfit_maxent90 <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.90)
summary(rqfit_maxent90,se = "nid") # use bootstrap better? or nid?
rqfit_maxent95 <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.95)
summary(rqfit_maxent95,se = "nid")
rqfit_maxent99 <- rq(pop_cell_suit_no$Population  ~ pop_cell_suit_no$Suitability, tau=0.99)
summary(rqfit_maxent99,se = "nid")

png ("suit_density.png", units="in", width=6, height=6, res=500) 
plot(pop_cell_suit_no$Suitability, pop_cell_suit_no$Population, ylab="Population per grid cell", xlab="Suitability", xlim=c(0,1), main="Population density and suitability", pch=16, col="#FFCC33")
abline(fit, col="#33CCCC", lwd=2)
abline(rqfit_maxent90, col="#FF9933", lwd=2)
abline(rqfit_maxent95, col="#FF6633", lwd=2)
abline(rqfit_maxent99, col="#CC3300", lwd=2)
legend("topleft", legend = c("Linear model", "90th Percentile",
                             "95th Percentile", "99th Percentile"), 
       col = c("#33CCCC","#FF9933","#FF6633", "#CC3300"), 
       lty=c(1,1,1,1), 
       lwd=c(1,1,1,1),
       bty = "n", 
       pt.cex = 2, 
       cex = 0.7, 
       text.col = "black")
dev.off()

# Calculate goodness of fit using R1 (Koenker and Machado, 1999; Muñoz et al 2015)

rqfit_0 <- rq(pop_cell_suit_no$Population~1,tau=0.80) # only intercept
rqfit_1 <- rq(pop_cell_no_02$Population~1,tau=0.99)

rho <- function(u,tau=.5)u*(tau - (u < 0))
R1 <- 1 - rqfit_maxent$rho/rqfit_0$rho # fit of 99th percentile model
R2 <- 1 - rqfit2$rho/rqfit_1$rho # fit of 95th percentile model


# Test for overfitting
test <- raster::extract(r_maxent_pred, df_pr)
png ("suit_camps.png", units="in", width=6, height=5, res=500) 
hist(test, main="Suitability where camps present", xlab="Suitability", xlim=0:1)
dev.off()

test2 <- raster::extract(r_maxent_pred, pop_data_glm)
png ("suit_camps_pop.png", units="in", width=6, height=5, res=500) 
hist(test2, main="Suitability where camps with pop data", xlab="Suitability", xlim=0:1)
dev.off()

# gross potential pop size with maxent (predictions from lm)
gpps_present_maxent <- sum(38.99+(suit_grid_df$predict.m_maxent..bio_curr_df..type....logistic..*43.38))

# metapop (gpps * grid cell size / avg subsistence area)
metapop_present_maxent <- gpps_present_maxent*123/1079
```


# 2. Calculte inter-camp connectivity

This code provides an example of how to calculate connectivity across the landscape for one time slice (in this case the present - but the exact same code can be used for any other time slice). Unfortunately, this computation has to be done individually for each time slice as it is VERY computationally expensive.

```
# Open global DEM (it's 4 files)
d <- list.files(pattern="gt30")
d1 <- raster(d[1])
d2 <- raster(d[2])
d3 <- raster(d[3])
d4 <- raster(d[4])

d1 <- aggregate(d1, fact=6)
d2 <- aggregate(d2, fact=6)
d3 <- aggregate(d3, fact=6)
d4 <- aggregate(d4, fact=6)

new_data <- raster::merge(d1, d2, d3, d4)
extent_presences <- raster::extent(r_maxent_bin)

# Prepare files for LCP analyses

dem_use <- crop(new_data, extent_presences) # approx 1km resolution
presences_use <- resample(r_maxent_bin, dem_use, method="ngb")
dem_coarse <- resample(dem_use, r_maxent_bin)
crs(dem_coarse) <- "+proj=laea"

# Create spatial points data frame with information
require(dismo)
require(movecost)
require(reshape2)

bio_curr_df <- DF_USE
maxent_bin_preds <- maxent_preds <- cbind(bio_curr_df[,c("Longitud", "Latitud")], predict(m_maxent, bio_curr_df, type="logistic"))
maxent_bin_preds[,3] <- ifelse(maxent_bin_preds[,3]>=thresh[3,2], 1, 0) 
xy <- maxent_bin_preds[,c("Longitud","Latitud")]

# Spatial point data frame with presences
new_df <- subset(maxent_bin_preds, maxent_bin_preds$`predict(m_maxent, bio_curr_df, type = "logistic")`==1)
xy <- new_df[,c("Longitud","Latitud")]

# Calculate camp number 
pr <- subset(camps, camps$P_A==1)

# Actual number of camps = 741
ratio_camp <- 741/nrow(pr)

# Predicted number of camps = cells with predicted presence*ratio of presence to camps
number_camps <- nrow(new_df)*ratio_camp
random_camps <- random_camps_sp <- new_df[sample(nrow(new_df), number_camps), ]
coordinates(random_camps_sp) <- c( "Longitud", "Latitud" )
crs(random_camps_sp) <- "+proj=laea"
random_camps$camp_ID <- row.names(random_camps)

# Make sure you create new folder before running this loop

for ( i in 1:number_camps) {
  camp <- random_camps[i,]
  coordinates(camp) <- c( "Longitud", "Latitud" )
  crs(camp) <- "+proj=laea"
  # walking time based on the Tobler's on-path hiking function
  tw <- movecost::movecost(dem_use, camp)
  
  # Points within 34.5km walking at 5km/h (average walking speed in flat surface)
  my.pts <- which(getValues(tw$accumulated.cost.raster) <= 7)
  na.pts <- which(is.na(getValues(tw$accumulated.cost.raster)))
  #name <- paste("temp_raster", i, sep = "")
  temp_raster <- raster::raster(nrows=nrow(tw$accumulated.cost.raster), ncols=ncol(tw$accumulated.cost.raster), res=res(tw$accumulated.cost.raster))
  nrow(temp_raster) <- nrow(tw$accumulated.cost.raster)
  ncol(temp_raster) <- ncol(tw$accumulated.cost.raster)
  extent(temp_raster) <- extent(tw$accumulated.cost.raster)
  crs(temp_raster) <- crs(tw$accumulated.cost.raster)
  res(temp_raster) <- res(tw$accumulated.cost.raster)
  temp_raster[my.pts] <- 1
  temp_raster[na.pts] <- 0
  na.pts2 <- which(is.na(getValues(temp_raster)))
  temp_raster[na.pts2] <- 0
  temp_values <- as.data.frame(matrix(nrow=nrow(dem_use), ncol=ncol(dem_use), data=values(temp_raster)))
  name_csv <- paste("temp_raster_7h", i, ".csv", sep = "")
  write.csv(temp_values, name_csv
  
  # Points within 5.5km walking at 5km/h (average walking speed Tobler's)
  my.pts <- which(getValues(tw$accumulated.cost.raster) <= 1)
  na.pts <- which(is.na(getValues(tw$accumulated.cost.raster)))
  #name <- paste("temp_raster", i, sep = "")
  temp_raster <- raster::raster(nrows=nrow(tw$accumulated.cost.raster), ncols=ncol(tw$accumulated.cost.raster), res=res(tw$accumulated.cost.raster))
  nrow(temp_raster) <- nrow(tw$accumulated.cost.raster)
  ncol(temp_raster) <- ncol(tw$accumulated.cost.raster)
  extent(temp_raster) <- extent(tw$accumulated.cost.raster)
  crs(temp_raster) <- crs(tw$accumulated.cost.raster)
  res(temp_raster) <- res(tw$accumulated.cost.raster)
  temp_raster[my.pts] <- 1
  temp_raster[na.pts] <- 0
  na.pts2 <- which(is.na(getValues(temp_raster)))
  temp_raster[na.pts2] <- 0
  temp_values <- as.data.frame(matrix(nrow=nrow(dem_use), ncol=ncol(dem_use), data=values(temp_raster)))
  name_csv <- paste("temp_raster_1h", i, ".csv", sep = "")
  write.csv(temp_values, name_csv)
  #name_pic <- paste("temp_raster", i, ".tif", sep = "")
  #tiff (name_pic, units="in", width=7, height=4, res=500) 
  #plot(temp_raster)
  #dev.off()
 
  
  # Points within 76km walking at 5km/h (average walking speed Tobler's)
  my.pts <- which(getValues(tw$accumulated.cost.raster) <= 15)
  na.pts <- which(is.na(getValues(tw$accumulated.cost.raster)))
  #name <- paste("temp_raster", i, sep = "")
  temp_raster <- raster::raster(nrows=nrow(tw$accumulated.cost.raster), ncols=ncol(tw$accumulated.cost.raster), res=res(tw$accumulated.cost.raster))
  nrow(temp_raster) <- nrow(tw$accumulated.cost.raster)
  ncol(temp_raster) <- ncol(tw$accumulated.cost.raster)
  extent(temp_raster) <- extent(tw$accumulated.cost.raster)
  crs(temp_raster) <- crs(tw$accumulated.cost.raster)
  res(temp_raster) <- res(tw$accumulated.cost.raster)
  temp_raster[my.pts] <- 1
  temp_raster[na.pts] <- 0
  na.pts2 <- which(is.na(getValues(temp_raster)))
  temp_raster[na.pts2] <- 0
  temp_values <- as.data.frame(matrix(nrow=nrow(dem_use), ncol=ncol(dem_use), data=values(temp_raster)))
  name_csv <- paste("temp_raster_15h", i, ".csv", sep = "")
  write.csv(temp_values, name_csv)
  #name_pic <- paste("temp_raster", i, ".tif", sep = "")
  #tiff (name_pic, units="in", width=7, height=4, res=500) 
  #plot(temp_raster)
  #dev.off()
  
}
```

## Sum values for all camps and produce maps

```
mypath <- "" #path where you have stored .csv files from previous step

# Here introduce the "pattern" you want to search for (e.g. 7h, 5h...)
multmerge = function(mypath) {
  filenames=list.files(path=mypath, pattern="_17h")
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T, row.names = 1)})
  #Reduce(function(x,y) {(x+y)}, datalist)
  Reduce("+", datalist)
}

added_cost <- multmerge(mypath)
#dd <- read.csv("temp_raster1.csv", header=T, row.names = 1)
added_cost <- as.matrix(added_cost)
added_cost <- as.vector(added_cost)
added_cost_raster <- raster::raster(nrows=nrow(dem_use), ncols=ncol(dem_use), res=res(dem_use))
nrow(added_cost_raster) <- nrow(dem_use)
ncol(added_cost_raster) <- ncol(dem_use)
extent(added_cost_raster) <- extent(dem_use)
crs(added_cost_raster) <- crs(dem_use)
res(added_cost_raster) <- res(dem_use)
values(added_cost_raster) <- added_cost

added_cost_raster[added_cost_raster == 0] <- NA # for plotting

png("connectivity_17h.png", units="in", width=7, height=5, res=500)
plot(congo_land_hr, col=rev(hcl.colors(100, palette="Earth")), legend=FALSE, main="Connectivity (17h walk)")
plot(added_cost_raster, add=T,col=rev(hcl.colors(10, palette="OrRd")) )
points(random_camps, cex=0.2, pch=4)
dev.off()
```
