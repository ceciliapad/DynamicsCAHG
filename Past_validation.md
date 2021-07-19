Code to extrapolate and validate models to the past

#  Code to create past projections and validate them

To optimise computational resources, the loops in the following code simultaneously perform the following analyses:

1. Project suitability and favourability predictions to paleoenvironmental reconstructions from the present until 120,000 years ago in 1000 or 2000 year time slices.
2. Estimate past CAHG metapopulation sizes 
3. Estimate the models' ability to predict the location and date of CAHG archaeological assemblages (as described in the main manuscript)

```

# Load archaeological site data after removing multiple dates at the same site
# within the same time slice

arch <- read.csv("stone_age_CAHG_nodups_2021.csv")


#############################################
#### Get MAXENT predictions at all sites ####
#############################################

# convert arch data to spatial points data frame
arch_sp <- arch
coordinates(arch_sp) = ~ Longitude + Latitude

# read for every time period the data and store in data frame
# select working directory 
setwd("/Users/Cecilia/Documents/PhD/African Pygmies/Pygmy SDM SM/27_05/lumped_biomes/Ecosystem_by_year")

rec <- list.files(pattern="congo")

# Generic loop to plot predictions for every time period in the past 120kya
# create DF to store predicted number of camps per time slice
camps_year <- matrix(0, nrow=72, ncol=2)
camps_year <- as.data.frame(camps_year)

# create df to store population predicted from Maxent regression
pop_year <- matrix(0, nrow=72, ncol=2)
pop_year <- as.data.frame(pop_year)
counter <- 0

for (i in rec) {
  
  counter <- counter + 1
  
  data_this_year <- read.csv(i, row.names = 1)
  
  i <- sub(".csv", "", i)
  i <- sub("congo", "", i)
  i <- sub("BP", "", i)
  i <- as.numeric(i)
  year_bp <- -i;  
  
  # Match with bio_curr_dataframe (make sure) it is loaded
  data_this_year <- data_this_year[complete.cases(data_this_year),]
  data_this_year <- subset(data_this_year, ID_01x01 %in% bio_curr_df$ID_01x01)
  data_this_year$Biome <- data_this_year$biome 
  # For maxent you don't need the exact same levels of biome
  
  # Map predictions (using only environmental variables)
  preds_this_year <- cbind(data_this_year[,c("Longitud", "Latitud")], 
                           predict(m_maxent, data_this_year, type="logistic"))
  
  colnames(preds_this_year)[3] <- "suitability"
  #name_table <- paste("maxent_", i, ".csv" ,sep="")
  #write.csv(preds_this_year, name_table)
  
  preds_this_year_noriver <- cbind(data_this_year[,c("Longitud", "Latitud")], 
                           predict(m_maxent_noriver, data_this_year, type="logistic"))
  
  colnames(preds_this_year_noriver)[3] <- "suitability"
  #name_table_noriver <- paste("maxent_noriver_", i, ".csv" ,sep="")
  #write.csv(preds_this_year_noriver, name_table_noriver)
  
  r_maxent_bin_this_year <- r_maxent_pred_this_year  <- rasterFromXYZ(preds_this_year)
  # Make sure this threshold matches optimum for maxent
  values(r_maxent_bin_this_year) <- ifelse(values(r_maxent_pred_this_year)>=thresh[3,2], 1, 0)
  
  r_maxent_bin_noriv_this_year <- r_maxent_pred_noriv_this_year  <- rasterFromXYZ(preds_this_year_noriver)
  # Make sure this threshold matches optimum for maxent
  values(r_maxent_bin_noriv_this_year) <- ifelse(values(r_maxent_pred_noriv_this_year)>=thresh_nr[3,2], 1, 0)
  
  # # store this 
  # name_pic <- paste("maxent", i, "BP", ".png", sep = "")
  # name_pic_norivers <- paste("maxent_noriv", i, "BP", ".png", sep = "")
  # 
  # png (name_pic, units="in", width=7, height=5, res=500) 
  # plot(r_maxent_pred_this_year, main=paste(i, "BP"), col = hcl.colors(50, "Geyser")[7:50])
  # dev.off()
  # 
  # #png (name_pic_norivers, units="in", width=7, height=5, res=500) 
  # #plot(r_maxent_pred_noriv_this_year, main=paste(i, "BP"), col = hcl.colors(50, "Geyser")[7:50])
  # #dev.off()
  # 
  # #Also record camp number
  # pr <- subset(camps, camps$P_A==1)
  # ab <- subset(camps, camps$P_A==0)
  # 
  # # Actual number of camps = 741
  # ratio_camp <- 741/nrow(pr)
  # # Predicted presences
  # cells_w_presence <- as.data.frame(table(values(r_maxent_bin_this_year)))
  # 
  # # Predicted number of camps = cells with predicted presence*ratio of presence
  # # to camps
  # pred_camps <- cells_w_presence$Freq[2]*ratio_camp
  # camps_year[counter,1] <- pred_camps
  # camps_year[counter,2] <- i
  # 
  # # gross potential pop size with maxent (predictions from lm)
  # gpps_present_maxent <- sum(37.01+(preds_this_year$suitability*45.17))
  # 
  # # metapop (gpps * grid cell size / avg subsistence area)
  # metapop_present_maxent <- gpps_present_maxent*123/1079
  # pop_year[counter,1] <- metapop_present_maxent
  # pop_year[counter,2] <- i
  
  if (i >= -46000) { # add column specifying whether real year or not!!
    arch_this_year <- arch
    arch_this_year$real_date <- 0
    if (i > -22000) {
      for (j in 1:nrow(arch_this_year)) {
        ifelse(arch_this_year[j,5] == year_bp, arch_this_year[j,15] <- 1,arch_this_year[j,15] <- 0 )
      }
    } else if (i <= -22000) {
      for (j in 1:nrow(arch_this_year)) {
        ifelse(arch_this_year[j,6] == year_bp, arch_this_year[j,15] <- 1,arch_this_year[j,15] <- 0 )
      }
    }
    suit_grid <- rasterToPolygons(r_maxent_pred_this_year)
    suit_grid_noriv <- rasterToPolygons(r_maxent_pred_noriv_this_year)
    arch_suit <- over(arch_sp,suit_grid)
    arch_suit_noriv <- over(arch_sp,suit_grid_noriv)
    arch_this_year <- cbind(arch_this_year, arch_suit)
    arch_this_year <- cbind(arch_this_year, arch_suit_noriv)
    colnames(arch_this_year)[16:17] <- c("maxent", "maxent_norivers" )
    name <- paste("sites", i, "BP", ".csv", sep = "")
    write.csv(arch_this_year, name)
  }
}

write.csv(camps_year, "camps_year_number_maxent.csv")
write.csv(camps_year_noriver, "camps_year_number_maxent.csv")
write.csv(pop_year, "population_suitability_regression.csv")

# Plot predicted number of camps over time

camps_year$V2 <- sub("BP", "", camps_year$V2) 
camps_year$V2 <- as.numeric(camps_year$V2)

png ("camps_over_time.png", units="in", width=7, height=4, res=500) 
plot(camps_year$V2, camps_year$V1, main="Predicted number of camps over time", 
     xlab="Years B.P.", ylab="Number of camps", pch=19, cex=0.5, col=holy_mountain(5)[5])
lines(camps_year$V2[order(camps_year$V2)], camps_year$V1[order(camps_year$V2)], xlim=range(camps_year$V2), ylim=range(camps_year$V1), pch=16, col=holy_mountain(5)[5])
dev.off()

png ("population_over_time_50ppl.png", units="in", width=7, height=4, res=500) 
camps_year$People <- camps_year$V1*50 # average size of our populated camps
plot(camps_year$V2, camps_year$People, main="Predicted population size over time", 
     xlab="Years B.P.", ylab="Population size", pch=19, cex=0.5, col=holy_mountain(5)[5])
lines(camps_year$V2[order(camps_year$V2)], camps_year$People[order(camps_year$V2)], xlim=range(camps_year$V2), ylim=range(camps_year$People), pch=16, col=holy_mountain(5)[5])
dev.off()

# Plot predicted population size over time

pop_year$V2 <- sub("BP", "", pop_year$V2) 
pop_year$V2 <- as.numeric(pop_year$V2)

png ("population_over_time_maxent_reg.png", units="in", width=7, height=4, res=500) 
plot(pop_year$V2, pop_year$V1, main="Predicted population size over time", 
     xlab="Years B.P.", ylab="Population size", pch=19, cex=0.5, col=holy_mountain(5)[6])
lines(pop_year$V2[order(pop_year$V2)], pop_year$V1[order(pop_year$V2)], xlim=range(pop_year$V2), ylim=range(pop_year$V1), pch=16, col=holy_mountain(5)[6])
dev.off()

# Read in all files

arch_matrix <- as.data.frame(matrix(0, 1, 17))
colnames(arch_matrix) <- colnames(arch_this_year)
rec <- list.files(pattern="sites")[42:76]

for (i in rec) {
  data_this_year <- read.csv(i, row.names = 1)
  arch_matrix <- rbind(arch_matrix, data_this_year)
}

arch_matrix$real_date <- as.factor(arch_matrix$real_date)

######################
## Permutation test ##
######################

real_time <- subset(arch_matrix, arch_matrix$real_date==1)
fake_time <- subset(arch_matrix, arch_matrix$real_date==0)

real_time$presences <- ifelse(real_time$maxent >= thresh[3,2],1,0)
fake_time$presences <- ifelse(fake_time$maxent >= thresh[3,2],1,0)

real_time$presences_nr <- ifelse(real_time$maxent_norivers >= thresh_nr[3,2],1,0)
fake_time$presences_nr <- ifelse(fake_time$maxent_norivers >= thresh_nr[3,2],1,0)

real_time$iter <- 0
fake_time$iter <- 0

# Create data frames to store results
permut_matrix <- as.data.frame(matrix(0, 1, 20))
colnames(permut_matrix) <- colnames(real_time)
store_results <- as.data.frame(matrix(0, 1000, 8))
colnames(store_results) <- c("absences_rt", "presences_rt", "absences_rt_nr", "presences_rt_nr",
                             "absences_ft", "presences_ft", "absences_ft_nr", "presences_ft_nr")

for ( k in 1:1000 ) { # permute 1000 times
  fake_samples <- sample_n(fake_time, nrow(real_time)) # sample same number of rows as real df
  permut_df <- rbind(real_time, fake_samples)
  permut_df$iter <- k
  
  # store number of sites in predicted presences at real and fake time periods
  store_results[k,1:2] <- table(real_time$presences)[1:2]
  store_results[k,3:4] <- table(real_time$presences_nr)[1:2]
  store_results[k,5:6] <- table(fake_samples$presences)[1:2]
  store_results[k,7:8] <- table(fake_samples$presences_nr)[1:2]
  
  # add stuff to matrix
  permut_matrix <- rbind(permut_matrix, permut_df)
  permut_matrix$real_date <- as.factor(permut_matrix$real_date)
}

require(plyr)
mu <- ddply(permut_matrix, "real_date", summarise, grp.mean=mean(maxent,na.rm=T))
mu_nr <- ddply(permut_matrix, "real_date", summarise, grp.mean=mean(maxent_norivers,na.rm=T))

# Change density plot line colors by groups
require(ggplot2)
ggplot(permut_matrix, aes(x=maxent, color=real_date)) +
  geom_density(size=0.7) +
  theme_minimal() +
  scale_x_continuous(name="Suitability") + 
  scale_color_manual(name="Real date", values = c(holy_mountain(1)[3], holy_mountain(2)[5])) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=real_date),
             linetype="dashed", size=0.7) +
  labs(title = "Suitability at archaeological site locations") 
ggsave("suitability_maxent_1000permutations.png", dpi=500)

ggplot(permut_matrix, aes(x=maxent_norivers, color=real_date)) +
  geom_density(size=0.7) +
  theme_minimal() +
  scale_x_continuous(name="Suitability") + 
  scale_color_manual(name="Real date", values = c(holy_mountain(1)[3], holy_mountain(2)[5])) +
  geom_vline(data=mu_nr, aes(xintercept=grp.mean, color=real_date),
             linetype="dashed", size=0.7) +
  labs(title = "Suitability at archaeological site locations") 
ggsave("suitability_maxent_noriver_1000permutations.png", dpi=500)

# t_test
matrix_real <- subset(permut_matrix, permut_matrix$real_date==1)
matrix_fake <- subset(permut_matrix, permut_matrix$real_date==0)
t.test(matrix_real$maxent, matrix_fake$maxent)
t.test(matrix_real$maxent_norivers, matrix_fake$maxent_norivers)

# chi square test of independence between whether sites are in their 
# real time periods and whether they lay in a predicted presence

prop.test(c(store_results$presences_ft, store_results$presences_rt),
          c((store_results$presences_ft + store_results$absences_ft), 
            (store_results$presences_rt + store_results$absences_rt)))
          
require(Jodorowsky)
ggplot(store_results, aes(x=presences_ft, name="Number of presences")) +
  geom_density(size=0, fill=holy_mountain(2)[4], alpha=0.6) +
  theme_classic() +
  scale_x_continuous(name="Sites in predicted presences") +
  geom_vline(xintercept=51, color=holy_mountain(2)[5], size=1,
             linetype="dashed")  + 
  labs(title = "Randomising dates (1000 permutations)")
ggsave("presences_maxent_1000permutations.png", dpi=500)

ggplot(store_results, aes(x= presences_ft_nr, name="Number of presences")) +
  geom_density(size=0, fill=holy_mountain(2)[4], alpha=0.6) +
  theme_classic() +
  scale_x_continuous(name="Sites in predicted presences") +
  geom_vline(xintercept=49, color=holy_mountain(2)[5], size=1,
             linetype="dashed")  + 
  labs(title = "Sites in predicted presences when randomising dates (1000 permutations)")
ggsave("presences_maxent_noriver_1000permutations.png", dpi=500)
write.csv(store_results, "results_permutations_presences_maxent.csv")

#########################################
## Get favourability-based predictions ##
#########################################

# read for every time period the data and store in data frame
# select working directory 
setwd("/Users/Cecilia/Documents/PhD/African Pygmies/Pygmy SDM SM/27_05/lumped_biomes/Ecosystem_by_year")

rec <- list.files(pattern="congo")

# Generic loop to plot predictions for every time period in the past 120kya
# create DF to store predicted number of camps per time slice
camps_year <- matrix(0, nrow=72, ncol=2)
camps_year <- as.data.frame(camps_year)

camps_year_noriver <- matrix(0, nrow=72, ncol=2)
camps_year_noriver <- as.data.frame(camps_year_noriver)

# create df to store population predicted from F categories
pop_year <- matrix(0, nrow=72, ncol=2)
pop_year <- as.data.frame(pop_year)

pop_year_noriver <- matrix(0, nrow=72, ncol=2)
pop_year_noriver <- as.data.frame(pop_year_noriver)

counter <- 0

for (i in rec) {
  
  counter <- counter + 1
  
  data_this_year <- read.csv(i, row.names = 1)
  
  i <- sub(".csv", "", i)
  i <- sub("congo", "", i)
  i <- sub("BP", "", i)
  i <- as.numeric(i)
  year_bp <- -i;  
  
  # Match with bio_curr_dataframe (make sure) it is loaded
  data_this_year <- data_this_year[complete.cases(data_this_year),]
  data_this_year <- subset(data_this_year, ID_01x01 %in% bio_curr_df$ID_01x01)
  data_this_year$Biome <- as.factor(data_this_year$biome)
  data_this_year$Biome <- sub("8", NA, data_this_year$Biome) 
  data_this_year$Biome <- sub("2", NA, data_this_year$Biome) 
  # 8 = Not present in original dataset
  data_this_year <- data_this_year[complete.cases(data_this_year),]
  data_this_year <- subset(data_this_year, ID_01x01 %in% bio_curr_df$ID_01x01)
  
  # Map predictions (using only environmental variables)
  preds_this_year <- cbind(data_this_year[,c("Longitud", "Latitud")], 
                           predict(m_abiotic, data_this_year, type="response"))
  
  preds_this_year_noriver <- cbind(data_this_year[,c("Longitud", "Latitud")], 
                           predict(m_abiotic_norivers, data_this_year, type="response"))
  
  colnames(preds_this_year)[3] <- "probability"
  colnames(preds_this_year_noriver)[3] <- "probability"
  # Also record camp number
  pr <- subset(camps, camps$P_A==1)
  ab <- subset(camps, camps$P_A==0)
  npr <- nrow(pr)
  nab <- nrow(ab)
  
  preds_this_year$favourability <- exp(log(preds_this_year$probability/(1- preds_this_year$probability)))/
    (npr/nab + exp(log(preds_this_year$probability/(1 - preds_this_year$probability))))
  
  preds_this_year_noriver$favourability <- exp(log(preds_this_year_noriver$probability/(1- preds_this_year_noriver$probability)))/
    (npr/nab + exp(log(preds_this_year_noriver$probability/(1 - preds_this_year_noriver$probability))))
  
  # name_table <- paste("fav_", i, ".csv" ,sep="")
  # write.csv(preds_this_year, name_table)
  # name_table_noriver <- paste("fav_noriver_", i, ".csv" ,sep="")
  # write.csv(preds_this_year_noriver, name_table)
  # 
  # Remove probability column to make raster
  preds_this_year$probability <-NULL
  preds_this_year_noriver$probability <-NULL
  
  r_fav_bin_this_year <- r_fav_pred_this_year  <- rasterFromXYZ(preds_this_year)
  r_fav_bin_this_year_noriver <- r_fav_pred_this_year_noriver  <- rasterFromXYZ(preds_this_year_noriver)
  
  # Replace this with threshold of F=0.5
  values(r_fav_bin_this_year) <- ifelse(values(r_fav_pred_this_year)>=0.5, 1, 0) 
  values(r_fav_bin_this_year_noriver) <- ifelse(values(r_fav_pred_this_year_noriver)>=0.5, 1, 0)
  # 
  # name_pic <- paste("fav_", i, ".tif" ,sep="")
   # name_pic2 <- paste("fav_prob_", i, ".png" ,sep="")
  # # name_pic2_noriver <- paste("fav_prob_noriver_", i, ".png" ,sep="")
  # # # 
  # # tiff (name_pic, units="in", width=7, height=4, res=500) 
  # # plot(stack(r_fav_pred_this_year, r_fav_bin_this_year),main=c(paste('Fav.', i, sep="") ,paste('Bin.', i, sep="")), axes=F)
  # # dev.off()
  # # # 
  # png (name_pic2, units="in", width=7, height=6, res=500)
  # plot(r_fav_pred_this_year ,main=paste( i, "BP", sep=""), axes=F, col=hcl.colors(50, "Geyser")[7:50])
  # dev.off()
  # 
  # #png (name_pic2, units="in", width=7, height=6, res=500) 
  # #plot(r_fav_pred_this_year ,main=paste( i, sep=""), axes=F, col=hcl.colors(50, "Geyser")[7:50])
  # #dev.off()
  # # 
  # # Actual number of camps = 741
  ratio_camp <- 741/nrow(pr)
  # # Predicted presences
  cells_w_presence <- as.data.frame(table(values(r_fav_bin_this_year)))
  cells_w_presence_noriver <- as.data.frame(table(values(r_fav_bin_this_year_noriver)))
  #
  # # Predicted number of camps = cells with predicted presence*ratio of presence
  # # to camps
  pred_camps <- cells_w_presence$Freq[2]*ratio_camp
  camps_year[counter,1] <- pred_camps
  camps_year[counter,2] <- i

  pred_camps_noriver <- cells_w_presence_noriver$Freq[2]*ratio_camp
  camps_year_noriver[counter,1] <- pred_camps_noriver
  camps_year_noriver[counter,2] <- i

  # gross potential pop size with F cells
  int1_this_year <- subset(preds_this_year, preds_this_year$favourability <= 0.5)
  int1_this_year <- subset(int1_this_year, int1_this_year$favourability >= 0.2)
  int2_this_year <- subset(preds_this_year, preds_this_year$favourability > 0.5)
  i1 <- nrow(int1_this_year)*39.71 # Time number of cells with F value * mean pop per F value
  i2 <- nrow(int2_this_year)*64.15

  gpps_present_fav <- i1 + i2

  # metapop (gpps * grid cell size / avg subsistence area)
  metapop_present_fav <- gpps_present_fav*123/1079
  pop_year[counter,1] <- metapop_present_fav
  pop_year[counter,2] <- i

  # gross potential pop size with F cells
  int1_this_year_noriver <- subset(preds_this_year_noriver, preds_this_year_noriver$favourability <= 0.5)
  int1_this_year_noriver <- subset(int1_this_year_noriver, int1_this_year_noriver$favourability >= 0.2)
  int2_this_year_noriver <- subset(preds_this_year_noriver, preds_this_year_noriver$favourability > 0.5)
  i1_noriver <- nrow(int1_this_year_noriver)*37.71 # Time number of cells with F value * mean pop per F value
  i2_noriver <- nrow(int2_this_year_noriver)*64.15

  gpps_present_fav_noriver <- i1_noriver + i2_noriver

  # metapop (gpps * grid cell size / avg subsistence area)
  metapop_present_fav_noriver <- gpps_present_fav_noriver*123/1079
  pop_year_noriver[counter,1] <- metapop_present_fav_noriver
  pop_year_noriver[counter,2] <- i
  
  if (i >= -46000) { # add column specifying whether real year or not!!
    arch_this_year <- arch
    arch_this_year$real_date <- 0
    if (i > -22000) {
      for (j in 1:nrow(arch_this_year)) {
        ifelse(arch_this_year[j,5] == year_bp, arch_this_year[j,15] <- 1,arch_this_year[j,15] <- 0 )
      }
    } else if (i <= -22000) {
      for (j in 1:nrow(arch_this_year)) {
        ifelse(arch_this_year[j,6] == year_bp, arch_this_year[j,15] <- 1,arch_this_year[j,15] <- 0 )
      }
    }
    fav_grid <- rasterToPolygons(r_fav_pred_this_year)
    fav_grid_noriv <- rasterToPolygons(r_fav_pred_this_year_noriver)
    arch_fav <- over(arch_sp,fav_grid)
    arch_fav_noriv <- over(arch_sp,fav_grid_noriv)
    arch_this_year <- cbind(arch_this_year, arch_fav)
    arch_this_year <- cbind(arch_this_year, arch_fav_noriv)
    colnames(arch_this_year)[16:17] <- c("fav", "fav_norivers" )
    name <- paste("sites_fav", i, "BP", ".csv", sep = "")
    write.csv(arch_this_year, name)
  }
}

write.csv(camps_year, "camps_year_number_fav.csv")
write.csv(camps_year_noriver, "camps_year_number_noriver_fav.csv")
write.csv(pop_year, "population_fav_gridcelltypes.csv")

# Plot predicted number of camps over time favourability

camps_year$V2 <- sub("BP", "", camps_year$V2) 
camps_year$V2 <- as.numeric(camps_year$V2)

png ("camps_over_time_fav.png", units="in", width=7, height=4, res=500) 
plot(camps_year$V2, camps_year$V1, main="Predicted number of camps over time", 
     xlab="Years B.P.", ylab="Number of camps", pch=19, cex=0.5, col=holy_mountain(5)[5])
lines(camps_year$V2[order(camps_year$V2)], camps_year$V1[order(camps_year$V2)], xlim=range(camps_year$V2), ylim=range(camps_year$V1), pch=16, col=holy_mountain(5)[5])
dev.off()

png ("population_over_time_50ppl_fav.png", units="in", width=7, height=4, res=500) 
camps_year$People <- camps_year$V1*50 # average size of our populated camps
plot(camps_year$V2, camps_year$People, main="Predicted population size over time", 
     xlab="Years B.P.", ylab="Population size", pch=19, cex=0.5, col=holy_mountain(5)[5])
lines(camps_year$V2[order(camps_year$V2)], camps_year$People[order(camps_year$V2)], xlim=range(camps_year$V2), ylim=range(camps_year$People), pch=16, col=holy_mountain(5)[5])
dev.off()

# Plot predicted population size over time

pop_year$V2 <- sub("BP", "", pop_year$V2) 
pop_year$V2 <- as.numeric(pop_year$V2)

png ("population_over_time_favgridcell.png", units="in", width=7, height=4, res=500) 
plot(pop_year$V2, pop_year$V1, main="Predicted population size over time", 
     xlab="Years B.P.", ylab="Population size", pch=19, cex=0.5, col=holy_mountain(5)[6])
lines(pop_year$V2[order(pop_year$V2)], pop_year$V1[order(pop_year$V2)], xlim=range(pop_year$V2), ylim=range(pop_year$V1), pch=16, col=holy_mountain(5)[6])
dev.off()

# Read in all files

arch_matrix <- as.data.frame(matrix(0, 1, 17))
colnames(arch_matrix) <- colnames(arch_this_year)
rec <- list.files(pattern="sites_fav")

for (i in rec) {
  data_this_year <- read.csv(i, row.names = 1)
  colnames(arch_matrix) <- colnames(data_this_year)
  arch_matrix <- rbind(arch_matrix, data_this_year)
}

arch_matrix$real_date <- as.factor(arch_matrix$real_date)

######################
## Permutation test ##
######################

real_time <- subset(arch_matrix, arch_matrix$real_date==1)
fake_time <- subset(arch_matrix, arch_matrix$real_date==0)

real_time$presences <- ifelse(real_time$fav >= 0.5,1,0)
fake_time$presences <- ifelse(fake_time$fav >= 0.5,1,0)

real_time$presences_nr <- ifelse(real_time$fav_norivers  >= 0.5,1,0)
fake_time$presences_nr <- ifelse(fake_time$fav_norivers  >= 0.5,1,0)

real_time$iter <- 0
fake_time$iter <- 0

# Create data frames to store results
permut_matrix <- as.data.frame(matrix(0, 1, 20))
colnames(permut_matrix) <- colnames(real_time)
store_results <- as.data.frame(matrix(0, 1000, 8))
colnames(store_results) <- c("absences_rt", "presences_rt", "absences_rt_nr", "presences_rt_nr",
                             "absences_ft", "presences_ft", "absences_ft_nr", "presences_ft_nr")

for ( k in 1:1000 ) { # permute 1000 times
  fake_samples <- sample_n(fake_time, nrow(real_time)) # sample same number of rows as real df
  permut_df <- rbind(real_time, fake_samples)
  permut_df$iter <- k
  
  # store number of sites in predicted presences at real and fake time periods
  store_results[k,1:2] <- table(real_time$presences)[1:2]
  store_results[k,3:4] <- table(real_time$presences_nr)[1:2]
  store_results[k,5:6] <- table(fake_samples$presences)[1:2]
  store_results[k,7:8] <- table(fake_samples$presences_nr)[1:2]
  
  # add stuff to matrix
  permut_matrix <- rbind(permut_matrix, permut_df)
  permut_matrix$real_date <- as.factor(permut_matrix$real_date)
}

require(plyr)
mu <- ddply(permut_matrix, "real_date", summarise, grp.mean=mean(fav,na.rm=T))
mu_nr <- ddply(permut_matrix, "real_date", summarise, grp.mean=mean(fav_norivers,na.rm=T))

# Change density plot line colors by groups
require(ggplot2)
ggplot(permut_matrix, aes(x=fav, color=real_date)) +
  geom_density(size=0.7) +
  theme_minimal() +
  scale_x_continuous(name="Favourability") +
  scale_color_manual(name="Real date", values = c(holy_mountain(2)[3], holy_mountain(2)[2])) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=real_date),
             linetype="dashed", size=0.7) +
  labs(title = "Favourability at archaeological site locations") 
ggsave("fav_glm_1000permutations.png", dpi=500)

ggplot(permut_matrix, aes(x=fav_norivers, color=real_date)) +
  geom_density(size=0.7) +
  theme_minimal() +
  scale_x_continuous(name="Favourability") +
  scale_color_manual(name="Real date", values = c(holy_mountain(2)[3], holy_mountain(2)[2])) +
  geom_vline(data=mu_nr, aes(xintercept=grp.mean, color=real_date),
             linetype="dashed", size=0.7) +
  labs(title = "Favourability at archaeological site locations") 
ggsave("fav_glm_noriver_1000permutations.png", dpi=500)

# real time = 52 - better than what proportion of the ones drom fake time
ft_good <- subset(store_results, store_results$presences_ft>52)
nrow(ft_good)/1000

# t_test
matrix_real <- subset(permut_matrix, permut_matrix$real_date==1)
matrix_fake <- subset(permut_matrix, permut_matrix$real_date==0)
t.test(matrix_real$fav, matrix_fake$fav)
t.test(matrix_real$fav_norivers, matrix_fake$fav_norivers)

# chi square test of independence between whether sites are in their 
# real time periods and whether they lay in a predicted presence

prop.test(c(store_results$presences_ft, store_results$presences_rt),
          c((store_results$presences_ft + store_results$absences_ft), 
            (store_results$presences_rt + store_results$absences_rt)))

require(Jodorowsky)
ggplot(store_results, aes(x=presences_ft, name="Number of presences")) +
  geom_density(size=0, fill=holy_mountain(2)[3], alpha=0.6) +
  theme_minimal() +
  scale_x_continuous(name="Sites in predicted presences") +
  geom_vline(xintercept=52, color=holy_mountain(2)[2], size=1,
             linetype="dashed")  + 
  labs(title = "Sites in predicted presences when randomising dates (1000 permutations)")
ggsave("presences_fav_1000permutations.png", dpi=500)

ggplot(store_results, aes(x= presences_ft_nr, name="Number of presences")) +
  geom_density(size=0, fill=holy_mountain(2)[3], alpha=0.6) +
  theme_minimal() +
  scale_x_continuous(name="Sites in predicted presences") +
  geom_vline(xintercept=52, color=holy_mountain(2)[2], size=1,
             linetype="dashed")  + 
  labs(title = "Sites in predicted presences when randomising dates (1000 permutations)")
ggsave("presences_fav_noriver_1000permutations.png", dpi=500)
write.csv(store_results, "results_permutations_presences_fav.csv")

# Create object to then plot with suit
store_results_fav <- store_results
permut_matrix_fav <- permut_matrix
camps_year_fav <- camps_year
camps_year_noriver_fav <- camps_year_noriver
pop_year_fav <- pop_year
pop_year_noriver_fav <- pop_year_noriver

# PLOT METHODS 1 AND 2 TOGETHER

suit_reg <- read.csv("population_suitability_regression.csv")
fav_ct <- read.csv("population_fav_gridcelltypes.csv")
fav_ct$V2 <- sub("BP", "", fav_ct$V2) 
fav_ct$V2 <- as.numeric(fav_ct$V2)
fav_ct$V2 <- -fav_ct$V2
suit_reg$V2 <- sub("BP", "", suit_reg$V2) 
suit_reg$V2 <- as.numeric(suit_reg$V2)
suit_reg$V2 <- - suit_reg$V2

png ("population_over_time_fav_suit_m1.png", units="in", width=7, height=4.5, res=500) 
plot(fav_ct$V2, fav_ct$V1, main="Predicted population size over time", font.main=1,
     xlab="Years ago", ylab="Population size", pch=19, cex=0.5, col=holy_mountain(5)[6], ylim=c(50000, 200000))
lines(fav_ct$V2[order(fav_ct$V2)], (fav_ct$V1)[order(fav_ct$V2)], xlim=range(fav_ct$V2), ylim=range(fav_ct$V1), pch=16, col=holy_mountain(5)[6])
points(suit_reg$V2, suit_reg$V1, pch=19, cex=0.5, col=holy_mountain(5)[5])
lines(suit_reg$V2[order(suit_reg$V2)], suit_reg$V1[order(suit_reg$V2)], xlim=range(suit_reg$V2), ylim=range(suit_reg$V1), pch=16, col=holy_mountain(5)[5])
legend("bottomleft",
       legend=c("Favourability","Suitability"), col=c(holy_mountain(5)[6],holy_mountain(5)[5]),
       pch=c(19,19),
       bty="n",
       cex=0.7,
       inset = c(0.0001, 0.0001))
dev.off()

suit_cn <- read.csv("camps_year_number_maxent.csv")
fav_cn <- read.csv("camps_year_number_fav.csv")
fav_cn$V2 <- sub("BP", "", fav_cn$V2) 
fav_cn$V2 <- as.numeric(fav_cn$V2)
fav_cn$V2 <- - fav_cn$V2
suit_cn$V2 <- sub("BP", "", suit_cn$V2) 
suit_cn$V2 <- as.numeric(suit_cn$V2)
suit_cn$V2 <- -suit_cn$V2

png ("population_over_time_fav_suit_m2.png", units="in", width=7, height=4.5, res=630) 
plot(fav_cn$V2, fav_cn$V1*50, main="Predicted population size over time", font.main=1,
     xlab="Years ago", ylab="Population size", pch=19, cex=0.5, col=holy_mountain(5)[6])
lines(fav_cn$V2[order(fav_cn$V2)], (fav_cn$V1*50)[order(fav_cn$V2)], xlim=range(fav_cn$V2), ylim=range(fav_cn$V1)*50, pch=16, col=holy_mountain(5)[6])
points(suit_cn$V2, suit_cn$V1*50, pch=19, cex=0.5, col=holy_mountain(5)[5])
lines(suit_cn$V2[order(suit_cn$V2)], (suit_cn$V1*50)[order(suit_cn$V2)], xlim=range(suit_cn$V2), ylim=range(suit_cn$V1)*50, pch=16, col=holy_mountain(5)[5])
legend("bottomleft",
       legend=c("Favourability","Suitability"), col=c(holy_mountain(5)[6],holy_mountain(5)[5]),
       pch=c(19,19),
       bty="n",
       cex=0.7,
       inset = c(0.0001, 0.0001))
dev.off()

png ("population_over_time_suit_both_methods.png", units="in", width=7, height=4.5, res=630) 
plot(suit_cn$V2, suit_cn$V1*50, main="Predicted population size over time", font.main=1,
     xlab="Years ago", ylab="Population size", pch=19, cex=0.5, col=holy_mountain(5)[6],
     cex.axis=0.6, ylim=c(10000,200000))
lines(suit_cn$V2[order(suit_cn$V2)], (suit_cn$V1*50)[order(suit_cn$V2)], xlim=range(suit_cn$V2), ylim=range(suit_cn$V1)*50, pch=16, col=holy_mountain(5)[6])
points(suit_reg$V2, suit_reg$V1, pch=19, cex=0.5, col=holy_mountain(5)[5])
lines(suit_reg$V2[order(suit_reg$V2)], suit_reg$V1[order(suit_reg$V2)], xlim=range(suit_reg$V2), ylim=range(suit_reg$V1), pch=16, col=holy_mountain(5)[5])
legend("bottomleft",
       legend=c("Method 1","Method 2"), col=c(holy_mountain(5)[6],holy_mountain(5)[5]),
       pch=c(19,19),
       bty="n",
       cex=0.7,
       inset = c(0.0001, 0.0001))
dev.off()

### Get range over time to see when range expansions and contractions happened

suit_range <- suit_cn
suit_range$cells_w_presence <- suit_range$V1/ratio_camp
head(suit_range)

png ("range_size_maxent.png", units="in", width=7, height=4.5, res=630) 
plot(suit_range$V2, suit_range$cells_w_presence, main="CAHG range size over time", font.main=1,
     xlab="Years ago", ylab="Number of cells with predicted presences", pch=19, cex=0.6, col=holy_mountain(5)[6],
     cex.axis=0.6, ylim=c(0,20000))
lines(suit_range$V2[order(suit_range$V2)], (suit_range$cells_w_presence)[order(suit_range$V2)], xlim=range(suit_range$V2), ylim=range(suit_range$cells_w_presence), pch=16, col=holy_mountain(5)[6])
dev.off()

write.csv(suit_range, "cells_over_time_maxent.csv")

fav_range <- fav_cn
fav_range$cells_w_presence <- fav_range$V1/ratio_camp
head(fav_range)

write.csv(fav_range, "cells_over_time_fav.csv")

png ("cell_no_over_time_fav_suit.png", units="in", width=7, height=4.5, res=630) 
plot(fav_range$V2, fav_range$cells_w_presence, main="CAHG range size over time", font.main=1,
     xlab="Years ago", ylab="Cells with predicted presences", pch=19, cex=0.5, col=holy_mountain(5)[6], ylim=c(0,15000))
lines(fav_range$V2[order(fav_range$V2)], fav_range$cells_w_presence[order(fav_range$V2)], xlim=range(fav_range$V2), ylim=range(fav_range$cells_w_presence), pch=16, col=holy_mountain(5)[6])
points(suit_range$V2, suit_range$cells_w_presence, pch=19, cex=0.5, col=holy_mountain(5)[5])
lines(suit_range$V2[order(suit_range$V2)], suit_range$cells_w_presence[order(suit_range$V2)], xlim=range(suit_range$V2), ylim=range(suit_range$cells_w_presence), pch=16, col=holy_mountain(5)[5])
legend("bottomleft",
       legend=c("Favourability","Suitability"), col=c(holy_mountain(5)[6],holy_mountain(5)[5]),
       pch=c(19,19),
       bty="n",
       cex=0.7,
       inset = c(0.0001, 0.0001))
dev.off()
```

## Now spatial randomisation of sites

The code velow tests whether our model predicts the location of archaeological sites better than if they were randomly distributed across the landscape for each time period. In the example below we test it with suitability models, but the same code can be used for favourability-based predictions.

```
#########################################################################
# Extract suitability from sites & number of presences that time period #
#########################################################################

rec <- list.files(pattern="maxent_-")

rec
presences_year <- matrix(0, nrow=72, ncol=5)
presences_year <- as.data.frame(presences_year)
colnames(presences_year) <- c("year", "pred_cells_presence", "sites_this_year", 
                              "sites_in_pred_presence", "sites_with_pred") 
counter <- 0

require(plyr)

for (i in rec) {
  
  counter <- counter + 1
  
  data_this_year <- read.csv(i, row.names = 1)
  
  i <- sub(".csv", "", i)
  i <- sub("maxent_", "", i)
  i <- sub("BP", "", i)
  i <- as.numeric(i)
  year_bp <- -i;  
  
  #data_this_year$Longitud <- round(data_this_year$Longitud,1)
  #data_this_year$Latitud <- round(data_this_year$Latitud,1)
  # If my dataset, unquote line below
  colnames(data_this_year)[1:2] <- c("Longitude", "Latitude")
  #data_this_year$id <- seq(1:nrow(data_this_year))
  data_this_year_r <- rasterFromXYZ(data_this_year[,c('Longitude',
                                                      'Latitude','suitability')])
  
  
  # number of cells with predicted presences that year
  presences_this_year <- table(data_this_year$suitability > thresh[3,2] )
  number_presences <- as.numeric(presences_this_year[2])
  presences_year[counter,1] <- i
  presences_year[counter,2] <- number_presences
  
  # Make sure for time periods older than 21,000BP it takes
  # dates rounded to the 2000 years
  if (year_bp < 22000) {
    arch_now <- subset(arch, arch$Date_1000==year_bp)
    presences_year[counter,3] <- nrow(arch_now) 
  } else {
    arch_now <- subset(arch, arch$Date_2000==year_bp)
    presences_year[counter,3] <- nrow(arch_now) 
  }
  
  
  if (nrow(arch_now) > 0 ) {
    sites_in_pres <- 0 # to store sites in cells in predicted presences
    sites_with_pres <- 0 # sites that are not NA
    DF_ARCH <- raster::extract(data_this_year_r, arch_now[,c("Longitude","Latitude")])
    DF_ARCH_DATA <- cbind(arch_now, DF_ARCH)
    colnames(DF_ARCH_DATA)[15]  <- "suitability"
    DF_ARCH_DATA[,12] <- NULL
    
    for(j in 1:nrow(DF_ARCH_DATA)) {
      row <- DF_ARCH_DATA[j,]
      if (!is.na(row$suitability)) {
        sites_with_pres <- sites_with_pres + 1
        if ( row$suitability >= thresh[3,2] ) {
          sites_in_pres <- sites_in_pres + 1
        }
      }
    }
    presences_year[counter,4] <- sites_in_pres
    presences_year[counter,5] <- sites_with_pres
    #name <- paste("sites", i, "BP", ".csv", sep = "")
    #name_var <- paste("sites", i, "BP", sep = "_")
    #assign(name_var, DF_ARCH_DATA)
    #write.csv(DF_ARCH_DATA, name)
  }
}

# expected number of sites in cells with predicted presences if site location 
# was independent of suitability
presences_year$expected <- presences_year$sites_with_pred*(
  presences_year$pred_cells_presence/nrow(data_this_year))

pres_year_comp <- presences_year[complete.cases(presences_year),]

# subset only those time periods for which there are archaeological sites
pres_year_comp <- subset(pres_year_comp, pres_year_comp$sites_this_year > 0)


# exact binomial test
prob_exp_mx <- cs_mx[6]/cs_mx[5] # expected proportion of sites in predicted presences

test <- prop.test(
  x = cs_mx[4], # number of cells with predicted presences
  n = cs_mx[5], # number of sites with data
  p = prob_exp_mx, # expected proportion
  alternative = "greater"
)

require(Jodorowsky)
# barplot
barplot(pred_arch_mx[2], # observed counts
        names.arg = "Cells with predicted presences containing sites", # rename labels
        ylab = "Frequency", # y-axis label,
        main = "All Stone Age",
        col=holy_mountain(2)[4],
        ylim=c(0,50)
)
abline(
  h = prob_exp_mx*cs_mx[5], # expected number of sites in presences if chance
  lty = 2, # dashed line
  lwd = 2
)


###############################
# Get counts per year, Maxent #
###############################

hist(arch_pred_year_mx$pred)
hist(arch_pred_year_glm$favourability)

arch_pred_year_mx$suit_arch <- ifelse(arch_pred_year_mx$pred>=thresh[3,2], 1, 0) # sites in presences
arch_pred_year_mx$sites_now <- 1 # total sites

arch_pred_year_mx_a <- subset(arch_pred_year_mx, arch_pred_year_mx$Date_BP < 22000)
arch_pred_year_mx_b <- subset(arch_pred_year_mx, arch_pred_year_mx$Date_BP >= 22000)

arch_max_dist_year_a <- aggregate( suit_arch ~ Date_1000, data = arch_pred_year_mx_a, sum)
arch_max_dist_year_a2 <- aggregate( sites_now ~ Date_1000, data = arch_pred_year_mx_a, sum )
arch_max_dist_year_a <- cbind(arch_max_dist_year_a, arch_max_dist_year_a2[,2])
arch_max_dist_year_a$prop_right <- arch_max_dist_year_a$suit_arch/arch_max_dist_year_a$`arch_max_dist_year_a2[, 2]`
colnames(arch_max_dist_year_a)[3] <- "sites_this_year"
colnames(arch_max_dist_year_a)[1] <- "rounded_date" #before 22,000ya, rounded to nearest 1000

arch_max_dist_year_b <- aggregate( suit_arch ~ Date_2000, data = arch_pred_year_mx_b, sum)
arch_max_dist_year_b2 <- aggregate( sites_now ~ Date_2000, data = arch_pred_year_mx_b, sum )
arch_max_dist_year_b <- cbind(arch_max_dist_year_b, arch_max_dist_year_b2[,2])
arch_max_dist_year_b$prop_right <- arch_max_dist_year_b$suit_arch/arch_max_dist_year_b$`arch_max_dist_year_b2[, 2]`
colnames(arch_max_dist_year_b)[3] <- "sites_this_year"
colnames(arch_max_dist_year_b)[1] <- "rounded_date" #after 22,000ya, rounded to nearest 2000

arch_max_dist_year <- rbind(arch_max_dist_year_a, arch_max_dist_year_b)
write_csv(arch_max_dist_year, "archaeology_per_year_mx.csv")


```
