library(geoR)
library(fields)

pt <- proc.time()
set.panel(2,2)
SEsum <- 133549.3
ABSsum <- 26164.87
formula <- c("elev","forest_cover","is","high","nldas_pevapsfc","nldas_dlwrfsfc","nldas_ugrd10m","narr_tmp1815mb","nldas_vgrd10m","nldas_cape","nldas_rh2m","nldas_pressfc","CMAQ1")
for (i in 326:365){
  one <- subset(dat, t == i & folds != 5)
  #temp.geodata <- as.geodata(one, coords.col = 3:4, data.col = 1, covar.col = 3:31)
  #summary(temp.geodata)
  #str(temp.geodata)
  
  x <- cbind(one$Longitude, one$Latitude)
  spp <- spatialProcess(x, one$PM, Z= one[,formula], cov.args = list(Covariance = "Exponential"), theta.start = 15)
  fold5 <- subset(dat, t == i & folds == 5)
  foldd <- fold5[,formula]
  foldd.co <- fold5[,c("Longitude","Latitude")]
  pred_stations <- predict(spp,Z=as.matrix(foldd),x=foldd.co)
  
  
  #phi <- 15
  #trend2 <- trend.spatial(model.matrix(~elev + CMAQ1, temp.geodata[[3]]),temp.geodata)
  #ml<- likfit(geodata = temp.geodata, cov.model = "exponential", ini = c(var(one$PM), phi), trend = trend2)
  
  #sim <- grf(length(one$PM), cov.pars = c(var(one$PM), max(PM)- min(PM)))
  #vario <- variog(sim)
  #plot(vario)
  
  
  #pred <- krige.conv(geodata = temp.geodata, loc= cbind(fold5$Longitude,fold5$Latitude), 
  #                   krige=krige.control(cov.model="exponential",cov.pars=c(ml$sigmasq, ml$phi), 
  #                                      nugget= ml$tausq,trend.l=trend.spatial(model.matrix(~elev+ CMAQ1, fold5),fold5)))

  if (i == 1){
    subb <- subset(mappred_df, t==i)
    pred_map <- predict(spp, Z=as.matrix(subb[,formula]) ,x= cbind(subb$Longitude,subb$Latitude))
    
    par(mar=c(3,3,3,3))
    quilt.plot(one$Longitude, one$Latitude, one$PM, zlim = c(-1,4),main= paste0("Day_",i," PM2.5 level"))
    US(add = T)
    
    par(mar=c(3,3,3,3))
    quilt.plot(subb$Longitude, subb$Latitude, pred_map, main = paste0("Spatial Analysis (Kriging) Pred on Jan 1st"), zlim = c(-1,4))
    US(add = T)
  }
  if (i == 213){
    subb <- subset(mappred_df, t==i)
    pred_map <- predict(spp, Z=as.matrix(subb[,formula]) ,x= cbind(subb$Longitude,subb$Latitude))
    
    par(mar=c(3,3,3,3))
    quilt.plot(one$Longitude, one$Latitude, one$PM, zlim = c(-1,4),main= paste0("Day_",i," PM2.5 level"))
    US(add = T)
    
    par(mar=c(3,3,3,3))
    quilt.plot(subb$Longitude, subb$Latitude, pred_map, main = paste0("Spatial Analysis (Kriging) Pred on Aug 1st"), zlim = c(-1,4))
    US(add = T)
  }
  
  #summ <- 0
  #pos <- as.data.frame(cbind(fold5$Longitude,fold5$Latitude,fold5$PM))
  #xcell <- unlist(lapply(pos[,1],function(x) min(min(which(long>=x)), 100))) - 1
  #ycell <- unlist(lapply(pos[,2],function(y) min(min(which(lat>=y)), 100)))
  #pos$cell <- (length(long)) * (ycell - 1) + xcell
  #for (row in 1:nrow(fold5)){
  #  summ <- summ + (exp(pos$V3[row]) - exp(pred$predict[pos$cell[row]]))^2
  #}
  #summ <- summ / nrow(fold5)
  #mse <<- append(mse, summ)
  
  summ <- sum((exp(fold5$PM) - exp(pred_stations))^2)
  SEsum <- SEsum + summ
  ABSsum <- ABSsum + sum(abs(exp(fold5$PM) - exp(pred_stations)))
}
MSE <- SEsum / nrow(subset(dat,t!=194 & t!=83 & t!=325 & folds==5))
MAD <- ABSsum / nrow(subset(dat, t!=194 & t!= 83 & t!=325& folds==5))
MSE
MAD
pt2 <- proc.time() - pt
pt2
