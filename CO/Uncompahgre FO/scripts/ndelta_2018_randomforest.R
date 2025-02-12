library(raster)
library(dplyr)
library(sf)
library(rgeos)
library(randomForest)
library(tmap)

#read in csv
ndelta<- read.csv("S:/Biological_Resources/LandHealthAssessments/North_Delta/2018/Data_Analysis/ND_LHA_2018.csv")

#cut down to only deer basin, petrie, and point creek
ndelta<- ndelta %>% filter(Allotment %in% c("Deer Basin-Midway", "Petrie Mesa", "Point Creek"))

# input lat/long for the entry missing it
ndelta$Latitude_NAD83[ndelta$Plot.ID=="67"]<- 38.819346
ndelta$Longitude_NAD83[ndelta$Plot.ID=="67"]<- -108.106124

#turn into spatial points
ndelta_shp <- SpatialPoints(cbind(ndelta$Longitude_NAD83, ndelta$Latitude_NAD83),  proj4string=CRS("+proj=longlat +datum=NAD83"))
ndelta_shp <- SpatialPointsDataFrame(ndelta_shp, ndelta)

#read in ndelta allotments polygons
ndelta_allotments<-shapefile("../input_data/north_delta_allotments.shp")

#reproject ndelta shp
ndelta_shp<-spTransform(ndelta_shp, ndelta_allotments@proj4string)

#
ndelta_shp$Std_1_num<-NA
ndelta_shp$Std_1_num[ndelta$Std_1 == "Meeting"] <- 1
ndelta_shp$Std_1_num[ndelta$Std_1 == "Not Meeting"] <- 0
#
ndelta_shp$Std_3_num<-NA
ndelta_shp$Std_3_num[ndelta$Std_3 == "Meeting"] <- 1
ndelta_shp$Std_3_num[ndelta$Std_3 == "Not Meeting"] <- 0


#make slightly larger extent
new_ext<-extent(ndelta_allotments)
new_ext[1]<-new_ext[1] - 1000
new_ext[2]<-new_ext[2] + 1000
new_ext[3]<-new_ext[3] - 1000
new_ext[4]<-new_ext[4] + 1000


# read in blm land ----
blm<-shapefile("../input_data/blm_dissolve_ufo.shp")

# read in data to incorporate into random forest
elevation<-raster("../input_data/elevation.tif")
slope<-raster("../input_data/slope.tif")
elevation<-resample(elevation, slope, "bilinear")

prism_ufo_extent<- extent(projectExtent(slope,"+proj=longlat +datum=NAD83 +no_defs"))
prism_ufo_extent[1]<-prism_ufo_extent[1]-0.05
prism_ufo_extent[2]<-prism_ufo_extent[2]+0.05 
prism_ufo_extent[3]<-prism_ufo_extent[3]-0.05 
prism_ufo_extent[4]<-prism_ufo_extent[4]+0.05 

ppt<- crop(raster("../input_data/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil"),prism_ufo_extent)
ppt<-projectRaster(ppt,slope,method="ngb")

tmax<- crop(raster("../input_data/PRISM_tmean_30yr_normal_800mM2_annual_bil.bil"),prism_ufo_extent)
tmax<-projectRaster(tmax,slope,method="ngb")

vpd_max<- crop(raster("../input_data/PRISM_vpdmax_30yr_normal_800mM2_annual_bil.bil"),prism_ufo_extent)
vpd_max<-projectRaster(vpd_max,slope,method="ngb")


###

awc<-rasterize(spTransform(shapefile("../input_data/awc.shp"),crs(slope)),y=slope, field='AWC')

range_prod<-rasterize(spTransform(shapefile("../input_data/range_prod_normal.shp"),crs(slope)),y=slope, field='RngProdNY')

org_matter<-rasterize(spTransform(shapefile("../input_data/organic.shp"),crs(slope)),y=slope, field='OrgMatter')

kfactor<-spTransform(shapefile("../input_data/kfactor.shp"),crs(slope))
kfactor$KfactWS<- as.numeric(kfactor$KfactWS)
kfactor<-rasterize(kfactor,y=slope, field='KfactWS')
kfactor[is.na(kfactor)]<-0

depth2res<-rasterize(spTransform(shapefile("../input_data/depth_to_res.shp"),crs(slope)),y=slope, field='Dep2ResLyr')

clay<-rasterize(spTransform(shapefile("../input_data/clay_percent.shp"),crs(slope)),y=slope, field='Clay')

ndelta_allotments2<- ndelta_allotments
ndelta_allotments2$ALLOT_NO<- as.numeric(ndelta_allotments2$ALLOT_NO)
allotment_num<- rasterize(x=ndelta_allotments2, y=slope, field='ALLOT_NO')

#change deer basin to different number
db_extent<-extent(allotment_num)
db_extent[3]<-4300100

allotment_num[db_extent][allotment_num[db_extent] == 14019] <- 14020

#laod in sample strata
sample_strata<-shapefile("T:/CO/GIS/giswork/ufo/projects/vegetation/Land Health Assessments/North Delta/layers/ND_LHA_2017/UFO_north_delta_intensification_2018_sample_frame.shp")

# need to make the text fields in strata into a number to rasterize
sample_strata@data$Strata_com_num<-NA
sample_strata@data$Strata_com_num[sample_strata@data$Strata_com=="Salt Flats"]<-1
sample_strata@data$Strata_com_num[sample_strata@data$Strata_com=="Loamy Foothills"]<-2
sample_strata@data$Strata_com_num[sample_strata@data$Strata_com=="Stony Saltdesert"]<-3
sample_strata@data$Strata_com_num[sample_strata@data$Strata_com=="Clayey Salt Desert"]<-4
sample_strata@data$Strata_com_num[sample_strata@data$Strata_com=="Dry Site Juniper"]<-5

sample_strata_ras<-rasterize(x=sample_strata, y=slope, field='Strata_com_num')

#sample_strata_ras_factor <- as.factor(sample_strata_ras)


#extract raster data onto shp
ndelta_shp$elevation<- extract(elevation, ndelta_shp, method="simple")
ndelta_shp$slope<- extract(slope, ndelta_shp, method="simple")
ndelta_shp$awc<- extract(awc, ndelta_shp, method="simple")
ndelta_shp$range_prod<- extract(range_prod, ndelta_shp, method="simple")
ndelta_shp$org_matter<- extract(org_matter, ndelta_shp, method="simple")
ndelta_shp$kfactor<- extract(kfactor, ndelta_shp, method="simple")
ndelta_shp$depth2res<- extract(depth2res, ndelta_shp, method="simple")
ndelta_shp$allotment_num<-extract(allotment_num, ndelta_shp, method="simple")
ndelta_shp$clay<-extract(clay, ndelta_shp, method="simple")
ndelta_shp$sample_strata<-extract(sample_strata_ras, ndelta_shp, method="simple")
ndelta_shp$vpd_max<-extract(vpd_max, ndelta_shp, method="simple")
ndelta_shp$tmax<-extract(tmax, ndelta_shp, method="simple")
ndelta_shp$ppt<-extract(ppt, ndelta_shp, method="simple")

ndelta_shp@data$Std_1_num_factor<- as.factor(ndelta_shp@data$Std_1_num)
ndelta_shp@data$Std_3_num_factor<- as.factor(ndelta_shp@data$Std_3_num)


df<-ndelta_shp@data[,c(1:10,140:159)]

# Calculate Standard 1 RF----
rf_std1<- randomForest(Std_1_num_factor ~
                        allotment_num+sample_strata+clay+range_prod+vpd_max+tmax,
                       data= ndelta_shp@data,
                       ntree=5000, mtry=3, importance=TRUE,corr.bias=F)



rf_std1
#varImpPlot(rf_std1)

test<-ndelta_shp@data
test$rf_predicted_std1<-predict(rf_std1, test)
test$rf_predicted_std1
test$Std_1_num_factor

# save out rf model
#saveRDS(rf_std1,"../rf_models/std1_march4.rds")

# load past rf model
#rf_std1<-readRDS("../rf_models/std1_march3.rds") 

# Calculate Standard 3  RF----
rf_std3<- randomForest(Std_3_num_factor ~
                       allotment_num+sample_strata+clay+range_prod+vpd_max+tmax,
                       data= ndelta_shp@data,
                       ntree=5000, mtry=3, importance=TRUE,corr.bias=T)


rf_std3
#varImpPlot(rf_std3)

test$rf_predicted_std3<-predict(rf_std3, test)
test$rf_predicted_std3
test$Std_3_num_factor

# save out rf model
#saveRDS(rf_std3,"../rf_models/std3_march4.rds")

# load past rf model
#rf_std3<-readRDS("../rf_models/std3_march3.rds") 



## Prepare raster stack for modeling ----
pred_stack<-stack(allotment_num, sample_strata_ras,clay,range_prod,vpd_max,tmax)
names(pred_stack)<- c("allotment_num","sample_strata","clay","range_prod","vpd_max","tmax")


## Standard 1 maps ----
std1_pred<-raster::predict(object=pred_stack,model=rf_std1,na.rm=T,inf.rm=T,format="GTiff",progress="text",datatype="INT1U")

#set as numeric rather than factor
std1_pred <- setValues(raster(std1_pred), std1_pred[])

#then mask to blm land
std1_pred<-mask(std1_pred, blm)

# how much is meeting, how much is not meeting
r_std1_petrie<-mask(std1_pred, ndelta_allotments[ndelta_allotments$ALLOT_NAME=="Petrie Mesa",])
r_std1_point<-mask(std1_pred, ndelta_allotments[ndelta_allotments$ALLOT_NAME=="Point Creek",])
r_std1_dbm<-mask(std1_pred, ndelta_allotments[ndelta_allotments$ALLOT_NAME=="Deer Basin - Midway",])

#percent
ncell(r_std1_petrie[r_std1_petrie==1]) / ncell(r_std1_petrie[!is.na(r_std1_petrie)])
ncell(r_std1_point[r_std1_point==1]) / ncell(r_std1_point[!is.na(r_std1_point)])
ncell(r_std1_dbm[r_std1_dbm==1]) / ncell(r_std1_dbm[!is.na(r_std1_dbm)])

#acres
ncell(r_std1_petrie[r_std1_petrie==1]) * res(std1_pred)[1]^2 / 4046.86
ncell(r_std1_petrie[r_std1_petrie==0]) * res(std1_pred)[1]^2 / 4046.86

ncell(r_std1_point[r_std1_point==1]) * res(std1_pred)[1]^2 / 4046.86
ncell(r_std1_point[r_std1_point==0]) * res(std1_pred)[1]^2 / 4046.86

ncell(r_std1_dbm[r_std1_dbm==1]) * res(std1_pred)[1]^2 / 4046.86
ncell(r_std1_dbm[r_std1_dbm==0]) * res(std1_pred)[1]^2 / 4046.86

# plot
#make slightly larger extent
new_ext<-extent(std1_pred)
new_ext[1]<-new_ext[1] - 1000
new_ext[2]<-new_ext[2] + 1000
new_ext[3]<-new_ext[3] - 1000
new_ext[4]<-new_ext[4] + 1000

#make palette
pal<-c("#b52309","#048f53")

jpeg("../plots/ndelta_2018_std1_randomforest_march4.jpeg", width=8,height=8,units="in",res=1200)
tm_shape(std1_pred, bbox=new_ext) + 
  tm_raster(n=2,palette = pal,
            title="Meeting Standard 1", 
            breaks = c(0,0.5,1),
            labels = c ("Not Meeting", "Meeting"))+ 
  tm_shape(ndelta_shp) + tm_dots(title="Plot Determinations",shape=21,size=0.3, labels = c("Not Meeting", "Meeting"), col = 'Std_1_num_factor', palette=c("#b52309", "#048f53"), border.lwd=2, border.col="black") +
  tm_shape(ndelta_allotments) + tm_borders(col="black", lwd=2)+
  tm_add_legend(type="fill",labels="Allotments",border.col="black",col="white",border.lwd=1.5)+
  tm_legend(legend.position = c("right", "top"))+
  tm_compass(north=0,position = c("right",0.08))+
  tm_scale_bar(position = c("right", 0.00),text.size=1.05,width=0.24)+
  tm_credits(text="Draft still in refinement", fontface="italic",
             size=1.3, position = c(0.4,0.95))+
  tm_credits(text="Petrie Mesa", fontface="italic",size=1, position = c(0.85,0.63))+
  tm_credits(text="Point Creek", fontface="italic",size=1, position = c(0.15,0.5))+
  tm_credits(text="Deer Basin - Midway", fontface="italic",size=1, position = c(0.7,0.2))+
  tm_credits(text="Deer Basin - Midway", fontface="italic",size=1, position = c(0.06,0.93))
dev.off()


# write raster
writeRaster(std1_pred, "../output_rasters/std1_randomforest_march4",format="GTiff")


## Standard 3 maps----
std3_pred<-raster::predict(object=pred_stack,model=rf_std3,na.rm=T,inf.rm=T,format="GTiff",progress="text",datatype="INT1U")

#set as numeric rather than factor
std3_pred <- setValues(raster(std3_pred), std3_pred[])

#then mask to blm
std3_pred<-mask(std3_pred, blm)

# how much is meeting, how much is not meeting
r_std3_petrie<-mask(std3_pred, ndelta_allotments[ndelta_allotments$ALLOT_NAME=="Petrie Mesa",])
r_std3_point<-mask(std3_pred, ndelta_allotments[ndelta_allotments$ALLOT_NAME=="Point Creek",])
r_std3_dbm<-mask(std3_pred, ndelta_allotments[ndelta_allotments$ALLOT_NAME=="Deer Basin - Midway",])

#percent
ncell(r_std3_petrie[r_std3_petrie==1]) / ncell(r_std3_petrie[!is.na(r_std3_petrie)])
ncell(r_std3_point[r_std3_point==1]) / ncell(r_std3_point[!is.na(r_std3_point)])
ncell(r_std3_dbm[r_std3_dbm==1]) / ncell(r_std3_dbm[!is.na(r_std3_dbm)])

#acres
ncell(r_std3_petrie[r_std3_petrie==1]) * res(std3_pred)[1]^2 / 4046.86
ncell(r_std3_petrie[r_std3_petrie==0]) * res(std3_pred)[1]^2 / 4046.86

ncell(r_std3_point[r_std3_point==1]) * res(std3_pred)[1]^2 / 4046.86
ncell(r_std3_point[r_std3_point==0]) * res(std3_pred)[1]^2 / 4046.86

ncell(r_std3_dbm[r_std3_dbm==1]) * res(std3_pred)[1]^2 / 4046.86
ncell(r_std3_dbm[r_std3_dbm==0]) * res(std3_pred)[1]^2 / 4046.86


jpeg("../plots/ndelta_2018_std3_randomforest_march4.jpeg", width=8,height=8,units="in",res=1200)
tm_shape(std3_pred,bbox=new_ext) + 
  tm_raster(n=2,palette = pal,
            title="Meeting Standard 3", 
            breaks = c(0,0.5,1),
            labels = c("Not Meeting", "Meeting"))+ 
  tm_shape(ndelta_shp) + tm_dots(title="Plot Determinations",shape=21,size=0.3, labels = c("Not Meeting", "Meeting"), col = 'Std_3_num_factor', palette=c("#b52309", "#048f53"), border.lwd=2, border.col="black") +
  tm_shape(ndelta_allotments) + tm_borders(col="black", lwd=2)+
  tm_add_legend(type="fill",labels="Allotments",border.col="black",col="white",border.lwd=1.5)+
  tm_legend(legend.position = c("right", "top"))+
  tm_compass(north=0,position = c("right",0.08))+
  tm_scale_bar(position = c("right", 0.00),text.size=1.05,width=0.24)+
  tm_credits(text="Draft still in refinement", fontface="italic",
             size=1.3, position = c(0.4,0.95))+
  tm_credits(text="Petrie Mesa", fontface="italic",size=1, position = c(0.85,0.63))+
  tm_credits(text="Point Creek", fontface="italic",size=1, position = c(0.15,0.5))+
  tm_credits(text="Deer Basin - Midway", fontface="italic",size=1, position = c(0.7,0.2))+
  tm_credits(text="Deer Basin - Midway", fontface="italic",size=1, position = c(0.06,0.93))
dev.off()


# write raster
writeRaster(std3_pred, "../output_rasters/std3_randomforest_march4",format="GTiff")




















### Visualizing trees ----
###
# Convert the result of a getTree call to a format compatible with tree
# 
# This function takes the results of a \code{randomForest::getTree} call and 
# converts the results to a form compatible with \code{tree}
# @param gTree The results of a call to \code{getTree}
# @param rforest The randomForest object 
# @return An object of class \code{tree}, which has a \code{frame} and sufficient
#     attributes to enable plotting
as.tree <- function(gTree,rforest,max.depth=3){
  if(is.numeric(gTree[,'split var'])) stop("labelVar=T required")
  bl <- matrix("", nrow=nrow(gTree), ncol=3)
  for(row in 1:nrow(gTree)){
    if(row==1){
      bl[row, 1:2] <- c('10','11')
      next
    }
    if(gTree[row,1]>0){
      bl[row,1:2] <- paste0(bl[which(gTree[,1:2]==row,arr.ind=T)], c('0','1'))
    } else {
      bl[row,3] <- bl[which(gTree[,1:2]==row, arr.ind=T)]
    }
  }
  bl <- data.frame(bl, stringsAsFactors=F); names(bl) <- c('left','right','terminal')
  fr <- list()
  fr$var <- as.character(gTree[,"split var"])
  fr$var[is.na(fr$var)] <- '<leaf>'
  fr$n <- fr$dev <- rep(0,length(fr$var))
  fr$yval <- gTree[,'prediction']
  
  # Need to work out split points based on classes of the splitting vars
  classes <- attributes(rforest$terms)$dataClasses
  blah <- data.frame(var=fr$var, splits=as.character(gTree[,'split point']), 
                     classes=classes[fr$var], stringsAsFactors=F)
  index <- which(blah$classes=='factor' & !is.na(blah$classes))
  blah$splits[index] <- sapply(blah$splits[index], factor.repr)  
  
  
  splits <- cbind(
    cutleft=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),': ','<'),
                   blah$splits), 
    cutright=paste0(ifelse(blah$classes=='factor' & !is.na(blah$classes),
                           ': ','>'),
                    blah$splits))
  splits[fr$var=='<leaf>',] <- ""
  
  fr <- as.data.frame(fr, stringsAsFactors=F)
  fr$splits <- splits
  x <- ifelse(fr$var=='<leaf>', bl[,3], gsub('.{1}$', '', bl[,1]))
  if(rforest$type=='classification'){
    fr$yprob = matrix(1/length(rforest$classes),nrow=nrow(fr), ncol=length(rforest$classes))
  }
  row.names(fr) <- strtoi(x,2)
  fr <- fr[order(x),]
  
  newtr <- list()
  newtr$frame=fr
  attr(newtr,'xlevels') <- rforest$forest$xlevels
  if(rforest$type=='classification') attr(newtr,'ylevels') <- rforest$classes
  class(newtr) <- 'tree'
  return(newtr)
}

# Compute a distance matrix between rows of a data matrix
# 
# This function takes a matrix or a data.frame, and computes the distance
# between the between the rows of the data matrix. It extends the function 
# \code{\link{dist}} by adding a new metric defined by the proportion of 
# mismatches between two vectors.
# 
# @param x a numeric matrix or data frame
# @param method the distance measure to be used. This must be one of "mismatch",
#      "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#      Any unambiguous substring can be given
# @param ... additional arguments to be passed to the \code{dist} function
# @keywords dist
dist.fn <- function(x, method='mismatch',...){
  METHODS <- c("euclidean", "maximum", "manhattan", "canberra", 
               "binary", "minkowski", "mismatch")
  method <- pmatch(method, METHODS)
  if(is.na(method)) stop("invalid distance method")
  if(METHODS[method] !="mismatch"){
    z <- as.matrix(dist(x, method=METHODS[method], ...))
  } else {
    z = matrix(0, nrow=nrow(x), ncol=nrow(x))
    for(k in 1:(nrow(x)-1)){
      for (l in (k+1):nrow(x)){
        z[k,l] <- mean(x[k,]!=x[l,])
        z[l,k] <- z[k,l]
      }}
  }
  dimnames(z)  <- list(dimnames(x)[[1]],dimnames(x)[[1]])
  return(z)
}

# Convert integers to binary representation
# 
# @param x integer to be converted
# @param reverse Should the ordering be reversed
int2bin <- function(x, reverse=F){
  y <- intToBits(x)
  yy <- paste(sapply(strsplit(paste(rev(y)),""),`[[`,2),collapse="")
  out <- gsub('^[0]+','',yy)
  if(reverse){
    bl <- rev(unlist(strsplit(out,'')))
    out <- paste(bl, collapse='')
  }
  return(out)
}

# Represent factor splits using letters
# 
# @param x character representation of integer in "split point"
factor.repr <- function(x){
  x <- int2bin(as.integer(x), reverse=T)
  n <- nchar(x)
  paste(letters[1:n][unlist(strsplit(x,''))=='1'],collapse='')
}

# Alternative version of the predict function
# 
# @param object Object from which predictions will be derived
predict2 <- function(object, ...){
  UseMethod('predict2')
}

# Alternative version of predict.randomForest, from randomForest package
# 
# @description
# This function adapts the predict.randomForest function to accomodate the situation
# where factors can have NA as a legitimate level
# 
predict2.randomForest <- function (object, newdata, type = "response", norm.votes = TRUE, 
                                   predict.all = FALSE, proximity = FALSE, nodes = FALSE, cutoff,
                                   ...) 
{
  if (!inherits(object, "randomForest")) 
    stop("object not of class randomForest")
  if (is.null(object$forest)) 
    stop("No forest component in the object")
  out.type <- charmatch(tolower(type), c("response", "prob", 
                                         "vote", "class"))
  if (is.na(out.type)) 
    stop("type must be one of 'response', 'prob', 'vote'")
  if (out.type == 4) 
    out.type <- 1
  if (out.type != 1 && object$type == "regression") 
    stop("'prob' or 'vote' not meaningful for regression")
  if (out.type == 2) 
    norm.votes <- TRUE
  if (missing(newdata)) {
    p <- if (!is.null(object$na.action)) {
      napredict(object$na.action, object$predicted)
    }
    else {
      object$predicted
    }
    if (object$type == "regression") 
      return(p)
    if (proximity & is.null(object$proximity)) 
      warning("cannot return proximity without new data if random forest object does not already have proximity")
    if (out.type == 1) {
      if (proximity) {
        return(list(pred = p, proximity = object$proximity))
      }
      else return(p)
    }
    v <- object$votes
    if (!is.null(object$na.action)) 
      v <- napredict(object$na.action, v)
    if (norm.votes) {
      t1 <- t(apply(v, 1, function(x) {
        x/sum(x)
      }))
      class(t1) <- c(class(t1), "votes")
      if (proximity) 
        return(list(pred = t1, proximity = object$proximity))
      else return(t1)
    }
    else {
      if (proximity) 
        return(list(pred = v, proximity = object$proximity))
      else return(v)
    }
  }
  if (missing(cutoff)) {
    cutoff <- object$forest$cutoff
  }
  else {
    if (sum(cutoff) > 1 || sum(cutoff) < 0 || !all(cutoff > 
                                                   0) || length(cutoff) != length(object$classes)) {
      stop("Incorrect cutoff specified.")
    }
    if (!is.null(names(cutoff))) {
      if (!all(names(cutoff) %in% object$classes)) {
        stop("Wrong name(s) for cutoff")
      }
      cutoff <- cutoff[object$classes]
    }
  }
  if (object$type == "unsupervised") 
    stop("Can't predict unsupervised forest.")
  if (inherits(object, "randomForest.formula")) {
    newdata <- as.data.frame(newdata)
    rn <- row.names(newdata)
    Terms <- delete.response(object$terms)
    x <- model.frame(Terms, newdata, na.action = na.omit)
    keep <- match(row.names(x), rn)
  }
  else {
    if (is.null(dim(newdata))) 
      dim(newdata) <- c(1, length(newdata))
    x <- newdata
    if (nrow(x) == 0) 
      stop("newdata has 0 rows")
    if (any(is.na(x))) 
      stop("missing values in newdata")
    keep <- 1:nrow(x)
    rn <- rownames(x)
    if (is.null(rn)) 
      rn <- keep
  }
  vname <- if (is.null(dim(object$importance))) {
    names(object$importance)
  }
  else {
    rownames(object$importance)
  }
  if (is.null(colnames(x))) {
    if (ncol(x) != length(vname)) {
      stop("number of variables in newdata does not match that in the training data")
    }
  }
  else {
    if (any(!vname %in% colnames(x))) 
      stop("variables in the training data missing in newdata")
    x <- x[, vname, drop = FALSE]
  }
  if (is.data.frame(x)) {
    isFactor <- function(x) is.factor(x) & !is.ordered(x)
    xfactor <- which(sapply(x, isFactor))
    if (length(xfactor) > 0 && "xlevels" %in% names(object$forest)) {
      for (i in xfactor) {
        if (any(!levels(x[[i]]) %in% object$forest$xlevels[[i]])) 
          stop("New factor levels not present in the training data")
        excl <- NA
        if(any(is.na(levels(x[[i]])))) excl <- NULL
        x[[i]] <- factor(x[[i]], levels = levels(x[[i]])[match(levels(x[[i]]), 
                                                               object$forest$xlevels[[i]])],
                         exclude=excl)
      }
    }
    cat.new <- sapply(x, function(x) if (is.factor(x) && 
                                         !is.ordered(x)) 
      length(levels(x))
      else 1)
    if (!all(object$forest$ncat == cat.new)) 
      stop("Type of predictors in new data do not match that of the training data.")
  }
  mdim <- ncol(x)
  ntest <- nrow(x)
  ntree <- object$forest$ntree
  maxcat <- max(object$forest$ncat)
  nclass <- object$forest$nclass
  nrnodes <- object$forest$nrnodes
  op <- options(warn = -1)
  on.exit(options(op))
  x <- t(data.matrix(x))
  if (predict.all) {
    treepred <- if (object$type == "regression") {
      matrix(double(ntest * ntree), ncol = ntree)
    }
    else {
      matrix(integer(ntest * ntree), ncol = ntree)
    }
  }
  else {
    treepred <- numeric(ntest)
  }
  proxmatrix <- if (proximity) 
    matrix(0, ntest, ntest)
  else numeric(1)
  nodexts <- if (nodes) 
    integer(ntest * ntree)
  else integer(ntest)
  if (object$type == "regression") {
    if (!is.null(object$forest$treemap)) {
      object$forest$leftDaughter <- object$forest$treemap[, 
                                                          1, , drop = FALSE]
      object$forest$rightDaughter <- object$forest$treemap[, 
                                                           2, , drop = FALSE]
      object$forest$treemap <- NULL
    }
    keepIndex <- "ypred"
    if (predict.all) 
      keepIndex <- c(keepIndex, "treepred")
    if (proximity) 
      keepIndex <- c(keepIndex, "proximity")
    if (nodes) 
      keepIndex <- c(keepIndex, "nodexts")
    if (!is.integer(object$forest$leftDaughter)) 
      storage.mode(object$forest$leftDaughter) <- "integer"
    if (!is.integer(object$forest$rightDaughter)) 
      storage.mode(object$forest$rightDaughter) <- "integer"
    if (!is.integer(object$forest$nodestatus)) 
      storage.mode(object$forest$nodestatus) <- "integer"
    if (!is.double(object$forest$xbestsplit)) 
      storage.mode(object$forest$xbestsplit) <- "double"
    if (!is.double(object$forest$nodepred)) 
      storage.mode(object$forest$nodepred) <- "double"
    if (!is.integer(object$forest$bestvar)) 
      storage.mode(object$forest$bestvar) <- "integer"
    if (!is.integer(object$forest$ndbigtree)) 
      storage.mode(object$forest$ndbigtree) <- "integer"
    if (!is.integer(object$forest$ncat)) 
      storage.mode(object$forest$ncat) <- "integer"
    ans <- .C("regForest", as.double(x), ypred = double(ntest), 
              as.integer(mdim), as.integer(ntest), as.integer(ntree), 
              object$forest$leftDaughter, object$forest$rightDaughter, 
              object$forest$nodestatus, nrnodes, object$forest$xbestsplit, 
              object$forest$nodepred, object$forest$bestvar, object$forest$ndbigtree, 
              object$forest$ncat, as.integer(maxcat), as.integer(predict.all), 
              treepred = as.double(treepred), as.integer(proximity), 
              proximity = as.double(proxmatrix), nodes = as.integer(nodes), 
              nodexts = as.integer(nodexts), DUP = FALSE, PACKAGE = "randomForest")[keepIndex]
    yhat <- rep(NA, length(rn))
    names(yhat) <- rn
    if (!is.null(object$coefs)) {
      yhat[keep] <- object$coefs[1] + object$coefs[2] * 
        ans$ypred
    }
    else {
      yhat[keep] <- ans$ypred
    }
    if (predict.all) {
      treepred <- matrix(NA, length(rn), ntree, dimnames = list(rn, 
                                                                NULL))
      treepred[keep, ] <- ans$treepred
    }
    if (!proximity) {
      res <- if (predict.all) 
        list(aggregate = yhat, individual = treepred)
      else yhat
    }
    else {
      res <- list(predicted = yhat, proximity = structure(ans$proximity, 
                                                          dim = c(ntest, ntest), dimnames = list(rn, rn)))
    }
    if (nodes) {
      attr(res, "nodes") <- matrix(ans$nodexts, ntest, 
                                   ntree, dimnames = list(rn[keep], 1:ntree))
    }
  }
  else {
    countts <- matrix(0, ntest, nclass)
    t1 <- .C("classForest", mdim = as.integer(mdim), ntest = as.integer(ntest), 
             nclass = as.integer(object$forest$nclass), maxcat = as.integer(maxcat), 
             nrnodes = as.integer(nrnodes), jbt = as.integer(ntree), 
             xts = as.double(x), xbestsplit = as.double(object$forest$xbestsplit), 
             pid = object$forest$pid, cutoff = as.double(cutoff), 
             countts = as.double(countts), treemap = as.integer(aperm(object$forest$treemap, 
                                                                      c(2, 1, 3))), nodestatus = as.integer(object$forest$nodestatus), 
             cat = as.integer(object$forest$ncat), nodepred = as.integer(object$forest$nodepred), 
             treepred = as.integer(treepred), jet = as.integer(numeric(ntest)), 
             bestvar = as.integer(object$forest$bestvar), nodexts = as.integer(nodexts), 
             ndbigtree = as.integer(object$forest$ndbigtree), 
             predict.all = as.integer(predict.all), prox = as.integer(proximity), 
             proxmatrix = as.double(proxmatrix), nodes = as.integer(nodes), 
             DUP = FALSE, PACKAGE = "randomForest")
    if (out.type > 1) {
      out.class.votes <- t(matrix(t1$countts, nrow = nclass, 
                                  ncol = ntest))
      if (norm.votes) 
        out.class.votes <- sweep(out.class.votes, 1, 
                                 rowSums(out.class.votes), "/")
      z <- matrix(NA, length(rn), nclass, dimnames = list(rn, 
                                                          object$classes))
      z[keep, ] <- out.class.votes
      class(z) <- c(class(z), "votes")
      res <- z
    }
    else {
      out.class <- factor(rep(NA, length(rn)), levels = 1:length(object$classes), 
                          labels = object$classes)
      out.class[keep] <- object$classes[t1$jet]
      names(out.class)[keep] <- rn[keep]
      res <- out.class
    }
    if (predict.all) {
      treepred <- matrix(object$classes[t1$treepred], nrow = length(keep), 
                         dimnames = list(rn[keep], NULL))
      res <- list(aggregate = res, individual = treepred)
    }
    if (proximity) 
      res <- list(predicted = res, proximity = structure(t1$proxmatrix, 
                                                         dim = c(ntest, ntest), dimnames = list(rn[keep], 
                                                                                                rn[keep])))
    if (nodes) 
      attr(res, "nodes") <- matrix(t1$nodexts, ntest, ntree, 
                                   dimnames = list(rn[keep], 1:ntree))
  }
  res
}



#' Representative trees from ensembles -----
#' 
#' This package implements the concept of representative trees from ensemble
#' tree-based learners introduced by Banerjee, et al, (2012). Representative trees
#' are, in some sense, trees in the ensemble which are on average the "closest" to 
#' all the other trees in the ensemble. Several trees can be representative of the 
#' ensemble in general. This package currently implements the d2 metric of tree closeness
#' (close in prediction) from Banerjee, et al.
#' 
#' @name reprtree-package
#' @references M. Banerjee, Y. Ding and A-M Noone (2012) "Identifying representative
#'     trees from ensembles". Statistics in Medicine, 31(15):1601-1616.
#' @import randomForest tree
#' @docType package
#' @name reprtree
#' @examples
#' library(reprtree)
#' rforest <- randomForest(Species~., data=iris)
#' reptree <- ReprTree(rforest, iris, metric='d2')
#' plot(reptree, index=1)
NULL


#' Identifying and extracting representative trees from a random forest 
#' 
#' This function takes a random forest object and data to run predictions on
#' and identifies representative trees based on the d0, d1 or d2 metric defined
#' in Banerjee, et al (2012). Currently only the d2 metric is implemented, using
#' either a euclidean distance (for numeric predictions) or a mismatch distance 
#' (for categorical predictions). The average distance D(T) of each tree in the 
#' set of trees in computed, and trees with the lowest D(T) value are extracted
#' and formatted to be compatible with the \code{\link{tree}} class. Trees can then
#' be visualized using \code{plot.tree} and \code{text.tree}, or using a custom
#' plot function (to be defined)
#' 
#' @param rforest A randomForest object
#' @param newdata The data on which predictions will be computed
#' @param metric The metric to be used to evaluate distance between trees. Currently
#'    only the d2 metric is implemented
#' @return A list object containing representations of the representative trees
#'    conformable with the \code{tree} class. Names of the list give the indices
#'    of the representative trees in the set of trees. 
#' @import randomForest tree
#' @export
#' @references M. Banerjee, Y. Ding and A-M Noone (2012) "Identifying representative
#'     trees from ensembles". Statistics in Medicine, 31(15):1601-1616.
#' @examples
#' library(randomForest)
#' library(tree)
#' rforest <- randomForest(Species~., data=iris)
#' reptree <- ReprTree(rforest, iris, metric='d2')
ReprTree <- function(rforest, newdata, metric='d2'){
  if(metric!='d2') stop('invalid metric!')
  require(randomForest)
  print('Constructing distance matrix...')
  preds <- predict2(rforest, newdata=newdata, predict.all=T)
  preds.indiv <- preds$individual
  d <- dist.fn(t(preds.indiv), method=ifelse(rforest$type=='classification',
                                             'mismatch',
                                             'euclidean'))
  print('Finding representative trees...')
  D <- colMeans(d)
  index <- which(D==min(D))
  trees <- lapply(as.list(index), function(i) getTree(rforest, i, labelVar=TRUE))
  names(trees) <- as.character(index)
  trees <- lapply(trees, as.tree, rforest)
  out <- list(trees=trees,D = D)
  class(out) <- c('reprtree','list')
  return(out)
}

plot.getTree <- function(rforest=NULL,tr=NULL,k=1, depth=0,main=NULL, ...){
  require(randomForest)
  if(is.null(rforest) && is.null(tr))stop('One of a random forest object or a tree object must be input')
  if(!is.null(rforest)){
    gTree <- getTree(rforest, k=k, labelVar=TRUE)
    x <- as.tree(gTree, rforest)
  } else {
    x <- tr
  }
  if(depth>0){
    x <- snip.depth(x,depth)
  }
  plot(x, type='uniform')
  text(x,split=FALSE,...)
  labelBG(x)
  labelYN(x)
  title(main=main)
}

plot.reprtree <- function(reptree, all=F, index = ifelse(all,NULL, 1), depth=0,
                          main=NULL,adj = 0.5,  ...){
  require(plotrix)
  require(tree)
  if(!is(reptree,'reprtree')) stop('Wrong class!')
  reptree <- reptree[['trees']]
  n <- length(reptree)
  if(all){
    for(i in 1:n){
      plot.getTree(tr=reptree[[i]], depth=depth, main=main, adj=0.5, ...)
      #       tr <- reptree[[i]]
      #       if(depth>0){
      #         tr <- snip.depth(reptree[[i]], depth)
      #       } 
      #       plot(tr, type='uniform')
      #       text(tr,adj=adj,cex=0.7, split=F,...)
      #       labelBG(tr)
      #       labelYN(tr)
      #       title(main=main)
    }
  } else {
    plot.getTree(tr=reptree[[index]],depth=depth, main=main, adj=0.5,...)
    #     tr <- reptree[[index]]
    #     if(depth>0){
    #       tr <- snip.depth(tr, depth)
    #     }
    #     plot(tr, type='uniform') 
    #     text(tr,adj=adj,split=F, cex=0.7, digits=2, ...)
    #     labelBG(tr)
    #     labelYN(tr)
    #     title(main=main)
  }
}

labelBG <- function(tr){
  require(plotrix)
  charht <- par('cxy')[2L]
  xy <- tree:::treeco(tr,uniform=TRUE)
  nodes <- as.integer(row.names(tr$frame))
  left.child <- match(2*nodes, nodes)
  rows <- tree:::labels.tree(tr)[left.child]
  rows <- gsub('NA,','',rows)
  ind <- !is.na(left.child)
  boxed.labels(xy$x[ind],xy$y[ind]+0.5*charht, rows[ind] , border=F, bg='white',
               cex=0.8, xpad=0.5, ypad=1)
}
labelYN <- function(tr){
  charht <- par('cxy')[2L]
  xy <- tree:::treeco(tr, uniform=T)
  nodes <- as.integer(row.names(tr$frame))
  left.child <- match(2*nodes, nodes)
  ind <- !is.na(left.child)
  text(xy$x[ind]-0.1, xy$y[ind]-0.2*charht, '<< Y',cex=0.6, adj=1)
  text(xy$x[ind]+0.1, xy$y[ind]-0.2*charht, 'N >>', cex=0.6, adj=0)
}

plot.tree <- function (x, y = NULL, type = c("proportional", "uniform"), ...) 
{
  if (inherits(x, "singlenode")) 
    stop("cannot plot singlenode tree")
  if (!inherits(x, "tree")) 
    stop("not legitimate tree")
  type <- match.arg(type)
  uniform <- type == "uniform"
  dev <- dev.cur()
  if (dev == 1L) 
    dev <- 2L
  #assign(paste0("device", dev), uniform, envir = tree_env)
  invisible(treepl(tree:::treeco(x, uniform), node = as.integer(row.names(x$frame)), 
                   ...))
}

treepl <- function (xy, node, erase = FALSE, ...) 
{
  # Modified from tree:::treepl
  x <- xy$x
  y <- xy$y
  parent <- match((node%/%2L), node)
  sibling <- match(ifelse(node%%2L, node - 1L, node + 1L), 
                   node)
  xx <- rbind(x, x, x[sibling], x[sibling], NA)
  yy <- rbind(y, y[parent], y[parent], y[sibling], NA)
  if (any(erase)) {
    lines(c(xx[, erase]), c(yy[, erase]), col = par("bg"))
    return(x = x[!erase], y = y[!erase])
  }
  plot(range(x), c(min(y)-0.5, max(y)+0.5), type = "n", axes = FALSE, xlab = "", 
       ylab = "")
  text(x[1L], y[1L], "|", ...)
  lines(c(xx[, -1L]), c(yy[, -1L]), ...)
  list(x = x, y = y)
}

#' Annotate a Tree Plot
#' 
#' @S3method text tree
#' @description Add text to a tree plot. Modification of \code{tree:::text.tree} to add uniform type of tree.
#' 
#' @param x an object of class "tree"
#' @param splits logical. If \code{TRUE} the splits are labelled
#' @param label The name of column in the \code{frame} component of \code{x}, to be used to label the nodes. Can be \code{NULL} to suppress node-labelling
#' @param all logical. By default, only the leaves are labelled, but if true interior nodes are also labelled
#' @param pretty the manipulation used for split labels infolving attributes. See Details.
#' @param digits significant digits for numerical labels
#' @param adj,xpd,... graphical parameters such as \code{cex} and \code{font}
#' @param uniform logical. Is \code{plot(..., type='uniform')} used to create the tree plot?
#' 
#' @details
#' If pretty = 0 then the level names of a factor split attributes are used unchanged. 
#' If pretty = NULL, the levels are presented by a, b, ... z, 0 ... 5. 
#' If pretty is a positive integer, abbreviate is applied to the labels with that value for its argument minlength.
#' 
#' If the lettering is vertical (par srt = 90) and adj is not supplied it is adjusted appropriately.
#' 
#' @author Abhijit Dasgupta, modifying original code by B.D. Ripley
#' @seealso \link{\code{plot.tree}}
text.tree <- function (x, splits = TRUE, label = "yval", all = FALSE, pretty = NULL, 
                       digits = getOption("digits") - 3, adj = par("adj"), xpd = TRUE, uniform=T,
                       ...) 
{
  oldxpd <- par(xpd = xpd)
  on.exit(par(oldxpd))
  if (inherits(x, "singlenode")) 
    stop("cannot plot singlenode tree")
  if (!inherits(x, "tree")) 
    stop("not legitimate tree")
  frame <- x$frame
  column <- names(frame)
  if (!is.null(ylevels <- attr(x, "ylevels"))) 
    column <- c(column, ylevels)
  if (!is.null(label) && is.na(match(label, column))) 
    stop("label must be a column label of the frame component of the tree")
  charht <- par("cxy")[2L]
  if (!is.null(srt <- list(...)$srt) && srt == 90) {
    if (missing(adj)) 
      adj <- 0
    ladj <- 1 - adj
  }
  else ladj <- adj
  xy <- tree:::treeco(x, uniform=uniform)
  if (splits) {
    node <- as.integer(row.names(frame))
    left.child <- match(2 * node, node)
    rows <- tree:::labels.tree(x, pretty = pretty)[left.child]
    ind <- !is.na(rows)
    text(xy$x[ind], xy$y[ind] + 0.5 * charht, rows[ind], 
         adj = adj, ...)
  }
  if (!is.null(label)) {
    leaves <- if (all) 
      rep(TRUE, nrow(frame))
    else frame$var == "<leaf>"
    if (label == "yval" & !is.null(ylevels)) 
      stat <- as.character(frame$yval[leaves])
    else if (!is.null(ylevels) && !is.na(lev <- match(label, 
                                                      ylevels))) 
      stat <- format(signif(frame$yprob[leaves, lev], digits = digits))
    else stat <- format(signif(frame[leaves, label], digits = digits))
    if (!is.null(dim(stat)) && dim(stat)[2L] > 1) {
      if (length(dimnames(stat)[[2L]])) 
        stat[1L, ] <- paste(sep = ":", dimnames(stat)[[2L]], 
                            stat[1L, ])
      stat <- do.call("paste", c(list(sep = "\n"), split(stat, 
                                                         col(stat))))
    }
    text(xy$x[leaves], xy$y[leaves] - 0.5 * charht, labels = stat, 
         adj = ladj, ...)
  }
  invisible()
}

#' Snipping a tree to a particular depth
#' 
#' @param tr The tree to be snipped
#' @param depth The depth at which to snip the tree
#' @return A tree object of the specified depth
snip.depth <- function(tr, depth){
  require(tree)
  nodes <- sapply(as.integer(row.names(tr$frame)), int2bin)
  nodes.to.snip <- strtoi(nodes[nchar(nodes)==depth & tr$frame$var !='<leaf>'],2)
  return(snip.tree(tr, nodes.to.snip))
}

#####
#jpeg("./defense_figures/reptree1.jpeg",width=10,height = 6,units = "in",res=600)
plot.getTree(rf_std1, k=5000, depth=7)

dev.off()
jpeg("./defense_figures/reptree2.jpeg",width=10,height = 6,units = "in",res=600)
plot.getTree(model, k=800, depth=4)
dev.off()
jpeg("./defense_figures/reptree3.jpeg",width=10,height = 6,units = "in",res=600)
plot.getTree(model, k=3, depth=4)
dev.off()
jpeg("./defense_figures/reptree4.jpeg",width=10,height = 6,units = "in",res=600)
plot.getTree(model, k=4, depth=4)
dev.off()


