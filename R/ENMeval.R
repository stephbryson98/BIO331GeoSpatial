library(ENMeval)
library(raster)
library(ggplot2)
library(rasterExtras)
library(RSpatial)
library(spocc)
library(dplyr)

taxon = 'Lophodytes cucullatus'


wc = getData('worldclim', var='bio', res = 5, path='tmp/')
ext = extent(-125, -55, 20, 60)
wc = crop(wc, ext)
wc_df = as.data.frame(wc, xy=TRUE) # for plotting

#downloading
spdist <- occ(query=taxon, limit=6500) # check limit for your species
sp_df = occ2df(spdist)

#filtering
sp_df = sp_df %>% filter(longitude>=ext[1], longitude<=ext[2], latitude>=ext[3], latitude <=ext[4]) #dplyr filter points to study area

#thinning
occ2thin = poThin(
  df = sp_df[c('longitude', 'latitude')],
  spacing = 25, #minimum distance between points in thinned data
  dimension = nrow(sp_df),
  lon = 'longitude',
  lat = 'latitude'
)

sp_df = sp_df[-occ2thin,] #thin using index returned from occ2thin
ggplot() +
  geom_raster(data = wc_df, aes(x = x, y = y, fill = bio12)) +
  geom_point(data=sp_df, aes(x=longitude, y=latitude), col='green', cex=0.1) +
  coord_quickmap() +
  theme_bw() + 
  scale_fill_gradientn(colours=c('darkred', 'grey', 'navy', 'green'),
                       na.value = "black")
#Picking variables

predvars = c(1,2,3,12,16)
preds = wc[[predvars]]

eval = ENMevaluate(occ=sp_df[,c('longitude', 'latitude')],
                   env = preds,
                   method='randomkfold',
                   kfolds=10,
                   parallel=TRUE,
                   numCores = 12,
                   fc=c("L", "Q", "LQ"),
                   RMvalues=seq(0.5, 2, 0.5),
                   rasterPreds=T)

bestmod = which(eval@results$AICc==min(eval@results$AICc))

eval@results[bestmod,]

pr = predict(preds, eval@models[[bestmod]], type = 'cloglog')

pr_df = as.data.frame(pr, xy=T)

#heatmap
ggplot() +
  geom_raster(data = pr_df, aes(x = x, y = y, fill = layer)) +
  geom_point(data=sp_df, aes(x=longitude, y=latitude), col='red', cex=0.05) +
  coord_quickmap() +
  theme_bw() + 
  scale_fill_gradientn(colours=viridis::viridis(99),
                       na.value = "black")

#extract model estimated suitability for occurrence localities
est.loc = extract(pr,  eval@occ.pts)
#extract model estimated suitability for background
est.bg = extract(pr, eval@bg.pts)
#evaluate predictive ability of model
ev = evaluate(est.loc, est.bg)
#detect possible thresholds 
thr = threshold(ev)
#plot using "equal sensitivity and specificity" criteria
pr_thr = pr>thr$sensitivity
pr_thr_df = as.data.frame(pr_thr, xy=TRUE)

ggplot() +
  geom_raster(data = pr_thr_df, aes(x = x, y = y, fill = layer)) +
  geom_point(data=sp_df, aes(x=longitude, y=latitude), col='red', cex=0.2) +
  scale_fill_manual(values = c('black', 'blue')) +
  coord_quickmap() +
  theme_bw() 