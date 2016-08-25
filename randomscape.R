# https://www.r-bloggers.com/programming-a-twitter-bot-and-the-rescue-from-procrastination/
# https://tgmstat.wordpress.com/2013/09/11/schedule-rscript-with-cron/
# https://cran.r-project.org/web/packages/twitteR/README.html

getmap = function(fi){
  ras = raster(paste0(di,fi))
  ras = aggregate(ras,2)
  sid = 5
  #rass = focal(ras, w=matrix(1/(sid^2), nc=sid, nr=sid))# smooth
  rass = focal(ras, w=matrix(1,sid,sid), fun=mean, na.rm=T)# smooth
  latd = substr(fi,1,1)
  lat = as.numeric(substr(fi,2,3)) + 0.5
  lngd = substr(fi,4,4)
  lng = as.numeric(substr(fi,5,7)) + 0.5
  coors = paste0(lat,"°",latd,", ",lng,"°",lngd)
  if(latd == "S") lat = -lat
  if(lngd == "W") lng = -lng
  jj = fromJSON(paste0("http://maps.googleapis.com/maps/api/geocode/json?latlng=",lat,",",lng))
  text = jj$results$formatted_address[grep("administrative_area_level",jj$results$types)][1]
  list(map = rass, site = text, coors = coors, ll = list(lat=lat,long=lng))
}


library(jsonlite)
library(raster)
library(twitteR)
ee = Sys.getenv(c("CONSKEY","CONSSEC","ACCTOK","ACCSEC"))
setup_twitter_oauth(ee[1],ee[2],ee[3],ee[4])

di = "GlobalMap/overs/"
load("currsites.Rda")
#ff = list.files(di)
#landscape_fingerprints
fi = sample(ff,1)
ff = ff[-which(ff==fi)]
save(ff,file="currsites.Rda")

m = getmap(fi)
narr = pretty(range(values(m$map),na.rm=T), n=15)
if(mean(diff(narr))>50) narr = seq(round(min(values(m$map),na.rm=T),-2), 5000, 50)
wide = seq(round(min(values(m$map),na.rm=T),-2), 5000, 200)
lincol = "#666666"
png("currmap.png",width=800,height=800)
par(mar=c(0,0,0,0),oma=c(2,0,0,0))
contour(m$map, levels=narr[!narr%in%wide], drawlabels=FALSE, labcex=0, col=lincol, lwd=0.5, xaxt="n", yaxt="n", bty="n")
contour(m$map, levels=wide, drawlabels=FALSE, labcex=0, col=lincol, lwd=1, add=T)
mtext(paste0("    ",m$site," (",m$coors,")"),side=1,line=-0.5,cex=2,col=lincol,adj=0)
dev.off()

tweet(text=m$site, lat=m$ll$lat, long=m$ll$long, displayCoords="true", mediaPath="currmap.png")
