######### alt_ext = function to extract altitudes over points

alt_ext = function(points, mnt){
  alti = over(points, mnt)
  points$alti = alti[,1]
  points = points@data
  return(points)
}


######### spatial_traj93 = function to transform data frame into a spatialpointsdf using a lambert 93 projection

spatial_traj93 <- function(traji, longitude, latitude){
  traji2 <- data.frame(longitude, latitude) # recup coords from 1st file
  traj_sp <- SpatialPointsDataFrame(traji2, traji, proj4string = CRS("+init=epsg:2154")) # coords et df into spatial
  return(traj_sp)
}

######## RealdistR = cf Readme.md

realdistR = function(p1, p2, reso, mnt_grid, profil_out = FALSE){
  
  lon = c(p1[,"x"], p2[,"x"])
  lat = c(p1[,"y"], p2[,"y"])
  
  d = dist(cbind(lon, lat))
  
  opt = d/reso
  
  if (d > reso){
    ns = as.data.frame(spline(lon, lat, opt))
  } else {
    ns = data.frame(x = lon, y = lat)
  }
  
  
  
  ns = alt_ext(spatial_traj93(ns, ns$x, ns$y), mnt_grid)
  
  ns$dists = 0
  ns$distr = 0
  ns$dcum = 0
  
  for (i in 2:nrow(ns)){
    ns$dists[i] = dist(rbind(c(ns$x[i], ns$y[i]), c(ns$x[i-1], ns$y[i-1])))
    h = ns$alti[i] - ns$alti[i-1]
    ns$distr[i] = sqrt(ns$dists[i]^2 + h^2)
    ns$dcum[i] = ns$dcum[i-1] + ns$dists[i]
  }
  
  if(profil_out){
  exe = ns[ns$alti > 0,]
  plot(x = ns$dcum, y = ns$alti, col = "white")
  polygon(c(ns$dcum, ns$dcum[nrow(ns)]), c(ns$alti,ns$alti[1]), border = NA, col = "lightblue")
  lines(x = ns$dcum, y = ns$alti, lwd = 2, col = "navy")
  }
  return(sum(ns$distr))
}

