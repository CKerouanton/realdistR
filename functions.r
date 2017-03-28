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

  lon = c(p1[,x], p2[,x])
  lat = c(p1[,y], p2[,y])
  
  d = dist(cbind(lon, lat))
  opt = d/reso
  
  ns = spline(lon, lat, opt)
  
  points = data.frame(x = ns$x, y = ns$y)
  points = alt_ext(spatial_traj93(points, points$x, points$y), mnt_grid)
  
  points$dist = 0
  points$distr = 0
  points$dcum = 0
  
  for (i in 2:nrow(points)){
    points$dist[i] = dist(rbind(c(points$x[i], points$y[i]), c(points$x[i-1], points$y[i-1])))
    h = points$alti[i] - points$alti[i-1]
    points$distr[i] = sqrt(points$dist[i]^2 + h^2)
    points$dcum[i] = points$dcum[i-1] + points$dist[i]
  }
  
  return(sum(points$distr))
  
  if(profil_out){
    exe = points[points$alti > 0,]
    plot(x = exe$dcum, y = exe$alti, type = "l", lwd = 2)
  }
}


