# RealdistR

R function to compute a distance with altitudes between two geographic points. Warning : the segmentation is not dependant of the euclidean distance, if reso = 15, there will be 15 altitudes between the two points.

### Parameters
- p1 and p2 = points with coordinates x and y
- reso = segmentation of the line between p1 and p2
- mnt_grid = altitudes in spatialgriddataframe format, same projection than p1 and p2
- profil_out = TRUE or FALSE, to plot topographic profile

