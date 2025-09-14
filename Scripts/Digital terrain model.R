#generating DTM is usually the second step in processing
#that follows classification of ground points
#DTM can described as an image of the ground
#there are several algorithims have been proposed for varios terrain situation
#construction of DTM start with
#known or sampled ground points and uses
#various spatial interpolation techniques
#to infer ground points at unsampled location
#Accuracy of the DTM is very important
#because errors will propagate to future processing stages
#like tree height segmentation
LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzc")
plot(las, size = 3, bg = "white")


#Triangular irregular network
#this method based  on tringular irregular network (TIN)
#of ground data to derive a bivariate function for each triangle
#which is then used to estimate the values at unsampled
#locations (between known ground points)
#the delaunay tringulation is unique and
#the linear interpolation is parameter free
#drawbacks of this method are 
#creates a non-smooth DTM and 
#it cannot extrapolate the terrain outside the convex
#hull delimited by the ground points
#since there are no triangle facet outside the convex hull
#the interpolation is weak at the edge
#because large irrelevant triangles are often created
# it important to compute the tringulation with a buffer
#to be able to crop the DTM and clear the edge artifacts
#To generate a DTM model with the TIN algorithm we use 
#rasterize_terrain() where algorithm = tin().
dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white")



#Invert distance weighting
#simplest and most readily available methods
#that can be applied to create DTMs
#It is based on an assumption that the value at an unsampled
#point can be approximated as weighted average of values at points
#can be approximated as a weighted average of values at
#points within a certain cutt-off distance d
#from a given number k of closest neighbours
#Generates terrains that are bumpy and probably not as
#realistic as those generated using TINs
#To generate a DTM model with the IDW algorithm we use rasterize_terrain()
#where algorithm = knnidw().
dtm_idw <- rasterize_terrain(las, algorithm = knnidw(k = 10L, p = 2))
plot_dtm3d(dtm_idw, bg = "white") 



#Kriging
#most advanced approach and utilize approach and utilize
#advanced geostatistical interpolation methods that take
#into account the relationship between the returns and their 
#respective distance from each other
#lidR uses the package gstat to perform the kriging
#this method is very advanced and difficult to manipulate
#and extremely slow to compute but probably
#provides the best results with minimal edge artifacts.
#To generate a DTM model with the kriging algorithm
#we use rasterize_terrain() where algorithm = kriging().
dtm_kriging <- rasterize_terrain(las, algorithm = kriging(k = 40))
plot_dtm3d(dtm_kriging, bg = "white")


#prons and cons of DTM methods
#1. Tringulation
#very fast and efficient method
#generates very good DTMs 
#robust to empty regions inside the point cloud

#weak in the edges
#Must therefore used with a buffer of extra points
#to ensure that the region of interest is not on an edge
#TIN is recommended for broad DTM computation
#but should be avoided for small regions of interest loaded without buffers

#2. Invert distance weighting
#is fast but approximately twice slower than TIN
#the terrain is not very realistic but edges are likely
#to be free of dtrong edge artifacts
#IDW is a comparison of TIN and Kriging
#its recommended if you cannot load
#a buffer, and its edge regions are important

#3. Kriging
#is very slow because its computationally demanding
#it's not recommended for use on medium to large areas 
#It can be used for small plots without buffers to get a
#nice DTM without strong edges artifact

#whatever the method used edges are critical.
#results will always be weak if the method needs to guess
#the local topography with only partial information on the neighborhood 
#best practice is always use a buffer to obtain some information
#about the neighborhood and remove the buffer once the terrain is computed.

#OTHER METHODS
#spatial interpolation is not limited to the 3 methods
#Many more have been presented and describe din the literature
# In Chapter 19 we will learn how to create a plugin algorithm compatible with 
#rasterize_terrain() based on a multilevel B-spline 
#approximation (MBA) using the MBA package.
dtm_mba <- rasterize_terrain(las, algorithm = mba())
plot_dtm3d(dtm_mba, bg = "white")


#Render shaded DTM
#generating a hillshade layer in R is relatively straight forward
#and is done using functions from terra package.
#the terrain() and hillshade() functions can be combined 
#to take the DTM raster layers as input and return a hillshade raster:

library(terra)
dtm <- rasterize_terrain(las, algorithm = tin())
dtm_prod <- terrain(dtm, v = c("slope", "aspect"), unit = "radians")
dtm_hillshade <- shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect)
plot(dtm_hillshade, col =gray(0:30/30), legend = FALSE)

#The rayshader package also provides interesting tools to 
#generate shaded DTM.The dtm must be a RasterLayer
library(rayshader)
dtm <- raster::raster(dtm)
elmat <- raster_to_matrix(dtm)
map <- elmat %>%
  sphere_shade(texture = "imhof1", progbar = FALSE) %>%
  add_water(detect_water(elmat), color = "imhof1") %>%
  add_shadow(ray_shade(elmat, progbar = FALSE), 0.5) %>%
  add_shadow(ambient_shade(elmat, progbar = FALSE), 0)
#2D plot
plot_map(map)
#3D plot
plot_3d(map, elmat, zscale = 1, windowsize = c(800, 800))