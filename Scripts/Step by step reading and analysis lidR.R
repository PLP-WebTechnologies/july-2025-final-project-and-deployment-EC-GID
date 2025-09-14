library(lidR)
las <- readLAS("D://Research//USGS_LPC_CO_NESEColorado_2019_C20_LD34661246.las")#reading data using lidR
#For a more in-depth print out of the data use the function summary() instead of print().
#To save memory, readLAS() can take an optional parameter select, 
las <- readLAS("file.las", select = "xyz")  # load XYZ only
las <- readLAS("file.las", select = "xyzi") # load XYZ and intensity only
#Examples of other attribute abbreviations are: t - gpstime, a - scan angle, n - number of returns, r - return number, c - classification, s - synthetic flag, k - keypoint flag, w - withheld flag, o - overlap flag (format 6+), u - user data, p - point source ID, e - edge of flight line flag, d - direction of scan flag
#filter allows selection of “rows” (or points) during the reading process. 
las <- readLAS("file.las", filter = "-keep_first") # Read only first returns
las1 <- readLAS("file.las", filter = "-keep_first")
las2 <- readLAS("file.las")
las2 <- filter_poi(las2, ReturnNumber == 1L)
#In the example above, we are (1) reading only the first returns or (2) reading all the points and then filtering the first returns in R.
#Multiple filter commands can be used simultaneously to, for example,
#read only the first returns between 5 and 50 meters.
las <-  readLAS("file.las",
                filter = "-keep_first -drop_z_below 5 -drop_z_above 50")
#Always make sure to run the las_check() function before delving deeply into your data.
las <- readLAS("data/chap1/corrupted.laz")
#> Warning: Invalid data: 174638 points with a 'return number' greater than the
#> 'number of returns'.
plot(las)
# Plot las object by scan angle, 
# make the background white, 
# display XYZ axis and  scale colors
plot(las, color = "ScanAngleRank", bg = "white", axis = TRUE, legend = TRUE)
plot(las, color = "RGB")
plot(las, color = "Intensity", breaks = "quantile", bg = "white")
#The package also provides some easy to use functions for common overlay. For example add_dtm3d() to add a digital terrain model
x <- plot(las, bg = "white", size = 3)
add_dtm3d(x, dtm)
x <- plot(las, bg = "white", size = 3)
add_treetops3d(x, ttops)

#It is also possible to combine two point clouds with different color palettes.
#In the following example we are using a previously classified point cloud.
#We first separate the vegetation and non vegetation points using filter_poi()
#and then plot both on top of each other with different colour schemes 
#using add options in plot()
nonveg <- filter_poi(las, Classification != LASHIGHVEGETATION)
veg <- filter_poi(las, Classification == LASHIGHVEGETATION)

x <- plot(nonveg, color = "Classification", bg = "white", size = 3)
plot(veg, add = x)

#Since lidR is based on rgl,
#it is easy to add objects to the main rendering using rgl
#functions such as rgl::point3d(), rgl::text(), rgl::surface3d(),
#and so on to produce publication-ready renderings.
#However, lidR introduces an additional challenge:
# it does not display the points with their actual coordinates.
#Instead, the points are shifted to be rendered close to (0, 0)
#due to accuracy issues, as rgl uses float (32-bit decimal numbers)
#rather than double (64-bit decimal numbers). 
#When plot() is used,
#it invisibly returns the shift values,
#which can later be used to realign other objects.
offsets <- plot(las)
print(offsets)
# [1]  391867.8 3901019.3
#The coordinates of the objects must be corrected to align with the point cloud.
#In the following we will add lines to render the trunks.

#We read a file, we locate the trees (see Section 7.1)), 
#we extract the coordinates and sizes of the trees and
#plot lines with rgl::segment3d().

#It is possible to render voxels.
#This is useful to render the output of the function voxelise_points() or
#voxel_metrics() for examples.
vox <- voxelize_points(las, 6)
plot(vox, voxel = TRUE, bg = "white")

#To better visualize the vertical structure of a point cloud,
#investigate classification results, 
#or compare the results of different interpolation routines,
#a cross section can be plotted.
#To do this, we first need to decide where the cross section will be located 
#(i.e., define the beginning and end) and specify its width.
#The point cloud can then be clipped,
#and the X and Z coordinates used to create the plot.
#For example, to create a 200 m long cross section,
#we might define the beginning and end,
#and then use the clip_transect() function to subset the point cloud.
p1 <- c(273457, 5274357)
p2 <- c(273542, 5274542)
las_tr <- clip_transect(las, p1, p2, width = 5, xz = TRUE)

#Rendering can be achieved with base plot or ggplot2.
#Notice the use of payload() to extract the data.
#frame from the LAS object.
library(ggplot2)
ggplot(payload(las_tr), aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))

#Classification of ground points is an important step
#in processing point cloud data. 
#Distinguishing between ground and 
#non-ground points allows creation of a continuous model of terrain elevation
#Many algorithms have been reported in the literature
#and lidR currently provides three of them: 
#Progressive Morphological Filter (PMF),
#Cloth Simulation Function (CSF) and 
#Multiscale Curvature Classification (MCC)
#usable with the function classify_ground().

#Progressive Morphological Filter (PMF),
# The original method is raster-based,
#while lidR performs point-based morphological operations because
#lidR is a point cloud oriented software
LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzrn")
las <- classify_ground(las, algorithm = pmf(ws = 5, th = 3))

#We can now visualize the result
plot(las, color = "Classification", size = 3, bg = "white")

#when additional filtering is needed
ws <- seq(3, 12, 3)
th <- seq(0.1, 1.5, length.out = length(ws))
las <- classify_ground(las, algorithm = pmf(ws = ws, th = th))

#Cloth Simulation Function (CSF)
# In this method the point cloud is turned upside down
#and then a cloth is dropped on the inverted surface.
#Ground points are determined by analyzing the interactions between
#the nodes of the cloth and the inverted surface
las <- classify_ground(las, algorithm = csf())

#While the default parameters of csf()
#are designed to be universal and provide accurate classification 
#results, according to the original paper, 
#sometimes that the algorithm not work properly
#In such cases the algorithm parameters need to be tuned
#to improve the result.
mycsf <- csf(sloop_smooth = TRUE, class_threshold = 1, cloth_resolution = 1, time_step = 1)
las <- classify_ground(las, mycsf)

#We can also subset only the ground point to display the result in 3D
gnd <- filter_ground(las)
plot(gnd, size = 3, bg = "white")

#Multiscale Curvature Classification (MCC)
#(MCC) uses the Evans and Hudak 2016 algorithm originally 
#implemented in the mcc-lidar software.
las <- classify_ground(las, mcc(1.5,0.3))

#Edge Artifacts
#no matter which algorithm is used in lidR
#ground classification will be weaker at the edges of point clouds
#as the algorithms must analyze the local neighbourhood
#which is missing on the edges
#to find ground points an algorithm need to
#analyze the local neighborhood or local context
#that is missing edge areas
#when processing point clouds its important to
#always consider a buffer around the region of interest
#to avoid edge artifacts
#lidR has tool to manage buffered tiles and


#Method and its parameters
#identifying the optimal algorithm
#is not a trival task and often requires several trial runs
#parameters need to be dynamically adjusted to the local context
#as parameters that work well in one file
#may yield inadequate results in another


#Other methods
#ground segmentation is not limited to the 3 methods described above
#there are many more
#also we can create plug in algorithm and testnit seamlessly
#in R with lidR using a syntax like
las <- classify_ground(las, algorithm = my_new_method(param1 = 0.05))
