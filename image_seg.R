# image segmentation demo
# author: rwalker (ryan@ryanwalker.us)
# license: MIT

library("jpeg")
library("png")
library("graphics")
library("grid")
library("ggplot2")
library("gridExtra")
library("tidyverse")

#######################################################################################
# This script demonstrates a very simple image segmenter on color scheme
#######################################################################################
#
# Kmeans based segmenter
# 
segment_image = function(img, n){
  # create a flat, segmented image data set using kmeans
  # Segment an RGB image into n groups based on color values using Kmeans
  df = data.frame(
    red = matrix(img[,,1], ncol=1),
    green = matrix(img[,,2], ncol=1),
    blue = matrix(img[,,3], ncol=1)
  )
  K = kmeans(df,n)
  df$label = K$cluster
  
  # compute rgb values and color codes based on Kmeans centers
  colors = data.frame(
    label = 1:nrow(K$centers), 
    R = K$centers[,"red"],
    G = K$centers[,"green"],
    B = K$centers[,"blue"],
    color=rgb(K$centers)
  )
  
  # merge color codes on to df but maintain the original order of df
  df$order = 1:nrow(df)
  df = merge(df, colors)
  df = df[order(df$order),]
  df$order = NULL
  
  return(df)
  
}

#
# reconstitue the segmented images to RGB matrix
#
build_segmented_image = function(df, img){
  # reconstitue the segmented images to RGB array
  
  # get mean color channel values for each row of the df.
  R = matrix(df$R, nrow=dim(img)[1])
  G = matrix(df$G, nrow=dim(img)[1])
  B = matrix(df$B, nrow=dim(img)[1])
  
  # reconsitute the segmented image in the same shape as the input image
  img_segmented = array(dim=dim(img))
  img_segmented[,,1] = R
  img_segmented[,,2] = G
  img_segmented[,,3] = B
  
  return(img_segmented)
}

#
# 2D projection for visualizing the kmeans clustering
#
project2D_from_RGB = function(df){
  # Compute the projection of the RGB channels into 2D
  PCA = prcomp(df[,c("red","green","blue")], center=TRUE, scale=TRUE)
  pc2 = PCA$x[,1:2]
  df$x = pc2[,1]
  df$y = pc2[,2]
  return(df[,c("x","y","label","R","G","B", "color")])
}

#
# Create the projection plot of the clustered segments
#
plot_projection <- function(df, sample.size){
  # plot the projection of the segmented image data in 2D, using the
  # mean segment colors as the colors for the points in the projection
  index = sample(1:nrow(df), sample.size)
  return(ggplot(df[index,], aes(x=x, y=y, col=color)) + geom_point(size=1) + scale_color_identity())
}

#
# Inspect
#
inspect_segmentation <- function(image.raw, image.segmented, image.proj){
  # helper function to review the results of segmentation visually
  img1 = rasterGrob(image.raw)
  img2 = rasterGrob(image.segmented)
  plt = plot_projection(image.proj, 50000)
  grid.arrange(arrangeGrob(img1,img2, nrow=1),plt)
}

##############################################################
# DEMO
##############################################################
# some interesting sample images -- download them if they aren't in the current working directory
# if(!file.exists("mandrill.png")){
  download.file(url = "https://everyone.plos.org/wp-content/uploads/sites/5/2020/05/sorghum-300x200.jpg", destfile="RGB_illumination.jpg")
# }

# we can work with both JPEGs and PNGS.  For simplicty, we'll always write out to PNG though.
rgb <- readJPEG("RGB_illumination.jpg")

# segment -- tune the number of segments for each image
rgb.df = segment_image(rgb, 12)

# project RGB channels
rgb.proj = project2D_from_RGB(rgb.df)

# create segmented image data structure and write to disk
rgb.segmented = build_segmented_image(rgb.df, rgb)

# write the segmented images to disk
writePNG(rgb.segmented, "rgb_illumination_segmented.png")

# inspect the results
dev.new()
inspect_segmentation(rgb, rgb.segmented, rgb.proj)

p2 <- rasterGrob(rgb.segmented)
p1 <- rasterGrob(rgb)
rgb.proj

dat.proj <- rgb.proj %>% 
  filter(G > R & G > B)
dat.segmented = build_segmented_image(rgb.df, rgb)
dev.new()
inspect_segmentation(rgb, dat.segmented, dat.proj)

sum(dat.proj$G)/(sum(rgb.proj$R)+sum(rgb.proj$G)+sum(rgb.proj$B))