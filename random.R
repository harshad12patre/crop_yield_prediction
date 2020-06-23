library(raster)
#Get some data
# duck.jpg<-tempfile()
download.file('http://www.pilgrimshospices.org/wp-content/uploads/Pilgrims-Hospice-Duck.jpg',"duck.jpg",mode="wb")

#Plug it into a stack object
list <- list.files(path='D:/r-projects/crop_yield_prediction', full.names=TRUE)
list
inimage <- stack(list[2])
inimage
# names(duck.raster)<-c('r','g','b')
#Look at it
# plotRGB(duck.raster)
# 
# duck.yellow<-duck.raster
# 
# duck.yellow$Yellow_spots<-0
# duck.yellow$Yellow_spots[duck.yellow$r<250&duck.yellow$g<250&duck.yellow$b>5]<-1
# plot(duck.yellow$Yellow_spots)