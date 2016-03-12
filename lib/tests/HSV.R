library(grDevices)
library(e1071)
# Convert 3d array of RGB to 2d matrix
setwd("images")

extract.features <- function(img){
  mat <- imageData(img)
  mat_rgb <- mat
  dim(mat_rgb) <- c(nrow(mat)*ncol(mat), 3)
  mat_hsv <- rgb2hsv(t(mat_rgb))
  nH <- 10
  nS <- 6
  nV <- 6
  # Caution: determine the bins using all images! The bins should be consistent across all images. The following code is only used for demonstration on a single image.
  hBin <- seq(0, 1, length.out=nH)
  sBin <- seq(0, 1, length.out=nS)
  vBin <- seq(0, 0.005, length.out=nV) 
  freq_hsv <- as.data.frame(table(factor(findInterval(mat_hsv[1,], hBin), levels=1:nH), 
                                  factor(findInterval(mat_hsv[2,], sBin), levels=1:nS), 
                                  factor(findInterval(mat_hsv[3,], vBin), levels=1:nV)))
  hsv_feature <- as.numeric(freq_hsv$Freq)/(ncol(mat)*nrow(mat)) # normalization
  return(hsv_feature)
}

labels <- read.table("annotations/list.txt",stringsAsFactors = F)
n <- 200
X <- array(rep(0,n*360),dim=c(n,360))
for (i in 1:n){
  img <- readImage(paste0(labels$V1[i],".jpg"))
  X[i,] <- extract.features(img)
}

X <- as.data.frame(X)
y <- as.factor(labels$V3[1:n])

svm.model<-svm(x = X[,1:10],y = y,kernel="linear",scale=F)



