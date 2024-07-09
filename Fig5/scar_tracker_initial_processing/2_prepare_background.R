#####
rm(list = ls())
library(tools)
library(Rvision)
library(Rfast)


#####
file <- file.choose()
img <- readMulti(file)


#####
mat <- array(0.0, dim = c(nrow(img[[1]]), ncol(img[[1]]), 1))
mat[,,1] <- Rfast::rowMedians(sapply(img, function(x) x[]))
bg <- image(mat)
changeBitDepth(bg, "8U", target = "self")
write.Image(bg, paste0(dirname(file), "/background.png"))
plot(bg)



