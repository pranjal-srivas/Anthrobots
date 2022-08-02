#####
rm(list = ls())
options(java.parameters = "-Xmx4g")
library(tools)
library(Rvision)
library(RBioFormats)
library(progress)
gc()
J("java.lang.Runtime")$getRuntime()$gc()


#####
file <- file.choose()

raw <- read.image(file)
N <- dim(raw)[3]
r <- range(raw)
out <- lapply(1:N, function(i) zeros(nrow(raw), ncol(raw), 1))

ref <- image(raw@.Data[,,round(N / 2),drop = FALSE], colorspace = "GRAY")
subtract(ref, r[1], "self")
divide(ref, r[2] - r[1], "self")
changeBitDepth(ref, "8U", 255, "self")

pb <- progress_bar$new(total = N)
for (i in 1:N) {
  frame <- image(raw@.Data[,,i,drop = FALSE], colorspace = "GRAY")
  changeBitDepth(frame, "8U", 255, "self")
  histmatch(frame, ref, "self")
  cloneImage(frame, out[[i]])
  pb$tick()
}

writeMulti(paste0(file_path_sans_ext(file), "_adj.tiff"), out, TRUE)
rm(raw)

#####


