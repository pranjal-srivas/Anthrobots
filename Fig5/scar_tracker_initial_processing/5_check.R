#####
rm(list = ls())
library(tools)
library(Rvision)

pol2cart <- function(r, theta) {
  c(x = r * cos(theta), y = r * sin(theta))
}


#####
file <- file.choose()
frames <- readMulti(file)
load(paste0(dirname(file), "/bots.rda"))
scar <- read.csv(paste0(dirname(file), "/scar.csv"))


#####
void <- lapply(frames, function(x) changeColorSpace(x, "BGR", "self"))
mem <- zeros(nrow(frames[[1]]), ncol(frames[[1]]))

out <- mapply(function(l1, l2) {
  disp <- cloneImage(l1)
  drawPolyline(disp, scar[, 2:3], TRUE, "yellow", 3)

  if (!is.null(l2$contour)) {
    fillPoly(mem, l2$contour[, 2:3], "#660000")
  }

  add(disp, mem, "self")

  if (l2$valid) {
    v <- l2$centroid
    v <- c(v,
           v + pol2cart(25, l2$heading),
           v + pol2cart(25, l2$heading - pi),
           v + pol2cart(25, l2$heading - pi / 2),
           v + pol2cart(25, l2$heading + pi / 2))
    drawLine(disp, v[1], v[2], v[3], v[4], thickness = 2, color = "green")
    drawLine(disp, v[1], v[2], v[5], v[6], thickness = 2, color = "green")
    drawLine(disp, v[1], v[2], v[7], v[8], thickness = 2, color = "red")
    drawLine(disp, v[1], v[2], v[9], v[10], thickness = 2, color = "red")
    drawCircle(disp, v[1], v[2], radius = 7, color = "white", thickness = -1)
  }
  disp
},
frames, bots, SIMPLIFY = FALSE)

vw <- videoWriter(paste0(dirname(file), "/tracked.mp4"), fourcc = "avc1",
                  fps = 12, height = nrow(mem), width = ncol(mem))
tmp <- lapply(out, function(x) writeFrame(vw, x))
release(vw)


