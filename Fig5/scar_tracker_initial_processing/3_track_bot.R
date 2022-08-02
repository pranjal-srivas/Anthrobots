#####
rm(list = ls())
library(tools)
library(Rvision)
library(pbapply)


#####
file <- file.choose()
frames <- readMulti(file)
bg <- image(paste0(dirname(file), "/background.png"))


#####
difs <- lapply(frames, function(x) absdiff(medianBlur(bg), medianBlur(x)))


#####
out <- pbsapply(difs, function(x) {
  th <- c(NA, NA)
  for (j in 0:255) {
    n <- connectedComponents(x > j)$n

    if (n == 1 & is.na(th[1]))
      th[1] <- j

    if (n == 0 & is.na(th[2])) {
      th[2] <- j
      break
    }
  }
  th
})

th <- max(out[1, ], na.rm = TRUE)
bws <- lapply(difs, function(x) x > th)
# bws <- lapply(1:ncol(out), function(i) difs[[i]] > out[1, i])


#####
bots <- mapply(function(l1, l2) {
  cc <- connectedComponents(l2)
  l <- list(valid = NA, centroid = c(NA, NA), bbox = matrix(NA, 2, 2), sub = NA)

  if (cc$n < 1 | nrow(cc$table) < 25) {
    l$valid <- FALSE
  } else {
    l$valid <- !(any(cc$table[, 1] == 1) | any(cc$table[, 2] == 1) |
                   any(cc$table[, 1] == ncol(l2)) | any(cc$table[, 2] == nrow(l2)))
    l$centroid <- c(mean(cc$table[, 1]), mean(cc$table[, 2]))
    l$bbox <- apply(cc$table[, 1:2], 2, range)
    l$contour <- findContours(l2, method = "none")$contours
    l$ellipse <- minAreaRect(cc$table[, 1], cc$table[, 2])

    sub1 <- subImage(l1, l$bbox[1, 1], l$bbox[1, 2],
                     l$bbox[2, 1] - l$bbox[1, 1] + 1,
                     l$bbox[2, 2] - l$bbox[1, 2] + 1)
    sub2 <- subImage(l2, l$bbox[1, 1], l$bbox[1, 2],
                     l$bbox[2, 1] - l$bbox[1, 1] + 1,
                     l$bbox[2, 2] - l$bbox[1, 2] + 1)
    l$sub <- sub1 * (sub2 / 255)
  }
  l
},
frames, bws, SIMPLIFY = FALSE)


#####
start_end <- range(which(sapply(bots, function(x) x$valid)))
bots[[start_end[1]]]$heading <- pi * bots[[start_end[1]]]$ellipse$angle / 180

for (i in (start_end[1] + 1):start_end[2]) {
  i1 <- cloneImage(bots[[i - 1]]$sub)
  i2 <- cloneImage(bots[[i]]$sub)

  dw <- ncol(i1) - ncol(i2)
  dh <- nrow(i1) - nrow(i2)

  if (dw < 0) {
    border(i1, 0, left = -dw, target = "self")
  } else if (dw > 0) {
    border(i2, 0, left = dw, target = "self")
  }

  if (dh < 0) {
    border(i1, 0, bottom = -dh, target = "self")
  } else if (dh > 0) {
    border(i2, 0, bottom = dh, target = "self")
  }

  border(i1, 5, target = "self")
  border(i2, 5, target = "self")

  trans <- findTransformECC(i1, i2, warp_mode = "euclidean")
  bots[[i]]$heading <- bots[[i - 1]]$heading - asin(trans[2,1])
}

bots <- lapply(bots, function(x) {
  if (isImage(x$sub))
    x$sub <- x$sub[]

  x
})

save(bots, file = paste0(dirname(file), "/bots.rda"))
