#####
rm(list = ls())
library(tools)
library(Rvision)


#####
file <- file.choose()
bg <- image(file)


#####
m <- mean(mean(bg))
bw <- canny(bg, m / 0.8, m / 0.4, 3)
morph(bw, "close", k_shape = "ellipse", k_height = 1, k_width = 1,
      iterations = 6, target = "self")
cc <- connectedComponents(invert(bw))
ix <- which.max(table(cc$table[, 3]))
bw <- cc$labels == ix
cc <- connectedComponents(invert(bw))
ix <- order(table(cc$table[, 3]), decreasing = TRUE)[1:4]
bw <- (cc$labels != ix[1]) & (cc$labels != ix[2]) & (cc$labels != ix[3]) & (cc$labels != ix[4])
ct <- findContours(bw, method = "none")

drawPolyline(bg, ct$contours[, 2:3], TRUE, "green", 3)
plot(bg)


#####
write.csv(ct$contours, paste0(dirname(file), "/scar.csv"), row.names = FALSE)
