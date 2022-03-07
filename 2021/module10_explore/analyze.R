
###########################################################################
## Visualization and reanalysis of European PCA in Novembre et al 2008   ##
###########################################################################


## libraries
library(tidyverse)
library(vegan) ## for procrustes analysis


## read dataset and color mapping
pca <- read_tsv("datasets/novembre_2008_pca.tsv")
colors <- read_tsv("datasets/novembre_2008_colours.tsv", col_names = c("country", "color"))


## perform rotation using procrustes analysis

### target matrix of longitude / latitude for samples
loc <- pca %>%
    select(longitude, latitude) %>%
    as.matrix()

###  matrix of PC1/PC2 to be rotated
pca1 <- pca %>%
    select(PC1, PC2) %>%
    as.matrix()


### procrustes results
rot <- procrustes(loc, pca1)
plot(rot)
pca <- pca %>%
    mutate(PC1_rot = rot$Yrot[,1], PC2_rot = rot$Yrot[,2])


## recreate Novembre et al PCA plot

### set up color scale
colorscale <- colors$color
names(colorscale) <- colors$country

### summary table of median PC values
pca_summary <- pca %>%
    group_by(country, alabels) %>%
    summarise_at(c("PC1_rot", "PC2_rot"), median)

### set up matrix of line segment points for rotated axis display
lim_pc1 <- extendrange(pca$PC1)
lim_pc2 <- extendrange(pca$PC2)

axis <- rbind(c(lim_pc1[1], 0), c(lim_pc1[2], 0), c(0, lim_pc2[1]), c(0, lim_pc2[2]))
axis_rot <- predict(rot, axis, truemean = FALSE)
axis_rot_df  <- tibble(x = axis_rot[c(1, 3),1], xend = axis_rot[c(2, 4),1], y = axis_rot[c(1, 3),2], yend = axis_rot[c(2, 4), 2])

### set up matrix for axis label positions
labels <- rbind(c(lim_pc1[2] * 0.85, 0), c(0, lim_pc2[2] * 0.85))
labels_rot <- predict(rot, labels, truemean = FALSE) %>%
    as_tibble() %>%
    mutate(labels = c("PC1", "PC2"))
colnames(labels_rot)[1:2] <- c("PC1_rot", "PC2_rot")

### angles for geom_text
theta1 <- acos(rot$rotation[1,1]) * 180 / pi

### plot
p <- ggplot(pca, aes(x = PC1_rot, y = PC2_rot))
p +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend), size = 0.25, arrow = arrow(ends = "both", angle = 10, type = "closed", length = unit(0.02, "npc")), data = axis_rot_df) +
    geom_point(size = 10, colour = "white", data = labels_rot) +
    geom_text(aes(label = labels), angle = c(theta1 + 180, theta1 + 270), data = labels_rot) +
    geom_text(aes(label = alabels, colour = country), alpha = 0.8, size = 2) +
    geom_point(aes(colour = country), size = 7, data = pca_summary) +
    geom_text(aes(label = alabels), size = 3, data = pca_summary) +
    scale_color_manual(name = "Country", values = colorscale) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none")



## linear model prediction of latitude / longitude

### set up linear model and predict lat/long for test individuals
test_country <- "Germany"
d_train <- filter(pca, country != test_country)
d_test <- filter(pca, country == test_country)

lm_long <- lm(longitude ~ PC1_rot * PC2_rot, data = d_train)
lm_lat <- lm(latitude ~ PC1_rot * PC2_rot, data = d_train)

d1 <- select(d_test, sampleId, longitude, latitude) %>%
    mutate(longitude_pr = predict(lm_long, d_test), latitude_pr = predict(lm_lat, d_test), longitude_orig = longitude, latitude_orig = latitude) %>%
    select(-longitude:-latitude) %>%
    pivot_longer(-sampleId) %>%
    separate(name, into = c("variable", "group")) %>%
    pivot_wider(names_from = variable, values_from = value)


### plot locations on map
w <- map_data("world")

lim_long <- extendrange(pca$longitude)
lim_lat <- extendrange(pca$latitude)

pal <- c("black", "red")
names(pal) <- c("orig", "pr")

sh <- c(16, 4)
names(sh) <- c("orig", "pr")

p <- ggplot(w)
p +
    geom_polygon(aes(long, lat, group = group), fill = "white", color = "grey", size = 0.25) +
    geom_point(aes(x = longitude, y = latitude, colour = group, shape = group), size = 2, data = d1) +
    scale_color_manual(values = pal) +
    scale_shape_manual(values = sh) +
    coord_quickmap(xlim = lim_long, ylim = lim_lat)

