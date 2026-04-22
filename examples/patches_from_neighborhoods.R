library(MerfishData)
library(SpatialExperiment)
library(ggspavis)

# devtools::install_github("https://github.com/Nanostring-Biostats/InSituCor")
library(InSituCor)

library(mclust)

source("./code/patchDE.R")

# Load the dataset
spe <- MerfishData::MouseColonIbdCadinu2024()
colnames(spe) <- paste0("Cell", seq_len(ncol(spe)))

# Choose sample 5 slice 1 sample (3 days of DSS treatment)
spe_sub <- spe[, spe$sample_id == 5 & spe$slice_id == 1]

# Get xy coords
xy <- as.matrix(spatialCoords(spe_sub))
rownames(xy) <- colnames(spe_sub)

# Define the biological question: How epithelial cells behave in proximity of 
# immune cells?
# get spatial neighbors matrix:
neighbors <- InSituCor:::nearestNeighborGraph(x = xy[,1], y = xy[,2], N = 50, subset = 1)
# count neighboring cell types in each cell's neighborhood:
env <- InSituCor:::neighbor_tabulate(as.vector(spe_sub$tier1), neighbors) 
rownames(env) <- rownames(xy)
head(env)

# Get the epithelial cells
sub <- spe_sub$tier1 == "Epithelial"
summary(sub) # Around 10k epithelial cells
# 100 patches could be enough?

# We use as X the number of Immune cells around the epithelial cells
# This is the exposure

# Create patches with getPatches
# 1 pixel ~ 109 nm
xy_mm <- xy * 0.000109
patches_100 <- getPatches(
    xy = xy_mm[sub,],
    X = env[sub, "Immune"],
    npatches = 100, maxradius = 0.1, bitesize = 0.1,
    effectivezerodist = 0.001,
    plotprogress = TRUE)

# Try the neighborhoods approach
slice_coords <- spatialCoords(spe_sub)
slice_df <- data.frame(x = slice_coords[,1],
                       y = slice_coords[,2],
                       tier1 = spe_sub$tier1,
                       leiden_neigh = spe_sub$leiden_neigh)
# Plot the neighborhoods and immune cells in black
ggplot(data = slice_df, aes(x = x, y = y, color = leiden_neigh)) +
    geom_point(shape = 19, size = 0.5) +
    guides(colour = guide_legend(override.aes = list(size = 2))) + 
    labs(color = "Neighborhoods") +
    theme_bw(10) +
    geom_point(data = slice_df[slice_df$tier1 == "Immune", ], 
        mapping = aes(x = x, y = y), color = "black", size = 0.1)

# Find patches within a neighborhood - MU4
mask <- spe_sub$leiden_neigh == "MU4"
summary(mask)

patches <- getPatches(
    xy = xy_mm[sub & mask,],
    X = env[sub & mask, "Immune"],
    npatches = 50, maxradius = 0.01, bitesize = 0.1,
    effectivezerodist = 0.001,
    plotprogress = TRUE)

DE_50 <- patchDE(y = as.matrix(t(assay(spe_sub, "logcounts")))[sub & mask, ], 
    df = data.frame(env[sub & mask, "Immune"]),
    patch = patches)

str(DE_50)

# Preparing for meta-analysis
# define patch polygons:
use <- sub & mask
polys <- list()
for (patch in setdiff(patches, NA)) {
    inds <- ((patches == patch) & !is.na(patches))
    polys[[patch]] <- xy_mm[use, ][inds, ][chull(xy_mm[use, ][inds, ]), ]
}

par(mfrow = c(1,1))
plot(xy_mm[use, ], pch = 16, cex = 0.5, col = "grey80")
points(xy_mm[use, ], pch = 16, cex = 0.5, col = rep(spe_sub@metadata$colors_tier1, 20)[as.numeric(as.factor(patches))])
for (patch in names(polys)) {
    polygon(polys[[patch]], border = "black")
}

## get high-level DE results matrix:
meanhere <- Matrix::colMeans(norm[use, ])
de <- res$est * (res$p < 0.05) / meanhere
de[is.na(de)] <- 0

# tstats:
ts <- res$ests / replace(res$ses, (is.na(res$ses) | (res$ses == 0)), 1e6)

