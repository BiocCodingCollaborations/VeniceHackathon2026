# for other sections & data formats, see 
# https://zenodo.org/records/15574384

# necessary libraries
library(data.table) # for more memory efficient data frames
library(Matrix) # for sparse matrices like our counts matrix
library(ggplot2)
library(InSituCor) # devtools::install_github("https://github.com/Nanostring-Biostats/InSituCor")
library(HieraType) # remotes::install_github("Nanostring-Biostats/CosMx-Analysis-Scratch-Space", subdir = "_code/HieraType", ref = "Main")
library(Seurat)
library(SpatialExperiment)
library(DelayedArray)
library(Matrix)


if (FALSE) {
  # retrieval
  options(timeout=1e3)
  url <- "https://zenodo.org/records/15574384/files/221.zip"
  tf <- tempfile(fileext=".zip")
  download.file(url, tf)
  # decompression
  pa <- dirname(tf)
  unzip(tf, exdir=pa)
  # importing
  library(alabaster.sce)
  nm <- file.path(pa, "221")
  sce <- readObject(nm)
  # coersion
  xy <- sprintf("Center%s_global_mm", c("X", "Y"))
  xy <- as.matrix(colData(sce)[xy])
  (spe <- toSpatialExperiment(sce, spatialCoords=xy))
  
  #saveRDS(spe, file = "data/spe.RDS")
  
  
  ## counts matrix (genes x cells)
  counts <- counts(spe)
  counts <- DelayedArray(counts)
  counts <- as(counts, "dgCMatrix")
  
  ## cell (column) metadata
  metadata <- as.data.frame(colData(spe))
  
  ## spatial coordinates (cells x dims, usually 2 columns)
  xy <- as.matrix(spatialCoords(spe))
  
  ## optional: make sure dimensions align
  stopifnot(ncol(counts) == nrow(metadata),
            ncol(counts) == nrow(xy))
  
  save(counts, metadata, xy, file = "basicdata.RData")
}

load("basicdata.RData")
counts <- Matrix::t(counts)

# normalize:
scale_row <- mean(metadata$nCount_RNA) / metadata$nCount_RNA
norm <- counts
norm@x <- norm@x * scale_row[norm@i + 1L]

##### explore:
granular <- as.vector(metadata$lv2)
granular[is.na(granular)] <- "unknown"

# big clusts:
clust <- granular
clust[grepl("epi", clust)] <- "epi"
clust[grepl("fib", clust)] <- "fib"
clust[grepl("CAF", clust)] <- "CAF"
clust[grepl("B\\.", clust)] <- "B"
clust[grepl("PC", clust)] <- "PC"
clust[grepl("SMC", clust)] <- "SMC"
clust[grepl("DC", clust)] <- "DC"

cellcols <- rep(c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
  "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62",
  "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494",
  "#B3B3B3", "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
  "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3",
  "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
  "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99",
  "#B15928", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072"
), 3)[1:length(unique(clust))]
names(cellcols) <- unique(clust)

par(mar = c(0,0,0,0))
plot(xy, asp = 1, cex = 0.1, pch = 16,
     col = cellcols[clust])
legend("topleft", pch = 16, col = cellcols, legend = names(cellcols))
#points(xy[is.element(clust, c("BC", "TC")), ], col = 1, cex = 0.2, pch = 16)


#### define "Z" / spatial context: ----------------------------

### NO: too complicated:
# embed cells via scPearson:
# get neighbors:
# random sketch over neighbors:

## simple Z: just count cell types:

# get neighbors:
if (FALSE) {
  neighbors <- InSituCor:::nearestNeighborGraph(x = xy[,1], y = xy[,2], N = 50, subset = 1)
  saveRDS(neighbors, file = "data/neighbors.RDS")
} else {
  neighbors <- readRDS(file = "data/neighbors.RDS")
}

# count neighboring cells:
if (FALSE) {
  env <- InSituCor:::neighbor_tabulate(clust, neighbors)
  saveRDS(env, file = "data/env.RDS")
} else {
  env <- readRDS(file = "data/env.RDS")
}


#### define design variable: ----------------------------------------

inds.tumor <- clust == "epi"
inds.t     <- clust == "Tc"
stopifnot(any(inds.tumor), any(inds.t))
dist2tcell <- FNN::get.knnx(
  data  = as.matrix(xy[inds.t, , drop=FALSE]),
  query = as.matrix(xy),
  k = 1
)$nn.dist[, 1]

#### define patches ------------------------------------------------------

source(url("https://raw.githubusercontent.com/Nanostring-Biostats/CosMx-Analysis-Scratch-Space/refs/heads/Main/_code/PatchDE/DEutils.R"))
#source(url("https://raw.githubusercontent.com/mcalgaro93/SpatialStratifiedDE/refs/heads/debug-getPatches/code/patchDE.R"))

# only use tumor cells kinda near T-cells:
use <- inds.tumor & (dist2tcell < 0.1)

patches <- getPatches(xy = xy[use, ], 
                      X = dist2tcell[use], 
                      npatches = 500,
                      bitesize = 0.025,
                      maxradius = 0.2,
                      roundness = 0.25,
                      initwithhotspots = TRUE,
                      n_iters = 5,
                      plotprogress = FALSE
) # produces a vector of patch assigments, potentially including NA's for cells not given a patch
#saveRDS(patches, file = "processed/patches1000.RDS")
patches <- readRDS("processed/patches500.RDS")

# split patches:
patches2 <- patches
subclusterK <- 5
for (patch in setdiff(unique(patches), NA)) {
  inds <- (patches == patch) & !is.na(patches)
  # subcluster:
  temp <- kmeans(x = xy[use, ][inds, ], centers = subclusterK)
  patches2[inds] <- paste0(patch, "_", temp$cluster)
}
patches0 <- patches
patches <- patches2
toofew <- names(which(table(patches) < 5))
patches[is.element(patches, toofew)] <- NA

# define patch polygons:
polys <- list()
for (patch in setdiff(unique(patches), NA)) {
  inds <- ((patches == patch) & !is.na(patches))
  polys[[patch]] <- xy[use, ][inds, ][chull(xy[use, ][inds, ]), ]
}


plot(xy[use, ], pch = 16, cex = 0.1, col = "grey80", asp = 1)
points(xy[use, ], pch = 16, cex = 0.1, col = rep(cellcols,1000)[as.numeric(as.factor(patches))])


plot(xy, pch = 16, cex = 0.5, col = c("grey80", "red", "dodgerblue")[1 + (inds.t) + 2*(inds.tumor)],
     asp = 1, xlim = c(7,8), ylim = c(12,13))
legend("topleft", pch =16, col = c("red", "dodgerblue"), legend = c("T cells", "epithelial cells"))

# subcluster each patch to make it small?

####  run DE: -----------------------------------------------------

# find genes useful in cancer:
set.seed(0)
sampletumor <- sample(which(inds.tumor), 1000)
meanincancer <- Matrix::colMeans(counts[sampletumor, ])
cancergenes <- meanincancer > 0.2
table(cancergenes)

# run DE:
res <- patchDE(y = norm[use, ], #[, cancergenes],
               df = data.frame("dist2tcell" = dist2tcell[use]),
               patch = patches)
str(res)
saveRDS(res, file = "processed/res2500.RDS")

res <- readRDS("processed/res2500.RDS")

#### get summary stats: ------------------------------------------

res <- res$dist2tcell
for (name in names(res)) {
  res[[name]] <- res[[name]][cancergenes, ]
}
str(res)

## get high-level DE results matrix:
meanhere <- Matrix::colMeans(norm[use, cancergenes])
de <- res$est * (res$p < 0.05) / meanhere
de[is.na(de)] <- 0

# tstats:
ts <- res$ests / replace(res$ses, (is.na(res$ses) | (res$ses == 0)), 1e6)


nhits <- rowSums(de != 0)
posrate <- rowSums(de > 0) / rowSums(de != 0)
consistent <- abs(posrate - 0.5) > 0.15

# meta analysis:
#metawts <- replace(res$ses, is.na(res$ses), 1e6)^-1/2
metawts <- pmax(res$ses, 0.1)^-1/2
metaest <- rowSums(res$est * metawts, na.rm = T) / rowSums(metawts, na.rm = T)
metaSE <- sqrt(1 / rowSums(metawts))
metaZ <- metaest / metaSE
consistent <- abs(metaZ) > 2

pheatmap::pheatmap(de[consistent, ], col = colorRampPalette(c("blue", "white", "red"))(101),
                   breaks = seq(-10,10,length.out=100))

pheatmap::pheatmap(replace(-ts, abs(ts) < 2, 0)[consistent, ], col = colorRampPalette(c("darkblue", "blue", "white", "red", "darkred"))(101),
                   breaks = seq(-10,10,length.out=100))

pheatmap::pheatmap(cor(t(de[consistent, ])), 
                   col = colorRampPalette(c("blue", "white", "red"))(101),
                   breaks = seq(-.5,.5,length.out=100))

pheatmap::pheatmap(cor(de[consistent, ]), 
                   col = colorRampPalette(c("blue", "white", "red"))(101),
                   breaks = seq(-.5,.5,length.out=100))


#### define "pZ" space: neighborhood context of patches: ---------------------

# find HVG's:
# recommend nfeatures = 3000 for WTX, 2000 for 6k panel, and using all features for 1k panels
if (FALSE ){
  seu <- Seurat::CreateSeuratObject(counts = Matrix::t(counts),
                                    meta.data = metadata)
  seu <- Seurat::FindVariableFeatures(seu, nfeatures = 3000) 
  hvgs <- setdiff(seu@assays$RNA@meta.data$var.features, NA)
  saveRDS(hvgs, file = paste0("processed/hvgs.RDS"))
} else {
  hvgs <- readRDS(paste0("processed/hvgs.RDS"))
}

# get PCs:
if (FALSE) {
  # install the sparse pearson PCA package:
  remotes::install_github("Nanostring-Biostats/CosMx-Analysis-Scratch-Space",
                          subdir = "_code/scPearsonPCA", ref = "Main")
  
  genefreq <- scPearsonPCA::gene_frequency(Matrix::t(counts)) ## gene frequency (across all cells)
  pcaobj <- scPearsonPCA::sparse_quasipoisson_pca_seurat(
    x = Matrix::t(counts[, hvgs]),
    totalcounts = metadata$nCount_RNA,
    grate = genefreq[hvgs],
    scale.max = 10, ## PC's reflect clipping pearson residuals > 10 SDs above the mean pearson residual
    do.scale = TRUE, ## PC's reflect as if pearson residuals for each gene were scaled to have standard deviation=1
    do.center = TRUE ## PC's reflect as if pearson residuals for each gene were centered to have mean=0
  )
  saveRDS(pcaobj, file = paste0("processed/pcaobj.RDS"))
} else {
  pcaobj <- readRDS(paste0("processed/pcaobj.RDS"))
}

## summarize neighborhood PCA values: (just take the mean and variance for now):

# single cell neighborhood embedding:
Z <- cbind(
  # mean cell PCs in the neighborhood:
  InSituCor:::neighbor_colMeans(pcaobj$reduction.data@cell.embeddings[, 1:20], neighbors),
  # mean of squared cell PCs in the neighborhood (scaled)
  0.1 * InSituCor:::neighbor_colMeans(pcaobj$reduction.data@cell.embeddings[, 1:20]^2, neighbors)
)
Z <- as.matrix(Z)

# mean embedding values per patch:
mz <- c()
patchnames <- setdiff(unique(patches), NA)
for (patch in patchnames) {
  inds <- (patches == patch) & !is.na(patches)
  mz <- rbind(mz, colMeans(Z[use, ][inds, ], na.rm = T))
  if (is.na(mz[1])) {print(patch)}
}
rownames(mz) <- patchnames

pheatmap::pheatmap(mz)

## get umap projection and nearest neighbors network:
umapobj <- uwot::umap(mz, ret_nn = TRUE, n_neighbors = 10)
um <- umapobj$embedding  # umap projection
zn <- umapobj$nn$euclidean        # nearest neighbors network   


## run metaanalysis over each patch's neighbors (in per-patch mean Z space):
dim(ts)
str(zn)

patchmetaZ <- c()
metawts <- pmax(res$ses, 0.1)^-1/2
for (patch in patchnames) {
  
  neighborpatches <- rownames(zn$idx)[zn$idx[patch, ]]
  
  metaest <- rowSums(res$est[, neighborpatches] * metawts[, neighborpatches], na.rm = T) / rowSums(metawts[, neighborpatches], na.rm = T)
  metaSE <- sqrt(1 / rowSums(metawts[, neighborpatches]))
  metaZ <- metaest / metaSE
  
  patchmetaZ <- rbind(patchmetaZ, metaZ)
}
rownames(patchmetaZ) <- patchnames

# heatmap:
pheatmap::pheatmap(replace(patchmetaZ, abs(patchmetaZ) < 2, 0), col = colorRampPalette(c("blue", "white", "red"))(101),
                   breaks = seq(-10,10,length.out=101))

# highly variable genes?
genevar <- apply(patchmetaZ, 2, sd)
# genes with consistent effects but weak results:
plot(posrate, rowMeans(abs(ts)), col = 0)
text(posrate, rowMeans(abs(ts)), names(posrate), cex = 0.5)


# umap:
genes <- c("B2M", "MUC2", names(genevar)[order(genevar, decreasing = TRUE)[1:2]])

par(mfrow = c(2,2))
par(mar = c(0,0,1,0))
for (gene in genes) {
  plot(um, pch = 16, cex = 1, main = gene,
       xaxt = "n", yaxt = "n",
       col = colorRampPalette(c("blue", "grey", "red"))(101)[
         pmax(1, pmin(101, 51 + 50 * patchmetaZ[, gene] / 10))])
}



### now what?

# - define clusters of genes (cluster across patches, not metaanalysis)
# - define clusters of patches - based on DE results, or based on Z?
# ---> a good framing: what neighborhood characteristics predict DE?  -> neighborhooz/Z-defined umap shows this well


# plot patches in space:
subinds <- sample(1:nrow(xy), 20000)
for (gene in genes[1:4]) {
  png(paste0("results/metaanalysis of ", gene, ".png"), width = 10, height = 5, units = "in", res = 300)
  par(mfrow = c(1,2))
  par(mar = c(0,0,2,0))
  plot(xy[subinds, ], col = "grey80", pch = 16, cex = 0.1, asp = 1, 
       xaxt = "n", yaxt = "n",
       main = paste0(gene, " per-patch t-statistic"))
  for (patch in names(polys)) {
    thispatchcol <- colorRampPalette(c("darkblue", "blue", "grey80", "red", "darkred"))(101)[
      pmax(1, pmin(101, 51 + 50 * ts[gene, patch] / 10))]
    polygon(polys[[patch]], border = thispatchcol,
            col = scales::alpha(thispatchcol, 0.75))
    if (abs(ts[gene, patch]) > 2) {
      points(median(xy[use, 1][patches == patch]), 
             median(xy[use, 2][patches == patch]),
             pch = 16, col = "yellow", cex = 0.25)
    }
  }
  plot(xy[subinds, ], col = "grey80", pch = 16, cex = 0.1, asp = 1,  
       xaxt = "n", yaxt = "n",
       main = paste0(gene, " per-patch metaanalysis Z-score"))
  for (patch in names(polys)) {
    thispatchcol <- colorRampPalette(c("darkblue", "blue", "grey80", "red", "darkred"))(101)[
      pmax(1, pmin(101, 51 + 50 * patchmetaZ[patch, gene] / 10))]
    polygon(polys[[patch]], border = thispatchcol,
            col = scales::alpha(thispatchcol, 0.75))
    if (abs(patchmetaZ[patch, gene]) > 2) {
      points(median(xy[use, 1][patches == patch], na.rm = TRUE), 
             median(xy[use, 2][patches == patch], na.rm = TRUE),
             pch = 16, col = "yellow", cex = 0.25)
    }
  }
  legend("topright", pch = 16, col = "yellow", legend = "p < 0.05")
  dev.off()
}


# snapshot for exploratory analyses:
save(xy, polys, ts, patchmetaZ, res, file = "results to explore.RData")


#### exploratory plots ----------------------------------------

