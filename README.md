# SpatialStratifiedDE
Analysis project within the VeniceHackathon 2026 Bioconductor event


# Names?

- SpaceMosaic

# Report

## Motivation

Spatial data is a powerful platform for differential expression analyses, letting us interrogate how 
 a given cell type changes phenotype/expression in response to its environment. 
 For example, we might ask how tumor cells respond to T-cell attack, or how 
 fibroblast gene expression changes regions with tissue damage.
 
This kind of phenotype ~ environment differential expression testing is typically performed 
 at the level of whole studies or whole tissues. This approach has two important limitations.
First, it ignores spatial autocorrelation in gene expression, leading both to biased estimates 
 and overconfident standard errors. 
Second, it ignores how correlations can be heterogeneous across the span of a tissue. 
 For example, tumor cells might deploy different pathways in response to T-cell attack 
 under different conditions. 
In short, spatial datasets are sufficiently big and complex that we have the (statistical) power
 and the motivation to pose differential expression questions at a finer spatial resolution. 
 
## Proposal

Instead of posing a DE question at the level of a whole tissue, we'll split the tissue
 into dozens or hundreds of small patches, then run DE separately in each patch. 
This reveals the precise regions where a given DE pattern occurs. 

Individual patches may be underpowered. To obtain high-confidence DE results, 
 we employ a spatially smoothed metaanalysis framework. 
For each patch, we identify the K patches that most resemble it - not in their DE results,
 but in some underlying traits like their cell type composition. 
Then we run a metaanalysis of DE results across this set of related patches, 
 regaining statistical power lost by stratification across fine-grained patches. 

## Methods

A number of functions are required to enable this workflow. 

1. **Patch definition**. 

The \code{getPatches} function employs an iterative algorithm for patch definition. 
This algorithm incrementally shifts the borders of patches to ensure adequate statistical
power in all patches. Specifically, patches are evaluated for their sum of squares of the design variable,
which determines their statistical power to evaluate it via OLS.
At each iteration, patches with low sum of squares expand into their higher sum of squares neighbors, 
 producing gradually more homogeneous per-patch statistical power. 
 
In its most simple version, patch definition can be performed given only the target cell type's xy coordinates and values 
 of the design variable. 
We also explored a more nuanced approach. An ideal patch would have high variance of the design variable
 while being highly homogeneous in all other regards - this ensures a well-controlled, minimally-confounded DE model. 
To pursue this goal, we first used standard spatial clustering approaches to partition the tissue into niches / spatial domains. 
We then applied getPatches separately within each domain. The resulting patches are more internally homogeneous - they do not span domains. 
This does constrain patches' ability to maximize statistical power, but we hypothesize that controlling within-patch spatial confounding
 is worth this loss in power.

2. **Stratified DE**. 

For each patch, we take two steps. 
First, we plan to transform its raw counts into Pearson residuals. 
(Whole tissue-level Pearson residuals are a large dense matrix, usually too big to fit into memory, 
so we calculate them on the fly for each patch.)
Second, we use a single matrix multiplication call to perform OLS simultaneously for all genes,
 obtaining a patch's complete DE results in seconds. 
This highly computationally efficient approach is necessary, as we could potentially model
 18000+ genes across >1000 patches. 
The function \code{patchDE} runs these steps; \code{hastyDE} runs the simultaneous OLS.

3. **Characterizing patches**. 

Patch-level DE results are valuable because they allow us to 
 study how DE trends vary across spatial contexts. 
So before continuing the patchDE pipeline, we must commit to a definition of "spatial context" for a study. 
Specifically, we need a matrix encoding each patch's "spatial context" in some reasonable number of features.
Many reasonable approaches to deriving such a matrix exist. For example:

- Simply take the mean gene expression across all cells - or just the target cell type - in each patch. Some subsequent dimension reduction, e.g. down to 50 PCs, is recommended.
- Record the cell type abundances in each patch. 
- Use PCA or similar to get a reduced-dimension embedding of each cell. Then, average these values across all cells in each patch. Alternatively, record both the per-patch average and variance of these PCs.
- Use a foundation model like Novae to get a spatial embedding of each cell in a patch, then average these values.

The right approach for a given study is a design question left to the analyst. 

4. **Unsupervised subgroup meta-analysis**. 

We begin with 1. per-patch DE estimates and standard errors, and 2. the "patch characteristics matrix" defined above.
Then, for each patch, we identify the K other patches that are most similar to the target patch *in the space of the patch characteristics matrix*.
The intent is that these patches are all biologically similar, e.g. coming from the same spatial context. 
Finally, we perform a basic metaanalysis across the results from patch and its K neighbors. 
This replaces our initially unstable per-patch results with more precise and confident estimates. 

5. **Pursuing biological understanding**

After all the above, we have a matrix of DE results for all patches x genes. 
This is a rich but daunting starting point for exploratory analyses. 
Some approaches we want to facilitate:

- Proper subgroup analyses: the clinical trials world has methods for finding subgroups enriched for strong results. 
Can we do something similar? It would be great to automatically get a well-characterized subgroup for where a given
gene's DE results are strongest. 
Or similarly, we could identify patches/subgroups where the DE results are most different from the average patch.
- Cluster genes into modules with similar DE results
- Cluster patches into groups with similar DE results
- Associate per-patch DE results with relevant variables, e.g. pathologist annotations of tissue regions, or local cell type abundance...

## Results

### getPatches successfully defines small patches retaining adequate statistical power:

<img width="1098" height="669" alt="image" src="https://github.com/user-attachments/assets/8b9f7912-7a94-4741-afa5-ae866a737d3a" />

Results from the first and final iterations are shown. Top panels show patch footprints; bottom panels show each patch's sum of squares of X, which determines statistical power. 

### Local meta-analysis obtains high-confidence results from individually under-powered patches:

<img width="1652" height="825" alt="image" src="https://github.com/user-attachments/assets/9aff4652-a142-49a1-a016-1879394a2718" />

Example per-patch DE results from a single gene. Polygon color shows (left) t statistics from within-patch DE results and (right) Z statistics from per-patch metaanalysis results. 
Yellow points indicate p < 0.05 patches.


## Next steps / missing pieces:

Methods:
- Recommendations / utility functions to define patch characteristics
- Recommendations / vignettes for trying to interpret subgroup results - e.g. what's the nature of patches with enriched DE results?
- Can the metaanalyses account for non-indepenence of patches? (see metapod package)
- Instead of a full metaanalysis across neighboring patches, can we use a patch's neighbors as a prior, then use the patch's data to update that prior?
- Cluster genes, cluster patches...


Code:
- Function to support metaanalyses
- bioconductor wrapper
- convenience functions to define X
- data package for LLM exploration
- vignettes / case studies


Applications:
- Use this approach in a real analysis to ensure it's getting us to useful answers
- Is this a hypothesis testing tool or an exploratory tool?


Other:
- Need a really cool name. SpaceMosaic?



## Discussion

We note that this approach is not limited to differential expression questions. 
Any method that produces a summary statistic and a standard error could be stratified over patches
and subjected to the above metaanalysis workflow.

There are some deep questions to wrestle with. Is this a hypothesis testing framework or an exploratory framework? 
How do we handle dependence structures among patches? How do we think about multiple testing? 


The ultimate goal here is to enable nuanced and **confident** understanding of a biological question. 
We want to be able to say, "[our cell type] up-regulates [this gene] in response to [X] in spatial regions with [these characteristics]",
 and not have to worry that our result is an artifact of some cryptic confounding. 
It remains to define the best post-hoc statistics and plots to get us to that point. 



 
