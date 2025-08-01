# Cross-expression

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Ameer Sarwar, Mara Rue, Leon French, Helen Cross, Sarah Choi, Xiaoyin Chen, and Jesse Gillis

Spatial transcriptomic technologies measure gene expression in individual cells and record their physical locations, allowing us to ask how cells influence one another within the tissue. To address this question, we introduce cross-expression, a novel framework for understanding coordinated gene expression between spatially adjacent cells. Whereas co-expression measures the degree to which the expression of two genes is coordinated within the same cells, cross-expression quantifies how their expression is associated across neighboring cells. Since two genes can trivially cross-express if they are co-expressed in neighboring cells, we define cross-expression mutually exclusively, where the target cell expresses gene A but not gene B and its neighbor expresses gene B but not gene A, thus revealing genuine coordination as opposed to mere co-localization. Here, we provide an efficient R package to perform cross-expression analysis, which makes pairwise comparisons between all genes and indicates which pairs are significantly cross-expressed across the tissue. Rather than targetting previously known genes, our framework uses the high-throughput of spatial transcriptomics data to discover spatial gene expression programs in tissue.

<img src="https://github.com/user-attachments/assets/63835a44-347e-4d1d-b6b8-d31a05a61a22" alt="Screenshot 2024-07-24 at 2 41 11 AM" width="1000"/>

## Part 1 - Setup

We will conduct cross-expression analysis on a dataset collected using BARseq (barcoded anatomy resolved by sequencing). As an example, here we provide one complete slice sectioned sagittally from the left hemisphere of an adult mouse brain.

### Package Installation

Install the package by running the following in R:

```{r}
install.packages("CrossExpression")
```
Alternatively

```{r}
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("gillislab/CrossExpression")
```

Once the package is installed, load the library and the sample data by running:

```{r}
library("CrossExpression")

data("expression")
data("locations")
```

Check that the datasets are loaded correctly by running:

```{r}
expression_df = as.matrix(expression)
locations_df  = as.data.frame(as.matrix(locations))

expression_df[1:5,1:5]
head(locations_df)
```

You should see the following:

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/e29da77f-e57e-4b34-9bf9-41e95b3e0adf" alt="Screenshot 2024-07-04 at 2 32 29 AM" width="400"/>

The `expression` is a cells by genes expression matrix with 133 genes assayed across 94,100 cells, and `locations` contains the x (`pos_x`) and y (`pos_y`) coordinates (centroids) for each cell.

## Part 2 - Core functions

Before analyzing our data, let us briefly look at it by running:

```{r}
library(ggplot2)
ggplot(locations_df) + aes(x = pos_x, y = pos_y) + geom_point(size = 0) + theme_classic()
```

This outputs the image shown below, where each dot is a cell plotted using its x and y coordinates.

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/7cd1017e-3ef4-47d2-990f-349da44fd1ab" alt="Screenshot 2024-07-05 at 12 32 00 AM" width="1153"/>

### Cross-expression across all gene pairs

We will now perform cross-expression analysis, which tells us whether a gene is preferentially expressed in cells whose neighbors express another gene. The two main inputs are the gene expression matrix `expression_df` and cell coordinates matrix `locations_df`. Run the function `cross_expression` and view its (default) output:

```{r}
cross = cross_expression(data = expression_df, locations = locations_df, neighbor = 1, alpha_cross = 0.05, alpha_co = 0, output_matrix = FALSE)
head(cross)
```

The output is given as a dataframe:

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/90b23fb7-987f-4ec2-8a6b-5240dddbc95f" alt="Screenshot 2024-07-05 at 12 44 51 AM" width="676"/>

The function compares each gene pair and reports the p-values of cross-expression before (`cross_pvalue`) and after (`cross_padj`) Benjamini-Hochberg false discovery rate (FDR) multiple test correction. It also reports whether the p-values (after FDR) are significant (`cross_sig`) at `alpha ≤ 0.05` (default) as well as the p-values of these genes' co-expression (`co_pvalue`) after FDR correction. You can play with the other parameters to test how the output changes.

A critical feature of `cross` is `cross_sig`, so let us only keep the gene pairs with significant cross-expression.

```{r}
cross = cross[as.logical(cross$cross_sig),]
head(cross)
```

Inspect the output:

<img src="https://github.com/user-attachments/assets/cc9b248e-122a-463e-b260-738f4b86ec8b" alt="Screenshot 2024-08-19 at 8 53 18 PM" width="800"/>

The gene pairs in `cross` now only include those showing statistically significant cross-expression across our tissue slice. In total, you should have `42` pairs (out of `8778` possible pairs in the `133` gene panel).

**!! Important !!**

Gene expression is spatially variable, e.g., a gene may be expressed in some slices but not in others. Since cross-expression captures coordinated gene expression, it may also be spatially variable, e.g., a gene pair may cross-express in some slices but not in others. Accordingly, understanding the input gene expression and cell coordinates is essential when interpreting the results of the `cross_expression` algorithm.

**!! Important !!**

Here, we have assumed that a gene is expressed within a cell if its count is non-zero. However, one may think that gene expression is stochastic and apply a threshold to distinguish background expression from genuine transcription. Consequently, the user may input the thresholded matrix, and the `cross_expression` function will count the non-zero entries as instances of gene expression.

### Cross-expressing cells on tissue

We now have statistical evidence that, for the genes listed in `cross`, the expression of one gene in a cell predicts the expression of another gene in the neighboring cell. But spatial transcriptomics allows us to see gene expression in space.

To this end, let us color the cells based on the expression of `Tafa1` and `Col19a1`, the first gene pair in `cross`.

Run the `tissue_expression_plot` function to do so:

```{r}
tissue_expression_plot(data = expression_df, locations = locations_df, gene1 = "Tafa1", gene2 = "Col19a1", cross_expression = FALSE, neighbor = 1, point_size = 0, scale_bar = 0)
```

This shows the following image, where the cells are colored by the expression, co-expression, or non-expression of these genes:

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/abee8094-b8f3-4dba-a9ba-91feb60bd768" alt="Screenshot 2024-07-05 at 1 25 33 AM" width="1038"/>

However, it is still difficult to distinguish the cross-expressing cell-neighbor pairs from individual cells expressing each gene. To only color cross-expressing cells, call the `tissue_expression_plot` function while setting `cross_expression = TRUE`:

```{r}
tissue_expression_plot(data = expression_df, locations = locations_df, gene1 = "Tafa1", gene2 = "Col19a1", cross_expression = TRUE, neighbor = 1, point_size = 0, scale_bar = 0)
```

This produces the following image, which clearly shows that these two genes are preferentially expressed in neighboring cells:

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/eccf7b7d-84ac-4d9c-968e-e1f5e75531e8" alt="Screenshot 2024-07-05 at 2 02 43 AM" width="1039"/>

This allows one to view any gene pair of interest and customize the plot using `R`'s `ggplot` library.

### Spatial enrichment of cross-expression

A salient feature of the last two images is that the cross-expressing cells are located towards the top of the slice (cortical brain regions) even though the individual genes, especially `Tafa1`, are expressed fairly broadly across the tissue. This raises the question, "are cross-expressing cells spatially enriched"?

We can test for the hypothesis that the average distance between cross-expressing cells is smaller than that between cross-expressing and randomly selected cells.

We do the test by running `spatial_enrichment`:

```{r}
enrich = spatial_enrichment(data = expression_df, locations = locations_df, gene1 = "Tafa1", gene2 = "Col19a1", neighbor = 1, max_pairs = 20000)
enrich$pvalue
```

The p-value from `enrich$pvalue` is smaller than `alpha = 0.05`, suggesting that cross-expression patterns between `Tafa1` and `Col19a1` are spatially enriched, confirming our observation that most such cell-neighbor pairs are towards the top of the tissue.

**!! Important !!**

The `spatial_enrichment` algorithm is stochastic in the sense that we do not use all of the non-cross-expressing cells when computing distances. Instead, we use `max_pairs = 20000` as default. Using a larger number makes the p-value more accurate but reduces computational efficiency.

We can view the distances using:

```{r}
enrich$plot
```

This shows the following image:

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/f9d77d36-e7be-40ce-92bd-e58d680a7789" alt="Screenshot 2024-07-05 at 2 32 47 AM" width="969"/>

The `spatial_enrichment` function also contains the two distance distributions, which can be obtained via `enrich$target` and `enrich$null` for further analysis.

### Cross-expression correlation

Cross-expression tells us whether the expression of one gene in a cell predicts the expression of another gene in its neighbor. The `cross_expression` algorithm quantifies this in terms of probabilities, where p-values lower than `alpha` indicate significant spatial coordination given the genes' counts in the population at large.

However, this formalism does not provide a continuous metric of the strength of the spatial relationship. Specifically, it does not tell us whether cells with high expression of a given gene are neighbors of cells with similarly high (or low) expression of another gene.

To this end, we compute Pearson's correlation between genes across cells and neighbors, namely gene A's expression across cells and gene B's expression across these cells' neighbors. Like before, the cell-neighbor pairs must show mutually exclusive expression.

Find the correlations between gene pairs using `cross_expression_correlation`:

```{r}
corr = cross_expression_correlation(data = expression_df, locations = locations_df, neighbor = 1, output_matrix = FALSE)
head(corr)
```

The output is as follows:

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/5f28d3bd-848a-4530-b636-b5e858163f22" alt="Screenshot 2024-07-08 at 10 23 18 PM" width="286"/>

The `cross_expression_correlation` function can be used in conjunction with `cross_expression`, e.g., by considering genes with significant cross-expression, or in isolation, e.g., comparing cross-expression correlations between nearby tissue sections.

## Part 3 - Auxiliary functions

We offer a few functions that augment the core analysis provided above. While these could be used independently, they will often be used as steps within longer analysis pipelines.

### Bulleseye scores - relative enrichment

In addition to statistical significance (`cross_expression`) and the strength of association (`cross_expression_correlation`), an important idea is `effect size`, which compares the number of neighbors expressing a gene to the number of target cells co-expressing it alongside the cognate gene.

The `effect size` - also called `bullseye scores` (see below) - are computed using:

```{r}
bull = bullseye_scores(data = expression_df, locations = locations_df, window_sizes = 1:5, ratio_to_co = FALSE, log_2 = FALSE, output_matrix = FALSE)
head(bull)
```

This generates the output:

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/f9959852-b604-48c5-8c34-dbd0246199f5" alt="Screenshot 2024-07-08 at 11 28 49 PM" width="636"/>

We can see the scores for the target cell (`Cell`) and the neighbors 1 to 5 as specified in `window_sizes = 1:5`. The `window_sizes` input can be non-continuous and in any order, e.g., `window_sizes = c(100, 2:5, 12)`

The bullseye scores can be used for subsequent analysis. For example, one can present the scores as ratio of the neighbor to target scores (`ratio_to_co = TRUE` in the `bullseye_scores` function) and compare these across different genes or tissues, etc.

**!! Important !!**

Cross-expression is conceptualized without considering direction. Specifically, the p-values in `cross_expression` or correlations in `cross_expression_correlation` consider the cases where gene A or gene B is in the target cell. The outputs are the FDR-corrected lower p-values and average correlation, respectively.

Like the two cross-expression functions, the `bullseye_scores` consider both cases. Unlike these functions, however, the `bullseye_scores` present both directions as outputs. Thus, the user can average the directional information or take the minimum, etc., if a unidirectional output is desired.

### Bullseye plot

An effective way to visualize the effect size is in the form of bullseye plots. These require a vector of input scores, where the first score corresponds to the target cell and subsequent scores represent `window_sizes`. The most straightforward way of making bullseye plots, therefore, is to extract the scores from `bull` (which is storing the output of the `bullseye_scores` function).

Here, we make a bullseye plot for an example gene pair `Galntl6` (central cell) and `Nrg1` (neighbors):

```{r}
# extract scores for relevant genes
score_vector = as.numeric(bull[bull$gene1 %in% "Galntl6" & bull$gene2 %in% "Nrg1", 3:ncol(bull)])

# plot bullseye using scores
bullseye_plot(scores = score_vector)
```

This creates the following plot:

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/ac0c53cc-cbd7-4b0e-9725-e1ce228139af" alt="Screenshot 2024-07-08 at 11 16 40 PM" width="653"/>

The plot - called the `bullseye plot` - shows fewer cells co-expressing `Nrg1` with `Galntl6` (center) and more nearest neighbors (first ring) expressing `Nrg1`. Importantly, the number of distant neighbors (subsequent rings) expressing `Nrg1` is about the same as the number of cells co-expressing both genes.

### Smooth gene expression

We consider cross-expression as a relation between individual cells. However, groups of cells may form spatially contiguous niches and cross-expression may obtain between niches.

We facilitate this analysis by smoothing (convolving) genes' expression in cells with that in their neighbors (kernel) using:

```{r}
smth = smooth_cells(data = expression_df, locations = locations_df, neighbors_smoothed = 5, corr = TRUE)
```

The `neighbors_smoothed = 0` performs no smoothing and positive integers specify the number of neighbors to use as the smoothing kernel. For example, `neighbors_smoothed = 5` smooths the gene expression using `5` nearest neigbhors. The `corr = TRUE` specifies that correlations between the 5-neighbor niches should be computed.

### Rotate tissue

During spatial transcriptomic data collection, tissues are often unaligned with respect to the glass slide or to each other. While sophisticated methods exist to non-linearly align them in the same coordinate plane, one is often interested in simply rotating, translating, or reflecting (linear transformations) the tissue to aid biological interpretation. For example, the cortical brain regions are usually presented towards the top of the slide.

```{r}
# rotate coordinates
locations2 = rotate_coordinates(x = locations_df$pos_x, y = locations_df$pos_y, n_degrees = 20, center = TRUE, scale = TRUE, flip_x = TRUE, flip_y = TRUE)

# view result
ggplot(locations2) + aes(x = pos_x, y = pos_y) + geom_point(size = 0) + theme_classic()
```

<img src="https://github.com/ameersarwar/cross_expression/assets/174621170/ed25f762-c505-4865-b7b5-e79e1c79834f" alt="Screenshot 2024-07-09 at 12 40 55 AM" width="1061"/>

The output in `locations2` rotates the xy coordinates `20` degrees counter-clockwise, z-score normalizes them (`center = TRUE` and `scale = TRUE`), and reflects them in the x and y axes (`flip_x = TRUE` and `flip_y = TRUE`).

In practice, we may begin with a tissue resembling `location2` and rotate it until it becomes close to the tissue in `locations`. Importantly, the linear transformations must be performed in light of the tissue biology and common practices concerning its presentation.

## Contact

If you have any questions, please email [ameer.sarwar\@mail.utoronto.ca](mailto:ameer.sarwar@mail.utoronto.ca) and copy [jesse.gillis\@utoronto.ca](mailto:jesse.gillis@utoronto.ca).

## Citation

If you find the cross-expression framework or the accompanying software useful, please cite the following manuscript:

```{sh}
Sarwar A, Rue M, French L, Cross H, Choi S, Chen X, & Gillis J.
Cross-expression meta-analysis of 695 brain samples reveals coordinated gene expression across spatially adjacent cells
bioRxiv (2025). https://doi.org/10.1101/2024.09.17.613579.
```
