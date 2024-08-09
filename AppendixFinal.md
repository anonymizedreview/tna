Supplementary material to: ‘Transition Network Analysis: A Novel Method
To Capture The Temporal, Relational And Centrality of The Learning
Process’
================

# Preparation

Importing libraries

``` r
library(dplyr)
library(TraMineR)
library(janitor)
library(tna) # devtools::install_github("anonymizedreview/tna", force = T)
library(ggseqplot)
library(seqHMM)
library(tidyr)
library(NetworkToolbox)
library(tibble)
library(knitr)
library(patchwork)
library(reshape2)
library(networktools)
options(scipen = 999)
```

Importing the dataset (https://www.delibot.xyz/) and preparing it

``` r
combo_data <- rio::import("combo_data.csv")
combo_data_filtered <- combo_data |> 
  filter(!annotation_target %in% c("", "0", "None")) 

combod_withseq <- combo_data_filtered |> 
  group_by(group_id) |> 
  mutate(Sequence = seq_along(message_id)) |> 
  ungroup()

performance <- combod_withseq |> 
 group_by(group_id) |> 
 mutate(interactions = n()) |> 
 summarise_at(vars(performance_change, team_performance, interactions), 
              mean, na.rm = TRUE)

combo_freqs <- combod_withseq |> 
  tabyl(group_id, annotation_target) |> 
  as.data.frame() |> 
  left_join(performance)

data_reshaped <- dcast(group_id ~ Sequence, 
                                 data = combod_withseq, 
                                 value.var = "annotation_target")

data_joined <- left_join(combo_freqs, data_reshaped)
```

# General model

Building a sequence

``` r
seq_data <- seqdef(data_joined, 10:50)

ggseqdplot(seq_data)
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-3-1.png"
data-fig-align="center" />

``` r
ggseqiplot(seq_data)
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-3-2.png"
data-fig-align="center" />

Building the TNA model

``` r
tna_model <- build_tna(seq_data)
```

Plotting the TNA model

``` r
plot(tna_model)
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-5-1.png"
data-fig-align="center" />

Initial probabilities

``` r
data.frame(`Initial prob.` = tna_model$inits[[1]]) |> 
  rownames_to_column("Interaction") |> arrange(desc(`Initial.prob.`)) |> 
  kable()
```

| Interaction | Initial.prob. |
|:------------|--------------:|
| Solution    |         0.560 |
| Moderation  |         0.266 |
| Reasoning   |         0.174 |
| Agree       |         0.000 |
| Disagree    |         0.000 |

Transition probabilities

``` r
tna_model$transits[[1]] |> data.frame() |> 
  rownames_to_column("From\\To") |> 
  kable()
```

| From       |     Agree |  Disagree | Moderation | Reasoning |  Solution |
|:-----------|----------:|----------:|-----------:|----------:|----------:|
| Agree      | 0.2882727 | 0.0080878 |  0.1190064 | 0.3200462 | 0.2645869 |
| Disagree   | 0.1208791 | 0.0659341 |  0.0439560 | 0.5494505 | 0.2197802 |
| Moderation | 0.2077562 | 0.0041551 |  0.1468144 | 0.2008310 | 0.4404432 |
| Reasoning  | 0.1683337 | 0.0107343 |  0.0336667 | 0.5398878 | 0.2473774 |
| Solution   | 0.1948216 | 0.0091569 |  0.0558889 | 0.3590148 | 0.3811178 |

Centrality measures

``` r
centrality_measures = c("Betweenness", "Closeness", "InStrength", "OutStrength")

cents_withoutloops1 <- centralities(tna_model, measures = centrality_measures[1:2], 
                                   loops = FALSE, normalize = TRUE)
cents_withoutloops2 <- centralities(tna_model, measures = centrality_measures[3:4], 
                                   loops = FALSE, normalize = FALSE)

plot(cents_withoutloops2, ncol = 2, reorder = T,
     point_color = tna_model$colors, line_color = tna_model$colors) /
plot(cents_withoutloops1, ncol = 2, reorder = T,
     point_color = tna_model$colors, line_color = tna_model$colors) 
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-8-1.png"
data-fig-align="center" />

# Clustering

Initialization

``` r
set.seed(265)

Nvar <- length(alphabet(seq_data))
Nclusters <- 3
trans_probs <- simulate_transition_probs(Nvar, Nclusters)
init_probs <- simulate_initial_probs(Nvar, Nclusters)
```

Building and fitting the model (this might take a while…)

``` r
set.seed(265)
modelTrans <- build_mmm(seq_data, transition_probs = trans_probs,
 initial_probs = init_probs, data = data_joined, 
 formula = ~ 0 + team_performance)

controlEM = list(restart = list(times = 500, n_optimum = 501))

fitModelTrans <- fit_model(
 modelTrans, 
 global_step = TRUE,
 control_global = list(algorithm = "NLOPT_GD_STOGO_RAND"),
 local_step = TRUE,
 threads = 60,
 control_em = controlEM)
```

Plotting each cluster sequence

``` r
clusternames <- c("Solvers", "Regulated", "Debaters")

cluster_names(fitModelTrans$model) <- clusternames
sumamaryx <- summary(fitModelTrans$model)
ggseqdplot(seq_data, group = sumamaryx$most_probable_cluster) + 
  theme(strip.text = element_text(size = 17))
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-13-1.png"
data-fig-align="center" />

``` r
ggseqiplot(seq_data, group = sumamaryx$most_probable_cluster) + 
  theme(strip.text = element_text(size = 17))
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-13-2.png"
data-fig-align="center" />

Covariate effects

``` r
summary(fitModelTrans$model)-> modelsummaryS

staderrors <- data.frame(
    sqrt(diag(modelsummaryS$vcov)), nrow(modelsummaryS$coefficients)) |> 
    rownames_to_column(var = "variables") |> rename(Error = 2) |> arrange(variables) 

coefficients <- modelsummaryS$coefficients|> as.data.frame() |> 
  rownames_to_column() |> as_tibble() |> 
  pivot_longer(c(3, 4)) |> 
  rename(Estimate = value, cluster = name) |> select (-2) |>
  mutate(variablescf= paste0(cluster, ": ", rowname)) |> arrange(variablescf)

calculatederrors <- cbind(coefficients, staderrors)

errorsx <- select(calculatederrors, variable = rowname, cluster, Estimate, Error)
errorsx$z <- errorsx$Estimate/errorsx$Error
errorsx$p <- (1 - pnorm(abs(errorsx$z), 0, 1)) * 2
errorsx$cilow=errorsx$Estimate -  qnorm(0.975) * errorsx$Error
errorsx$cihi=errorsx$Estimate +  qnorm(0.975) * errorsx$Error
errorsx |> kable()  
```

| variable         | cluster   |   Estimate |     Error |         z |         p |     cilow |       cihi |
|:-----------------|:----------|-----------:|----------:|----------:|----------:|----------:|-----------:|
| team_performance | Debaters  | -0.7944944 | 0.1791831 | -4.433980 | 0.0000093 | -1.145687 | -0.4433020 |
| team_performance | Regulated | -1.0717673 | 0.1892147 | -5.664292 | 0.0000000 | -1.442621 | -0.7009133 |

Creating a new model with the cluster information

``` r
tna_model_clus <- build_tna(fitModelTrans$model)
```

Initial probabilities

``` r
data.frame(tna_model_clus$inits) |> 
  rownames_to_column("Interaction") |>  
  kable()
```

| Interaction |   Solvers | Regulated |  Debaters |
|:------------|----------:|----------:|----------:|
| Agree       | 0.0000000 | 0.0000000 | 0.0000000 |
| Disagree    | 0.0000000 | 0.0000000 | 0.0000000 |
| Moderation  | 0.1070365 | 0.3516261 | 0.4435101 |
| Reasoning   | 0.1547647 | 0.1257223 | 0.2453459 |
| Solution    | 0.7381988 | 0.5226516 | 0.3111440 |

Transition probabilities

``` r
tna_model_clus$transits[[1]] |> data.frame() |> 
  rownames_to_column("From\\To") |> kable()
```

| From       |     Agree |  Disagree | Moderation | Reasoning |  Solution |
|:-----------|----------:|----------:|-----------:|----------:|----------:|
| Agree      | 0.3330973 | 0.0053901 |  0.1465906 | 0.2625205 | 0.2524016 |
| Disagree   | 0.1739650 | 0.0516213 |  0.0000000 | 0.4067892 | 0.3676245 |
| Moderation | 0.3872613 | 0.0000000 |  0.1512532 | 0.1487654 | 0.3127201 |
| Reasoning  | 0.1868743 | 0.0082033 |  0.0238354 | 0.5254289 | 0.2556581 |
| Solution   | 0.2140167 | 0.0083689 |  0.0713205 | 0.3169894 | 0.3893045 |

``` r
tna_model_clus$transits[[2]] |> data.frame() |> 
  rownames_to_column("From\\To") |> kable()
```

| From       |     Agree |  Disagree | Moderation | Reasoning |  Solution |
|:-----------|----------:|----------:|-----------:|----------:|----------:|
| Agree      | 0.0848807 | 0.0000000 |  0.0378919 | 0.5262225 | 0.3510048 |
| Disagree   | 0.1051044 | 0.0573183 |  0.0000000 | 0.5866010 | 0.2509763 |
| Moderation | 0.0126669 | 0.0126024 |  0.0808113 | 0.2373875 | 0.6565319 |
| Reasoning  | 0.1105501 | 0.0135863 |  0.0146049 | 0.6391984 | 0.2220604 |
| Solution   | 0.1239837 | 0.0148440 |  0.0431694 | 0.4284747 | 0.3895282 |

``` r
tna_model_clus$transits[[3]] |> data.frame() |> 
  rownames_to_column("From\\To") |> kable()
```

| From       |     Agree |  Disagree | Moderation | Reasoning |  Solution |
|:-----------|----------:|----------:|-----------:|----------:|----------:|
| Agree      | 0.3161281 | 0.0168447 |  0.1135354 | 0.3124509 | 0.2410410 |
| Disagree   | 0.0783536 | 0.0947788 |  0.1561492 | 0.6707184 | 0.0000000 |
| Moderation | 0.0773280 | 0.0054750 |  0.1700369 | 0.2473256 | 0.4998345 |
| Reasoning  | 0.2287844 | 0.0104913 |  0.0806620 | 0.4064681 | 0.2735942 |
| Solution   | 0.2369794 | 0.0044583 |  0.0425548 | 0.3581436 | 0.3578639 |

Plotting the cluster transitions

``` r
layout(t(1:3))
plot(tna_model_clus, cluster = 1, edge.label.cex = 2, vsize = 18, 
     title = clusternames[1], title.cex = 2.5)
plot(tna_model_clus, cluster = 2, edge.label.cex = 2, vsize = 18, 
     title = clusternames[2], title.cex = 2.5)
plot(tna_model_clus, cluster = 3, edge.label.cex = 2, vsize = 18, 
     title = clusternames[3], title.cex = 2.5)
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-20-1.png"
data-fig-align="center" />

Cluster centralities

``` r
layout(t(1:2))
centralities_per_cluster1 <- centralities(tna_model_clus, normalize = T, 
                                         measures = centrality_measures[1:2])

centralities_per_cluster2 <- centralities(tna_model_clus, normalize = F, 
                                         measures = centrality_measures[3:4])

centralities_per_cluster <- cbind(centralities_per_cluster1 |> select(1:3),
                     centralities_per_cluster2 |> select(2:4)) |> as_centralities()
  
plot(centralities_per_cluster)
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-21-1.png"
data-fig-align="center" />

Communities

``` r
set.seed(265)

layout(t(1:3))
Bridging1 <- bridge(fitModelTrans$model$transition_probs$Solvers) 
plot(tna_model_clus, cluster = 1, edge.label.cex = 2, pie = NULL, vsize = 20,
     color = Bridging1$communities, title = clusternames[1], title.cex = 2.5)

Bridging2 <- bridge(fitModelTrans$model$transition_probs$Regulated) 
plot(tna_model_clus, cluster = 2, edge.label.cex = 2, pie = NULL, vsize = 20, 
     color = Bridging2$communities, title = clusternames[2], title.cex = 2.5)

Bridging3 <- bridge(fitModelTrans$model$transition_probs$Debaters) 
plot(tna_model_clus, cluster = 3, edge.label.cex = 2, pie = NULL, vsize = 20, 
     color = Bridging3$communities, title = clusternames[3], title.cex = 2.5)
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-22-1.png"
data-fig-align="center" />

# Comparing high achievers to the rest

Identify high achievers

``` r
top10 <- quantile(data_joined$team_performance, c(0.9))
```

Create one model for high achievers and one for low achievers

``` r
layout(t(1:2))

#Difference
LowAchievers = data_joined$team_performance < top10 
HighAchievers <- data_joined$team_performance > top10 

low_model <- build_tna(seq_data[LowAchievers,]); 
plot(low_model, title = "Low achievers")

high_model <- build_tna(seq_data[HighAchievers,]); 
plot(high_model, title = "High achievers")
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-24-1.png"
data-fig-align="center" />

Comparing high vs. low achievers’ transitions

``` r
layout(t(1:2))

plot(high_model, title = "High achievers")
plot_compare(high_model, low_model, title = "High - Low" )
```

<img src="AppendixFinal_files/figure-commonmark/unnamed-chunk-25-1.png"
data-fig-align="center" />
