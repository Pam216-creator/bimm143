# Class 7 Lab: Machine Learning I
Pamelina Lo (PID:A16735368)

Today we are going to learn how to apply different machine learning
methods, beginning with clustering:

The goal here is to find groups/ clusters in you input data.

First, I will make up some data with clear groups. For this I will use
the `rnorm()` function:

``` r
rnorm(10)
```

     [1] -0.33182385 -0.69465224  0.77074026 -0.40460340  1.13220117  0.15247167
     [7]  0.12044009 -1.30174617  0.07260084  0.18689283

``` r
hist(rnorm(10000, mean = 3))
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-2-1.png)

You can put this into a vector using ‘c()’ function.

``` r
n=30
x <- (c(rnorm(n, mean =-3), rnorm(n, mean =3)))
y <- rev(x)
z <- cbind(x,y)

head(z)
```

                 x        y
    [1,] -3.836847 4.484144
    [2,] -1.076203 1.134999
    [3,] -2.097966 3.781375
    [4,] -4.494567 2.371636
    [5,] -3.647916 2.897293
    [6,] -3.966026 2.789517

``` r
plot(z)
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-4-1.png)

## Class Discussion:

## kmeans()

Use the `kmeans()` dunction k to 2 and nstart=20

Inspect/print results

> Q. How many points are in each cluster? Q. What ‘components’ of your
> results - cluster size? - cluster alignment/membership? - cluster
> center?

``` r
km <- kmeans(z, centers =2)
km
```

    K-means clustering with 2 clusters of sizes 30, 30

    Cluster means:
              x         y
    1 -3.021599  3.297852
    2  3.297852 -3.021599

    Clustering vector:
     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

    Within cluster sum of squares by cluster:
    [1] 62.59405 62.59405
     (between_SS / total_SS =  90.5 %)

    Available components:

    [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    [6] "betweenss"    "size"         "iter"         "ifault"      

Results in kmeans object `km`

``` r
attributes(km)
```

    $names
    [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    [6] "betweenss"    "size"         "iter"         "ifault"      

    $class
    [1] "kmeans"

cluster size?

``` r
km$size
```

    [1] 30 30

cluster assignment/memebership?

``` r
km$cluster
```

     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

cluster center?

``` r
km$centers
```

              x         y
    1 -3.021599  3.297852
    2  3.297852 -3.021599

> Q. Plot x colored by the kmeans cluster assignment add cluster centers
> as blue points

``` r
plot(z,col="red")
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-10-1.png)

``` r
plot(z,col= c("red", "blue"))
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-11-1.png)

R will re-cycle the shorter color vector to be the same length as the
longer (number of data points) in it.

``` r
plot(z, col=100)
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-12-1.png)

``` r
plot(z, col=km$cluster)
points(km$centers, col="blue", pch=18, cex=3)
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-13-1.png)

> Q. Can you turn kmeans and ask for 4 clusters please and plot the
> results like we have done above?

``` r
pm <- kmeans(z, centers =4)
pm
```

    K-means clustering with 4 clusters of sizes 30, 13, 8, 9

    Cluster means:
              x         y
    1  3.297852 -3.021599
    2 -4.039605  3.185549
    3 -2.352214  4.447772
    4 -2.146154  2.437915

    Clustering vector:
     [1] 2 4 3 2 2 2 2 3 4 2 3 3 4 2 2 3 4 3 4 2 4 3 3 4 2 4 2 2 4 2 1 1 1 1 1 1 1 1
    [39] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

    Within cluster sum of squares by cluster:
    [1] 62.594052 11.996955  2.409075  6.835479
     (between_SS / total_SS =  93.7 %)

    Available components:

    [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    [6] "betweenss"    "size"         "iter"         "ifault"      

``` r
attributes(pm)
```

    $names
    [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    [6] "betweenss"    "size"         "iter"         "ifault"      

    $class
    [1] "kmeans"

``` r
plot(z, col=pm$cluster)
points(pm$centers, col="blue", pch=18, cex=1)
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-14-1.png)

You can add more clusters by replacing the number of centers with
`kmeans()` functions. See above.

## Hierarchical Clustering

Let’s take our some made-up data `z` and see how hclust works.

First we need a distance matrix for our data to be clustered.

``` r
d <- dist(z)
hc <- hclust(d)
hc
```


    Call:
    hclust(d = d)

    Cluster method   : complete 
    Distance         : euclidean 
    Number of objects: 60 

``` r
plot(hc)

#To add a line on your plot
abline(h=8, col = "red")
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-16-1.png)

I can get my cluster membership vectory by “cutting the tree” with the
`cutree()` function like so:

``` r
grps <- cutree(hc, h=8)
grps
```

     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

Can you plot `z` colored by our hclust results:

``` r
plot(z, col= grps)
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-18-1.png)

## PCA of UK food data

Read data from the UK food consumption in different parts of the UK.

``` r
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url, row.names=1)
head(x)
```

                   England Wales Scotland N.Ireland
    Cheese             105   103      103        66
    Carcass_meat       245   227      242       267
    Other_meat         685   803      750       586
    Fish               147   160      122        93
    Fats_and_oils      193   235      184       209
    Sugars             156   175      147       139

**Q1. How many rows and columns are in your new data frame named x? What
R functions could you use to answer this questions?**

``` r
dim(x)
```

    [1] 17  4

``` r
ncol(x)
```

    [1] 4

``` r
nrow(x)
```

    [1] 17

> Checking your data

``` r
## Preview the first 6 rows
head(x)
```

                   England Wales Scotland N.Ireland
    Cheese             105   103      103        66
    Carcass_meat       245   227      242       267
    Other_meat         685   803      750       586
    Fish               147   160      122        93
    Fats_and_oils      193   235      184       209
    Sugars             156   175      147       139

``` r
# Note how the minus indexing works
rownames(x) <- x[,1]
x <- x[,-1]
head(x)
```

        Wales Scotland N.Ireland
    105   103      103        66
    245   227      242       267
    685   803      750       586
    147   160      122        93
    193   235      184       209
    156   175      147       139

``` r
dim(x)
```

    [1] 17  3

``` r
x <- read.csv(url, row.names=1)
head(x)
```

                   England Wales Scotland N.Ireland
    Cheese             105   103      103        66
    Carcass_meat       245   227      242       267
    Other_meat         685   803      750       586
    Fish               147   160      122        93
    Fats_and_oils      193   235      184       209
    Sugars             156   175      147       139

**Q2. Which approach to solving the ‘row-names problem’ mentioned above
do you prefer and why? Is one approach more robust than another under
certain circumstances?** The ‘row.names()’ function would be the best
approach to solve the problem because the output appears more organized
and mucn more cleaner. Yes, this approach is more robust than the other.

``` r
x <- read.csv(url, row.names=1)
head(x)
```

                   England Wales Scotland N.Ireland
    Cheese             105   103      103        66
    Carcass_meat       245   227      242       267
    Other_meat         685   803      750       586
    Fish               147   160      122        93
    Fats_and_oils      193   235      184       209
    Sugars             156   175      147       139

**Q3: Changing what optional argument in the above barplot() function
results in the following plot?** You change the `beside =` function from
TRUE (T) to FALSE (F) in the barplot() function results in the following
plot.

``` r
barplot(as.matrix(x), beside=F, col=rainbow(nrow(x)))
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-25-1.png)

**Q5: Generating all pairwise plots may help somewhat. Can you make
sense of the following code and resulting figure? What does it mean if a
given point lies on the diagonal for a given plot?**

``` r
pairs(x, col=rainbow(10), pch=16)
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-26-1.png)

Its hard to see structure and trends and even this small data set. How
will we ever do this when wh have big datta sets with 1,000s or 10s of
thousands of things we are measuring.

**Q6. What is the main differences between N. Ireland and the other
countries of the UK in terms of this data-set?** The main difference
between N. Ireland and the other countries of the UK is that N. Ireland
has a some scattered data/points (which falls out of the diagonal line
of data) compared to the other UK countries which suggests that the data
has some differences than the other data.

## PCA to the Rescue

Let’s see how PCS deals with thsi dataset. So main functions in base R
to do PCA is called `prcomp()`.

``` r
pca <- prcomp(t(x))
summary(pca)
```

    Importance of components:
                                PC1      PC2      PC3       PC4
    Standard deviation     324.1502 212.7478 73.87622 3.176e-14
    Proportion of Variance   0.6744   0.2905  0.03503 0.000e+00
    Cumulative Proportion    0.6744   0.9650  1.00000 1.000e+00

Let’s see what is in inside this `pca` object that we created from
running `prcomp()`.

``` r
attributes(pca)
```

    $names
    [1] "sdev"     "rotation" "center"   "scale"    "x"       

    $class
    [1] "prcomp"

``` r
pca$x
```

                     PC1         PC2        PC3           PC4
    England   -144.99315   -2.532999 105.768945 -4.894696e-14
    Wales     -240.52915 -224.646925 -56.475555  5.700024e-13
    Scotland   -91.86934  286.081786 -44.415495 -7.460785e-13
    N.Ireland  477.39164  -58.901862  -4.877895  2.321303e-13

``` r
plot(pca$x[,1], pca$x[,2], 
     col=c("black", "red","blue", "darkgreen"), pch=16,
     xlab = "PC1(67,4%)" , ylab = "PC2(29%)" )
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-30-1.png)

**Q7. Complete the code below to generate a plot of PC1 vs PC2. The
second line adds text labels over the data points.**

``` r
# Plot PC1 vs PC2
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(x))
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-31-1.png)

**Q8. Customize your plot so that the colors of the country names match
the colors in our UK and Ireland map and table at start of this
document.**

``` r
plot(pca$x[,1], pca$x[,2],
     xlab = "PC1(67,4%)" , ylab = "PC2(29%)") 
     text(pca$x[,1], pca$x[,2], colnames(x), col=c("black", "red","blue", "darkgreen", pch=16))
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-32-1.png)

**Q9: Generate a similar ‘loadings plot’ for PC2. What two food groups
feature prominantely and what does PC2 maninly tell us about?**

``` r
## Lets focus on PC1 as it accounts for > 90% of variance 
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,2], las=2 )
```

![](Class-7-Lab_files/figure-commonmark/unnamed-chunk-33-1.png)

The two food groups Fresh_potatoes and Soft_drinks. PC2 tells us about
the food of the best possible loading scores in all UK countries in
which the best possible loading score for fresh potatoes is Scotland and
N. Ireland, England, and Wales have the best loading score in soft
drinks.

## PCA of RNA-seq data

``` r
url2 <- "https://tinyurl.com/expression-CSV"
rna.data <- read.csv(url2, row.names=1)
head(rna.data)
```

           wt1 wt2  wt3  wt4 wt5 ko1 ko2 ko3 ko4 ko5
    gene1  439 458  408  429 420  90  88  86  90  93
    gene2  219 200  204  210 187 427 423 434 433 426
    gene3 1006 989 1030 1017 973 252 237 238 226 210
    gene4  783 792  829  856 760 849 856 835 885 894
    gene5  181 249  204  244 225 277 305 272 270 279
    gene6  460 502  491  491 493 612 594 577 618 638

**Q10. How many genes and samples are in this data?**

``` r
dim(rna.data)
```

    [1] 100  10

``` r
ncol(rna.data)
```

    [1] 10

``` r
nrow(rna.data)
```

    [1] 100
