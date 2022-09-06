
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecointeraction

<!-- badges: start -->
<!-- badges: end -->

This is ecointeraction package. The main goal is provide tools to model
and potential interactions given known interactions and predict unknown
interactions based, but not limited, on phylogenetic, environmental and
geographic distance among the host.

## Installation

You can install the development version of ecointeraction from
[GitHub](https://github.com/alrobles/ecointeraction) with:

``` r
# install.packages("devtools")
devtools::install_github("alrobles/ecointeraction")
```

## Example

We explore a raw data base of mammal virus.

| reference | sampling_year | country    | diagnostic_test                      | diagnostic_test_type | subclade | virus | order        | family    | mammal_species         | individual_tested | incidence |
|:----------|:--------------|:-----------|:-------------------------------------|:---------------------|:---------|:------|:-------------|:----------|:-----------------------|------------------:|----------:|
| Ba_2001   | ND            | Mauritania | NA                                   | Serology             | Ae       | SABV  | Rodentia     | Muridae   | Acomys_minous          |                 8 |         0 |
| Ba_2001   | ND            | Mauritania | NA                                   | Serology             | Ae       | SABV  | Rodentia     | Muridae   | Arvicanthis_niloticus  |                 1 |         0 |
| Ba_2001   | ND            | Mauritania | NA                                   | Serology             | Ae       | SABV  | Rodentia     | Muridae   | Desmodilliscus_braueri |                59 |         0 |
| Ba_2001   | ND            | Mauritania | NA                                   | Serology             | Ae       | SABV  | Rodentia     | Muridae   | Gerbillus_tarabuli     |                 1 |         0 |
| Ba_2001   | ND            | Mauritania | NA                                   | Serology             | Ae       | SABV  | Rodentia     | Dipodidae | Jaculus_jaculus        |                 4 |         0 |
| Baba_1990 | ND            | Nigeria    | Hemagglutination-inhibition antibody | Serology             | Ae       | BANV  | Artiodactyla | Camelidae | Camelus_dromedarius    |               269 |        13 |

We use the function accumulate_incidence to get how many reported cases
there is for each host species. We use this information to create the
datasets to train.

``` r
mammal_flavivirus_incidence <- accumulate_incidence(data = mammalvirus,
                                                    group = mammal_species,
                                                    incidence = incidence)
```

We print here the frist 6 with head.

|  id | mammal_species         | incidence | cummulativesum |
|----:|:-----------------------|----------:|---------------:|
|   1 | Camelus_dromedarius    |       510 |            510 |
|   2 | Sus_scrofa             |       336 |            846 |
|   3 | Alouatta_caraya        |       301 |           1147 |
|   4 | Odocoileus_virginianus |       262 |           1409 |
|   5 | Procyon_lotor          |       217 |           1626 |
|   6 | Sciurus_carolinensis   |       183 |           1809 |

We use the cutoff_incidence function to keet just the portion of host
that has more information about the system.

``` r
flavivirus_host <- cutoff_incidence(mammal_flavivirus_incidence)
```

|  id | mammal_species         | incidence | cummulativesum |
|----:|:-----------------------|----------:|---------------:|
|   1 | Camelus_dromedarius    |       510 |            510 |
|   2 | Sus_scrofa             |       336 |            846 |
|   3 | Alouatta_caraya        |       301 |           1147 |
|   4 | Odocoileus_virginianus |       262 |           1409 |
|   5 | Procyon_lotor          |       217 |           1626 |
|   6 | Sciurus_carolinensis   |       183 |           1809 |
|   7 | Tadarida_brasiliensis  |       180 |           1989 |
|   8 | Choloepus_hoffmanni    |       176 |           2165 |
|   9 | Arvicanthis_niloticus  |       168 |           2333 |
|  10 | Sciurus_niger          |       160 |           2493 |
|  11 | Cercopithecus_ascanius |       157 |           2650 |
|  12 | Crocidura_olivieri     |       133 |           2783 |
|  13 | Aethomys_kaiseri       |       126 |           2909 |
|  14 | Didelphis_virginiana   |       113 |           3022 |
|  15 | Lemur_catta            |        97 |           3119 |
|  16 | Canis_latrans          |        95 |           3214 |
|  17 | Dasypus_novemcinctus   |        93 |           3307 |
|  18 | Cervus_elaphus         |        80 |           3387 |
|  19 | Artibeus_jamaicensis   |        78 |           3465 |
|  20 | Rattus_norvegicus      |        74 |           3539 |

## Prepare data for modelling

Now we can prepare the dataset for modelling using a distance table. We
provide here a distance table calculated befare using mean distance from
one target host to the other host. We show here the full pipeline using
a count table of plasmodium reported for each host species. Then we use
the `accumulate_incidence` function and the `cuttoff_incidence` to keep
the host with the most information. Then we use `prep_incidence_data` to
prepare the data for modelling using the `birdsdistance` dataset we
provide and print the fifteen first rows

``` r
plasmodium_data <- birdsplasmodium %>%
     accumulate_incidence(group = species) %>%
     cutoff_incidence() %>%
     prep_incidence_data(distance  = birdsdistance) 
#> Joining, by = c("species", "geo_distance", "env_distance", "phylo_distance")
```

    #> [2022-09-06 10:42:01] Training of 'Ranger' started.
    #> Loading required package: ggplot2
    #> Loading required package: lattice
    #> [2022-09-06 10:42:37] Training of 'Ranger' completed.
    #> # A tibble: 1 x 3
    #>   .metric .estimator .estimate
    #>   <chr>   <chr>          <dbl>
    #> 1 roc_auc binary         0.726
