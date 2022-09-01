
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

| species                   | geo_distance | env_distance | phylo_distance | incidence   |
|:--------------------------|-------------:|-------------:|---------------:|:------------|
| Iridosornis reinhardti    |    0.1397235 |   -0.5630546 |     -0.2912661 | unknown     |
| Myzomela kuehni           |    0.3488246 |   -0.5975868 |     -0.2912661 | unknown     |
| Carpodacus puniceus       |   -0.1014722 |    0.4244484 |     -0.2912661 | unknown     |
| Dysithamnus stictothorax  |   -0.3269031 |   -0.5953258 |     -0.2912661 | unknown     |
| Neochmia phaeton          |    0.4387309 |   -0.2816839 |     -0.2912661 | unknown     |
| Dicrurus montanus         |    0.2600139 |   -0.1821658 |     -0.2912661 | unknown     |
| Pteridophora alberti      |    0.5307526 |    1.7042303 |     -0.2912661 | unknown     |
| Cardellina rubrifrons     |    0.2637090 |   -0.3498805 |     -0.2912661 | unknown     |
| Ficedula albicollis       |    0.0670664 |    0.0896840 |     -0.2912661 | susceptible |
| Columba elphinstonii      |   -0.2481989 |   -0.5263623 |      0.7790859 | unknown     |
| Acrocephalus arundinaceus |   -0.0214733 |    0.5824534 |     -0.2912661 | susceptible |
| Phaenicophaeus diardi     |    0.1397283 |    0.4139493 |      0.4670119 | unknown     |
| Falco fasciinucha         |    0.1112847 |   -0.5305628 |      0.3534907 | unknown     |
| Myzomela erythromelas     |    0.6220754 |    1.5898503 |     -0.2912661 | unknown     |
| Garrulax pectoralis       |    0.1013659 |   -0.3192215 |     -0.2912661 | unknown     |
