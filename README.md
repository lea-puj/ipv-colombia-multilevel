This repository contains the data and code used in a study on the individual and municipal determinants of intimate partner violence (IPV) in Colombia, based on the 2015 National Demographic and Health Survey (ENDS 2015), spatial autocorrelation analysis, and multilevel logistic models.

This repository is organized as follows:

```mermaid
graph TD
    R["ipv-colombia-multilevel"] --> D["data"]
    R --> S["scripts"]
    R --> O["output"]

    D --> DR["raw"]
    D --> DP["processed"]
    DR --> DR1["dhs"]
    DR --> DR2["shapefiles"]
    DR --> DR3["municipal"]

    S --> S1["01_data_preparation"]
    S --> S2["02_spatial_analysis"]
    S --> S3["03_multilevel_model"]

    S1 --> S11["01_build_individual_data.R"]
    S1 --> S12["02_merge_municipal_covariates.R"]
    S1 --> S13["03_build_municipal_aggregates.R"]
    S1 --> S14["04_descriptives.R"]

    S2 --> S21["01_spatial_data.R"]
    S2 --> S22["02_moran_global.R"]
    S2 --> S23["03_lisa.R"]

    S3 --> S31["01_multilevel_model.R"]
    S3 --> S32["02_robustness.R"]

    O --> O1["01_data_preparation"]
    O --> O2["02_spatial_analysis"]
    O --> O3["03_multilevel_model"]
```



