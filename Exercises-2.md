#### Saratoga House Prices

    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.1.8
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

    library(ggplot2)
    library(modelr)
    library(rsample)
    library(mosaic)

    ## Registered S3 method overwritten by 'mosaic':
    ##   method                           from   
    ##   fortify.SpatialPolygonsDataFrame ggplot2
    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.
    ## 
    ## Attaching package: 'mosaic'
    ## 
    ## The following object is masked from 'package:Matrix':
    ## 
    ##     mean
    ## 
    ## The following object is masked from 'package:modelr':
    ## 
    ##     resample
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     cross
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     stat
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
    ##     quantile, sd, t.test, var
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

    data(SaratogaHouses)

    glimpse(SaratogaHouses)

    ## Rows: 1,728
    ## Columns: 16
    ## $ price           <int> 132500, 181115, 109000, 155000, 86060, 120000, 153000,…
    ## $ lotSize         <dbl> 0.09, 0.92, 0.19, 0.41, 0.11, 0.68, 0.40, 1.21, 0.83, …
    ## $ age             <int> 42, 0, 133, 13, 0, 31, 33, 23, 36, 4, 123, 1, 13, 153,…
    ## $ landValue       <int> 50000, 22300, 7300, 18700, 15000, 14000, 23300, 14600,…
    ## $ livingArea      <int> 906, 1953, 1944, 1944, 840, 1152, 2752, 1662, 1632, 14…
    ## $ pctCollege      <int> 35, 51, 51, 51, 51, 22, 51, 35, 51, 44, 51, 51, 41, 57…
    ## $ bedrooms        <int> 2, 3, 4, 3, 2, 4, 4, 4, 3, 3, 7, 3, 2, 3, 3, 3, 3, 4, …
    ## $ fireplaces      <int> 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, …
    ## $ bathrooms       <dbl> 1.0, 2.5, 1.0, 1.5, 1.0, 1.0, 1.5, 1.5, 1.5, 1.5, 1.0,…
    ## $ rooms           <int> 5, 6, 8, 5, 3, 8, 8, 9, 8, 6, 12, 6, 4, 5, 8, 4, 7, 12…
    ## $ heating         <fct> electric, hot water/steam, hot water/steam, hot air, h…
    ## $ fuel            <fct> electric, gas, gas, gas, gas, gas, oil, oil, electric,…
    ## $ sewer           <fct> septic, septic, public/commercial, septic, public/comm…
    ## $ waterfront      <fct> No, No, No, No, No, No, No, No, No, No, No, No, No, No…
    ## $ newConstruction <fct> No, No, No, No, Yes, No, No, No, No, No, No, No, No, N…
    ## $ centralAir      <fct> No, No, No, No, Yes, No, No, No, No, No, No, No, No, N…

#### 

# Compare out-of-sample predictive performance

#### 

# Split into training and testing sets

    saratoga_split = initial_split(SaratogaHouses, prop = 0.8)
    saratoga_train = training(saratoga_split)
    saratoga_test = testing(saratoga_split)

# Fit to the training data

# Sometimes it’s easier to name the variables we want to leave out

# The command below yields exactly the same model.

# the dot (.) means “all variables not named”

# the minus (-) means “exclude this variable”

    lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_train)
    lm2 = lm(price ~ . - pctCollege - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
    lm3 = lm(price ~ (. - pctCollege - sewer - waterfront - landValue - newConstruction)^2, data=saratoga_train)

    coef(lm1) %>% round(0)

    ## (Intercept)     lotSize    bedrooms   bathrooms 
    ##       -1174       11842       18622       77804

    coef(lm2) %>% round(0)

    ##            (Intercept)                lotSize                    age 
    ##                  43999                   7524                     91 
    ##             livingArea               bedrooms             fireplaces 
    ##                     92                 -15286                   1856 
    ##              bathrooms                  rooms heatinghot water/steam 
    ##                  24525                   2628                  -7229 
    ##        heatingelectric           fuelelectric                fueloil 
    ##                   7612                 -25450                 -14981 
    ##           centralAirNo 
    ##                 -17785

    coef(lm3) %>% round(0)

    ##                         (Intercept)                             lotSize 
    ##                                8613                                5040 
    ##                                 age                          livingArea 
    ##                                -810                                 125 
    ##                            bedrooms                          fireplaces 
    ##                              -11983                               -7525 
    ##                           bathrooms                               rooms 
    ##                               13598                                2873 
    ##              heatinghot water/steam                     heatingelectric 
    ##                               46046                              -80726 
    ##                        fuelelectric                             fueloil 
    ##                               46729                              103738 
    ##                        centralAirNo                         lotSize:age 
    ##                               17216                                -211 
    ##                  lotSize:livingArea                    lotSize:bedrooms 
    ##                                  -6                               15060 
    ##                  lotSize:fireplaces                   lotSize:bathrooms 
    ##                               -7966                               -6772 
    ##                       lotSize:rooms      lotSize:heatinghot water/steam 
    ##                                -970                               13795 
    ##             lotSize:heatingelectric                lotSize:fuelelectric 
    ##                               75934                              -62259 
    ##                     lotSize:fueloil                lotSize:centralAirNo 
    ##                                8338                              -15919 
    ##                      age:livingArea                        age:bedrooms 
    ##                                   0                                 102 
    ##                      age:fireplaces                       age:bathrooms 
    ##                                   4                                 236 
    ##                           age:rooms          age:heatinghot water/steam 
    ##                                 -52                                 267 
    ##                 age:heatingelectric                    age:fuelelectric 
    ##                                1566                                -891 
    ##                         age:fueloil                    age:centralAirNo 
    ##                                -306                                 513 
    ##                 livingArea:bedrooms               livingArea:fireplaces 
    ##                                 -10                                  19 
    ##                livingArea:bathrooms                    livingArea:rooms 
    ##                                  -1                                   1 
    ##   livingArea:heatinghot water/steam          livingArea:heatingelectric 
    ##                                   8                                 -72 
    ##             livingArea:fuelelectric                  livingArea:fueloil 
    ##                                  73                                 -59 
    ##             livingArea:centralAirNo                 bedrooms:fireplaces 
    ##                                 -29                              -13121 
    ##                  bedrooms:bathrooms                      bedrooms:rooms 
    ##                                5588                                  84 
    ##     bedrooms:heatinghot water/steam            bedrooms:heatingelectric 
    ##                               -9751                               56705 
    ##               bedrooms:fuelelectric                    bedrooms:fueloil 
    ##                              -49536                               -6078 
    ##               bedrooms:centralAirNo                fireplaces:bathrooms 
    ##                                7980                                8438 
    ##                    fireplaces:rooms   fireplaces:heatinghot water/steam 
    ##                               -1256                               -6149 
    ##          fireplaces:heatingelectric             fireplaces:fuelelectric 
    ##                              111633                             -101232 
    ##                  fireplaces:fueloil             fireplaces:centralAirNo 
    ##                               12505                               17068 
    ##                     bathrooms:rooms    bathrooms:heatinghot water/steam 
    ##                                 -89                               -8654 
    ##           bathrooms:heatingelectric              bathrooms:fuelelectric 
    ##                                3283                              -17310 
    ##                   bathrooms:fueloil              bathrooms:centralAirNo 
    ##                                2664                              -10983 
    ##        rooms:heatinghot water/steam               rooms:heatingelectric 
    ##                               -3266                              -22020 
    ##                  rooms:fuelelectric                       rooms:fueloil 
    ##                               18034                                3720 
    ##                  rooms:centralAirNo heatinghot water/steam:fuelelectric 
    ##                                 253                                  NA 
    ##        heatingelectric:fuelelectric      heatinghot water/steam:fueloil 
    ##                               11126                              -25416 
    ##             heatingelectric:fueloil heatinghot water/steam:centralAirNo 
    ##                              -63228                               -2588 
    ##        heatingelectric:centralAirNo           fuelelectric:centralAirNo 
    ##                              101364                              -89147 
    ##                fueloil:centralAirNo 
    ##                              -27344

# Predictions out of sample

# Root mean squared error

    rmse(lm1, saratoga_test)

    ## [1] 72182.91

    rmse(lm2, saratoga_test)

    ## [1] 60731.49

    rmse(lm3, saratoga_test)

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## [1] 74750.25

# Can you hand-build a model that improves on all three?

# Remember feature engineering, and remember not just to rely on a single train/test split

    library(foreach)

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

# baseline medium model with 11 main effects

    lm_medium = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
            fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=saratoga_train)

### forward selection (best AIC was 30685 with pctCollege:centralAir)

    lm0 = lm(price ~ 1, data=saratoga_train)
    lm_forward = step(lm0, direction='forward',
        scope=~(lotSize + age + livingArea + pctCollege + bedrooms + 
                  fireplaces + bathrooms + rooms + heating + fuel + centralAir)^2)

    ## Start:  AIC=31803.7
    ## price ~ 1
    ## 
    ##              Df  Sum of Sq        RSS   AIC
    ## + livingArea  1 6.7742e+12 6.8469e+12 30855
    ## + bathrooms   1 4.7940e+12 8.8271e+12 31206
    ## + rooms       1 3.7058e+12 9.9154e+12 31367
    ## + bedrooms    1 2.1619e+12 1.1459e+13 31567
    ## + fireplaces  1 1.9303e+12 1.1691e+13 31595
    ## + centralAir  1 1.5016e+12 1.2120e+13 31644
    ## + fuel        2 1.0427e+12 1.2578e+13 31698
    ## + heating     2 7.9950e+11 1.2822e+13 31724
    ## + pctCollege  1 4.9425e+11 1.3127e+13 31755
    ## + age         1 4.8487e+11 1.3136e+13 31756
    ## + lotSize     1 2.9074e+11 1.3330e+13 31776
    ## <none>                     1.3621e+13 31804
    ## 
    ## Step:  AIC=30855.13
    ## price ~ livingArea
    ## 
    ##              Df  Sum of Sq        RSS   AIC
    ## + bathrooms   1 2.3832e+11 6.6086e+12 30808
    ## + centralAir  1 2.2038e+11 6.6266e+12 30812
    ## + bedrooms    1 9.2202e+10 6.7547e+12 30838
    ## + fuel        2 1.0168e+11 6.7453e+12 30838
    ## + heating     2 1.0078e+11 6.7462e+12 30839
    ## + age         1 4.4290e+10 6.8027e+12 30848
    ## + pctCollege  1 4.2881e+10 6.8041e+12 30848
    ## + fireplaces  1 3.1203e+10 6.8157e+12 30851
    ## <none>                     6.8469e+12 30855
    ## + lotSize     1 3.3307e+09 6.8436e+12 30857
    ## + rooms       1 6.9866e+07 6.8469e+12 30857
    ## 
    ## Step:  AIC=30808.17
    ## price ~ livingArea + bathrooms
    ## 
    ##                        Df  Sum of Sq        RSS   AIC
    ## + centralAir            1 1.4129e+11 6.4673e+12 30780
    ## + livingArea:bathrooms  1 1.4093e+11 6.4677e+12 30780
    ## + bedrooms              1 8.8295e+10 6.5203e+12 30792
    ## + heating               2 7.9429e+10 6.5292e+12 30796
    ## + fuel                  2 6.6647e+10 6.5420e+12 30798
    ## + pctCollege            1 3.6069e+10 6.5726e+12 30803
    ## + fireplaces            1 9.9581e+09 6.5987e+12 30808
    ## <none>                               6.6086e+12 30808
    ## + lotSize               1 6.8678e+09 6.6018e+12 30809
    ## + age                   1 1.6346e+09 6.6070e+12 30810
    ## + rooms                 1 2.9467e+08 6.6083e+12 30810
    ## 
    ## Step:  AIC=30780.3
    ## price ~ livingArea + bathrooms + centralAir
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + livingArea:centralAir  1 2.4347e+11 6.2239e+12 30729
    ## + bathrooms:centralAir   1 1.9645e+11 6.2709e+12 30740
    ## + livingArea:bathrooms   1 1.5004e+11 6.3173e+12 30750
    ## + bedrooms               1 6.3101e+10 6.4042e+12 30769
    ## + fuel                   2 4.1895e+10 6.4254e+12 30775
    ## + heating                2 3.2340e+10 6.4350e+12 30777
    ## + pctCollege             1 1.3920e+10 6.4534e+12 30779
    ## + lotSize                1 1.2930e+10 6.4544e+12 30780
    ## <none>                                6.4673e+12 30780
    ## + fireplaces             1 2.1496e+09 6.4652e+12 30782
    ## + rooms                  1 1.9422e+09 6.4654e+12 30782
    ## + age                    1 7.7617e+08 6.4666e+12 30782
    ## 
    ## Step:  AIC=30729.27
    ## price ~ livingArea + bathrooms + centralAir + livingArea:centralAir
    ## 
    ##                        Df  Sum of Sq        RSS   AIC
    ## + fuel                  2 6.0420e+10 6.1634e+12 30720
    ## + bedrooms              1 4.3307e+10 6.1806e+12 30722
    ## + livingArea:bathrooms  1 3.6352e+10 6.1875e+12 30723
    ## + heating               2 3.9442e+10 6.1844e+12 30725
    ## + bathrooms:centralAir  1 2.1346e+10 6.2025e+12 30727
    ## + pctCollege            1 2.1017e+10 6.2028e+12 30727
    ## <none>                               6.2239e+12 30729
    ## + lotSize               1 6.4334e+09 6.2174e+12 30730
    ## + fireplaces            1 4.3684e+09 6.2195e+12 30730
    ## + age                   1 1.5868e+09 6.2223e+12 30731
    ## + rooms                 1 1.4806e+09 6.2224e+12 30731
    ## 
    ## Step:  AIC=30719.79
    ## price ~ livingArea + bathrooms + centralAir + fuel + livingArea:centralAir
    ## 
    ##                        Df  Sum of Sq        RSS   AIC
    ## + bedrooms              1 5.9429e+10 6.1040e+12 30708
    ## + livingArea:fuel       2 6.4558e+10 6.0989e+12 30709
    ## + livingArea:bathrooms  1 3.4635e+10 6.1288e+12 30714
    ## + bathrooms:centralAir  1 2.1333e+10 6.1421e+12 30717
    ## + lotSize               1 1.4336e+10 6.1491e+12 30719
    ## + pctCollege            1 1.4187e+10 6.1493e+12 30719
    ## <none>                               6.1634e+12 30720
    ## + fuel:centralAir       2 1.7532e+10 6.1459e+12 30720
    ## + bathrooms:fuel        2 1.6094e+10 6.1474e+12 30720
    ## + fireplaces            1 3.1945e+09 6.1603e+12 30721
    ## + age                   1 4.1282e+08 6.1630e+12 30722
    ## + rooms                 1 4.0384e+08 6.1630e+12 30722
    ## + heating               2 2.0459e+09 6.1614e+12 30723
    ## 
    ## Step:  AIC=30708.4
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     livingArea:centralAir
    ## 
    ##                        Df  Sum of Sq        RSS   AIC
    ## + livingArea:fuel       2 6.3089e+10 6.0409e+12 30698
    ## + livingArea:bathrooms  1 2.5436e+10 6.0786e+12 30705
    ## + bedrooms:centralAir   1 2.0490e+10 6.0835e+12 30706
    ## + bathrooms:centralAir  1 2.0153e+10 6.0839e+12 30706
    ## + pctCollege            1 1.8023e+10 6.0860e+12 30706
    ## + lotSize               1 1.5974e+10 6.0880e+12 30707
    ## + rooms                 1 1.4184e+10 6.0898e+12 30707
    ## + fuel:centralAir       2 2.1023e+10 6.0830e+12 30708
    ## + livingArea:bedrooms   1 1.0968e+10 6.0931e+12 30708
    ## + bedrooms:fuel         2 1.9748e+10 6.0843e+12 30708
    ## <none>                               6.1040e+12 30708
    ## + bathrooms:fuel        2 1.6473e+10 6.0875e+12 30709
    ## + bedrooms:bathrooms    1 3.0570e+09 6.1010e+12 30710
    ## + age                   1 2.9746e+09 6.1010e+12 30710
    ## + fireplaces            1 2.2977e+09 6.1017e+12 30710
    ## + heating               2 5.0851e+08 6.1035e+12 30712
    ## 
    ## Step:  AIC=30698.04
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     livingArea:centralAir + livingArea:fuel
    ## 
    ##                        Df  Sum of Sq        RSS   AIC
    ## + fuel:centralAir       2 4.2857e+10 5.9981e+12 30692
    ## + lotSize               1 2.6622e+10 6.0143e+12 30694
    ## + bedrooms:centralAir   1 1.9079e+10 6.0218e+12 30696
    ## + bedrooms:fuel         2 2.7579e+10 6.0134e+12 30696
    ## + bathrooms:centralAir  1 1.7473e+10 6.0235e+12 30696
    ## + rooms                 1 1.5193e+10 6.0257e+12 30697
    ## + bathrooms:fuel        2 2.3711e+10 6.0172e+12 30697
    ## + pctCollege            1 1.4600e+10 6.0263e+12 30697
    ## + livingArea:bathrooms  1 1.2937e+10 6.0280e+12 30697
    ## + livingArea:bedrooms   1 1.0883e+10 6.0300e+12 30698
    ## + bedrooms:bathrooms    1 8.8583e+09 6.0321e+12 30698
    ## <none>                               6.0409e+12 30698
    ## + age                   1 2.9400e+09 6.0380e+12 30699
    ## + fireplaces            1 1.0632e+09 6.0399e+12 30700
    ## + heating               2 9.0792e+08 6.0400e+12 30702
    ## 
    ## Step:  AIC=30692.2
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     livingArea:centralAir + livingArea:fuel + centralAir:fuel
    ## 
    ##                        Df  Sum of Sq        RSS   AIC
    ## + lotSize               1 2.5635e+10 5.9724e+12 30688
    ## + bedrooms:centralAir   1 2.4961e+10 5.9731e+12 30688
    ## + bathrooms:centralAir  1 1.9911e+10 5.9782e+12 30690
    ## + bedrooms:fuel         2 2.6630e+10 5.9714e+12 30690
    ## + pctCollege            1 1.5500e+10 5.9826e+12 30691
    ## + rooms                 1 1.4701e+10 5.9834e+12 30691
    ## + livingArea:bathrooms  1 1.3150e+10 5.9849e+12 30691
    ## + livingArea:bedrooms   1 1.1066e+10 5.9870e+12 30692
    ## + bedrooms:bathrooms    1 9.4878e+09 5.9886e+12 30692
    ## <none>                               5.9981e+12 30692
    ## + bathrooms:fuel        2 1.4568e+10 5.9835e+12 30693
    ## + age                   1 4.2203e+09 5.9939e+12 30693
    ## + fireplaces            1 1.4036e+09 5.9967e+12 30694
    ## + heating               2 6.1365e+07 5.9980e+12 30696
    ## 
    ## Step:  AIC=30688.28
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     lotSize + livingArea:centralAir + livingArea:fuel + centralAir:fuel
    ## 
    ##                        Df  Sum of Sq        RSS   AIC
    ## + bedrooms:centralAir   1 2.3304e+10 5.9491e+12 30685
    ## + bathrooms:centralAir  1 2.2692e+10 5.9497e+12 30685
    ## + bedrooms:fuel         2 2.7564e+10 5.9449e+12 30686
    ## + pctCollege            1 1.6409e+10 5.9560e+12 30687
    ## + rooms                 1 1.4932e+10 5.9575e+12 30687
    ## + livingArea:bathrooms  1 1.2741e+10 5.9597e+12 30687
    ## + livingArea:bedrooms   1 1.0477e+10 5.9620e+12 30688
    ## + bedrooms:bathrooms    1 9.8955e+09 5.9625e+12 30688
    ## + lotSize:livingArea    1 8.9200e+09 5.9635e+12 30688
    ## <none>                               5.9724e+12 30688
    ## + lotSize:fuel          2 1.5179e+10 5.9573e+12 30689
    ## + age                   1 5.3633e+09 5.9671e+12 30689
    ## + bathrooms:fuel        2 1.2948e+10 5.9595e+12 30689
    ## + lotSize:bathrooms     1 3.4152e+09 5.9690e+12 30690
    ## + lotSize:bedrooms      1 1.5199e+09 5.9709e+12 30690
    ## + lotSize:centralAir    1 1.1349e+09 5.9713e+12 30690
    ## + fireplaces            1 5.6434e+08 5.9719e+12 30690
    ## + heating               2 3.7254e+08 5.9721e+12 30692
    ## 
    ## Step:  AIC=30684.88
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     lotSize + livingArea:centralAir + livingArea:fuel + centralAir:fuel + 
    ##     centralAir:bedrooms
    ## 
    ##                        Df  Sum of Sq        RSS   AIC
    ## + bathrooms:centralAir  1 2.2505e+10 5.9266e+12 30682
    ## + pctCollege            1 1.7060e+10 5.9321e+12 30683
    ## + rooms                 1 1.4154e+10 5.9350e+12 30684
    ## + lotSize:livingArea    1 1.0720e+10 5.9384e+12 30684
    ## + livingArea:bathrooms  1 9.3433e+09 5.9398e+12 30685
    ## <none>                               5.9491e+12 30685
    ## + livingArea:bedrooms   1 7.5680e+09 5.9416e+12 30685
    ## + bedrooms:fuel         2 1.6097e+10 5.9330e+12 30685
    ## + lotSize:fuel          2 1.5220e+10 5.9339e+12 30685
    ## + age                   1 4.7665e+09 5.9444e+12 30686
    ## + bathrooms:fuel        2 1.3332e+10 5.9358e+12 30686
    ## + lotSize:bathrooms     1 3.6487e+09 5.9455e+12 30686
    ## + bedrooms:bathrooms    1 3.4791e+09 5.9457e+12 30686
    ## + lotSize:centralAir    1 1.1385e+09 5.9480e+12 30687
    ## + lotSize:bedrooms      1 7.9289e+08 5.9483e+12 30687
    ## + fireplaces            1 4.1417e+08 5.9487e+12 30687
    ## + heating               2 7.3168e+08 5.9484e+12 30689
    ## 
    ## Step:  AIC=30681.64
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     lotSize + livingArea:centralAir + livingArea:fuel + centralAir:fuel + 
    ##     centralAir:bedrooms + bathrooms:centralAir
    ## 
    ##                        Df  Sum of Sq        RSS   AIC
    ## + pctCollege            1 1.8301e+10 5.9083e+12 30679
    ## + rooms                 1 1.4911e+10 5.9117e+12 30680
    ## <none>                               5.9266e+12 30682
    ## + lotSize:livingArea    1 8.5672e+09 5.9181e+12 30682
    ## + livingArea:bathrooms  1 7.6211e+09 5.9190e+12 30682
    ## + bedrooms:fuel         2 1.6009e+10 5.9106e+12 30682
    ## + lotSize:fuel          2 1.5171e+10 5.9115e+12 30682
    ## + livingArea:bedrooms   1 5.8426e+09 5.9208e+12 30682
    ## + bathrooms:fuel        2 1.2021e+10 5.9146e+12 30683
    ## + bedrooms:bathrooms    1 3.1663e+09 5.9235e+12 30683
    ## + lotSize:bathrooms     1 2.5017e+09 5.9241e+12 30683
    ## + age                   1 2.1834e+09 5.9244e+12 30683
    ## + lotSize:bedrooms      1 1.0916e+09 5.9255e+12 30683
    ## + fireplaces            1 9.1214e+08 5.9257e+12 30683
    ## + lotSize:centralAir    1 7.3741e+08 5.9259e+12 30684
    ## + heating               2 1.3210e+09 5.9253e+12 30685
    ## 
    ## Step:  AIC=30679.37
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     lotSize + pctCollege + livingArea:centralAir + livingArea:fuel + 
    ##     centralAir:fuel + centralAir:bedrooms + bathrooms:centralAir
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + lotSize:pctCollege     1 3.3278e+10 5.8750e+12 30674
    ## + pctCollege:fuel        2 2.4723e+10 5.8836e+12 30678
    ## + rooms                  1 1.4520e+10 5.8938e+12 30678
    ## + pctCollege:bathrooms   1 9.9780e+09 5.8983e+12 30679
    ## + lotSize:livingArea     1 9.6595e+09 5.8987e+12 30679
    ## + livingArea:bathrooms   1 9.4347e+09 5.8989e+12 30679
    ## + bedrooms:fuel          2 1.7365e+10 5.8910e+12 30679
    ## <none>                                5.9083e+12 30679
    ## + lotSize:fuel           2 1.7051e+10 5.8913e+12 30679
    ## + pctCollege:centralAir  1 7.8070e+09 5.9005e+12 30680
    ## + livingArea:bedrooms    1 5.6774e+09 5.9026e+12 30680
    ## + bathrooms:fuel         2 1.2921e+10 5.8954e+12 30680
    ## + bedrooms:bathrooms     1 3.6776e+09 5.9046e+12 30681
    ## + lotSize:bathrooms      1 2.8939e+09 5.9054e+12 30681
    ## + livingArea:pctCollege  1 2.7284e+09 5.9056e+12 30681
    ## + age                    1 1.3658e+09 5.9070e+12 30681
    ## + lotSize:bedrooms       1 1.1513e+09 5.9072e+12 30681
    ## + lotSize:centralAir     1 8.4085e+08 5.9075e+12 30681
    ## + fireplaces             1 1.0156e+08 5.9082e+12 30681
    ## + pctCollege:bedrooms    1 8.8270e+07 5.9082e+12 30681
    ## + heating                2 2.0231e+09 5.9063e+12 30683
    ## 
    ## Step:  AIC=30673.56
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     lotSize + pctCollege + livingArea:centralAir + livingArea:fuel + 
    ##     centralAir:fuel + centralAir:bedrooms + bathrooms:centralAir + 
    ##     lotSize:pctCollege
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + pctCollege:fuel        2 2.2600e+10 5.8524e+12 30672
    ## + rooms                  1 1.2134e+10 5.8629e+12 30673
    ## + livingArea:bathrooms   1 9.1953e+09 5.8659e+12 30673
    ## + pctCollege:centralAir  1 9.1405e+09 5.8659e+12 30673
    ## <none>                                5.8750e+12 30674
    ## + bedrooms:fuel          2 1.6927e+10 5.8581e+12 30674
    ## + pctCollege:bathrooms   1 5.8421e+09 5.8692e+12 30674
    ## + lotSize:bedrooms       1 5.4559e+09 5.8696e+12 30674
    ## + livingArea:bedrooms    1 4.7404e+09 5.8703e+12 30674
    ## + bedrooms:bathrooms     1 2.8967e+09 5.8722e+12 30675
    ## + age                    1 2.1109e+09 5.8729e+12 30675
    ## + lotSize:centralAir     1 9.8936e+08 5.8741e+12 30675
    ## + bathrooms:fuel         2 9.4774e+09 5.8656e+12 30675
    ## + livingArea:pctCollege  1 5.6275e+08 5.8745e+12 30675
    ## + lotSize:livingArea     1 4.9888e+08 5.8745e+12 30675
    ## + lotSize:bathrooms      1 3.0487e+08 5.8747e+12 30676
    ## + fireplaces             1 2.5134e+08 5.8748e+12 30676
    ## + pctCollege:bedrooms    1 1.5083e+08 5.8749e+12 30676
    ## + lotSize:fuel           2 3.0880e+09 5.8720e+12 30677
    ## + heating                2 1.8772e+09 5.8732e+12 30677
    ## 
    ## Step:  AIC=30672.23
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     lotSize + pctCollege + livingArea:centralAir + livingArea:fuel + 
    ##     centralAir:fuel + centralAir:bedrooms + bathrooms:centralAir + 
    ##     lotSize:pctCollege + fuel:pctCollege
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + pctCollege:centralAir  1 1.2941e+10 5.8395e+12 30671
    ## + rooms                  1 1.2621e+10 5.8398e+12 30671
    ## + bedrooms:fuel          2 1.7904e+10 5.8345e+12 30672
    ## + pctCollege:bathrooms   1 9.1232e+09 5.8433e+12 30672
    ## + livingArea:bathrooms   1 8.7605e+09 5.8437e+12 30672
    ## <none>                                5.8524e+12 30672
    ## + lotSize:bedrooms       1 6.7990e+09 5.8456e+12 30673
    ## + livingArea:bedrooms    1 5.4180e+09 5.8470e+12 30673
    ## + bathrooms:fuel         2 1.1848e+10 5.8406e+12 30673
    ## + bedrooms:bathrooms     1 3.1009e+09 5.8493e+12 30674
    ## + livingArea:pctCollege  1 2.3514e+09 5.8501e+12 30674
    ## + age                    1 1.4837e+09 5.8510e+12 30674
    ## + lotSize:centralAir     1 1.4462e+09 5.8510e+12 30674
    ## + lotSize:bathrooms      1 8.2449e+08 5.8516e+12 30674
    ## + pctCollege:bedrooms    1 2.3340e+08 5.8522e+12 30674
    ## + lotSize:livingArea     1 1.8844e+08 5.8523e+12 30674
    ## + fireplaces             1 9.2606e+07 5.8524e+12 30674
    ## + heating                2 2.7852e+09 5.8497e+12 30676
    ## + lotSize:fuel           2 2.1139e+09 5.8503e+12 30676
    ## 
    ## Step:  AIC=30671.17
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     lotSize + pctCollege + livingArea:centralAir + livingArea:fuel + 
    ##     centralAir:fuel + centralAir:bedrooms + bathrooms:centralAir + 
    ##     lotSize:pctCollege + fuel:pctCollege + centralAir:pctCollege
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + rooms                  1 1.2004e+10 5.8275e+12 30670
    ## + bedrooms:fuel          2 1.8145e+10 5.8214e+12 30671
    ## + livingArea:bathrooms   1 9.2963e+09 5.8302e+12 30671
    ## <none>                                5.8395e+12 30671
    ## + lotSize:bedrooms       1 7.0599e+09 5.8324e+12 30672
    ## + livingArea:bedrooms    1 5.0940e+09 5.8344e+12 30672
    ## + pctCollege:bathrooms   1 4.5038e+09 5.8350e+12 30672
    ## + bedrooms:bathrooms     1 3.0111e+09 5.8365e+12 30673
    ## + bathrooms:fuel         2 1.1354e+10 5.8282e+12 30673
    ## + age                    1 1.7194e+09 5.8378e+12 30673
    ## + lotSize:centralAir     1 1.3191e+09 5.8382e+12 30673
    ## + livingArea:pctCollege  1 7.2716e+08 5.8388e+12 30673
    ## + lotSize:bathrooms      1 7.0187e+08 5.8388e+12 30673
    ## + lotSize:livingArea     1 1.7173e+08 5.8393e+12 30673
    ## + pctCollege:bedrooms    1 1.3733e+08 5.8394e+12 30673
    ## + fireplaces             1 6.0006e+07 5.8394e+12 30673
    ## + heating                2 4.7179e+09 5.8348e+12 30674
    ## + lotSize:fuel           2 2.4848e+09 5.8370e+12 30675
    ## 
    ## Step:  AIC=30670.33
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     lotSize + pctCollege + rooms + livingArea:centralAir + livingArea:fuel + 
    ##     centralAir:fuel + centralAir:bedrooms + bathrooms:centralAir + 
    ##     lotSize:pctCollege + fuel:pctCollege + centralAir:pctCollege
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + bedrooms:fuel          2 1.8232e+10 5.8093e+12 30670
    ## <none>                                5.8275e+12 30670
    ## + livingArea:bathrooms   1 7.6428e+09 5.8199e+12 30671
    ## + lotSize:bedrooms       1 7.6094e+09 5.8199e+12 30671
    ## + pctCollege:bathrooms   1 5.2741e+09 5.8222e+12 30671
    ## + livingArea:bedrooms    1 5.2683e+09 5.8222e+12 30671
    ## + bedrooms:rooms         1 4.4976e+09 5.8230e+12 30671
    ## + bedrooms:bathrooms     1 3.6427e+09 5.8239e+12 30672
    ## + bathrooms:fuel         2 1.0537e+10 5.8170e+12 30672
    ## + age                    1 1.6873e+09 5.8258e+12 30672
    ## + lotSize:centralAir     1 1.3659e+09 5.8261e+12 30672
    ## + livingArea:pctCollege  1 7.9473e+08 5.8267e+12 30672
    ## + rooms:centralAir       1 7.7001e+08 5.8267e+12 30672
    ## + lotSize:bathrooms      1 7.0992e+08 5.8268e+12 30672
    ## + lotSize:rooms          1 7.0606e+08 5.8268e+12 30672
    ## + bathrooms:rooms        1 6.7780e+08 5.8268e+12 30672
    ## + livingArea:rooms       1 6.2861e+08 5.8269e+12 30672
    ## + pctCollege:rooms       1 6.1526e+08 5.8269e+12 30672
    ## + lotSize:livingArea     1 1.7138e+08 5.8273e+12 30672
    ## + fireplaces             1 1.5885e+08 5.8273e+12 30672
    ## + pctCollege:bedrooms    1 4.8759e+07 5.8275e+12 30672
    ## + heating                2 4.0383e+09 5.8235e+12 30673
    ## + rooms:fuel             2 3.6085e+09 5.8239e+12 30674
    ## + lotSize:fuel           2 2.5149e+09 5.8250e+12 30674
    ## 
    ## Step:  AIC=30670
    ## price ~ livingArea + bathrooms + centralAir + fuel + bedrooms + 
    ##     lotSize + pctCollege + rooms + livingArea:centralAir + livingArea:fuel + 
    ##     centralAir:fuel + centralAir:bedrooms + bathrooms:centralAir + 
    ##     lotSize:pctCollege + fuel:pctCollege + centralAir:pctCollege + 
    ##     fuel:bedrooms
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## <none>                                5.8093e+12 30670
    ## + livingArea:bathrooms   1 7.4256e+09 5.8018e+12 30670
    ## + lotSize:bedrooms       1 6.6427e+09 5.8026e+12 30670
    ## + pctCollege:bathrooms   1 4.5235e+09 5.8047e+12 30671
    ## + livingArea:bedrooms    1 3.1358e+09 5.8061e+12 30671
    ## + bathrooms:fuel         2 1.0903e+10 5.7984e+12 30671
    ## + age                    1 2.0559e+09 5.8072e+12 30672
    ## + bedrooms:rooms         1 1.9504e+09 5.8073e+12 30672
    ## + lotSize:centralAir     1 1.6663e+09 5.8076e+12 30672
    ## + bathrooms:rooms        1 1.1526e+09 5.8081e+12 30672
    ## + bedrooms:bathrooms     1 1.0796e+09 5.8082e+12 30672
    ## + livingArea:rooms       1 8.9703e+08 5.8084e+12 30672
    ## + lotSize:bathrooms      1 8.4305e+08 5.8084e+12 30672
    ## + rooms:centralAir       1 8.0700e+08 5.8085e+12 30672
    ## + livingArea:pctCollege  1 6.7696e+08 5.8086e+12 30672
    ## + lotSize:rooms          1 5.4390e+08 5.8087e+12 30672
    ## + fireplaces             1 3.1174e+08 5.8090e+12 30672
    ## + pctCollege:rooms       1 3.0617e+08 5.8090e+12 30672
    ## + pctCollege:bedrooms    1 2.9349e+08 5.8090e+12 30672
    ## + lotSize:livingArea     1 1.6281e+08 5.8091e+12 30672
    ## + rooms:fuel             2 4.1037e+09 5.8052e+12 30673
    ## + heating                2 2.8197e+09 5.8065e+12 30673
    ## + lotSize:fuel           2 2.2495e+09 5.8070e+12 30674

# backward selection?

    lm_big = lm(price ~ (lotSize + age + livingArea + pctCollege + bedrooms + 
            fireplaces + bathrooms + rooms + heating + fuel + centralAir +
            landValue + sewer + newConstruction + waterfront)^2, data= saratoga_train)
    drop1(lm_big)

    ## Single term deletions
    ## 
    ## Model:
    ## price ~ (lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     landValue + sewer + newConstruction + waterfront)^2
    ##                            Df  Sum of Sq        RSS   AIC
    ## <none>                                   3.8493e+12 30357
    ## lotSize:age                 1 2.0997e+09 3.8514e+12 30356
    ## lotSize:livingArea          1 1.0607e+09 3.8503e+12 30356
    ## lotSize:pctCollege          1 1.2604e+08 3.8494e+12 30355
    ## lotSize:bedrooms            1 1.5046e+10 3.8643e+12 30361
    ## lotSize:fireplaces          1 4.8215e+08 3.8497e+12 30355
    ## lotSize:bathrooms           1 4.0549e+09 3.8533e+12 30357
    ## lotSize:rooms               1 1.6890e+07 3.8493e+12 30355
    ## lotSize:heating             2 4.6248e+09 3.8539e+12 30355
    ## lotSize:fuel                2 2.9314e+09 3.8522e+12 30354
    ## lotSize:centralAir          1 1.0783e+09 3.8503e+12 30356
    ## lotSize:landValue           1 1.7187e+09 3.8510e+12 30356
    ## lotSize:sewer               1 6.3187e+08 3.8499e+12 30355
    ## lotSize:newConstruction     1 5.3463e+09 3.8546e+12 30357
    ## lotSize:waterfront          0 0.0000e+00 3.8493e+12 30357
    ## age:livingArea              1 4.0364e+09 3.8533e+12 30357
    ## age:pctCollege              1 2.8975e+10 3.8782e+12 30366
    ## age:bedrooms                1 3.4034e+09 3.8527e+12 30356
    ## age:fireplaces              1 2.5110e+09 3.8518e+12 30356
    ## age:bathrooms               1 2.7600e+09 3.8520e+12 30356
    ## age:rooms                   1 1.0149e+10 3.8594e+12 30359
    ## age:heating                 2 2.2485e+09 3.8515e+12 30354
    ## age:fuel                    2 2.5535e+09 3.8518e+12 30354
    ## age:centralAir              1 4.3255e+10 3.8925e+12 30371
    ## age:landValue               1 5.3796e+09 3.8546e+12 30357
    ## age:sewer                   1 8.1306e+09 3.8574e+12 30358
    ## age:newConstruction         1 5.6100e+09 3.8549e+12 30357
    ## age:waterfront              0 1.0000e+00 3.8493e+12 30357
    ## livingArea:pctCollege       1 3.5412e+09 3.8528e+12 30357
    ## livingArea:bedrooms         1 5.0354e+08 3.8498e+12 30355
    ## livingArea:fireplaces       1 1.3538e+10 3.8628e+12 30360
    ## livingArea:bathrooms        1 1.0625e+08 3.8494e+12 30355
    ## livingArea:rooms            1 2.7533e+09 3.8520e+12 30356
    ## livingArea:heating          2 8.0583e+08 3.8501e+12 30354
    ## livingArea:fuel             2 1.5369e+10 3.8646e+12 30359
    ## livingArea:centralAir       1 5.1036e+09 3.8544e+12 30357
    ## livingArea:landValue        1 1.4251e+10 3.8635e+12 30360
    ## livingArea:sewer            1 8.4725e+08 3.8501e+12 30356
    ## livingArea:newConstruction  1 7.9569e+07 3.8493e+12 30355
    ## livingArea:waterfront       0 0.0000e+00 3.8493e+12 30357
    ## pctCollege:bedrooms         1 1.7343e+09 3.8510e+12 30356
    ## pctCollege:fireplaces       1 2.2874e+10 3.8721e+12 30363
    ## pctCollege:bathrooms        1 1.7927e+09 3.8511e+12 30356
    ## pctCollege:rooms            1 2.8276e+08 3.8495e+12 30355
    ## pctCollege:heating          2 3.1600e+09 3.8524e+12 30354
    ## pctCollege:fuel             2 3.0094e+09 3.8523e+12 30354
    ## pctCollege:centralAir       1 1.5229e+07 3.8493e+12 30355
    ## pctCollege:landValue        1 1.7945e+10 3.8672e+12 30362
    ## pctCollege:sewer            1 5.2558e+08 3.8498e+12 30355
    ## pctCollege:newConstruction  1 7.1888e+08 3.8500e+12 30356
    ## pctCollege:waterfront       0 0.0000e+00 3.8493e+12 30357
    ## bedrooms:fireplaces         1 1.7423e+10 3.8667e+12 30361
    ## bedrooms:bathrooms          1 1.3762e+09 3.8506e+12 30356
    ## bedrooms:rooms              1 5.8623e+09 3.8551e+12 30357
    ## bedrooms:heating            2 5.1770e+09 3.8544e+12 30355
    ## bedrooms:fuel               2 1.1574e+10 3.8608e+12 30357
    ## bedrooms:centralAir         1 5.1306e+08 3.8498e+12 30355
    ## bedrooms:landValue          1 1.0187e+07 3.8493e+12 30355
    ## bedrooms:sewer              1 1.6415e+10 3.8657e+12 30361
    ## bedrooms:newConstruction    1 4.4522e+09 3.8537e+12 30357
    ## bedrooms:waterfront         0 0.0000e+00 3.8493e+12 30357
    ## fireplaces:bathrooms        1 7.1482e+08 3.8500e+12 30356
    ## fireplaces:rooms            1 4.8305e+08 3.8497e+12 30355
    ## fireplaces:heating          2 3.6248e+09 3.8529e+12 30355
    ## fireplaces:fuel             2 5.9863e+09 3.8552e+12 30355
    ## fireplaces:centralAir       1 1.3878e+06 3.8493e+12 30355
    ## fireplaces:landValue        1 2.5367e+10 3.8746e+12 30364
    ## fireplaces:sewer            1 9.3400e+09 3.8586e+12 30359
    ## fireplaces:newConstruction  1 4.7396e+08 3.8497e+12 30355
    ## fireplaces:waterfront       0 0.0000e+00 3.8493e+12 30357
    ## bathrooms:rooms             1 2.8901e+08 3.8496e+12 30355
    ## bathrooms:heating           2 1.2523e+10 3.8618e+12 30358
    ## bathrooms:fuel              2 4.0514e+09 3.8533e+12 30355
    ## bathrooms:centralAir        1 1.8030e+09 3.8511e+12 30356
    ## bathrooms:landValue         1 2.3087e+10 3.8723e+12 30364
    ## bathrooms:sewer             1 4.4008e+09 3.8537e+12 30357
    ## bathrooms:newConstruction   1 8.0507e+09 3.8573e+12 30358
    ## bathrooms:waterfront        0 0.0000e+00 3.8493e+12 30357
    ## rooms:heating               2 5.5360e+09 3.8548e+12 30355
    ## rooms:fuel                  2 4.7613e+09 3.8540e+12 30355
    ## rooms:centralAir            1 2.4334e+07 3.8493e+12 30355
    ## rooms:landValue             1 1.4881e+08 3.8494e+12 30355
    ## rooms:sewer                 1 2.0224e+08 3.8495e+12 30355
    ## rooms:newConstruction       1 4.5823e+08 3.8497e+12 30355
    ## rooms:waterfront            0 0.0000e+00 3.8493e+12 30357
    ## heating:fuel                3 7.7805e+09 3.8570e+12 30354
    ## heating:centralAir          2 8.4966e+09 3.8578e+12 30356
    ## heating:landValue           2 8.1085e+09 3.8574e+12 30356
    ## heating:sewer               2 2.7452e+10 3.8767e+12 30363
    ## heating:newConstruction     0 0.0000e+00 3.8493e+12 30357
    ## heating:waterfront          0 0.0000e+00 3.8493e+12 30357
    ## fuel:centralAir             2 1.3462e+10 3.8627e+12 30358
    ## fuel:landValue              2 1.6214e+10 3.8655e+12 30359
    ## fuel:sewer                  2 7.8694e+09 3.8571e+12 30356
    ## fuel:newConstruction        0 0.0000e+00 3.8493e+12 30357
    ## fuel:waterfront             0 0.0000e+00 3.8493e+12 30357
    ## centralAir:landValue        1 3.1417e+08 3.8496e+12 30355
    ## centralAir:sewer            1 1.1403e+10 3.8607e+12 30359
    ## centralAir:newConstruction  1 1.1492e+10 3.8608e+12 30359
    ## centralAir:waterfront       0 0.0000e+00 3.8493e+12 30357
    ## landValue:sewer             1 3.4575e+08 3.8496e+12 30355
    ## landValue:newConstruction   1 1.0096e+10 3.8594e+12 30359
    ## landValue:waterfront        0 0.0000e+00 3.8493e+12 30357
    ## sewer:newConstruction       1 2.8184e+07 3.8493e+12 30355
    ## sewer:waterfront            0 0.0000e+00 3.8493e+12 30357
    ## newConstruction:waterfront  0 0.0000e+00 3.8493e+12 30357

# stepwise selection

# note that we start with a reasonable guess (MinAIC=30643)

    lm_step = step(lm_medium, 
                scope=~(.)^2)

    ## Start:  AIC=30758.11
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + livingArea:centralAir  1 2.2331e+11 6.0496e+12 30710
    ## + bathrooms:centralAir   1 1.9071e+11 6.0822e+12 30717
    ## + livingArea:bathrooms   1 1.1798e+11 6.1549e+12 30734
    ## + rooms:centralAir       1 1.1614e+11 6.1568e+12 30734
    ## + livingArea:fuel        2 1.2408e+11 6.1488e+12 30735
    ## + livingArea:fireplaces  1 9.7086e+10 6.1758e+12 30739
    ## + fireplaces:bathrooms   1 8.0531e+10 6.1924e+12 30742
    ## + bathrooms:heating      2 8.8972e+10 6.1839e+12 30742
    ## + age:pctCollege         1 7.9830e+10 6.1931e+12 30742
    ## + livingArea:heating     2 8.4472e+10 6.1884e+12 30743
    ## + rooms:heating          2 7.0709e+10 6.2022e+12 30746
    ## + lotSize:age            1 6.1341e+10 6.2115e+12 30747
    ## + age:livingArea         1 5.6653e+10 6.2162e+12 30748
    ## + age:fuel               2 6.3221e+10 6.2097e+12 30748
    ## + age:heating            2 5.7405e+10 6.2155e+12 30749
    ## + bathrooms:rooms        1 4.5909e+10 6.2270e+12 30750
    ## + age:centralAir         1 4.2729e+10 6.2302e+12 30751
    ## + pctCollege:fireplaces  1 4.2292e+10 6.2306e+12 30751
    ## + pctCollege:heating     2 4.8293e+10 6.2246e+12 30751
    ## + bedrooms:centralAir    1 3.9019e+10 6.2339e+12 30752
    ## + fireplaces:rooms       1 3.6520e+10 6.2364e+12 30752
    ## + age:rooms              1 3.5570e+10 6.2373e+12 30752
    ## + rooms:fuel             2 4.4555e+10 6.2283e+12 30752
    ## + livingArea:rooms       1 3.3796e+10 6.2391e+12 30753
    ## + bathrooms:fuel         2 3.8190e+10 6.2347e+12 30754
    ## + pctCollege:fuel        2 3.7441e+10 6.2354e+12 30754
    ## + fireplaces:heating     2 3.4022e+10 6.2389e+12 30755
    ## + age:bathrooms          1 2.4860e+10 6.2480e+12 30755
    ## + bedrooms:heating       2 3.2900e+10 6.2400e+12 30755
    ## + fuel:centralAir        2 3.1268e+10 6.2416e+12 30755
    ## + fireplaces:centralAir  1 2.0330e+10 6.2526e+12 30756
    ## + heating:centralAir     2 2.8485e+10 6.2444e+12 30756
    ## - fireplaces             1 2.1199e+08 6.2731e+12 30756
    ## + lotSize:livingArea     1 1.7398e+10 6.2555e+12 30756
    ## + lotSize:pctCollege     1 1.7186e+10 6.2557e+12 30756
    ## - heating                2 1.0311e+10 6.2832e+12 30756
    ## + lotSize:heating        2 2.5771e+10 6.2471e+12 30756
    ## + livingArea:pctCollege  1 1.6480e+10 6.2564e+12 30757
    ## + lotSize:centralAir     1 1.5397e+10 6.2575e+12 30757
    ## + bedrooms:fuel          2 2.2116e+10 6.2508e+12 30757
    ## + lotSize:fuel           2 2.1895e+10 6.2510e+12 30757
    ## - age                    1 5.7668e+09 6.2787e+12 30757
    ## + pctCollege:rooms       1 9.2921e+09 6.2636e+12 30758
    ## + pctCollege:centralAir  1 9.1304e+09 6.2638e+12 30758
    ## <none>                                6.2729e+12 30758
    ## + pctCollege:bedrooms    1 6.4654e+09 6.2664e+12 30759
    ## + lotSize:fireplaces     1 5.6329e+09 6.2673e+12 30759
    ## + lotSize:rooms          1 5.1970e+09 6.2677e+12 30759
    ## - pctCollege             1 1.3298e+10 6.2862e+12 30759
    ## + age:bedrooms           1 4.5963e+09 6.2683e+12 30759
    ## + lotSize:bathrooms      1 3.7240e+09 6.2692e+12 30759
    ## + bedrooms:rooms         1 1.9970e+09 6.2709e+12 30760
    ## + lotSize:bedrooms       1 1.0187e+09 6.2719e+12 30760
    ## + livingArea:bedrooms    1 7.5790e+08 6.2721e+12 30760
    ## + pctCollege:bathrooms   1 5.0135e+08 6.2724e+12 30760
    ## + bedrooms:bathrooms     1 1.8655e+08 6.2727e+12 30760
    ## + age:fireplaces         1 1.2686e+08 6.2728e+12 30760
    ## + bedrooms:fireplaces    1 9.9039e+07 6.2728e+12 30760
    ## - rooms                  1 1.9156e+10 6.2920e+12 30760
    ## - fuel                   2 3.0784e+10 6.3037e+12 30761
    ## + heating:fuel           3 1.1015e+10 6.2619e+12 30762
    ## + fireplaces:fuel        2 7.1891e+08 6.2722e+12 30762
    ## - lotSize                1 2.8689e+10 6.3016e+12 30762
    ## - centralAir             1 6.1142e+10 6.3340e+12 30770
    ## - bedrooms               1 1.0110e+11 6.3740e+12 30778
    ## - bathrooms              1 1.5073e+11 6.4236e+12 30789
    ## - livingArea             1 1.1780e+12 7.4509e+12 30994
    ## 
    ## Step:  AIC=30710.02
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + age:pctCollege         1 9.5834e+10 5.9537e+12 30690
    ## + livingArea:fuel        2 7.1622e+10 5.9780e+12 30698
    ## + pctCollege:fireplaces  1 6.2452e+10 5.9871e+12 30698
    ## + age:fuel               2 5.6624e+10 5.9930e+12 30701
    ## + age:heating            2 5.0638e+10 5.9989e+12 30702
    ## + lotSize:age            1 3.3557e+10 6.0160e+12 30704
    ## + lotSize:livingArea     1 3.2714e+10 6.0169e+12 30705
    ## + bathrooms:heating      2 4.0290e+10 6.0093e+12 30705
    ## + lotSize:pctCollege     1 2.8812e+10 6.0208e+12 30705
    ## + pctCollege:heating     2 3.6129e+10 6.0135e+12 30706
    ## + fireplaces:bathrooms   1 2.7328e+10 6.0223e+12 30706
    ## + livingArea:fireplaces  1 2.5591e+10 6.0240e+12 30706
    ## + livingArea:bathrooms   1 2.4231e+10 6.0253e+12 30707
    ## + bathrooms:centralAir   1 2.3974e+10 6.0256e+12 30707
    ## - heating                2 2.2859e+09 6.0519e+12 30707
    ## + pctCollege:fuel        2 3.1522e+10 6.0181e+12 30707
    ## + pctCollege:centralAir  1 1.9654e+10 6.0299e+12 30708
    ## + bedrooms:centralAir    1 1.9171e+10 6.0304e+12 30708
    ## - fireplaces             1 7.2162e+08 6.0503e+12 30708
    ## - age                    1 3.8742e+09 6.0535e+12 30709
    ## + rooms:heating          2 2.1267e+10 6.0283e+12 30709
    ## + bedrooms:fuel          2 2.0719e+10 6.0289e+12 30709
    ## + age:centralAir         1 1.1952e+10 6.0376e+12 30709
    ## + fuel:centralAir        2 2.0663e+10 6.0289e+12 30709
    ## + livingArea:bedrooms    1 1.1844e+10 6.0377e+12 30709
    ## + lotSize:fireplaces     1 1.1400e+10 6.0382e+12 30709
    ## + lotSize:heating        2 1.9621e+10 6.0300e+12 30710
    ## + rooms:fuel             2 1.8990e+10 6.0306e+12 30710
    ## + lotSize:fuel           2 1.8599e+10 6.0310e+12 30710
    ## + bedrooms:fireplaces    1 9.0642e+09 6.0405e+12 30710
    ## + lotSize:rooms          1 8.8862e+09 6.0407e+12 30710
    ## <none>                                6.0496e+12 30710
    ## + lotSize:bathrooms      1 8.5432e+09 6.0410e+12 30710
    ## + bedrooms:heating       2 1.6658e+10 6.0329e+12 30710
    ## + livingArea:heating     2 1.6458e+10 6.0331e+12 30710
    ## + age:livingArea         1 7.0056e+09 6.0426e+12 30710
    ## + bathrooms:fuel         2 1.5220e+10 6.0344e+12 30711
    ## + age:bathrooms          1 5.5140e+09 6.0441e+12 30711
    ## + bedrooms:rooms         1 5.3703e+09 6.0442e+12 30711
    ## + age:rooms              1 5.3241e+09 6.0443e+12 30711
    ## + bathrooms:rooms        1 4.9865e+09 6.0446e+12 30711
    ## + fireplaces:heating     2 1.3705e+10 6.0359e+12 30711
    ## + pctCollege:bathrooms   1 4.8444e+09 6.0447e+12 30711
    ## + fireplaces:rooms       1 4.1459e+09 6.0454e+12 30711
    ## + bedrooms:bathrooms     1 4.0846e+09 6.0455e+12 30711
    ## - rooms                  1 1.3703e+10 6.0633e+12 30711
    ## + fireplaces:centralAir  1 3.0988e+09 6.0465e+12 30711
    ## + lotSize:bedrooms       1 1.8632e+09 6.0477e+12 30712
    ## + age:fireplaces         1 1.1506e+09 6.0484e+12 30712
    ## - pctCollege             1 1.6746e+10 6.0663e+12 30712
    ## + age:bedrooms           1 5.7727e+08 6.0490e+12 30712
    ## + livingArea:rooms       1 4.8343e+08 6.0491e+12 30712
    ## + livingArea:pctCollege  1 4.2627e+08 6.0492e+12 30712
    ## + lotSize:centralAir     1 8.9189e+07 6.0495e+12 30712
    ## + pctCollege:rooms       1 8.0040e+07 6.0495e+12 30712
    ## + rooms:centralAir       1 7.7968e+07 6.0495e+12 30712
    ## + pctCollege:bedrooms    1 9.4959e+04 6.0496e+12 30712
    ## - lotSize                1 1.7996e+10 6.0676e+12 30712
    ## + heating:centralAir     2 8.0306e+09 6.0415e+12 30712
    ## - fuel                   2 2.9127e+10 6.0787e+12 30713
    ## + heating:fuel           3 1.1926e+10 6.0377e+12 30713
    ## + fireplaces:fuel        2 1.2198e+09 6.0484e+12 30714
    ## - bedrooms               1 7.7921e+10 6.1275e+12 30726
    ## - bathrooms              1 1.5557e+11 6.2051e+12 30743
    ## - livingArea:centralAir  1 2.2331e+11 6.2729e+12 30758
    ## 
    ## Step:  AIC=30689.95
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + livingArea:fuel        2 6.5586e+10 5.8882e+12 30679
    ## + age:fuel               2 5.3613e+10 5.9001e+12 30681
    ## + age:heating            2 5.2322e+10 5.9014e+12 30682
    ## + pctCollege:fireplaces  1 3.8586e+10 5.9152e+12 30683
    ## + bathrooms:heating      2 4.3127e+10 5.9106e+12 30684
    ## + fireplaces:bathrooms   1 3.1878e+10 5.9219e+12 30685
    ## + lotSize:livingArea     1 2.7809e+10 5.9259e+12 30686
    ## + livingArea:bathrooms   1 2.5831e+10 5.9279e+12 30686
    ## + bathrooms:centralAir   1 2.5731e+10 5.9280e+12 30686
    ## + pctCollege:fuel        2 3.3947e+10 5.9198e+12 30686
    ## + livingArea:fireplaces  1 2.4176e+10 5.9296e+12 30686
    ## + lotSize:pctCollege     1 2.3849e+10 5.9299e+12 30686
    ## + lotSize:age            1 2.2892e+10 5.9309e+12 30687
    ## + pctCollege:heating     2 3.1352e+10 5.9224e+12 30687
    ## - heating                2 4.0212e+09 5.9578e+12 30687
    ## + age:centralAir         1 2.1465e+10 5.9323e+12 30687
    ## + bedrooms:centralAir    1 2.1269e+10 5.9325e+12 30687
    ## - fireplaces             1 2.4221e+08 5.9540e+12 30688
    ## + lotSize:fireplaces     1 1.3246e+10 5.9405e+12 30689
    ## + rooms:heating          2 2.0604e+10 5.9331e+12 30689
    ## + bedrooms:fireplaces    1 1.1535e+10 5.9422e+12 30689
    ## + fireplaces:heating     2 2.0065e+10 5.9337e+12 30689
    ## + age:livingArea         1 1.0936e+10 5.9428e+12 30689
    ## + livingArea:bedrooms    1 1.0356e+10 5.9434e+12 30690
    ## + bathrooms:fuel         2 1.8787e+10 5.9350e+12 30690
    ## + age:rooms              1 9.8741e+09 5.9439e+12 30690
    ## + age:bathrooms          1 8.8503e+09 5.9449e+12 30690
    ## + livingArea:heating     2 1.7247e+10 5.9365e+12 30690
    ## + bedrooms:fuel          2 1.7212e+10 5.9365e+12 30690
    ## <none>                                5.9537e+12 30690
    ## + lotSize:bathrooms      1 8.0402e+09 5.9457e+12 30690
    ## + lotSize:heating        2 1.6402e+10 5.9373e+12 30690
    ## + lotSize:fuel           2 1.6113e+10 5.9376e+12 30690
    ## + rooms:fuel             2 1.5087e+10 5.9387e+12 30690
    ## + fuel:centralAir        2 1.4235e+10 5.9395e+12 30691
    ## + bathrooms:rooms        1 5.5901e+09 5.9482e+12 30691
    ## + lotSize:rooms          1 5.5048e+09 5.9482e+12 30691
    ## + pctCollege:bathrooms   1 4.3274e+09 5.9494e+12 30691
    ## + pctCollege:centralAir  1 4.2514e+09 5.9495e+12 30691
    ## + bedrooms:heating       2 1.2718e+10 5.9410e+12 30691
    ## + bedrooms:bathrooms     1 4.0580e+09 5.9497e+12 30691
    ## + bedrooms:rooms         1 3.9001e+09 5.9498e+12 30691
    ## + fireplaces:rooms       1 3.5687e+09 5.9502e+12 30691
    ## + livingArea:pctCollege  1 2.8587e+09 5.9509e+12 30691
    ## + age:fireplaces         1 2.8510e+09 5.9509e+12 30691
    ## - rooms                  1 1.4846e+10 5.9686e+12 30691
    ## + fireplaces:centralAir  1 2.0668e+09 5.9517e+12 30692
    ## - lotSize                1 1.5688e+10 5.9694e+12 30692
    ## + livingArea:rooms       1 9.8418e+08 5.9528e+12 30692
    ## + pctCollege:rooms       1 5.8983e+08 5.9532e+12 30692
    ## + rooms:centralAir       1 4.0869e+08 5.9533e+12 30692
    ## + age:bedrooms           1 1.5569e+08 5.9536e+12 30692
    ## + lotSize:bedrooms       1 1.1872e+08 5.9536e+12 30692
    ## + pctCollege:bedrooms    1 7.0415e+07 5.9537e+12 30692
    ## + lotSize:centralAir     1 5.0809e+04 5.9537e+12 30692
    ## + heating:centralAir     2 7.1238e+09 5.9466e+12 30692
    ## + fireplaces:fuel        2 3.2987e+09 5.9504e+12 30693
    ## - fuel                   2 3.2923e+10 5.9867e+12 30694
    ## + heating:fuel           3 9.0891e+09 5.9447e+12 30694
    ## - bedrooms               1 7.7878e+10 6.0316e+12 30706
    ## - age:pctCollege         1 9.5834e+10 6.0496e+12 30710
    ## - bathrooms              1 1.5940e+11 6.1131e+12 30725
    ## - livingArea:centralAir  1 2.3931e+11 6.1931e+12 30742
    ## 
    ## Step:  AIC=30678.64
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + age:fuel               2 6.6761e+10 5.8214e+12 30667
    ## + age:heating            2 5.2973e+10 5.8352e+12 30670
    ## + pctCollege:fireplaces  1 4.4005e+10 5.8442e+12 30670
    ## + bathrooms:heating      2 4.8496e+10 5.8397e+12 30671
    ## + fireplaces:bathrooms   1 2.5694e+10 5.8625e+12 30675
    ## + lotSize:pctCollege     1 2.4422e+10 5.8637e+12 30675
    ## + pctCollege:heating     2 3.2491e+10 5.8557e+12 30675
    ## + fuel:centralAir        2 3.2153e+10 5.8560e+12 30675
    ## + bathrooms:centralAir   1 2.2824e+10 5.8653e+12 30675
    ## + bedrooms:fuel          2 3.0234e+10 5.8579e+12 30676
    ## - heating                2 4.5734e+09 5.8927e+12 30676
    ## + age:centralAir         1 2.0712e+10 5.8674e+12 30676
    ## + livingArea:fireplaces  1 2.0449e+10 5.8677e+12 30676
    ## + pctCollege:fuel        2 2.8470e+10 5.8597e+12 30676
    ## + bedrooms:centralAir    1 1.9628e+10 5.8685e+12 30676
    ## - fireplaces             1 1.9663e+04 5.8882e+12 30677
    ## + lotSize:age            1 1.5024e+10 5.8731e+12 30677
    ## + bedrooms:fireplaces    1 1.4902e+10 5.8733e+12 30677
    ## + bathrooms:fuel         2 2.3109e+10 5.8651e+12 30677
    ## + livingArea:bathrooms   1 1.2915e+10 5.8752e+12 30678
    ## + rooms:heating          2 2.0472e+10 5.8677e+12 30678
    ## + fireplaces:heating     2 2.0193e+10 5.8680e+12 30678
    ## + bedrooms:bathrooms     1 1.0370e+10 5.8778e+12 30678
    ## + livingArea:bedrooms    1 9.8717e+09 5.8783e+12 30678
    ## + livingArea:heating     2 1.8200e+10 5.8700e+12 30678
    ## <none>                                5.8882e+12 30679
    ## + lotSize:livingArea     1 8.1240e+09 5.8800e+12 30679
    ## + lotSize:fireplaces     1 7.5670e+09 5.8806e+12 30679
    ## + lotSize:fuel           2 1.4634e+10 5.8735e+12 30679
    ## + bedrooms:heating       2 1.3591e+10 5.8746e+12 30679
    ## + pctCollege:centralAir  1 4.5638e+09 5.8836e+12 30680
    ## + lotSize:bathrooms      1 4.1752e+09 5.8840e+12 30680
    ## + lotSize:bedrooms       1 3.9926e+09 5.8842e+12 30680
    ## + bedrooms:rooms         1 3.9648e+09 5.8842e+12 30680
    ## + age:fireplaces         1 3.9260e+09 5.8842e+12 30680
    ## + age:bathrooms          1 3.8566e+09 5.8843e+12 30680
    ## + age:bedrooms           1 2.9914e+09 5.8852e+12 30680
    ## + age:rooms              1 2.3782e+09 5.8858e+12 30680
    ## + lotSize:heating        2 1.0830e+10 5.8773e+12 30680
    ## + age:livingArea         1 1.6134e+09 5.8865e+12 30680
    ## + bathrooms:rooms        1 1.5394e+09 5.8866e+12 30680
    ## + fireplaces:rooms       1 1.4903e+09 5.8867e+12 30680
    ## - rooms                  1 1.5635e+10 5.9038e+12 30680
    ## + fireplaces:centralAir  1 1.3827e+09 5.8868e+12 30680
    ## + pctCollege:bathrooms   1 1.2059e+09 5.8870e+12 30680
    ## + heating:centralAir     2 9.5860e+09 5.8786e+12 30680
    ## + livingArea:rooms       1 8.4241e+08 5.8873e+12 30680
    ## + rooms:centralAir       1 7.8174e+08 5.8874e+12 30681
    ## + livingArea:pctCollege  1 5.7527e+08 5.8876e+12 30681
    ## + lotSize:centralAir     1 1.3714e+08 5.8880e+12 30681
    ## + pctCollege:bedrooms    1 8.7756e+07 5.8881e+12 30681
    ## + pctCollege:rooms       1 2.5315e+07 5.8881e+12 30681
    ## + lotSize:rooms          1 4.6570e+06 5.8882e+12 30681
    ## + rooms:fuel             2 8.3411e+09 5.8798e+12 30681
    ## + fireplaces:fuel        2 3.6647e+09 5.8845e+12 30682
    ## - lotSize                1 2.5944e+10 5.9141e+12 30683
    ## + heating:fuel           3 7.1592e+09 5.8810e+12 30683
    ## - livingArea:fuel        2 6.5586e+10 5.9537e+12 30690
    ## - bedrooms               1 7.7460e+10 5.9656e+12 30695
    ## - age:pctCollege         1 8.9799e+10 5.9780e+12 30698
    ## - bathrooms              1 1.5042e+11 6.0386e+12 30712
    ## - livingArea:centralAir  1 1.8461e+11 6.0728e+12 30719
    ## 
    ## Step:  AIC=30666.88
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + pctCollege:fireplaces  1 4.2226e+10 5.7792e+12 30659
    ## + bathrooms:heating      2 4.7348e+10 5.7741e+12 30660
    ## + age:centralAir         1 3.0070e+10 5.7913e+12 30662
    ## + lotSize:pctCollege     1 2.6549e+10 5.7948e+12 30663
    ## + age:heating            2 3.4453e+10 5.7869e+12 30663
    ## + bedrooms:fuel          2 3.2914e+10 5.7885e+12 30663
    ## + pctCollege:heating     2 3.2441e+10 5.7890e+12 30663
    ## + fireplaces:bathrooms   1 2.2607e+10 5.7988e+12 30664
    ## + bathrooms:centralAir   1 2.0313e+10 5.8011e+12 30664
    ## + bedrooms:centralAir    1 2.0300e+10 5.8011e+12 30664
    ## - heating                2 7.2973e+09 5.8287e+12 30665
    ## + bedrooms:fireplaces    1 1.6753e+10 5.8046e+12 30665
    ## - fireplaces             1 1.9074e+08 5.8216e+12 30665
    ## + livingArea:fireplaces  1 1.6470e+10 5.8049e+12 30665
    ## + pctCollege:fuel        2 2.3729e+10 5.7977e+12 30665
    ## + rooms:heating          2 2.2540e+10 5.7989e+12 30666
    ## + bedrooms:bathrooms     1 1.3956e+10 5.8074e+12 30666
    ## + livingArea:heating     2 2.2054e+10 5.7993e+12 30666
    ## + lotSize:fireplaces     1 1.3621e+10 5.8078e+12 30666
    ## + fireplaces:heating     2 1.9904e+10 5.8015e+12 30666
    ## + livingArea:bathrooms   1 1.1151e+10 5.8102e+12 30666
    ## + fuel:centralAir        2 1.9026e+10 5.8024e+12 30666
    ## + age:fireplaces         1 8.9986e+09 5.8124e+12 30667
    ## + age:bedrooms           1 8.7300e+09 5.8127e+12 30667
    ## <none>                                5.8214e+12 30667
    ## + livingArea:bedrooms    1 8.3026e+09 5.8131e+12 30667
    ## + lotSize:livingArea     1 7.6684e+09 5.8137e+12 30667
    ## + lotSize:bathrooms      1 7.3594e+09 5.8140e+12 30667
    ## + lotSize:bedrooms       1 7.1896e+09 5.8142e+12 30667
    ## + lotSize:fuel           2 1.2836e+10 5.8086e+12 30668
    ## + pctCollege:centralAir  1 4.1868e+09 5.8172e+12 30668
    ## + age:bathrooms          1 3.0093e+09 5.8184e+12 30668
    ## + bedrooms:rooms         1 2.6603e+09 5.8187e+12 30668
    ## + lotSize:heating        2 1.0315e+10 5.8111e+12 30668
    ## - rooms                  1 1.5003e+10 5.8364e+12 30668
    ## + rooms:fuel             2 1.0005e+10 5.8114e+12 30669
    ## + bathrooms:fuel         2 9.9713e+09 5.8114e+12 30669
    ## + bedrooms:heating       2 9.6730e+09 5.8117e+12 30669
    ## + rooms:centralAir       1 1.2111e+09 5.8202e+12 30669
    ## + livingArea:rooms       1 1.1759e+09 5.8202e+12 30669
    ## + fireplaces:centralAir  1 7.7179e+08 5.8206e+12 30669
    ## + pctCollege:bedrooms    1 6.5973e+08 5.8207e+12 30669
    ## + pctCollege:bathrooms   1 6.4264e+08 5.8208e+12 30669
    ## + bathrooms:rooms        1 6.0585e+08 5.8208e+12 30669
    ## + age:rooms              1 5.5568e+08 5.8208e+12 30669
    ## + fireplaces:rooms       1 5.2338e+08 5.8209e+12 30669
    ## + lotSize:age            1 4.7307e+08 5.8209e+12 30669
    ## + pctCollege:rooms       1 4.6941e+08 5.8209e+12 30669
    ## + age:livingArea         1 2.0199e+08 5.8212e+12 30669
    ## + lotSize:rooms          1 8.3264e+07 5.8213e+12 30669
    ## + livingArea:pctCollege  1 7.3918e+07 5.8213e+12 30669
    ## + lotSize:centralAir     1 3.6130e+07 5.8214e+12 30669
    ## + heating:centralAir     2 8.3759e+09 5.8130e+12 30669
    ## + fireplaces:fuel        2 2.2265e+09 5.8192e+12 30670
    ## - lotSize                1 2.6734e+10 5.8481e+12 30671
    ## + heating:fuel           3 2.8321e+09 5.8186e+12 30672
    ## - age:fuel               2 6.6761e+10 5.8882e+12 30679
    ## - livingArea:fuel        2 7.8734e+10 5.9001e+12 30681
    ## - bedrooms               1 8.3419e+10 5.9048e+12 30685
    ## - age:pctCollege         1 8.4583e+10 5.9060e+12 30685
    ## - bathrooms              1 1.6577e+11 5.9872e+12 30704
    ## - livingArea:centralAir  1 1.7352e+11 5.9949e+12 30706
    ## 
    ## Step:  AIC=30658.82
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + bathrooms:heating      2 4.8586e+10 5.7306e+12 30651
    ## + pctCollege:heating     2 4.0235e+10 5.7389e+12 30653
    ## + fireplaces:bathrooms   1 2.7708e+10 5.7515e+12 30654
    ## + age:centralAir         1 2.6804e+10 5.7524e+12 30654
    ## + bedrooms:fuel          2 3.4900e+10 5.7443e+12 30654
    ## + livingArea:fireplaces  1 2.6056e+10 5.7531e+12 30655
    ## + age:heating            2 3.4253e+10 5.7449e+12 30655
    ## + pctCollege:fuel        2 3.3061e+10 5.7461e+12 30655
    ## + bedrooms:centralAir    1 2.0318e+10 5.7589e+12 30656
    ## + bathrooms:centralAir   1 1.9729e+10 5.7594e+12 30656
    ## - heating                2 6.2112e+09 5.7854e+12 30656
    ## + lotSize:pctCollege     1 1.7363e+10 5.7618e+12 30657
    ## + rooms:heating          2 2.4485e+10 5.7547e+12 30657
    ## + lotSize:fireplaces     1 1.4084e+10 5.7651e+12 30657
    ## + livingArea:bathrooms   1 1.3584e+10 5.7656e+12 30658
    ## + pctCollege:bathrooms   1 1.3427e+10 5.7657e+12 30658
    ## + livingArea:heating     2 2.1356e+10 5.7578e+12 30658
    ## + bedrooms:fireplaces    1 1.1606e+10 5.7676e+12 30658
    ## + livingArea:pctCollege  1 1.1072e+10 5.7681e+12 30658
    ## + fireplaces:heating     2 1.9296e+10 5.7599e+12 30658
    ## + fuel:centralAir        2 1.8225e+10 5.7609e+12 30659
    ## + bedrooms:bathrooms     1 9.5687e+09 5.7696e+12 30659
    ## + age:bedrooms           1 9.4163e+09 5.7698e+12 30659
    ## <none>                                5.7792e+12 30659
    ## + lotSize:bedrooms       1 8.1405e+09 5.7710e+12 30659
    ## + lotSize:livingArea     1 7.9147e+09 5.7713e+12 30659
    ## + lotSize:bathrooms      1 7.3314e+09 5.7718e+12 30659
    ## + lotSize:fuel           2 1.3391e+10 5.7658e+12 30660
    ## + age:fireplaces         1 5.0214e+09 5.7742e+12 30660
    ## + livingArea:bedrooms    1 4.9984e+09 5.7742e+12 30660
    ## + bathrooms:fuel         2 1.1773e+10 5.7674e+12 30660
    ## + lotSize:heating        2 1.1581e+10 5.7676e+12 30660
    ## - rooms                  1 1.3711e+10 5.7929e+12 30660
    ## + livingArea:rooms       1 2.5828e+09 5.7766e+12 30660
    ## + fireplaces:rooms       1 2.0549e+09 5.7771e+12 30660
    ## + age:bathrooms          1 1.9560e+09 5.7772e+12 30660
    ## + rooms:centralAir       1 1.3387e+09 5.7778e+12 30661
    ## + bathrooms:rooms        1 1.1292e+09 5.7780e+12 30661
    ## + bedrooms:rooms         1 1.1204e+09 5.7781e+12 30661
    ## + pctCollege:rooms       1 7.2792e+08 5.7784e+12 30661
    ## + pctCollege:centralAir  1 2.6944e+08 5.7789e+12 30661
    ## + age:rooms              1 2.6603e+08 5.7789e+12 30661
    ## + rooms:fuel             2 8.5880e+09 5.7706e+12 30661
    ## + pctCollege:bedrooms    1 1.9299e+08 5.7790e+12 30661
    ## + lotSize:centralAir     1 1.5797e+08 5.7790e+12 30661
    ## + fireplaces:centralAir  1 1.5792e+08 5.7790e+12 30661
    ## + lotSize:age            1 1.3618e+08 5.7790e+12 30661
    ## + age:livingArea         1 3.9520e+06 5.7792e+12 30661
    ## + lotSize:rooms          1 5.7612e+05 5.7792e+12 30661
    ## + bedrooms:heating       2 8.3281e+09 5.7708e+12 30661
    ## + heating:centralAir     2 7.7594e+09 5.7714e+12 30661
    ## + fireplaces:fuel        2 9.4872e+08 5.7782e+12 30663
    ## - lotSize                1 2.9274e+10 5.8084e+12 30664
    ## + heating:fuel           3 2.7757e+09 5.7764e+12 30664
    ## - pctCollege:fireplaces  1 4.2226e+10 5.8214e+12 30667
    ## - age:fuel               2 6.4981e+10 5.8442e+12 30670
    ## - age:pctCollege         1 6.1190e+10 5.8404e+12 30671
    ## - livingArea:fuel        2 8.4616e+10 5.8638e+12 30675
    ## - bedrooms               1 7.8499e+10 5.8577e+12 30676
    ## - bathrooms              1 1.5729e+11 5.9365e+12 30694
    ## - livingArea:centralAir  1 1.8348e+11 5.9627e+12 30700
    ## 
    ## Step:  AIC=30651.15
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + livingArea:fireplaces  1 3.2265e+10 5.6983e+12 30645
    ## + fireplaces:bathrooms   1 3.0724e+10 5.6999e+12 30646
    ## + pctCollege:fuel        2 3.7633e+10 5.6930e+12 30646
    ## + pctCollege:heating     2 3.6187e+10 5.6944e+12 30646
    ## + bedrooms:fuel          2 3.2908e+10 5.6977e+12 30647
    ## + bedrooms:centralAir    1 2.2822e+10 5.7078e+12 30648
    ## + age:centralAir         1 2.0884e+10 5.7097e+12 30648
    ## + livingArea:bathrooms   1 1.5325e+10 5.7153e+12 30650
    ## + pctCollege:bathrooms   1 1.4166e+10 5.7164e+12 30650
    ## + livingArea:pctCollege  1 1.4113e+10 5.7165e+12 30650
    ## + lotSize:pctCollege     1 1.3675e+10 5.7169e+12 30650
    ## + age:bedrooms           1 1.0517e+10 5.7201e+12 30651
    ## + lotSize:bedrooms       1 1.0372e+10 5.7202e+12 30651
    ## + lotSize:fireplaces     1 1.0305e+10 5.7203e+12 30651
    ## + fuel:centralAir        2 1.7507e+10 5.7131e+12 30651
    ## + bathrooms:centralAir   1 8.8032e+09 5.7218e+12 30651
    ## <none>                                5.7306e+12 30651
    ## + bedrooms:fireplaces    1 7.0702e+09 5.7235e+12 30651
    ## + bedrooms:bathrooms     1 6.4669e+09 5.7241e+12 30652
    ## + livingArea:rooms       1 6.2957e+09 5.7243e+12 30652
    ## + age:heating            2 1.4421e+10 5.7162e+12 30652
    ## + lotSize:livingArea     1 5.1107e+09 5.7255e+12 30652
    ## + fireplaces:rooms       1 4.4144e+09 5.7262e+12 30652
    ## + lotSize:bathrooms      1 3.9687e+09 5.7266e+12 30652
    ## + age:fireplaces         1 3.8225e+09 5.7268e+12 30652
    ## - rooms                  1 1.3969e+10 5.7446e+12 30653
    ## + lotSize:fuel           2 9.9278e+09 5.7207e+12 30653
    ## + bathrooms:rooms        1 1.6252e+09 5.7290e+12 30653
    ## + lotSize:heating        2 9.7181e+09 5.7209e+12 30653
    ## + livingArea:bedrooms    1 1.4023e+09 5.7292e+12 30653
    ## + pctCollege:rooms       1 1.1477e+09 5.7294e+12 30653
    ## + rooms:centralAir       1 1.1277e+09 5.7295e+12 30653
    ## + pctCollege:bedrooms    1 9.7389e+08 5.7296e+12 30653
    ## + age:livingArea         1 5.0137e+08 5.7301e+12 30653
    ## + rooms:fuel             2 8.6179e+09 5.7220e+12 30653
    ## + pctCollege:centralAir  1 2.7982e+08 5.7303e+12 30653
    ## + bedrooms:rooms         1 1.7669e+08 5.7304e+12 30653
    ## + lotSize:age            1 1.3545e+08 5.7305e+12 30653
    ## + fireplaces:centralAir  1 4.3662e+07 5.7305e+12 30653
    ## + lotSize:rooms          1 4.1432e+07 5.7305e+12 30653
    ## + lotSize:centralAir     1 2.3868e+07 5.7306e+12 30653
    ## + age:bathrooms          1 1.8555e+07 5.7306e+12 30653
    ## + age:rooms              1 2.8291e+06 5.7306e+12 30653
    ## + rooms:heating          2 7.1626e+09 5.7234e+12 30653
    ## + fireplaces:heating     2 6.9016e+09 5.7237e+12 30654
    ## + bedrooms:heating       2 5.7107e+09 5.7249e+12 30654
    ## + heating:centralAir     2 5.1788e+09 5.7254e+12 30654
    ## + fireplaces:fuel        2 2.1909e+09 5.7284e+12 30655
    ## + livingArea:heating     2 1.6223e+09 5.7290e+12 30655
    ## + bathrooms:fuel         2 2.7952e+08 5.7303e+12 30655
    ## + heating:fuel           3 8.0523e+09 5.7225e+12 30655
    ## - lotSize                1 3.2582e+10 5.7632e+12 30657
    ## - bathrooms:heating      2 4.8586e+10 5.7792e+12 30659
    ## - pctCollege:fireplaces  1 4.3464e+10 5.7741e+12 30660
    ## - age:fuel               2 6.3959e+10 5.7945e+12 30663
    ## - age:pctCollege         1 6.1423e+10 5.7920e+12 30664
    ## - bedrooms               1 6.9284e+10 5.7999e+12 30666
    ## - livingArea:fuel        2 8.8437e+10 5.8190e+12 30668
    ## - livingArea:centralAir  1 1.4817e+11 5.8788e+12 30684
    ## 
    ## Step:  AIC=30645.35
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + bedrooms:fireplaces    1 5.1042e+10 5.6473e+12 30635
    ## + pctCollege:fuel        2 3.9908e+10 5.6584e+12 30640
    ## + pctCollege:heating     2 3.7120e+10 5.6612e+12 30640
    ## + age:centralAir         1 2.5757e+10 5.6726e+12 30641
    ## + bedrooms:fuel          2 3.0235e+10 5.6681e+12 30642
    ## + bedrooms:centralAir    1 1.8567e+10 5.6798e+12 30643
    ## + lotSize:fireplaces     1 1.4929e+10 5.6834e+12 30644
    ## + lotSize:pctCollege     1 1.2792e+10 5.6855e+12 30644
    ## + pctCollege:bathrooms   1 1.2352e+10 5.6860e+12 30644
    ## + bedrooms:bathrooms     1 1.2182e+10 5.6861e+12 30644
    ## + bathrooms:centralAir   1 1.0716e+10 5.6876e+12 30645
    ## + age:bedrooms           1 9.8466e+09 5.6885e+12 30645
    ## + fuel:centralAir        2 1.7122e+10 5.6812e+12 30645
    ## + livingArea:pctCollege  1 8.7557e+09 5.6896e+12 30645
    ## + lotSize:bedrooms       1 8.2919e+09 5.6900e+12 30645
    ## <none>                                5.6983e+12 30645
    ## + livingArea:bedrooms    1 8.0479e+09 5.6903e+12 30645
    ## + fireplaces:rooms       1 8.0132e+09 5.6903e+12 30645
    ## + lotSize:livingArea     1 7.8694e+09 5.6905e+12 30645
    ## + age:heating            2 1.4371e+10 5.6839e+12 30646
    ## + lotSize:bathrooms      1 4.4075e+09 5.6939e+12 30646
    ## + fireplaces:bathrooms   1 3.4584e+09 5.6949e+12 30647
    ## + age:fireplaces         1 2.7053e+09 5.6956e+12 30647
    ## + fireplaces:centralAir  1 2.5528e+09 5.6958e+12 30647
    ## - rooms                  1 1.4271e+10 5.7126e+12 30647
    ## + livingArea:bathrooms   1 1.4752e+09 5.6968e+12 30647
    ## + fireplaces:heating     2 9.5243e+09 5.6888e+12 30647
    ## + bedrooms:rooms         1 1.1753e+09 5.6971e+12 30647
    ## + age:livingArea         1 8.0557e+08 5.6975e+12 30647
    ## + lotSize:heating        2 9.0113e+09 5.6893e+12 30647
    ## + rooms:fuel             2 8.8676e+09 5.6895e+12 30647
    ## + pctCollege:rooms       1 5.6880e+08 5.6978e+12 30647
    ## + rooms:heating          2 8.6878e+09 5.6896e+12 30647
    ## + pctCollege:bedrooms    1 4.0477e+08 5.6979e+12 30647
    ## + lotSize:fuel           2 8.6233e+09 5.6897e+12 30647
    ## + rooms:centralAir       1 3.5933e+08 5.6980e+12 30647
    ## + livingArea:rooms       1 3.1637e+08 5.6980e+12 30647
    ## + age:bathrooms          1 1.3974e+08 5.6982e+12 30647
    ## + pctCollege:centralAir  1 9.5558e+07 5.6982e+12 30647
    ## + lotSize:age            1 3.0249e+07 5.6983e+12 30647
    ## + lotSize:centralAir     1 2.3982e+07 5.6983e+12 30647
    ## + lotSize:rooms          1 4.0419e+06 5.6983e+12 30647
    ## + bathrooms:rooms        1 9.2660e+05 5.6983e+12 30647
    ## + age:rooms              1 5.7731e+04 5.6983e+12 30647
    ## + heating:centralAir     2 6.2747e+09 5.6920e+12 30648
    ## + fireplaces:fuel        2 5.8898e+09 5.6924e+12 30648
    ## + bedrooms:heating       2 4.7861e+09 5.6935e+12 30648
    ## + livingArea:heating     2 1.4805e+09 5.6968e+12 30649
    ## + heating:fuel           3 8.9742e+09 5.6893e+12 30649
    ## + bathrooms:fuel         2 2.2457e+08 5.6981e+12 30649
    ## - livingArea:fireplaces  1 3.2265e+10 5.7306e+12 30651
    ## - lotSize                1 3.2906e+10 5.7312e+12 30651
    ## - bathrooms:heating      2 5.4796e+10 5.7531e+12 30655
    ## - age:fuel               2 5.7892e+10 5.7562e+12 30655
    ## - pctCollege:fireplaces  1 5.4466e+10 5.7528e+12 30657
    ## - age:pctCollege         1 5.8190e+10 5.7565e+12 30657
    ## - bedrooms               1 6.3194e+10 5.7615e+12 30659
    ## - livingArea:fuel        2 8.4020e+10 5.7823e+12 30662
    ## - livingArea:centralAir  1 9.5237e+10 5.7936e+12 30666
    ## 
    ## Step:  AIC=30634.91
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + pctCollege:fuel        2 4.0680e+10 5.6066e+12 30629
    ## + pctCollege:heating     2 4.0294e+10 5.6070e+12 30629
    ## + age:centralAir         1 2.6490e+10 5.6208e+12 30630
    ## + pctCollege:bathrooms   1 1.9199e+10 5.6281e+12 30632
    ## + livingArea:pctCollege  1 1.3800e+10 5.6335e+12 30634
    ## + bathrooms:centralAir   1 1.0841e+10 5.6364e+12 30634
    ## + lotSize:fireplaces     1 1.0739e+10 5.6365e+12 30634
    ## + fuel:centralAir        2 1.8674e+10 5.6286e+12 30634
    ## + pctCollege:bedrooms    1 9.8478e+09 5.6374e+12 30635
    ## + lotSize:pctCollege     1 9.8410e+09 5.6374e+12 30635
    ## + lotSize:bedrooms       1 8.8805e+09 5.6384e+12 30635
    ## <none>                                5.6473e+12 30635
    ## + lotSize:livingArea     1 6.9256e+09 5.6404e+12 30635
    ## + rooms:heating          2 1.4167e+10 5.6331e+12 30635
    ## + age:bedrooms           1 4.7756e+09 5.6425e+12 30636
    ## + age:heating            2 1.2547e+10 5.6347e+12 30636
    ## + bedrooms:fuel          2 1.2524e+10 5.6348e+12 30636
    ## + fireplaces:centralAir  1 4.3158e+09 5.6430e+12 30636
    ## + pctCollege:rooms       1 4.2876e+09 5.6430e+12 30636
    ## + bedrooms:centralAir    1 3.9569e+09 5.6433e+12 30636
    ## + lotSize:bathrooms      1 3.6106e+09 5.6437e+12 30636
    ## + fireplaces:bathrooms   1 3.3580e+09 5.6439e+12 30636
    ## - rooms                  1 1.4228e+10 5.6615e+12 30636
    ## + age:livingArea         1 1.2806e+09 5.6460e+12 30637
    ## + lotSize:heating        2 9.3972e+09 5.6379e+12 30637
    ## + lotSize:fuel           2 9.1293e+09 5.6381e+12 30637
    ## + livingArea:rooms       1 9.5615e+08 5.6463e+12 30637
    ## + bathrooms:rooms        1 5.9228e+08 5.6467e+12 30637
    ## + lotSize:age            1 4.3394e+08 5.6468e+12 30637
    ## + rooms:centralAir       1 3.2137e+08 5.6470e+12 30637
    ## + livingArea:bathrooms   1 3.0765e+08 5.6470e+12 30637
    ## + bedrooms:rooms         1 2.3507e+08 5.6470e+12 30637
    ## + bedrooms:bathrooms     1 1.9469e+08 5.6471e+12 30637
    ## + age:bathrooms          1 1.7913e+08 5.6471e+12 30637
    ## + age:rooms              1 8.0452e+07 5.6472e+12 30637
    ## + lotSize:rooms          1 5.7645e+07 5.6472e+12 30637
    ## + livingArea:bedrooms    1 5.6294e+07 5.6472e+12 30637
    ## + fireplaces:rooms       1 3.3842e+07 5.6472e+12 30637
    ## + pctCollege:centralAir  1 2.6886e+07 5.6473e+12 30637
    ## + lotSize:centralAir     1 7.4297e+06 5.6473e+12 30637
    ## + age:fireplaces         1 5.7485e+06 5.6473e+12 30637
    ## + heating:centralAir     2 7.8719e+09 5.6394e+12 30637
    ## + rooms:fuel             2 6.7578e+09 5.6405e+12 30637
    ## + fireplaces:heating     2 3.3247e+09 5.6440e+12 30638
    ## + heating:fuel           3 1.1305e+10 5.6360e+12 30638
    ## + livingArea:heating     2 1.9275e+09 5.6454e+12 30638
    ## + fireplaces:fuel        2 1.6458e+09 5.6456e+12 30639
    ## + bedrooms:heating       2 1.0528e+09 5.6462e+12 30639
    ## + bathrooms:fuel         2 5.6167e+07 5.6472e+12 30639
    ## - lotSize                1 3.2321e+10 5.6796e+12 30641
    ## - bathrooms:heating      2 4.6548e+10 5.6938e+12 30642
    ## - age:fuel               2 5.7087e+10 5.7044e+12 30645
    ## - bedrooms:fireplaces    1 5.1042e+10 5.6983e+12 30645
    ## - pctCollege:fireplaces  1 5.1870e+10 5.6991e+12 30646
    ## - age:pctCollege         1 6.2164e+10 5.7094e+12 30648
    ## - livingArea:fireplaces  1 7.6238e+10 5.7235e+12 30651
    ## - livingArea:fuel        2 8.8095e+10 5.7354e+12 30652
    ## - livingArea:centralAir  1 9.2539e+10 5.7398e+12 30655
    ## 
    ## Step:  AIC=30628.92
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces + pctCollege:fuel
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + age:centralAir         1 2.7579e+10 5.5790e+12 30624
    ## + pctCollege:bathrooms   1 1.3631e+10 5.5930e+12 30628
    ## + lotSize:bedrooms       1 1.0687e+10 5.5959e+12 30628
    ## + bathrooms:centralAir   1 9.2945e+09 5.5973e+12 30629
    ## + fuel:centralAir        2 1.7241e+10 5.5894e+12 30629
    ## + livingArea:pctCollege  1 9.1445e+09 5.5975e+12 30629
    ## <none>                                5.6066e+12 30629
    ## + lotSize:fireplaces     1 7.7294e+09 5.5989e+12 30629
    ## + rooms:heating          2 1.5733e+10 5.5909e+12 30629
    ## + bedrooms:centralAir    1 5.6196e+09 5.6010e+12 30630
    ## + lotSize:pctCollege     1 5.4222e+09 5.6012e+12 30630
    ## + age:bedrooms           1 4.9838e+09 5.6016e+12 30630
    ## + lotSize:livingArea     1 4.7197e+09 5.6019e+12 30630
    ## + bedrooms:fuel          2 1.2740e+10 5.5939e+12 30630
    ## + fireplaces:centralAir  1 4.5515e+09 5.6020e+12 30630
    ## + pctCollege:bedrooms    1 4.5358e+09 5.6021e+12 30630
    ## + age:heating            2 1.2572e+10 5.5940e+12 30630
    ## + fireplaces:bathrooms   1 2.3414e+09 5.6043e+12 30630
    ## + lotSize:bathrooms      1 2.2535e+09 5.6043e+12 30630
    ## + pctCollege:rooms       1 1.6294e+09 5.6050e+12 30631
    ## + pctCollege:centralAir  1 1.5173e+09 5.6051e+12 30631
    ## - rooms                  1 1.4739e+10 5.6213e+12 30631
    ## + pctCollege:heating     2 9.5881e+09 5.5970e+12 30631
    ## + livingArea:rooms       1 1.2055e+09 5.6054e+12 30631
    ## + age:livingArea         1 9.4932e+08 5.6056e+12 30631
    ## + bathrooms:rooms        1 6.6418e+08 5.6059e+12 30631
    ## + livingArea:bathrooms   1 5.2782e+08 5.6061e+12 30631
    ## + lotSize:age            1 3.3717e+08 5.6063e+12 30631
    ## + bedrooms:rooms         1 2.5959e+08 5.6063e+12 30631
    ## + rooms:centralAir       1 2.0870e+08 5.6064e+12 30631
    ## + bedrooms:bathrooms     1 1.8805e+08 5.6064e+12 30631
    ## + age:rooms              1 1.3536e+08 5.6065e+12 30631
    ## + lotSize:centralAir     1 1.0025e+08 5.6065e+12 30631
    ## + age:bathrooms          1 5.2855e+07 5.6065e+12 30631
    ## + lotSize:rooms          1 4.5834e+07 5.6066e+12 30631
    ## + livingArea:bedrooms    1 1.5269e+07 5.6066e+12 30631
    ## + fireplaces:rooms       1 2.9398e+06 5.6066e+12 30631
    ## + age:fireplaces         1 7.8427e+04 5.6066e+12 30631
    ## + rooms:fuel             2 6.6573e+09 5.5999e+12 30631
    ## + lotSize:heating        2 6.0398e+09 5.6006e+12 30631
    ## + lotSize:fuel           2 5.5673e+09 5.6010e+12 30632
    ## + fireplaces:heating     2 5.5520e+09 5.6010e+12 30632
    ## + fireplaces:fuel        2 4.6099e+09 5.6020e+12 30632
    ## + heating:fuel           3 1.2204e+10 5.5944e+12 30632
    ## + livingArea:heating     2 2.7470e+09 5.6039e+12 30632
    ## + heating:centralAir     2 1.4076e+09 5.6052e+12 30633
    ## + bedrooms:heating       2 6.6424e+08 5.6059e+12 30633
    ## + bathrooms:fuel         2 7.0478e+07 5.6065e+12 30633
    ## - lotSize                1 2.7988e+10 5.6346e+12 30634
    ## - pctCollege:fuel        2 4.0680e+10 5.6473e+12 30635
    ## - bathrooms:heating      2 5.1155e+10 5.6578e+12 30638
    ## - age:fuel               2 5.2318e+10 5.6589e+12 30638
    ## - bedrooms:fireplaces    1 5.1814e+10 5.6584e+12 30640
    ## - age:pctCollege         1 5.5801e+10 5.6624e+12 30641
    ## - pctCollege:fireplaces  1 6.3690e+10 5.6703e+12 30643
    ## - livingArea:fuel        2 8.3225e+10 5.6898e+12 30645
    ## - livingArea:fireplaces  1 7.9655e+10 5.6863e+12 30646
    ## - livingArea:centralAir  1 9.0740e+10 5.6973e+12 30649
    ## 
    ## Step:  AIC=30624.11
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces + pctCollege:fuel + age:centralAir
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + fuel:centralAir        2 2.8273e+10 5.5507e+12 30621
    ## + pctCollege:bathrooms   1 1.3776e+10 5.5652e+12 30623
    ## + rooms:heating          2 1.8559e+10 5.5605e+12 30624
    ## + livingArea:pctCollege  1 9.3654e+09 5.5697e+12 30624
    ## + lotSize:bedrooms       1 9.0489e+09 5.5700e+12 30624
    ## + lotSize:fireplaces     1 8.7123e+09 5.5703e+12 30624
    ## <none>                                5.5790e+12 30624
    ## + age:bedrooms           1 5.2139e+09 5.5738e+12 30625
    ## + pctCollege:bedrooms    1 5.1159e+09 5.5739e+12 30625
    ## + lotSize:livingArea     1 4.9667e+09 5.5741e+12 30625
    ## + lotSize:pctCollege     1 4.3734e+09 5.5746e+12 30625
    ## + fireplaces:centralAir  1 4.1898e+09 5.5748e+12 30625
    ## + bathrooms:centralAir   1 3.9879e+09 5.5750e+12 30625
    ## + bedrooms:fuel          2 1.1524e+10 5.5675e+12 30625
    ## + lotSize:bathrooms      1 3.3407e+09 5.5757e+12 30625
    ## + fireplaces:bathrooms   1 2.9509e+09 5.5761e+12 30625
    ## + age:bathrooms          1 2.6570e+09 5.5764e+12 30626
    ## + age:livingArea         1 2.5587e+09 5.5765e+12 30626
    ## + pctCollege:rooms       1 2.1439e+09 5.5769e+12 30626
    ## + bedrooms:centralAir    1 1.6977e+09 5.5773e+12 30626
    ## + bathrooms:rooms        1 1.0629e+09 5.5780e+12 30626
    ## - rooms                  1 1.5206e+10 5.5942e+12 30626
    ## + livingArea:rooms       1 9.2930e+08 5.5781e+12 30626
    ## + rooms:centralAir       1 8.8863e+08 5.5781e+12 30626
    ## + livingArea:bathrooms   1 5.3368e+08 5.5785e+12 30626
    ## + lotSize:centralAir     1 3.8421e+08 5.5786e+12 30626
    ## + bedrooms:bathrooms     1 2.9772e+08 5.5787e+12 30626
    ## + livingArea:bedrooms    1 1.8097e+08 5.5788e+12 30626
    ## + pctCollege:heating     2 8.2333e+09 5.5708e+12 30626
    ## + pctCollege:centralAir  1 1.5147e+08 5.5789e+12 30626
    ## + lotSize:age            1 1.2720e+08 5.5789e+12 30626
    ## + bedrooms:rooms         1 6.5884e+07 5.5790e+12 30626
    ## + age:fireplaces         1 3.4309e+07 5.5790e+12 30626
    ## + fireplaces:rooms       1 2.2871e+07 5.5790e+12 30626
    ## + age:rooms              1 2.2198e+07 5.5790e+12 30626
    ## + lotSize:rooms          1 6.2903e+05 5.5790e+12 30626
    ## + age:heating            2 7.9377e+09 5.5711e+12 30626
    ## + lotSize:heating        2 6.5969e+09 5.5724e+12 30627
    ## + rooms:fuel             2 5.9589e+09 5.5731e+12 30627
    ## + lotSize:fuel           2 5.8481e+09 5.5732e+12 30627
    ## + fireplaces:heating     2 5.3661e+09 5.5737e+12 30627
    ## + fireplaces:fuel        2 4.9809e+09 5.5740e+12 30627
    ## + heating:centralAir     2 4.0839e+09 5.5749e+12 30627
    ## + livingArea:heating     2 2.6447e+09 5.5764e+12 30628
    ## + heating:fuel           3 1.0428e+10 5.5686e+12 30628
    ## + bedrooms:heating       2 9.4819e+08 5.5781e+12 30628
    ## + bathrooms:fuel         2 2.2047e+08 5.5788e+12 30628
    ## - age:centralAir         1 2.7579e+10 5.6066e+12 30629
    ## - lotSize                1 2.8790e+10 5.6078e+12 30629
    ## - pctCollege:fuel        2 4.1769e+10 5.6208e+12 30630
    ## - bathrooms:heating      2 4.5331e+10 5.6244e+12 30631
    ## - age:fuel               2 6.0542e+10 5.6396e+12 30635
    ## - bedrooms:fireplaces    1 5.2676e+10 5.6317e+12 30635
    ## - pctCollege:fireplaces  1 6.1238e+10 5.6403e+12 30637
    ## - age:pctCollege         1 6.2865e+10 5.6419e+12 30638
    ## - livingArea:centralAir  1 6.8832e+10 5.6479e+12 30639
    ## - livingArea:fuel        2 8.4656e+10 5.6637e+12 30641
    ## - livingArea:fireplaces  1 8.6590e+10 5.6656e+12 30643
    ## 
    ## Step:  AIC=30621.09
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces + pctCollege:fuel + age:centralAir + 
    ##     fuel:centralAir
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + pctCollege:bathrooms   1 1.5628e+10 5.5351e+12 30619
    ## + lotSize:fireplaces     1 9.5286e+09 5.5412e+12 30621
    ## + lotSize:bedrooms       1 9.0570e+09 5.5417e+12 30621
    ## + rooms:heating          2 1.6767e+10 5.5340e+12 30621
    ## <none>                                5.5507e+12 30621
    ## + livingArea:pctCollege  1 7.9804e+09 5.5428e+12 30621
    ## + lotSize:pctCollege     1 5.9833e+09 5.5448e+12 30622
    ## + age:bedrooms           1 5.9445e+09 5.5448e+12 30622
    ## + lotSize:livingArea     1 5.5583e+09 5.5452e+12 30622
    ## + fireplaces:bathrooms   1 4.4826e+09 5.5463e+12 30622
    ## + bathrooms:centralAir   1 4.2377e+09 5.5465e+12 30622
    ## + age:bathrooms          1 4.1303e+09 5.5466e+12 30622
    ## + age:livingArea         1 4.0554e+09 5.5467e+12 30622
    ## + pctCollege:bedrooms    1 3.6939e+09 5.5471e+12 30622
    ## + fireplaces:centralAir  1 3.1658e+09 5.5476e+12 30622
    ## + lotSize:bathrooms      1 3.0466e+09 5.5477e+12 30622
    ## + bedrooms:fuel          2 1.0770e+10 5.5400e+12 30622
    ## + bedrooms:centralAir    1 1.7214e+09 5.5490e+12 30623
    ## + bathrooms:rooms        1 1.3193e+09 5.5494e+12 30623
    ## + pctCollege:rooms       1 1.2153e+09 5.5495e+12 30623
    ## + pctCollege:heating     2 9.0813e+09 5.5417e+12 30623
    ## - rooms                  1 1.5170e+10 5.5659e+12 30623
    ## + livingArea:rooms       1 8.1162e+08 5.5499e+12 30623
    ## + lotSize:centralAir     1 6.9972e+08 5.5500e+12 30623
    ## + rooms:centralAir       1 6.9688e+08 5.5501e+12 30623
    ## + livingArea:bathrooms   1 6.7242e+08 5.5501e+12 30623
    ## + livingArea:bedrooms    1 3.0907e+08 5.5504e+12 30623
    ## + bedrooms:bathrooms     1 2.2936e+08 5.5505e+12 30623
    ## + pctCollege:centralAir  1 2.2338e+08 5.5505e+12 30623
    ## + fireplaces:rooms       1 1.1083e+08 5.5506e+12 30623
    ## + lotSize:age            1 8.2541e+07 5.5507e+12 30623
    ## + age:rooms              1 4.7505e+07 5.5507e+12 30623
    ## + bedrooms:rooms         1 3.3930e+07 5.5507e+12 30623
    ## + lotSize:rooms          1 2.2720e+07 5.5507e+12 30623
    ## + age:fireplaces         1 4.0848e+06 5.5507e+12 30623
    ## + lotSize:heating        2 7.8515e+09 5.5429e+12 30623
    ## + lotSize:fuel           2 6.2965e+09 5.5445e+12 30624
    ## + age:heating            2 6.1043e+09 5.5446e+12 30624
    ## + rooms:fuel             2 5.5505e+09 5.5452e+12 30624
    ## + fireplaces:heating     2 5.4550e+09 5.5453e+12 30624
    ## + fireplaces:fuel        2 4.9299e+09 5.5458e+12 30624
    ## + heating:centralAir     2 4.3191e+09 5.5464e+12 30624
    ## - fuel:centralAir        2 2.8273e+10 5.5790e+12 30624
    ## + livingArea:heating     2 2.5717e+09 5.5482e+12 30624
    ## + bathrooms:fuel         2 1.4858e+09 5.5493e+12 30625
    ## + bedrooms:heating       2 9.4648e+08 5.5498e+12 30625
    ## + heating:fuel           3 5.6981e+09 5.5450e+12 30626
    ## - lotSize                1 2.8355e+10 5.5791e+12 30626
    ## - pctCollege:fuel        2 4.0193e+10 5.5909e+12 30627
    ## - bathrooms:heating      2 4.3332e+10 5.5941e+12 30628
    ## - age:fuel               2 4.4837e+10 5.5956e+12 30628
    ## - age:centralAir         1 3.8611e+10 5.5894e+12 30629
    ## - bedrooms:fireplaces    1 5.3392e+10 5.6041e+12 30632
    ## - pctCollege:fireplaces  1 5.8839e+10 5.6096e+12 30634
    ## - age:pctCollege         1 6.2403e+10 5.6132e+12 30635
    ## - livingArea:centralAir  1 6.5370e+10 5.6161e+12 30635
    ## - livingArea:fireplaces  1 8.6194e+10 5.6369e+12 30640
    ## - livingArea:fuel        2 1.0030e+11 5.6511e+12 30642
    ## 
    ## Step:  AIC=30619.19
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces + pctCollege:fuel + age:centralAir + 
    ##     fuel:centralAir + pctCollege:bathrooms
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + lotSize:fireplaces     1 9.9212e+09 5.5252e+12 30619
    ## + lotSize:bedrooms       1 8.9464e+09 5.5262e+12 30619
    ## + rooms:heating          2 1.6825e+10 5.5183e+12 30619
    ## <none>                                5.5351e+12 30619
    ## + lotSize:pctCollege     1 7.1851e+09 5.5279e+12 30619
    ## + lotSize:livingArea     1 5.3099e+09 5.5298e+12 30620
    ## + age:bedrooms           1 4.9392e+09 5.5302e+12 30620
    ## + bedrooms:fuel          2 1.1698e+10 5.5234e+12 30620
    ## + age:bathrooms          1 3.5957e+09 5.5315e+12 30620
    ## + age:livingArea         1 3.4576e+09 5.5317e+12 30620
    ## + lotSize:bathrooms      1 2.5884e+09 5.5325e+12 30621
    ## + fireplaces:centralAir  1 2.2835e+09 5.5328e+12 30621
    ## + fireplaces:bathrooms   1 2.1602e+09 5.5330e+12 30621
    ## + bathrooms:centralAir   1 2.1390e+09 5.5330e+12 30621
    ## - rooms                  1 1.4087e+10 5.5492e+12 30621
    ## + bathrooms:rooms        1 1.5272e+09 5.5336e+12 30621
    ## + bedrooms:centralAir    1 1.4345e+09 5.5337e+12 30621
    ## + lotSize:centralAir     1 8.3731e+08 5.5343e+12 30621
    ## + rooms:centralAir       1 7.9818e+08 5.5343e+12 30621
    ## + livingArea:rooms       1 7.3006e+08 5.5344e+12 30621
    ## + livingArea:bathrooms   1 5.7700e+08 5.5345e+12 30621
    ## - pctCollege:bathrooms   1 1.5628e+10 5.5507e+12 30621
    ## + livingArea:pctCollege  1 2.9866e+08 5.5348e+12 30621
    ## + livingArea:bedrooms    1 2.4016e+08 5.5349e+12 30621
    ## + pctCollege:rooms       1 1.6278e+08 5.5350e+12 30621
    ## + pctCollege:bedrooms    1 1.5076e+08 5.5350e+12 30621
    ## + fireplaces:rooms       1 1.3749e+08 5.5350e+12 30621
    ## + bedrooms:bathrooms     1 9.9488e+07 5.5350e+12 30621
    ## + lotSize:age            1 4.8119e+07 5.5351e+12 30621
    ## + lotSize:rooms          1 4.0355e+07 5.5351e+12 30621
    ## + bedrooms:rooms         1 2.7367e+07 5.5351e+12 30621
    ## + age:fireplaces         1 3.4038e+06 5.5351e+12 30621
    ## + pctCollege:centralAir  1 1.3065e+06 5.5351e+12 30621
    ## + age:rooms              1 1.2386e+04 5.5351e+12 30621
    ## + pctCollege:heating     2 7.9546e+09 5.5272e+12 30621
    ## + lotSize:heating        2 7.3983e+09 5.5277e+12 30621
    ## + age:heating            2 6.5350e+09 5.5286e+12 30622
    ## + lotSize:fuel           2 5.8205e+09 5.5293e+12 30622
    ## + rooms:fuel             2 5.6330e+09 5.5295e+12 30622
    ## + fireplaces:heating     2 5.2089e+09 5.5299e+12 30622
    ## + fireplaces:fuel        2 4.7101e+09 5.5304e+12 30622
    ## + heating:centralAir     2 4.2661e+09 5.5309e+12 30622
    ## + livingArea:heating     2 2.3742e+09 5.5327e+12 30623
    ## - fuel:centralAir        2 3.0125e+10 5.5652e+12 30623
    ## + bedrooms:heating       2 9.7796e+08 5.5341e+12 30623
    ## + bathrooms:fuel         2 1.2961e+08 5.5350e+12 30623
    ## - pctCollege:fuel        2 3.4607e+10 5.5697e+12 30624
    ## - lotSize                1 2.6676e+10 5.5618e+12 30624
    ## + heating:fuel           3 4.9416e+09 5.5302e+12 30624
    ## - bathrooms:heating      2 4.2518e+10 5.5776e+12 30626
    ## - age:fuel               2 4.2709e+10 5.5778e+12 30626
    ## - age:centralAir         1 3.9267e+10 5.5744e+12 30627
    ## - livingArea:centralAir  1 5.6913e+10 5.5920e+12 30631
    ## - bedrooms:fireplaces    1 5.9559e+10 5.5947e+12 30632
    ## - pctCollege:fireplaces  1 7.3715e+10 5.6088e+12 30636
    ## - age:pctCollege         1 7.7754e+10 5.6129e+12 30637
    ## - livingArea:fuel        2 9.2178e+10 5.6273e+12 30638
    ## - livingArea:fireplaces  1 8.8142e+10 5.6233e+12 30639
    ## 
    ## Step:  AIC=30618.71
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces + pctCollege:fuel + age:centralAir + 
    ##     fuel:centralAir + pctCollege:bathrooms + lotSize:fireplaces
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + lotSize:bedrooms       1 2.1443e+10 5.5038e+12 30615
    ## + rooms:heating          2 1.8114e+10 5.5071e+12 30618
    ## <none>                                5.5252e+12 30619
    ## - lotSize:fireplaces     1 9.9212e+09 5.5351e+12 30619
    ## + age:bedrooms           1 3.8549e+09 5.5213e+12 30620
    ## + fireplaces:centralAir  1 3.3161e+09 5.5219e+12 30620
    ## + bedrooms:fuel          2 1.1233e+10 5.5140e+12 30620
    ## - rooms                  1 1.3077e+10 5.5383e+12 30620
    ## + age:bathrooms          1 2.7396e+09 5.5225e+12 30620
    ## + fireplaces:bathrooms   1 2.1696e+09 5.5230e+12 30620
    ## + age:livingArea         1 2.1203e+09 5.5231e+12 30620
    ## + bathrooms:centralAir   1 1.8103e+09 5.5234e+12 30620
    ## + bedrooms:centralAir    1 1.5248e+09 5.5237e+12 30620
    ## + lotSize:rooms          1 1.4692e+09 5.5237e+12 30620
    ## + bathrooms:rooms        1 1.0831e+09 5.5241e+12 30620
    ## + lotSize:pctCollege     1 1.0595e+09 5.5241e+12 30620
    ## + rooms:centralAir       1 8.2560e+08 5.5244e+12 30621
    ## + livingArea:bathrooms   1 4.5496e+08 5.5247e+12 30621
    ## + livingArea:rooms       1 4.3806e+08 5.5248e+12 30621
    ## + lotSize:livingArea     1 3.3817e+08 5.5249e+12 30621
    ## + livingArea:bedrooms    1 3.3609e+08 5.5249e+12 30621
    ## + pctCollege:rooms       1 2.6153e+08 5.5249e+12 30621
    ## + livingArea:pctCollege  1 2.4347e+08 5.5250e+12 30621
    ## + pctCollege:bedrooms    1 1.9755e+08 5.5250e+12 30621
    ## + bedrooms:bathrooms     1 1.5452e+08 5.5250e+12 30621
    ## + age:rooms              1 1.0821e+08 5.5251e+12 30621
    ## + lotSize:bathrooms      1 7.9878e+07 5.5251e+12 30621
    ## + lotSize:centralAir     1 4.9136e+07 5.5251e+12 30621
    ## + fireplaces:rooms       1 2.8704e+07 5.5252e+12 30621
    ## + bedrooms:rooms         1 7.8806e+06 5.5252e+12 30621
    ## + lotSize:age            1 7.0893e+06 5.5252e+12 30621
    ## + pctCollege:centralAir  1 6.6935e+06 5.5252e+12 30621
    ## + age:fireplaces         1 1.8739e+06 5.5252e+12 30621
    ## - pctCollege:bathrooms   1 1.6021e+10 5.5412e+12 30621
    ## + pctCollege:heating     2 7.9021e+09 5.5173e+12 30621
    ## + age:heating            2 6.3665e+09 5.5188e+12 30621
    ## + fireplaces:fuel        2 6.2595e+09 5.5189e+12 30621
    ## + lotSize:heating        2 5.1542e+09 5.5200e+12 30621
    ## + rooms:fuel             2 4.7084e+09 5.5205e+12 30622
    ## + fireplaces:heating     2 4.5550e+09 5.5206e+12 30622
    ## + heating:centralAir     2 3.9645e+09 5.5212e+12 30622
    ## + livingArea:heating     2 2.6703e+09 5.5225e+12 30622
    ## + bedrooms:heating       2 1.2818e+09 5.5239e+12 30622
    ## - fuel:centralAir        2 3.1008e+10 5.5562e+12 30622
    ## + lotSize:fuel           2 6.0066e+08 5.5246e+12 30623
    ## - pctCollege:fuel        2 3.1556e+10 5.5568e+12 30623
    ## + bathrooms:fuel         2 2.9614e+08 5.5249e+12 30623
    ## + heating:fuel           3 4.6774e+09 5.5205e+12 30624
    ## - bathrooms:heating      2 3.9369e+10 5.5646e+12 30625
    ## - age:fuel               2 4.6817e+10 5.5720e+12 30626
    ## - age:centralAir         1 4.0687e+10 5.5659e+12 30627
    ## - bedrooms:fireplaces    1 5.5312e+10 5.5805e+12 30631
    ## - livingArea:centralAir  1 5.9476e+10 5.5847e+12 30632
    ## - pctCollege:fireplaces  1 7.4804e+10 5.6000e+12 30635
    ## - age:pctCollege         1 7.8545e+10 5.6037e+12 30636
    ## - livingArea:fuel        2 8.6705e+10 5.6119e+12 30636
    ## - livingArea:fireplaces  1 9.0660e+10 5.6159e+12 30639
    ## 
    ## Step:  AIC=30615.34
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces + pctCollege:fuel + age:centralAir + 
    ##     fuel:centralAir + pctCollege:bathrooms + lotSize:fireplaces + 
    ##     lotSize:bedrooms
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## + lotSize:livingArea     1 1.1727e+10 5.4920e+12 30614
    ## + rooms:heating          2 1.7979e+10 5.4858e+12 30615
    ## <none>                                5.5038e+12 30615
    ## + livingArea:bedrooms    1 4.9425e+09 5.4988e+12 30616
    ## + age:bathrooms          1 3.2306e+09 5.5005e+12 30617
    ## + lotSize:rooms          1 3.1939e+09 5.5006e+12 30617
    ## + fireplaces:centralAir  1 3.1720e+09 5.5006e+12 30617
    ## - rooms                  1 1.3772e+10 5.5175e+12 30617
    ## + fireplaces:bathrooms   1 2.0432e+09 5.5017e+12 30617
    ## + bedrooms:rooms         1 2.0289e+09 5.5017e+12 30617
    ## + bathrooms:centralAir   1 2.0183e+09 5.5017e+12 30617
    ## + age:livingArea         1 1.8505e+09 5.5019e+12 30617
    ## + lotSize:bathrooms      1 1.7506e+09 5.5020e+12 30617
    ## + pctCollege:bedrooms    1 1.5555e+09 5.5022e+12 30617
    ## + age:bedrooms           1 1.1730e+09 5.5026e+12 30617
    ## + bathrooms:rooms        1 1.1274e+09 5.5026e+12 30617
    ## + rooms:centralAir       1 9.8903e+08 5.5028e+12 30617
    ## + bedrooms:centralAir    1 9.2146e+08 5.5028e+12 30617
    ## + lotSize:age            1 9.1307e+08 5.5028e+12 30617
    ## + lotSize:pctCollege     1 7.4353e+08 5.5030e+12 30617
    ## + age:rooms              1 5.9748e+08 5.5032e+12 30617
    ## + livingArea:pctCollege  1 2.7347e+08 5.5035e+12 30617
    ## + lotSize:heating        2 8.2052e+09 5.4955e+12 30617
    ## + livingArea:bathrooms   1 1.7741e+08 5.5036e+12 30617
    ## + fireplaces:rooms       1 1.5886e+08 5.5036e+12 30617
    ## + pctCollege:rooms       1 8.2272e+07 5.5037e+12 30617
    ## + bedrooms:bathrooms     1 6.6993e+07 5.5037e+12 30617
    ## + age:fireplaces         1 5.9598e+07 5.5037e+12 30617
    ## + pctCollege:heating     2 8.0178e+09 5.4957e+12 30617
    ## + pctCollege:centralAir  1 7.4158e+06 5.5037e+12 30617
    ## + lotSize:centralAir     1 7.1324e+06 5.5037e+12 30617
    ## + livingArea:rooms       1 2.4622e+06 5.5038e+12 30617
    ## - pctCollege:bathrooms   1 1.6087e+10 5.5198e+12 30617
    ## + fireplaces:fuel        2 7.7600e+09 5.4960e+12 30617
    ## + bedrooms:fuel          2 6.8417e+09 5.4969e+12 30618
    ## + age:heating            2 5.6988e+09 5.4981e+12 30618
    ## + lotSize:fuel           2 4.6373e+09 5.4991e+12 30618
    ## + heating:centralAir     2 4.0764e+09 5.4997e+12 30618
    ## + fireplaces:heating     2 3.7688e+09 5.5000e+12 30618
    ## + bedrooms:heating       2 3.4497e+09 5.5003e+12 30619
    ## + livingArea:heating     2 2.8103e+09 5.5009e+12 30619
    ## - lotSize:bedrooms       1 2.1443e+10 5.5252e+12 30619
    ## + rooms:fuel             2 2.3597e+09 5.5014e+12 30619
    ## - lotSize:fireplaces     1 2.2418e+10 5.5262e+12 30619
    ## - pctCollege:fuel        2 3.1464e+10 5.5352e+12 30619
    ## - fuel:centralAir        2 3.1608e+10 5.5354e+12 30619
    ## + bathrooms:fuel         2 8.7928e+06 5.5037e+12 30619
    ## + heating:fuel           3 4.5188e+09 5.4992e+12 30620
    ## - bathrooms:heating      2 4.0667e+10 5.5444e+12 30622
    ## - age:centralAir         1 3.8448e+10 5.5422e+12 30623
    ## - age:fuel               2 5.5351e+10 5.5591e+12 30625
    ## - bedrooms:fireplaces    1 5.4073e+10 5.5578e+12 30627
    ## - livingArea:centralAir  1 5.7151e+10 5.5609e+12 30628
    ## - pctCollege:fireplaces  1 7.7445e+10 5.5812e+12 30633
    ## - age:pctCollege         1 8.3472e+10 5.5872e+12 30634
    ## - livingArea:fireplaces  1 8.8237e+10 5.5920e+12 30635
    ## - livingArea:fuel        2 1.0430e+11 5.6081e+12 30637
    ## 
    ## Step:  AIC=30614.39
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces + pctCollege:fuel + age:centralAir + 
    ##     fuel:centralAir + pctCollege:bathrooms + lotSize:fireplaces + 
    ##     lotSize:bedrooms + lotSize:livingArea
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## - lotSize:fireplaces     1 6.7536e+09 5.4988e+12 30614
    ## + rooms:heating          2 1.6682e+10 5.4753e+12 30614
    ## <none>                                5.4920e+12 30614
    ## + lotSize:centralAir     1 5.7546e+09 5.4863e+12 30615
    ## - lotSize:livingArea     1 1.1727e+10 5.5038e+12 30615
    ## + age:bathrooms          1 3.8777e+09 5.4881e+12 30615
    ## + livingArea:bedrooms    1 3.2717e+09 5.4888e+12 30616
    ## + fireplaces:centralAir  1 2.8614e+09 5.4892e+12 30616
    ## + pctCollege:bedrooms    1 2.6446e+09 5.4894e+12 30616
    ## + age:livingArea         1 1.8909e+09 5.4901e+12 30616
    ## + bathrooms:rooms        1 1.8194e+09 5.4902e+12 30616
    ## + bedrooms:rooms         1 1.7808e+09 5.4902e+12 30616
    ## - rooms                  1 1.4274e+10 5.5063e+12 30616
    ## + bathrooms:centralAir   1 1.5687e+09 5.4905e+12 30616
    ## + lotSize:age            1 1.4293e+09 5.4906e+12 30616
    ## + fireplaces:bathrooms   1 1.1621e+09 5.4909e+12 30616
    ## + bedrooms:centralAir    1 1.1227e+09 5.4909e+12 30616
    ## + age:bedrooms           1 7.3399e+08 5.4913e+12 30616
    ## - pctCollege:bathrooms   1 1.5220e+10 5.5072e+12 30616
    ## + livingArea:bathrooms   1 6.7202e+08 5.4914e+12 30616
    ## + lotSize:rooms          1 6.5031e+08 5.4914e+12 30616
    ## + rooms:centralAir       1 6.2635e+08 5.4914e+12 30616
    ## + pctCollege:heating     2 8.4495e+09 5.4836e+12 30616
    ## + livingArea:pctCollege  1 4.2712e+08 5.4916e+12 30616
    ## + age:rooms              1 4.2543e+08 5.4916e+12 30616
    ## + fireplaces:rooms       1 2.1495e+08 5.4918e+12 30616
    ## + livingArea:rooms       1 2.0298e+08 5.4918e+12 30616
    ## + lotSize:pctCollege     1 4.5734e+07 5.4920e+12 30616
    ## + lotSize:bathrooms      1 1.9655e+07 5.4920e+12 30616
    ## + pctCollege:rooms       1 1.5390e+07 5.4920e+12 30616
    ## + age:fireplaces         1 1.4629e+07 5.4920e+12 30616
    ## + pctCollege:centralAir  1 4.9129e+05 5.4920e+12 30616
    ## + bedrooms:bathrooms     1 1.2617e+05 5.4920e+12 30616
    ## + fireplaces:fuel        2 7.7127e+09 5.4843e+12 30616
    ## + bedrooms:fuel          2 6.4616e+09 5.4856e+12 30617
    ## + age:heating            2 5.5038e+09 5.4865e+12 30617
    ## + fireplaces:heating     2 4.5031e+09 5.4875e+12 30617
    ## + heating:centralAir     2 4.2200e+09 5.4878e+12 30617
    ## + bedrooms:heating       2 3.9576e+09 5.4881e+12 30617
    ## + livingArea:heating     2 2.5115e+09 5.4895e+12 30618
    ## + lotSize:heating        2 2.2313e+09 5.4898e+12 30618
    ## + rooms:fuel             2 1.7703e+09 5.4903e+12 30618
    ## + lotSize:fuel           2 1.7006e+09 5.4903e+12 30618
    ## - pctCollege:fuel        2 3.0916e+10 5.5229e+12 30618
    ## + bathrooms:fuel         2 1.2712e+07 5.4920e+12 30618
    ## - fuel:centralAir        2 3.2286e+10 5.5243e+12 30619
    ## + heating:fuel           3 5.1566e+09 5.4869e+12 30619
    ## - bathrooms:heating      2 3.9220e+10 5.5312e+12 30620
    ## - lotSize:bedrooms       1 3.2832e+10 5.5249e+12 30621
    ## - age:centralAir         1 3.7049e+10 5.5291e+12 30622
    ## - age:fuel               2 5.2797e+10 5.5448e+12 30624
    ## - bedrooms:fireplaces    1 5.4882e+10 5.5469e+12 30626
    ## - livingArea:centralAir  1 6.3571e+10 5.5556e+12 30628
    ## - pctCollege:fireplaces  1 7.7920e+10 5.5699e+12 30632
    ## - livingArea:fuel        2 8.7359e+10 5.5794e+12 30632
    ## - age:pctCollege         1 8.2678e+10 5.5747e+12 30633
    ## - livingArea:fireplaces  1 9.0164e+10 5.5822e+12 30635
    ## 
    ## Step:  AIC=30614.09
    ## price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
    ##     fireplaces + bathrooms + rooms + heating + fuel + centralAir + 
    ##     livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces + pctCollege:fuel + age:centralAir + 
    ##     fuel:centralAir + pctCollege:bathrooms + lotSize:bedrooms + 
    ##     lotSize:livingArea
    ## 
    ##                         Df  Sum of Sq        RSS   AIC
    ## <none>                                5.4988e+12 30614
    ## + rooms:heating          2 1.5372e+10 5.4834e+12 30614
    ## + lotSize:centralAir     1 6.9755e+09 5.4918e+12 30614
    ## + lotSize:fireplaces     1 6.7536e+09 5.4920e+12 30614
    ## + age:bathrooms          1 4.6673e+09 5.4941e+12 30615
    ## + age:livingArea         1 2.7860e+09 5.4960e+12 30615
    ## + lotSize:rooms          1 2.6444e+09 5.4961e+12 30615
    ## + bathrooms:rooms        1 2.4693e+09 5.4963e+12 30616
    ## + pctCollege:bedrooms    1 2.4333e+09 5.4963e+12 30616
    ## + fireplaces:centralAir  1 2.1292e+09 5.4967e+12 30616
    ## + lotSize:pctCollege     1 1.9364e+09 5.4968e+12 30616
    ## + bathrooms:centralAir   1 1.5796e+09 5.4972e+12 30616
    ## + livingArea:bedrooms    1 1.5493e+09 5.4972e+12 30616
    ## + lotSize:age            1 1.4380e+09 5.4973e+12 30616
    ## + age:bedrooms           1 1.2867e+09 5.4975e+12 30616
    ## - pctCollege:bathrooms   1 1.4668e+10 5.5134e+12 30616
    ## + bedrooms:centralAir    1 1.2641e+09 5.4975e+12 30616
    ## + livingArea:bathrooms   1 1.1299e+09 5.4977e+12 30616
    ## - rooms                  1 1.5017e+10 5.5138e+12 30616
    ## + fireplaces:bathrooms   1 9.1528e+08 5.4979e+12 30616
    ## + livingArea:rooms       1 7.9355e+08 5.4980e+12 30616
    ## + bedrooms:rooms         1 7.1756e+08 5.4981e+12 30616
    ## + pctCollege:heating     2 8.6240e+09 5.4902e+12 30616
    ## + livingArea:pctCollege  1 5.3142e+08 5.4982e+12 30616
    ## + rooms:centralAir       1 4.7803e+08 5.4983e+12 30616
    ## + lotSize:bathrooms      1 4.5990e+08 5.4983e+12 30616
    ## + fireplaces:rooms       1 3.3206e+08 5.4984e+12 30616
    ## + age:rooms              1 8.7121e+07 5.4987e+12 30616
    ## + bedrooms:bathrooms     1 1.7460e+07 5.4988e+12 30616
    ## + age:fireplaces         1 2.2755e+06 5.4988e+12 30616
    ## + pctCollege:rooms       1 1.5062e+06 5.4988e+12 30616
    ## + pctCollege:centralAir  1 1.0144e+06 5.4988e+12 30616
    ## + bedrooms:fuel          2 6.2320e+09 5.4925e+12 30617
    ## + fireplaces:fuel        2 5.9289e+09 5.4929e+12 30617
    ## + age:heating            2 5.6317e+09 5.4931e+12 30617
    ## + fireplaces:heating     2 5.5689e+09 5.4932e+12 30617
    ## + lotSize:fuel           2 4.4643e+09 5.4943e+12 30617
    ## + heating:centralAir     2 4.4525e+09 5.4943e+12 30617
    ## + lotSize:heating        2 4.3831e+09 5.4944e+12 30617
    ## + bedrooms:heating       2 3.2066e+09 5.4956e+12 30617
    ## + rooms:fuel             2 2.2693e+09 5.4965e+12 30618
    ## + livingArea:heating     2 2.2188e+09 5.4966e+12 30618
    ## - fuel:centralAir        2 3.1806e+10 5.5306e+12 30618
    ## + bathrooms:fuel         2 1.7378e+06 5.4988e+12 30618
    ## - pctCollege:fuel        2 3.2707e+10 5.5315e+12 30618
    ## + heating:fuel           3 5.5832e+09 5.4932e+12 30619
    ## - lotSize:livingArea     1 2.7392e+10 5.5262e+12 30619
    ## - lotSize:bedrooms       1 3.1028e+10 5.5298e+12 30620
    ## - bathrooms:heating      2 4.0448e+10 5.5392e+12 30620
    ## - age:centralAir         1 3.6085e+10 5.5349e+12 30621
    ## - age:fuel               2 4.8154e+10 5.5469e+12 30622
    ## - bedrooms:fireplaces    1 5.8358e+10 5.5571e+12 30627
    ## - livingArea:centralAir  1 6.4969e+10 5.5637e+12 30628
    ## - livingArea:fuel        2 8.2755e+10 5.5815e+12 30631
    ## - pctCollege:fireplaces  1 7.6827e+10 5.5756e+12 30631
    ## - age:pctCollege         1 8.0938e+10 5.5797e+12 30632
    ## - livingArea:fireplaces  1 8.9548e+10 5.5883e+12 30634

# the scope statement says:

# “consider all two-way interactions for everything in lm\_medium (.)

# what variables are included?

    getCall(lm_step)

    ## lm(formula = price ~ lotSize + age + livingArea + pctCollege + 
    ##     bedrooms + fireplaces + bathrooms + rooms + heating + fuel + 
    ##     centralAir + livingArea:centralAir + age:pctCollege + livingArea:fuel + 
    ##     age:fuel + pctCollege:fireplaces + bathrooms:heating + livingArea:fireplaces + 
    ##     bedrooms:fireplaces + pctCollege:fuel + age:centralAir + 
    ##     fuel:centralAir + pctCollege:bathrooms + lotSize:bedrooms + 
    ##     lotSize:livingArea, data = saratoga_train)

    coef(lm_step)

    ##                      (Intercept)                          lotSize 
    ##                      10678.32197                       -290.93848 
    ##                              age                       livingArea 
    ##                      -1949.50878                         99.70343 
    ##                       pctCollege                         bedrooms 
    ##                       -298.87046                      -8684.56565 
    ##                       fireplaces                        bathrooms 
    ##                      97475.70330                       -790.99609 
    ##                            rooms           heatinghot water/steam 
    ##                       2298.27304                      26924.67511 
    ##                  heatingelectric                     fuelelectric 
    ##                      50839.96254                      15344.67134 
    ##                          fueloil                     centralAirNo 
    ##                     149682.52882                      24840.74742 
    ##          livingArea:centralAirNo                   age:pctCollege 
    ##                        -28.03113                         29.69133 
    ##          livingArea:fuelelectric               livingArea:fueloil 
    ##                         12.70606                        -41.98961 
    ##                 age:fuelelectric                      age:fueloil 
    ##                        557.21788                       -458.86069 
    ##            pctCollege:fireplaces bathrooms:heatinghot water/steam 
    ##                      -1476.74432                     -18583.77867 
    ##        bathrooms:heatingelectric            livingArea:fireplaces 
    ##                     -24713.89076                         22.03299 
    ##              bedrooms:fireplaces          pctCollege:fuelelectric 
    ##                     -17374.10914                      -1308.37071 
    ##               pctCollege:fueloil                 age:centralAirNo 
    ##                       -756.61258                        671.24276 
    ##        fuelelectric:centralAirNo             fueloil:centralAirNo 
    ##                        423.60845                     -40915.55313 
    ##             pctCollege:bathrooms                 lotSize:bedrooms 
    ##                        602.30425                      10222.77628 
    ##               lotSize:livingArea 
    ##                        -12.60569

    rmse(lm_medium, saratoga_test)

    ## [1] 60708.7

    rmse(lm_big, saratoga_test)

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## [1] 130302.7

    rmse(lm_forward, saratoga_test)

    ## [1] 60667.41

    rmse(lm_step, saratoga_test)

    ## [1] 60593.01

# re-split into train and test cases for best two linear models

    rmse_sim = do(10)*{
    saratoga_split = initial_split(SaratogaHouses, prop = 0.8)
    saratoga_train = training(saratoga_split)
    saratoga_test = testing(saratoga_split)

    lm_forward = update(lm_forward, data=saratoga_train)
    lm_step = update(lm_step, data=saratoga_train)

    model_errors = c(rmse(lm_forward, saratoga_test), rmse(lm_step, saratoga_test))
    model_errors
    }
    colMeans(rmse_sim)

    ##       V1       V2 
    ## 66657.43 65859.77

# Best was lm\_step which included a second degree polynomial term

    library(caret)

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:mosaic':
    ## 
    ##     dotPlot

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

    library(modelr)
    library(parallel)
    library(foreach)

# Standardize variables: Centering/Grand Mean Centering

    SaratogaHouses$p_price <- SaratogaHouses$price - mean(SaratogaHouses$price, na.rm=TRUE)

# KNN with K = 100

    knn100 = knnreg(price ~ lotSize + age + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=saratoga_train, k=100)
    modelr::rmse(knn100, saratoga_test)

    ## [1] 78809.9

    K_folds = 5

    SaratogaHouses = SaratogaHouses %>%
      mutate(fold_id = rep(1:K_folds, length=nrow(SaratogaHouses)) %>% sample)

    head(SaratogaHouses)

    ##    price lotSize age landValue livingArea pctCollege bedrooms fireplaces
    ## 1 132500    0.09  42     50000        906         35        2          1
    ## 2 181115    0.92   0     22300       1953         51        3          0
    ## 3 109000    0.19 133      7300       1944         51        4          1
    ## 4 155000    0.41  13     18700       1944         51        3          1
    ## 5  86060    0.11   0     15000        840         51        2          0
    ## 6 120000    0.68  31     14000       1152         22        4          1
    ##   bathrooms rooms         heating     fuel             sewer waterfront
    ## 1       1.0     5        electric electric            septic         No
    ## 2       2.5     6 hot water/steam      gas            septic         No
    ## 3       1.0     8 hot water/steam      gas public/commercial         No
    ## 4       1.5     5         hot air      gas            septic         No
    ## 5       1.0     3         hot air      gas public/commercial         No
    ## 6       1.0     8         hot air      gas            septic         No
    ##   newConstruction centralAir    p_price fold_id
    ## 1              No         No  -79466.71       3
    ## 2              No         No  -30851.71       1
    ## 3              No         No -102966.71       4
    ## 4              No         No  -56966.71       5
    ## 5             Yes        Yes -125906.71       1
    ## 6              No         No  -91966.71       5

    rmse_cv = foreach(fold = 1:K_folds, .combine='c') %do% {
    knn100 = knnreg(price ~ lotSize + age + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=filter(SaratogaHouses, fold_id == fold), k=100)
    modelr::rmse(knn100, data=filter(SaratogaHouses, fold_id == fold))
    }

    rmse_cv

    ## [1] 66600.49 83103.90 73861.22 72082.76 71347.09

    mean(rmse_cv) 

    ## [1] 73399.09

    sd(rmse_cv)/sqrt(K_folds)

    ## [1] 2707.04

    saratoga_folds = crossv_kfold(SaratogaHouses, k=K_folds)

    # map the model-fitting function over the training sets
    models = map(saratoga_folds$train, ~ knnreg(price ~ lotSize + age + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + centralAir, k=100, data = ., use.all=FALSE))


    # map the RMSE calculation over the trained models and test sets simultaneously
    errs = map2_dbl(models, saratoga_folds$test, modelr::rmse)


    mean(errs)

    ## [1] 69865.37

    sd(errs)/sqrt(K_folds)

    ## [1] 2743.931

    k_grid = c(2, 3, 4, 6, 8, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 250, 300)

    # Notice I used the same folds for each value of k
    cv_grid = foreach(k = k_grid, .combine='rbind') %dopar% {
    models = map(saratoga_folds$train, ~ knnreg(price ~ lotSize + age + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + centralAir, k=k, data = ., use.all=FALSE))
    errs = map2_dbl(models, saratoga_folds$test, modelr::rmse)
    c(k=k, err = mean(errs), std_err = sd(errs)/sqrt(K_folds))
    } %>% as.data.frame

    ## Warning: executing %dopar% sequentially: no parallel backend registered

    head(cv_grid)

    ##           k      err  std_err
    ## result.1  2 76593.18 2481.162
    ## result.2  3 75126.31 1611.367
    ## result.3  4 72726.86 1734.571
    ## result.4  6 70793.13 1507.432
    ## result.5  8 69866.11 1759.697
    ## result.6 10 69181.83 2381.138

    # plot means and std errors versus k
    ggplot(cv_grid) + geom_point(aes(x=k, y=err)) + geom_errorbar(aes(x=k, ymin = err-std_err, ymax = err+std_err)) + scale_x_log10()

![](Exercises-2_files/figure-markdown_strict/unnamed-chunk-19-1.png)

    ## The cv_grid and the plot show k=15 has the lowest error

# KNN k=15

    knn15 = knnreg(price ~ lotSize + age + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=saratoga_train, k=15)
    modelr::rmse(knn15, saratoga_test)

    ## [1] 76002.42

## While k=15 did the best for KNN regression, it was the linear models that did better at achieving lower out-of-sample mean-squared error

## The stepwise selection linear model with a second degree polynomial performed best

#### Classification and Retrospective Sampling

    library(readr)
    german_credit <- read_csv("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/german_credit.csv")

    ## New names:
    ## Rows: 1000 Columns: 23
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (13): checkingstatus1, history, purpose, savings, employ, status, others... dbl
    ## (9): ...1, Default, duration, amount, installment, residence, age, card... lgl
    ## (1): rent
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

    View(german_credit)

    library(mosaic)
    library(ggplot2)
    library(caret)
    library(dplyr)

    default_probability = german_credit %>%
    group_by(history) %>%
    summarise(default_probability = mean(Default))

    table(default_probability)

    ##           default_probability
    ## history    0.170648464163823 0.318770226537217 0.595505617977528
    ##   good                     0                 0                 1
    ##   poor                     0                 1                 0
    ##   terrible                 1                 0                 0

    ggplot(default_probability) + geom_col(aes(x=history, y=default_probability)) + labs(x="Credit History", y="Default Probability", title="Default Probability by Credit History")

![](Exercises-2_files/figure-markdown_strict/unnamed-chunk-23-1.png)

    credit_split = initial_split(german_credit, prop = 0.8)
    credit_train = training(credit_split)
    credit_test = testing(credit_split)

    logit_default = glm(Default ~ duration + amount + installment + age + history + purpose + foreign, data=credit_train, family='binomial')
    summary(logit_default)

    ## 
    ## Call:
    ## glm(formula = Default ~ duration + amount + installment + age + 
    ##     history + purpose + foreign, family = "binomial", data = credit_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3775  -0.7948  -0.5646   1.0244   2.4806  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.284e+00  5.553e-01  -2.312 0.020779 *  
    ## duration             2.292e-02  8.983e-03   2.551 0.010735 *  
    ## amount               1.232e-04  4.011e-05   3.070 0.002139 ** 
    ## installment          2.458e-01  8.623e-02   2.851 0.004363 ** 
    ## age                 -1.786e-02  8.047e-03  -2.219 0.026482 *  
    ## historypoor         -8.906e-01  2.708e-01  -3.289 0.001005 ** 
    ## historyterrible     -1.814e+00  3.142e-01  -5.772 7.84e-09 ***
    ## purposeedu           9.271e-01  4.226e-01   2.194 0.028245 *  
    ## purposegoods/repair  2.852e-01  3.058e-01   0.933 0.351068    
    ## purposenewcar        1.110e+00  3.257e-01   3.408 0.000656 ***
    ## purposeusedcar      -6.198e-01  4.062e-01  -1.526 0.127051    
    ## foreigngerman       -1.193e+00  5.848e-01  -2.039 0.041401 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 972.25  on 799  degrees of freedom
    ## Residual deviance: 848.91  on 788  degrees of freedom
    ## AIC: 872.91
    ## 
    ## Number of Fisher Scoring iterations: 4

    coef(logit_default)

    ##         (Intercept)            duration              amount         installment 
    ##       -1.2837337040        0.0229170379        0.0001231454        0.2458165697 
    ##                 age         historypoor     historyterrible          purposeedu 
    ##       -0.0178575160       -0.8906236891       -1.8136306478        0.9270947515 
    ## purposegoods/repair       purposenewcar      purposeusedcar       foreigngerman 
    ##        0.2852092235        1.1098781874       -0.6198214165       -1.1927326291

    #historyterrible effect 
    exp(-1.9763916224)

    ## [1] 0.1385683

    #historypoor effect 
    exp(-1.2312793871)

    ## [1] 0.2919189

    phat_train = predict(logit_default, credit_train, type='response')


    phat_test_logit = predict(logit_default, credit_test, type='response')
    yhat_test_logit = ifelse(phat_test_logit > 0.5, 1, 0)
    confusion_out_logit = table(Default = credit_test$Default, yhat = yhat_test_logit)
    confusion_out_logit

    ##        yhat
    ## Default   0   1
    ##       0 126  11
    ##       1  49  14

    #Error rate is 29% and we have an accuracy of 72%

    sum(diag(confusion_out_logit))/sum(confusion_out_logit)

    ## [1] 0.7

    table(credit_train$Default)

    ## 
    ##   0   1 
    ## 563 237

    table(credit_test$Default)

    ## 
    ##   0   1 
    ## 137  63

    233/(567+233)

    ## [1] 0.29125

    67/sum(table(credit_test$Default))

    ## [1] 0.335

    table(german_credit$history)

    ## 
    ##     good     poor terrible 
    ##       89      618      293

    table(credit_train$history)

    ## 
    ##     good     poor terrible 
    ##       71      499      230

    table(credit_test$history)

    ## 
    ##     good     poor terrible 
    ##       18      119       63

    table(german_credit$Default)

    ## 
    ##   0   1 
    ## 700 300

# Credit History has small coefficients that reduced the probability of default for example a “terrible” credit history has an effect of 0.14 while a “poor” credit history has an effect of 0.33

# From the bar plot, we see that there is a much larger probability of default for those with “good” credit history

# However, this seems more like a result from poor sampling rather than reflecting reality. The accuracy of our logistic regression model will be low and provide incorrect estimates of the effects.

# The bank should add many more observations and include less actual defaults or defaults of those with good credit scores and opt for a more balanced sample.

#### Children and Hotel Reservations

    library(readr)
    hotels_dev <- read_csv("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/hotels_dev.csv")

    ## Rows: 45000 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (9): hotel, meal, market_segment, distribution_channel, reserved_room_...
    ## dbl  (12): lead_time, stays_in_weekend_nights, stays_in_week_nights, adults,...
    ## date  (1): arrival_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    View(hotels_dev)

    library(readr)
    hotels_val <- read_csv("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/hotels_val.csv")

    ## Rows: 4999 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (9): hotel, meal, market_segment, distribution_channel, reserved_room_...
    ## dbl  (12): lead_time, stays_in_weekend_nights, stays_in_week_nights, adults,...
    ## date  (1): arrival_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    View(hotels_val)

    baseline_1_test = do(5)*{
    hotels_dev_split = initial_split(hotels_dev, prop=0.8)
    hotels_dev_train = training(hotels_dev_split)
    hotels_dev_test = testing(hotels_dev_split)
    baseline_1_model = lm(children ~ market_segment + adults + customer_type + is_repeated_guest, data = hotels_dev_train)
    err_1 = rmse(baseline_1_model, hotels_dev_test)
    }

    ## Using parallel package.
    ##   * Set seed with set.rseed().
    ##   * Disable this message with options(`mosaic:parallelMessage` = FALSE)

    rmse_baseline_1 = colMeans(baseline_1_test)

    baseline_2_test = do(5)*{
    hotels_dev_split = initial_split(hotels_dev, prop=0.8)
    hotels_dev_train = training(hotels_dev_split)
    hotels_dev_test = testing(hotels_dev_split)
    baseline_2_model = lm(children ~ - arrival_date, data = hotels_dev_train)
    err_2 = rmse(baseline_2_model, hotels_dev_test)
    }

    ## Using parallel package.
    ##   * Set seed with set.rseed().
    ##   * Disable this message with options(`mosaic:parallelMessage` = FALSE)

    rmse_baseline_2 = colMeans(baseline_2_test)

    set.seed(123)
    baseline_3_test = do(5)*{
    hotels_dev_split = initial_split(hotels_dev, prop=0.8)
    hotels_dev_train = training(hotels_dev_split)
    hotels_dev_test = testing(hotels_dev_split)
    baseline_3_model = lm(children ~ . - arrival_date - deposit_type + adults*reserved_room_type + hotel*reserved_room_type + adults*distribution_channel, data = hotels_dev_train)
    err_3 = rmse(baseline_3_model, hotels_dev_test)
    }

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Using parallel package.
    ##   * Set seed with set.rseed().
    ##   * Disable this message with options(`mosaic:parallelMessage` = FALSE)

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    rmse_baseline_3 = colMeans(baseline_3_test)

# Model Validation: Step 1

    baseline_3_model = lm(children ~ . - arrival_date - deposit_type + adults*reserved_room_type + hotel*reserved_room_type + adults*distribution_channel, data = hotels_dev_train)
    phat_baseline_3 = predict(baseline_3_model, hotels_val, type="response")

    ## Warning in predict.lm(baseline_3_model, hotels_val, type = "response"):
    ## prediction from a rank-deficient fit may be misleading

    values = seq(0.99, 0.01, by = -0.01)
    roc = foreach(thresh = values, .combine = "rbind") %do% {
    yhat_baseline_3 = ifelse(phat_baseline_3 > thresh, 1, 0)
    confusion_out_baseline_3 = table(y = hotels_val$children, yhat = yhat_baseline_3)
    accuracy_rate = (confusion_out_baseline_3[1,1] + confusion_out_baseline_3[2,2])/(confusion_out_baseline_3[1,1] + confusion_out_baseline_3[2,2])
    TPR = confusion_out_baseline_3[2,2]/confusion_out_baseline_3[2,1] + confusion_out_baseline_3[2,2]
    FPR = confusion_out_baseline_3[1,2]/confusion_out_baseline_3[1,2] + confusion_out_baseline_3[1,1]
    df_rates = data.frame(TPR, FPR)
    rbind(df_rates)
    }

    ggplot(roc, aes(x=FPR, y=TPR)) + geom_line() + labs(x="False Positive Rate", y="True Positive Rate") + ggtitle("ROC Curve")

![](Exercises-2_files/figure-markdown_strict/unnamed-chunk-34-1.png)

# Model Validation: Step 2

    k_folds = 20
    hotels_val = hotels_val %>%
      mutate(fold_number = rep(1:k_folds, length = nrow(hotels_val)) %>% sample())
    actual = list()
    expected = list()
    difference = list()

    for (x in 1:20) {
    fold = hotels_val %>%
      filter(fold_number == x)
    phat = predict(baseline_3_model, fold)
    expected[[x]] = round(sum(phat), 2)
    actual[[x]] = sum(fold$children)
    difference[[x]] = round(expected[[x]] - actual[[x]], 2)
    }

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    ## Warning in predict.lm(baseline_3_model, fold): prediction from a rank-deficient
    ## fit may be misleading

    fold_id = list(seq(1, 20, by=1))
    df_predicts = data.frame("Fold_ID" = unlist(fold_id), "Expected" = unlist(expected), "Actual" = unlist(actual), "Difference" = unlist(difference))
    library(ggpubr)
    ggtexttable(df_predicts, theme = ttheme(base_size = 7, padding = unit(c(2, 1.25), "mm")), rows = NULL)

![](Exercises-2_files/figure-markdown_strict/unnamed-chunk-37-1.png)
