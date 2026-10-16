

## Case 2 Inflation & Loss Cost Trend


set.seed(4663529) # set the seed to allow ability to replicate results

## Create inflation & loss cost trend 


Cal_Yr_Trend_Case_Inflation <- expand_grid(Cal_Yr = 2001:2060) |>
  group_by(Cal_Yr) |>
  
  ## Assume that inflation and loss cost trend are multiplicative and that they 
  ## both follow the lognormal distribution with different parameters 
  
  ## Assume that the average payment within a year is mid-year.
  
  mutate( Cal_Yr_Infl_Chng = 2022,
          Infl_Index_beg = rlnorm(1,.025,.01),
          Infl_Index_chng_1 = rlnorm(1,.06,.01),
          Infl_Index_chng_2 = rlnorm(1,.05,.01),
          Infl_Index_chng_3 = rlnorm(1,.04,.01),
          
          Loss_Cost_Adj_beg = rlnorm(1,.02,.005),
          Loss_Cost_Adj_chng_1 = rlnorm(1,.02,.005),
          Loss_Cost_Adj_chng_2 = rlnorm(1,.020,.005),
          Loss_Cost_Adj_chng_3 = rlnorm(1,.02,.005),
          
          
          Infl_Index_rvsd = case_when (
            Cal_Yr < Cal_Yr_Infl_Chng ~  Infl_Index_beg,
            Cal_Yr == Cal_Yr_Infl_Chng ~  Infl_Index_chng_1,
            Cal_Yr == Cal_Yr_Infl_Chng +1 ~ Infl_Index_chng_2,
            Cal_Yr == Cal_Yr_Infl_Chng +2  ~ Infl_Index_chng_3,
            Cal_Yr >= Cal_Yr_Infl_Chng  +2 ~ Infl_Index_beg
          ),
          
          Loss_Cost_Adj_rvsd = case_when (
            Cal_Yr < Cal_Yr_Infl_Chng ~  Loss_Cost_Adj_beg,
            Cal_Yr == Cal_Yr_Infl_Chng ~  Loss_Cost_Adj_chng_1,
            Cal_Yr == Cal_Yr_Infl_Chng +1 ~ Loss_Cost_Adj_chng_2,
            Cal_Yr == Cal_Yr_Infl_Chng +2  ~ Loss_Cost_Adj_chng_3,
            Cal_Yr >= Cal_Yr_Infl_Chng  +2 ~ Loss_Cost_Adj_beg
          ), 
          
          Loss_Cost_Trend_rvsd = Infl_Index_rvsd* Loss_Cost_Adj_rvsd,
          
          Infl_Index_Mid_rvsd =Infl_Index_rvsd **.5,
          Loss_Cost_Adj_Mid_rvsd =Loss_Cost_Adj_rvsd **.5 ,
          
          Loss_Cost_Trend_Mid_rvsd =Infl_Index_Mid_rvsd * Loss_Cost_Adj_Mid_rvsd,
          
          Loss_Cost_Trend_init = Infl_Index_beg* Loss_Cost_Adj_beg,
          Infl_Index_Mid_init =Infl_Index_beg **.5,
          Loss_Cost_Adj_Mid_init =Loss_Cost_Adj_beg **.5 ,
          Loss_Cost_Trend_Mid_init =Infl_Index_Mid_init * Loss_Cost_Adj_Mid_init)

## Accumulate the annual effects of inflation and loss cost trend


Cal_Yr_Trend_Acc_Case_Inflation <- Cal_Yr_Trend_Case_Inflation |>
  ungroup() |>
  mutate(Accum_Trend_init =cumprod(Loss_Cost_Trend_init),
         Accum_Infl_Index_init =cumprod(Infl_Index_beg),
         
         Accum_Trend_rvsd =cumprod(Loss_Cost_Trend_rvsd),
         Accum_Infl_Index_rvsd =cumprod(Infl_Index_rvsd),
         
         Accum_Loss_Cost_Trend_Mid_init =if_else(Cal_Yr == min(Cal_Yr),
                                                 Loss_Cost_Trend_Mid_init,
                                                 Loss_Cost_Trend_Mid_init* lag(Accum_Trend_init)),
         Accum_Infl_Index_Mid_init =if_else(Cal_Yr == min(Cal_Yr),
                                            Infl_Index_Mid_init,
                                            Infl_Index_Mid_init* lag(Accum_Infl_Index_init)),
         
         Accum_Loss_Cost_Trend_Mid_rvsd =if_else(Cal_Yr == min(Cal_Yr),
                                                 Loss_Cost_Trend_Mid_rvsd,
                                                 Loss_Cost_Trend_Mid_rvsd* lag(Accum_Trend_rvsd)),
         Accum_Infl_Index_Mid_rvsd =if_else(Cal_Yr == min(Cal_Yr),
                                            Infl_Index_Mid_rvsd,
                                            Infl_Index_Mid_rvsd* lag(Accum_Infl_Index_rvsd)))


Cal_Yr_Infl_Case_Inflation <- Cal_Yr_Trend_Acc_Case_Inflation |>
  select( Cal_Yr,
          Infl_Index_rvsd,
          Infl_Index_Mid_rvsd,
          Accum_Infl_Index_rvsd,
          Accum_Infl_Index_Mid_rvsd) |>
  rename( Infl_Index = Infl_Index_rvsd,
          Infl_Index_Mid =  Infl_Index_Mid_rvsd,
          Accum_Infl_Index = Accum_Infl_Index_rvsd,
          Accum_Infl_Index_Mid = Accum_Infl_Index_Mid_rvsd )

write_xlsx(Cal_Yr_Infl_Case_Inflation,  "Cal_Yr_Infl_Case_Inflation.xlsx")



## Case 5 Inflation & Loss Cost Trend


set.seed(4663529) # set the seed to allow ability to replicate results

## Create inflation & loss cost trend 


Cal_Yr_Trend_Case_Underwriting <- expand_grid(Cal_Yr = 2001:2060) |>
  group_by(Cal_Yr) |>
  
  ## Assume that inflation and loss cost trend are multiplicative and that they 
  ## both follow the lognormal distribution with different parameters 
  
  ## Assume that the average payment within a year is mid-year.
  
  mutate( Cal_Yr_Infl_Chng = 2061,
          Infl_Index_beg = rlnorm(1,.03,.01),
          Infl_Index_chng_1 = rlnorm(1,.06,.01),
          Infl_Index_chng_2 = rlnorm(1,.05,.01),
          Infl_Index_chng_3 = rlnorm(1,.04,.01),
          
          Loss_Cost_Adj_beg = rlnorm(1,.015,.005),
          Loss_Cost_Adj_chng_1 = rlnorm(1,.03,.005),
          Loss_Cost_Adj_chng_2 = rlnorm(1,.025,.005),
          Loss_Cost_Adj_chng_3 = rlnorm(1,.02,.005),
          
          
          Infl_Index_rvsd = case_when (
            Cal_Yr < Cal_Yr_Infl_Chng ~  Infl_Index_beg,
            Cal_Yr == Cal_Yr_Infl_Chng ~  Infl_Index_chng_1,
            Cal_Yr == Cal_Yr_Infl_Chng +1 ~ Infl_Index_chng_2,
            Cal_Yr == Cal_Yr_Infl_Chng +2  ~ Infl_Index_chng_3,
            Cal_Yr >= Cal_Yr_Infl_Chng  +2 ~ Infl_Index_beg
          ),
          
          Loss_Cost_Adj_rvsd = case_when (
            Cal_Yr < Cal_Yr_Infl_Chng ~  Loss_Cost_Adj_beg,
            Cal_Yr == Cal_Yr_Infl_Chng ~  Loss_Cost_Adj_chng_1,
            Cal_Yr == Cal_Yr_Infl_Chng +1 ~ Loss_Cost_Adj_chng_2,
            Cal_Yr == Cal_Yr_Infl_Chng +2  ~ Loss_Cost_Adj_chng_3,
            Cal_Yr >= Cal_Yr_Infl_Chng  +2 ~ Loss_Cost_Adj_beg
          ), 
          
          Loss_Cost_Trend_rvsd = Infl_Index_rvsd* Loss_Cost_Adj_rvsd,
          
          Infl_Index_Mid_rvsd =Infl_Index_rvsd **.5,
          Loss_Cost_Adj_Mid_rvsd =Loss_Cost_Adj_rvsd **.5 ,
          
          Loss_Cost_Trend_Mid_rvsd =Infl_Index_Mid_rvsd * Loss_Cost_Adj_Mid_rvsd,
          
          Loss_Cost_Trend_init = Infl_Index_beg* Loss_Cost_Adj_beg,
          Infl_Index_Mid_init =Infl_Index_beg **.5,
          Loss_Cost_Adj_Mid_init =Loss_Cost_Adj_beg **.5 ,
          Loss_Cost_Trend_Mid_init =Infl_Index_Mid_init * Loss_Cost_Adj_Mid_init)

## Accumulate the annual effects of inflation and loss cost trend


Cal_Yr_Trend_Acc_Case_Underwriting <- Cal_Yr_Trend_Case_Underwriting |>
  ungroup() |>
  mutate(Accum_Trend_init =cumprod(Loss_Cost_Trend_init),
         Accum_Infl_Index_init =cumprod(Infl_Index_beg),
         
         Accum_Trend_rvsd =cumprod(Loss_Cost_Trend_rvsd),
         Accum_Infl_Index_rvsd =cumprod(Infl_Index_rvsd),
         
         Accum_Loss_Cost_Trend_Mid_init =if_else(Cal_Yr == min(Cal_Yr),
                                                 Loss_Cost_Trend_Mid_init,
                                                 Loss_Cost_Trend_Mid_init* lag(Accum_Trend_init)),
         Accum_Infl_Index_Mid_init =if_else(Cal_Yr == min(Cal_Yr),
                                            Infl_Index_Mid_init,
                                            Infl_Index_Mid_init* lag(Accum_Infl_Index_init)),
         
         Accum_Loss_Cost_Trend_Mid_rvsd =if_else(Cal_Yr == min(Cal_Yr),
                                                 Loss_Cost_Trend_Mid_rvsd,
                                                 Loss_Cost_Trend_Mid_rvsd* lag(Accum_Trend_rvsd)),
         Accum_Infl_Index_Mid_rvsd =if_else(Cal_Yr == min(Cal_Yr),
                                            Infl_Index_Mid_rvsd,
                                            Infl_Index_Mid_rvsd* lag(Accum_Infl_Index_rvsd)))


Cal_Yr_Infl_Case_Underwriting <- Cal_Yr_Trend_Acc_Case_Underwriting |>
  select( Cal_Yr,
          Infl_Index_rvsd,
          Infl_Index_Mid_rvsd,
          Accum_Infl_Index_rvsd,
          Accum_Infl_Index_Mid_rvsd) |>
  rename( Infl_Index = Infl_Index_rvsd,
          Infl_Index_Mid =  Infl_Index_Mid_rvsd,
          Accum_Infl_Index = Accum_Infl_Index_rvsd,
          Accum_Infl_Index_Mid = Accum_Infl_Index_Mid_rvsd )

write_xlsx(Cal_Yr_Infl_Case_Underwriting,  "Cal_Yr_Infl_Case_Underwriting.xlsx")




## Case 6 Inflation & Loss Cost Trend


set.seed(4663529) # set the seed to allow ability to replicate results

## Create inflation & loss cost trend 


Cal_Yr_Trend_Case_Mixture <- expand_grid(Cal_Yr = 2001:2060) |>
  group_by(Cal_Yr) |>
  
  ## Assume that inflation and loss cost trend are multiplicative and that they 
  ## both follow the lognormal distribution with different parameters 
  
  ## Assume that the average payment within a year is mid-year.
  
  mutate( Cal_Yr_Infl_Chng = 2061,
          Infl_Index_beg = rlnorm(1,.03,.005),
          Infl_Index_chng_1 = rlnorm(1,.06,.01),
          Infl_Index_chng_2 = rlnorm(1,.05,.01),
          Infl_Index_chng_3 = rlnorm(1,.04,.01),
          
          Loss_Cost_Adj_beg = rlnorm(1,.02,.005),
          Loss_Cost_Adj_chng_1 = rlnorm(1,.03,.005),
          Loss_Cost_Adj_chng_2 = rlnorm(1,.025,.005),
          Loss_Cost_Adj_chng_3 = rlnorm(1,.02,.005),
          
          
          Infl_Index_rvsd = case_when (
            Cal_Yr < Cal_Yr_Infl_Chng ~  Infl_Index_beg,
            Cal_Yr == Cal_Yr_Infl_Chng ~  Infl_Index_chng_1,
            Cal_Yr == Cal_Yr_Infl_Chng +1 ~ Infl_Index_chng_2,
            Cal_Yr == Cal_Yr_Infl_Chng +2  ~ Infl_Index_chng_3,
            Cal_Yr >= Cal_Yr_Infl_Chng  +2 ~ Infl_Index_beg
          ),
          
          Loss_Cost_Adj_rvsd = case_when (
            Cal_Yr < Cal_Yr_Infl_Chng ~  Loss_Cost_Adj_beg,
            Cal_Yr == Cal_Yr_Infl_Chng ~  Loss_Cost_Adj_chng_1,
            Cal_Yr == Cal_Yr_Infl_Chng +1 ~ Loss_Cost_Adj_chng_2,
            Cal_Yr == Cal_Yr_Infl_Chng +2  ~ Loss_Cost_Adj_chng_3,
            Cal_Yr >= Cal_Yr_Infl_Chng  +2 ~ Loss_Cost_Adj_beg
          ), 
          
          Loss_Cost_Trend_rvsd = Infl_Index_rvsd* Loss_Cost_Adj_rvsd,
          
          Infl_Index_Mid_rvsd =Infl_Index_rvsd **.5,
          Loss_Cost_Adj_Mid_rvsd =Loss_Cost_Adj_rvsd **.5 ,
          
          Loss_Cost_Trend_Mid_rvsd =Infl_Index_Mid_rvsd * Loss_Cost_Adj_Mid_rvsd,
          
          Loss_Cost_Trend_init = Infl_Index_beg* Loss_Cost_Adj_beg,
          Infl_Index_Mid_init =Infl_Index_beg **.5,
          Loss_Cost_Adj_Mid_init =Loss_Cost_Adj_beg **.5 ,
          Loss_Cost_Trend_Mid_init =Infl_Index_Mid_init * Loss_Cost_Adj_Mid_init)

## Accumulate the annual effects of inflation and loss cost trend


Cal_Yr_Trend_Acc_Case_Mixture <- Cal_Yr_Trend_Case_Mixture |>
  ungroup() |>
  mutate(Accum_Trend_init =cumprod(Loss_Cost_Trend_init),
         Accum_Infl_Index_init =cumprod(Infl_Index_beg),
         
         Accum_Trend_rvsd =cumprod(Loss_Cost_Trend_rvsd),
         Accum_Infl_Index_rvsd =cumprod(Infl_Index_rvsd),
         
         Accum_Loss_Cost_Trend_Mid_init =if_else(Cal_Yr == min(Cal_Yr),
                                                 Loss_Cost_Trend_Mid_init,
                                                 Loss_Cost_Trend_Mid_init* lag(Accum_Trend_init)),
         Accum_Infl_Index_Mid_init =if_else(Cal_Yr == min(Cal_Yr),
                                            Infl_Index_Mid_init,
                                            Infl_Index_Mid_init* lag(Accum_Infl_Index_init)),
         
         Accum_Loss_Cost_Trend_Mid_rvsd =if_else(Cal_Yr == min(Cal_Yr),
                                                 Loss_Cost_Trend_Mid_rvsd,
                                                 Loss_Cost_Trend_Mid_rvsd* lag(Accum_Trend_rvsd)),
         Accum_Infl_Index_Mid_rvsd =if_else(Cal_Yr == min(Cal_Yr),
                                            Infl_Index_Mid_rvsd,
                                            Infl_Index_Mid_rvsd* lag(Accum_Infl_Index_rvsd)))


Cal_Yr_Infl_Case_Mixture <- Cal_Yr_Trend_Acc_Case_Mixture |>
  select( Cal_Yr,
          Infl_Index_rvsd,
          Infl_Index_Mid_rvsd,
          Accum_Infl_Index_rvsd,
          Accum_Infl_Index_Mid_rvsd) |>
  rename( Infl_Index = Infl_Index_rvsd,
          Infl_Index_Mid =  Infl_Index_Mid_rvsd,
          Accum_Infl_Index = Accum_Infl_Index_rvsd,
          Accum_Infl_Index_Mid = Accum_Infl_Index_Mid_rvsd )

write_xlsx(Cal_Yr_Infl_Case_Mixture,  "Cal_Yr_Infl_Case_Mixture.xlsx")


