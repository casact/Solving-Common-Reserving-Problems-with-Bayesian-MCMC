
# This program was created to build the files for the mixture reserving
# example in the call paper: Solving Common Reserving Problems with Bayesian MCMC  

# The general structure is outlined in Appendix F of that paper.

# There will be a few comments interspersed between code sections to explain
# what a given section of code is intended to accomplish, but the R coding
# itself won't be explained. 


# Comments on inverting distributions within the program.

## There are several points at which distributions are inverted
## in this program to generate the counts and amounts for a loss triangle. 
##  Those points where a distribution is inverted include: 
##  Generate the number of claims by report year lag within an accident year.
##  For each claim, the length of time the claim is open.
##  For each claim for each year it is open, the number of claim payments
##  For each claim in the year it is open, the number of payments.
##  For each each claim for each year open with at least one payment, 
##   the average payment before inflation.
##  For each calendar year, a general inflation result and the 
##   additional loss cost trend on top of the general inflation result.

## The comments explaining the parameter derivation used to drive the 
##  simulation for a given distribution are placed in the section of 
##  the program where the distribution is inverted.

## set a seed to be able to replicate the simulated data sets

set.seed(4663529) # set the seed to allow ability to replicate results

## create the counts data set 


data_counts_rpdt_Case_Mixture <- expand_grid(Acc_Yr= 2006:2025, Rpt_Yr_Lag=1:15) |>
  
  group_by(Acc_Yr, Rpt_Yr_Lag) |>
  mutate(Ln_Rpt_Yr_Lag = log(Rpt_Yr_Lag),
         Rpt_Yr_Lag_Sqrd= Rpt_Yr_Lag * Rpt_Yr_Lag,
         ## set exposure count by accident year
         
         Exp_Cnt = case_when(
           
           
           Acc_Yr == 2006 ~ 990,
           Acc_Yr == 2007 ~ 1050,
           Acc_Yr == 2008 ~ 1030,
           Acc_Yr == 2009 ~ 980,
           Acc_Yr == 2010 ~ 1000,
           Acc_Yr == 2011 ~ 970,
           Acc_Yr == 2012 ~ 1020,
           Acc_Yr == 2013 ~ 1030,
           Acc_Yr == 2014 ~ 990,
           Acc_Yr == 2015 ~ 1050,
           Acc_Yr == 2016 ~ 1030,
           Acc_Yr == 2017 ~ 1010,
           Acc_Yr == 2018 ~ 1070,
           Acc_Yr == 2019 ~ 1040,
           Acc_Yr == 2020 ~ 1030,
           Acc_Yr == 2021 ~ 1020,
           Acc_Yr == 2022 ~ 990,
           Acc_Yr == 2023 ~ 1040,
           Acc_Yr == 2024 ~ 990,
           Acc_Yr == 2025 ~ 1030),
         
         ## poisson rate of reported claims by report year
         ## base varies by accident year but same report year reporting pattern
         
         p_rate = case_when(
           Acc_Yr == 2006 ~ exp((-2.3 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2007 ~ exp((-2.6 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2008 ~ exp((-2.4 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2009 ~ exp((-2.6 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2010 ~ exp((-2.4 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2011 ~ exp((-2.3 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2012 ~ exp((-2.5 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2013 ~ exp((-2.4 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2014 ~ exp((-2.6 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2015 ~ exp((-2.3 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2016 ~ exp((-2.5 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2017 ~ exp((-2.7 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2018 ~ exp((-2.3 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2019 ~ exp((-2.5 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2020 ~ exp((-2.6 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2021 ~ exp((-2.5 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2022 ~ exp((-2.4 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2023 ~ exp((-2.4 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2024 ~ exp((-2.4 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8))),
           Acc_Yr == 2025 ~ exp((-2.6 +Rpt_Yr_Lag*(-.7) +Rpt_Yr_Lag_Sqrd *(0)+ Ln_Rpt_Yr_Lag * (2.8)))),
         
         
         ## p_rate varies by report year within accident year 
         ## and is then adjusted using the exposure count,Exp_Cnt, 
         ## before inverting the Poisson distribution.
         
         p_rate_adj =Exp_Cnt * p_rate,
         Rptd_Cnt = rpois(1,lambda =p_rate_adj) ) 


set.seed(4663529) # set the seed to allow ability to replicate results


# Generate individual claim records

## expand the date data set to create a row for each claim reported
## within a combination of accident year and report year elapsed for 
## Create a unique claim number at the same time

## note that ran_grp is created to allow one to later create randomized
## groups of claimant within an accident year to facilitate calculating
## the variance.


## create claim number identifier as unique combination of 
## report year, accident year and the numeric sequence of claim



data_counts_claim_Case_Mixture <- data_counts_rpdt_Case_Mixture  |>
  
  uncount(Rptd_Cnt) |>
  
  mutate( claim_num_incr = 1 ,
          claim_num_ex_yr = cumsum(claim_num_incr),
          claim_num = paste(Acc_Yr, Rpt_Yr_Lag, claim_num_ex_yr, sep = "_"),
          ran_grp = ceiling(round(runif(1,0,1) * 10 )/ 10)
  ) |>
  ungroup() |>
  group_by (claim_num) |>
  mutate( 
    ran_grp_temp = runif(1,0,1) * 10,
    ran_grp = ceiling(ran_grp_temp )
                            )

# assign parameters to drive time to closing calculations


data_counts_close_param_Case_Mixture <- data_counts_rpdt_Case_Mixture |>
                                  ungroup() |>
                                  select(Rpt_Yr_Lag) |>
                                  unique() |>
  
  mutate(  
    Cal_Yr_Clm_Chng = 2061,
    rate_gamma_init =  1.4, 
    rate_gamma_rvsd =  1.8,
    alpha_gamma = case_when( 
      Rpt_Yr_Lag == 1 ~ 3.0,
      Rpt_Yr_Lag == 2 ~ 3.0,
      Rpt_Yr_Lag == 3 ~ 3.0,
      Rpt_Yr_Lag == 4 ~ 3.0,
      Rpt_Yr_Lag == 5 ~ 3.0,
      Rpt_Yr_Lag == 6 ~ 3.0,
      Rpt_Yr_Lag == 7 ~ 3.5,
      Rpt_Yr_Lag == 8 ~ 3.5,
      Rpt_Yr_Lag == 9 ~ 4.0,
      Rpt_Yr_Lag == 10 ~ 4.0,
      Rpt_Yr_Lag == 11 ~ 4.0,
      Rpt_Yr_Lag == 12 ~ 4.0,
      Rpt_Yr_Lag == 13 ~ 4.0,
      Rpt_Yr_Lag == 14 ~ 4.0,
      Rpt_Yr_Lag == 15 ~ 4.0)
  )
    


## assign Gamma distribution parameters to set up length of time
## claim is open and invert the Gamma distribution for closing time.


## Use the rgamma function after setting the rate and alpha parameters 
## by report year lag to generate the length of time a claim 
## stays open then round to the nearest integer


set.seed(4663529) # set the seed to allow ability to replicate results

data_counts_clsd_init_Case_Mixture  <- left_join(data_counts_claim_Case_Mixture,
                                                   data_counts_close_param_Case_Mixture,
                                                   join_by(Rpt_Yr_Lag)) |>
  
  ## Use the rgamma function after setting the rate and alpha parameters 
  ## by report year lag to generate the length of time a claim 
  ## stays open then round to the nearest integer
  
  
  group_by(claim_num) |>
  
  mutate(  
           
            
            clsd_time_init = rgamma(1,alpha_gamma,rate_gamma_init),
            clsd_time_rnd_init = round(clsd_time_init +.5),
            Closed_Yr_init =Acc_Yr + Rpt_Yr_Lag+ clsd_time_rnd_init - 2
            
            
            
  ) 

 
  
set.seed(4663529) # set the seed to allow ability to replicate results

data_counts_clsd_rvsd_Case_Mixture  <- left_join(data_counts_claim_Case_Mixture,
                                                   data_counts_close_param_Case_Mixture,
                                                   join_by(Rpt_Yr_Lag)) |>
  
  ## Use the rgamma function after setting the rate and alpha parameters 
  ## by report year lag to generate the length of time a claim 
  ## stays open then round to the nearest integer
  
  
  group_by(claim_num) |>
  
  mutate(   
            clsd_time_rvsd_dist = rgamma(1,alpha_gamma,rate_gamma_rvsd),
            clsd_time_rnd_rvsd_dist = round(clsd_time_rvsd_dist +.5),
            Closed_Yr_rvsd_dist =Acc_Yr + Rpt_Yr_Lag+ clsd_time_rnd_rvsd_dist - 2,
            
            
            
  ) 

# for each claim setup maximum lifetime


data_counts_clsd_time_Case_Mixture <- left_join( data_counts_clsd_init_Case_Mixture,
                                        data_counts_clsd_rvsd_Case_Mixture,
                                        by ="claim_num")   |>

  rename(Acc_Yr =Acc_Yr.x,
         Rpt_Yr_Lag = Rpt_Yr_Lag.x,
         Exp_Cnt = Exp_Cnt.x,
         claim_num_incr = claim_num_incr.x,
         claim_num_ex_yr = claim_num_ex_yr.x,
         ran_grp = ran_grp.x,
         Cal_Yr_Clm_Chng = Cal_Yr_Clm_Chng.x) |>


  mutate( Closed_Yr_rvsd = case_when( 
    Closed_Yr_init   <= Cal_Yr_Clm_Chng ~ Closed_Yr_init,
    Closed_Yr_rvsd_dist <  Cal_Yr_Clm_Chng ~ Cal_Yr_Clm_Chng,
    .default =  Closed_Yr_rvsd_dist),
    
    clsd_time_rnd_rvsd = case_when( 
      Closed_Yr_init   <= Cal_Yr_Clm_Chng ~ clsd_time_rnd_init,
      Closed_Yr_rvsd_dist <  Cal_Yr_Clm_Chng ~ Cal_Yr_Clm_Chng - Closed_Yr_rvsd_dist ,
      .default =  clsd_time_rnd_rvsd_dist),
    
    clsd_time_rvsd = case_when( 
      Closed_Yr_init   <= Cal_Yr_Clm_Chng ~ Closed_Yr_init,
      Closed_Yr_rvsd_dist <  Cal_Yr_Clm_Chng ~ Cal_Yr_Clm_Chng - Closed_Yr_rvsd_dist +
        clsd_time_rvsd_dist -clsd_time_rnd_rvsd_dist,
      .default =  clsd_time_rvsd_dist),
    
    max_Closed_Yr = max(Closed_Yr_init,Closed_Yr_rvsd),
    max_clsd_time_rnd = max(clsd_time_rnd_init,clsd_time_rnd_rvsd),
    max_clsd_time = max(clsd_time_init,clsd_time_rvsd)
  ) |>
  
  select(claim_num,Acc_Yr, ran_grp,
         Rpt_Yr_Lag, Exp_Cnt,
         claim_num_incr,claim_num_ex_yr,Cal_Yr_Clm_Chng,
         max_Closed_Yr, Closed_Yr_init,Closed_Yr_rvsd,
         max_clsd_time_rnd, clsd_time_rnd_init,clsd_time_rnd_rvsd,
         max_clsd_time, clsd_time_init,clsd_time_rvsd,
         clsd_time_rvsd_dist ,
         clsd_time_rnd_rvsd_dist ,
         Closed_Yr_rvsd_dist )

# examine results of time to closure calculation

clsd_time_comparison  <-data_counts_clsd_time_Case_Mixture |> 
  group_by(Acc_Yr, Rpt_Yr_Lag) |>
  summarize(
    avg_clsd_time_rnd_init =mean(clsd_time_rnd_init),
    avg_clsd_time_rnd_rvsd =mean(clsd_time_rnd_rvsd),
    avg_clsd_time_rnd_rvsd_dist=mean(clsd_time_rnd_rvsd_dist),
    avg_Closed_Yr_init =mean(Closed_Yr_init),
    avg_Closed_Yr_rvsd =mean(Closed_Yr_rvsd),
    avg_Closed_Yr_rvsd_dist =mean(Closed_Yr_rvsd_dist),
    .groups = "keep"
  ) 

ggplot(data = clsd_time_comparison,
       aes(x=Rpt_Yr_Lag,y=avg_clsd_time_rnd_init)) +
  geom_point()

ggplot(data = data_counts_clsd_time_Case_Mixture,
       aes(x=Rpt_Yr_Lag,y=clsd_time_rnd_init, group=Rpt_Yr_Lag)) +
  geom_boxplot()


## create one row for each year a claim is open to set up payments

 
  
       
data_counts_structure_Case_Mixture  <- data_counts_clsd_time_Case_Mixture |>
  
  
  ungroup() |>
  
  uncount(max_clsd_time_rnd) |>
  
  
  
  group_by(claim_num) |>
  
  mutate(
    cum_yrs_open =cumsum(claim_num_incr),
    Dev_Yr = Rpt_Yr_Lag + cum_yrs_open - 1,
    Dev_Yr_Sqrd = Dev_Yr * Dev_Yr,
    Ln_Dev_Yr = log(Dev_Yr),
    Spline_Dev_Yr_10 = if_else(Dev_Yr<10,0,Dev_Yr-10),
    cum_yrs_open_sqrd = cum_yrs_open * cum_yrs_open,
    ln_cum_yrs_open = log(cum_yrs_open),
    Cal_Yr = Acc_Yr + Dev_Yr -1,
    open_ind_init = if_else(Cal_Yr < Closed_Yr_init,1,0),
    open_ind_rvsd = if_else(Cal_Yr < Closed_Yr_rvsd,1,0)
  ) |>
  ungroup() 




## create a data set that has all the frequency and severity parameters by
## development period


set.seed(4663529) # set the seed to allow ability to replicate results

data_freq_sev_param_Case_Mixture <- data_counts_structure_Case_Mixture |>
            select( Acc_Yr, Dev_Yr, 
                    Dev_Yr_Sqrd, Ln_Dev_Yr,
                    open_ind_init,open_ind_rvsd) |>
            unique()|>
            mutate(
              p_rate_pymnt_init =if_else (open_ind_init == 1,
                                  case_when(
                                    Acc_Yr == 2006 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2007 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2008 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2009 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2010 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2011 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2012 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2013 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2014 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2015 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2016 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2017 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2018 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2019 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2020 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2021 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2022 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2023 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2024 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2025 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5)))),
                                  case_when(
                                    Acc_Yr == 2006 ~ exp((-1.98 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2007 ~ exp((-1.95 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2008 ~ exp((-1.97 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2009 ~ exp((-1.94 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2010 ~ exp((-1.99 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2011 ~ exp((-1.98 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2012 ~ exp((-1.95 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2013 ~ exp((-1.97 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2014 ~ exp((-1.94 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2015 ~ exp((-1.99 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2016 ~ exp((-1.94 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2017 ~ exp((-1.93 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2018 ~ exp((-1.96 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2019 ~ exp((-1.95 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2020 ~ exp((-1.96 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2021 ~ exp((-1.96 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2022 ~ exp((-1.97 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2023 ~ exp((-1.98 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2024 ~ exp((-1.95 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2025 ~ exp((-1.96 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))))
              ),
              p_rate_pymnt_rvsd =if_else (open_ind_init == 1,
                                  case_when(
                                    Acc_Yr == 2006 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2007 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2008 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2009 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2010 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2011 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2012 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2013 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2014 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2015 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2016 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2017 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2018 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2019 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2020 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2021 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2022 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2023 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2024 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5))),
                                    Acc_Yr == 2025 ~ exp((-5 +Dev_Yr*(-.1) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (-.5)))),
                                  case_when(
                                    Acc_Yr == 2006 ~ exp((-1.98 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2007 ~ exp((-1.95 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2008 ~ exp((-1.97 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2009 ~ exp((-1.94 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2010 ~ exp((-1.99 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2011 ~ exp((-1.98 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2012 ~ exp((-1.95 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2013 ~ exp((-1.97 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2014 ~ exp((-1.94 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2015 ~ exp((-1.99 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2016 ~ exp((-1.94 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2017 ~ exp((-1.93 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2018 ~ exp((-1.96 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2019 ~ exp((-1.95 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2020 ~ exp((-1.96 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2021 ~ exp((-1.96 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2022 ~ exp((-1.97 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2023 ~ exp((-1.98 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2024 ~ exp((-1.95 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                    Acc_Yr == 2025 ~ exp((-1.96 +Dev_Yr*( 0 ) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))))
              ),
              mu_ln_init =if_else (open_ind_init == 1,
                                   case_when(
                                     Acc_Yr == 2006 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2007 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2008 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2009 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2010 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2011 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2012 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2013 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2014 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2015 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2016 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2017 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2018 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2019 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2020 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2021 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2022 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2023 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2024 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2025 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0)))),
                                   case_when(
                                     Acc_Yr == 2006 ~ exp((1.99 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2007 ~ exp((2.01 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2008 ~ exp((2 +  Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2009 ~ exp((1.98 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2010 ~ exp((1.99 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2011 ~ exp((1.98 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2012 ~ exp((2.01 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2013 ~ exp((2 +  Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2014 ~ exp((2.02 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2015 ~ exp((1.99 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2011 ~ exp((1.98 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2012 ~ exp((2.01 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2013 ~ exp((2 +  Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2014 ~ exp((2.02 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2015 ~ exp((1.99 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2016 ~ exp((1.97 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2017 ~ exp((2.02 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2018 ~ exp((1.99 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2019 ~ exp((1.98 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2020 ~ exp((2.01 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2021 ~ exp((1.98 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2022 ~ exp((2.01 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2023 ~ exp((1.97 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2024 ~ exp((2.01 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2025 ~ exp((1.98 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))))
              ),
              
              
              sigma_ln_init =if_else (open_ind_init == 1,
                                      case_when(
                                        Acc_Yr == 2006 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2007 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2008 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2009 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2010 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2011 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2012 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2013 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2014 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2015 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2016 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2017 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2018 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2019 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2020 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2021 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2022 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2023 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2024 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2025 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6)))),
                                      case_when(
                                        Acc_Yr == 2006 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2007 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2008 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2009 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2010 ~ exp((-1.98 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2011 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2012 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2013 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2014 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2015 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2016 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2017 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2018 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2019 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2020 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2021 ~ exp((-1.98 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2022 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2023 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2024 ~ exp((-1.98 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2025 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))))
              ),
              mu_ln_rvsd =if_else (open_ind_init == 1,
                                   case_when(
                                     Acc_Yr == 2006 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2007 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2008 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2009 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2010 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2011 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2012 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2013 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2014 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2015 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2016 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2017 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2018 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2019 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2020 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2021 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2022 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2023 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2024 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2025 ~ exp((2 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0)))),
                                   case_when(
                                     Acc_Yr == 2006 ~ exp((1.99 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2007 ~ exp((2.01 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2008 ~ exp((2 +  Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2009 ~ exp((1.98 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2010 ~ exp((1.99 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2011 ~ exp((1.98 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2012 ~ exp((2.01 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2013 ~ exp((2 +  Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2014 ~ exp((2.02 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2015 ~ exp((1.99 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2011 ~ exp((1.98 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2012 ~ exp((2.01 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2013 ~ exp((2 +  Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2014 ~ exp((2.02 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2015 ~ exp((1.99 +Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2016 ~ exp((1.97 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2017 ~ exp((2.02 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2018 ~ exp((1.99 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2019 ~ exp((1.98 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2020 ~ exp((2.01 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2021 ~ exp((1.98 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2022 ~ exp((2.01 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2023 ~ exp((1.97 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2024 ~ exp((2.01 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))),
                                     Acc_Yr == 2025 ~ exp((1.98 + Dev_Yr*(.01) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (0))))
              ),
              
              
              sigma_ln_rvsd =if_else (open_ind_init == 1,
                                      case_when(
                                        Acc_Yr == 2006 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2007 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2008 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2009 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2010 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2011 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2012 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2013 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2014 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2015 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2016 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2017 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2018 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2019 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2020 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2021 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2022 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2023 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2024 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2025 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6)))),
                                      case_when(
                                        Acc_Yr == 2006 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2007 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2008 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2009 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2010 ~ exp((-1.98 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2011 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2012 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2013 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2014 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2015 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2016 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2017 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2018 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2019 ~ exp((-2 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2020 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2021 ~ exp((-1.98 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2022 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2023 ~ exp((-1.99 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2024 ~ exp((-1.98 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))),
                                        Acc_Yr == 2025 ~ exp((-2.01 +Dev_Yr*(0) +Dev_Yr_Sqrd *(0)+ Ln_Dev_Yr * (.6))))
              )
              
              
                  ) |>
  arrange(open_ind_init,Acc_Yr,Dev_Yr)

# review results of parameter assignments



data_freq_sev_param_Case_Mixture |>
  filter(open_ind_init ==1) |>

ggplot(
       aes(x=Dev_Yr,y=p_rate_pymnt_init,color=Acc_Yr)) +
  geom_point()


data_test_close_param <-data_freq_sev_param_Case_Mixture |>
  filter(open_ind_init ==0) |>
  mutate(mean_sev_init = exp(mu_ln_init + (.5) * sigma_ln_init))

data_test_close_param_missing <- data_test_close_param |>
                          filter(is.na(mean_sev_init))

  
  ggplot( data =data_test_close_param,
    aes(x=Dev_Yr,y=p_rate_pymnt_init,color=Acc_Yr)) +
  geom_point()
  
  

ggplot(data =data_test_close_param,
  aes(x=Dev_Yr,y=mu_ln_init,color=Acc_Yr)) +
  geom_point()

ggplot(data =data_test_close_param,
       aes(x=Dev_Yr,y=mean_sev_init,color=Acc_Yr)) +
  geom_point() 

ggplot(data =data_test_close_param,
       aes(x=Dev_Yr,y=sigma_ln_init,color=Acc_Yr)) +
  geom_point()




data_freq_sev_param_Case_Mixture |>
  filter(p_rate_pymnt_init <.5 )


# join the parameters with the claim records then invert to get counts and amounts

## Use the rpois function to invert the Poisson distribution to create
## the number of payments, Paid_Cnt, by claim for each year it is open
## the rate, p_rate_pymnt, is set by development year with the development
## year parameters set differently depending on if it closes in that year
## or stays open for another year. The assumptions is that a payment 
## is much more likely to take place in a year it does not close as
## opposed to the year it closes.

## use the rlnorm function to arrive at claim payment given there is a payment

set.seed(4663529) # set the seed to allow ability to replicate results


data_counts_incr_amts_Case_Mixture <- left_join(data_counts_structure_Case_Mixture,
                                                  data_freq_sev_param_Case_Mixture,
                                                  join_by(Acc_Yr,Dev_Yr,open_ind_init,open_ind_rvsd))|>
                  ungroup()|>
                  group_by(claim_num,Dev_Yr) |>
  
                                  mutate(
                                    Paid_Cnt_Clmnt_init = rpois(1,lambda =p_rate_pymnt_init),
                                    
                                    
                                    Paid_Cnt_init = if_else(Paid_Cnt_Clmnt_init == 0,0,1),
                                    
                                    Paid_Cnt_Clmnt_rvsd = rpois(1,lambda =p_rate_pymnt_rvsd),
                                    
                                    
                                    
                                    Paid_Cnt_rvsd = if_else(Paid_Cnt_Clmnt_rvsd == 0,0,1) ,
                                    
                                    Mean_Payment_init = if_else (Paid_Cnt_init ==0, 0,
                                                                 mean(rlnorm(Paid_Cnt_Clmnt_init,
                                                                             meanlog = mu_ln_init,
                                                                             sdlog=sigma_ln_init)) ),
                                    
                                    Incr_Payment_init = Paid_Cnt_Clmnt_init * Mean_Payment_init, 
                                    
                                    Mean_Payment_rvsd = if_else (Paid_Cnt_rvsd ==0, 0,
                                                                 mean(rlnorm(Paid_Cnt_Clmnt_rvsd,
                                                                             meanlog = mu_ln_rvsd,
                                                                             sdlog=sigma_ln_rvsd) )),
                                    
                                    
                                    
                                    
                                    Incr_Payment_rvsd = Paid_Cnt_Clmnt_rvsd * Mean_Payment_rvsd  
                                    
                                    
                                     )


# some ad hoc test of what has taken place for counts and amounts after inversion


data_counts_incr_amts_Case_Mixture_closing <- data_counts_incr_amts_Case_Mixture |>
                filter(open_ind_init == 0)


  ggplot( data = data_counts_incr_amts_Case_Mixture_closing,
           aes( x = Dev_Yr, y =Incr_Payment_init, group= Dev_Yr)) +
    geom_boxplot()
  
  ggplot( data = data_counts_incr_amts_Case_Mixture,
          aes( x = Dev_Yr, y =Incr_Payment_init, group= Dev_Yr)) +
    geom_boxplot()
  
  
  data_counts_incr_amts_Case_Mixture_closing_w_Pay <- data_counts_incr_amts_Case_Mixture_closing |>
    filter(Paid_Cnt_init == 1) 
  
  ggplot( data =  data_counts_incr_amts_Case_Mixture_closing_w_Pay,
          aes( x = Dev_Yr, y =Incr_Payment_init, group= Dev_Yr)) +
    geom_boxplot() + 
    labs(title = "Closing With Payment Only")
  
  ggplot( data =  data_counts_incr_amts_Case_Mixture_closing_w_Pay,
          aes( x = mu_ln_init, y =Incr_Payment_init, group= Dev_Yr)) +
    geom_point() + 
    labs(title = "Closing With Payment Only") 
  
  
  ggplot( data = data_counts_incr_amts_Case_Mixture,
          aes( x = Dev_Yr, y =Paid_Cnt_Clmnt_init, group= Dev_Yr)) +
    geom_boxplot()

  
  ggplot( data = data_counts_incr_amts_Case_Mixture_closing,
          aes( x = Dev_Yr, y =Incr_Payment_init, group= Dev_Yr)) +
    geom_boxplot()
  
  ggplot( data = data_counts_incr_amts_Case_Mixture_closing,
          aes( x = Dev_Yr, y =Paid_Cnt_Clmnt_init, group= Dev_Yr)) +
    geom_boxplot()
  
  
  
  
  
## create an exposure data set for total by accident year


data_exposure_Case_Mixture <- data_counts_incr_amts_Case_Mixture|>
  filter (Rpt_Yr_Lag ==1) |>
  group_by(Acc_Yr) |>
  select(Acc_Yr, claim_num,claim_num_incr) |>
  unique() |>
  mutate(    Rptd_Cnt_Yr_1 = sum(claim_num_incr)
  ) |>
  select(Acc_Yr,Rptd_Cnt_Yr_1 ) |>
  unique()

## create an exposure data set for random group within accident year 



data_exposure_ran_grp_Case_Mixture <- data_counts_incr_amts_Case_Mixture|>
  filter (Rpt_Yr_Lag ==1) |>
  group_by(Acc_Yr,ran_grp) |>
  select(Acc_Yr, ran_grp,claim_num,claim_num_incr) |>
  unique() |>
  mutate(    Rptd_Cnt_Yr_1 = sum(claim_num_incr)
  ) |>
  select(Acc_Yr,ran_grp,Rptd_Cnt_Yr_1 ) |>
  unique()


## Join inflation and Loss cost trends to incremental amounts

## calculate the amounts to insert on the claim given the scenario under which
## the simulation is run. 

## Note that the amounts attached to a claim can be dependent on the calendar
## year in which a payment is made dependent on if the calendar year is less
## than the calendar in which claim department changes (Cal_Yr_Clm_Chng) or
## the calendar year in whic inflation changes (Cal_Yr_Infl_Chng)  

## the term "init" is used to indicate initial condition

## the term "rvsd" is used to indicate revised condition for the amount
## attached to a claim

data_counts_incr_amts_infl_Case_Mixture <- left_join(data_counts_incr_amts_Case_Mixture,
                                                         Cal_Yr_Trend_Acc_Case_Mixture, by=c('Cal_Yr')) |>
  group_by(claim_num, Cal_Yr) |>
  mutate (
    Incr_Payment_Infl_t_init_c_init = Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_init,
    
    Incr_Payment_Defl_t_init_c_init =  Incr_Payment_Infl_t_init_c_init/ 
      Accum_Infl_Index_Mid_init,
    
    Incr_Payment_Infl_t_init_c_rvsd = case_when( Cal_Yr < Cal_Yr_Clm_Chng  ~ 
                                                   Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_init,  
                                                 Closed_Yr_rvsd < Cal_Yr_Clm_Chng ~
                                                   Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_init,  
                                                 Closed_Yr_init < Cal_Yr_Clm_Chng ~
                                                   Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_init,
                                                 .default =Incr_Payment_rvsd * Accum_Loss_Cost_Trend_Mid_init,
    ),
    
    Incr_Payment_Defl_t_init_c_rvsd =  Incr_Payment_Infl_t_init_c_rvsd/ 
      Accum_Infl_Index_Mid_init,
    
    Incr_Payment_Infl_t_rvsd_c_init = if_else(Cal_Yr >= Cal_Yr_Infl_Chng ,
                                              Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_rvsd,
                                              Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_init),
    
    Incr_Payment_Defl_t_rvsd_c_init =   if_else(Cal_Yr >= Cal_Yr_Infl_Chng ,
                                                Incr_Payment_Infl_t_rvsd_c_init/ 
                                                  Accum_Infl_Index_Mid_rvsd,
                                                Incr_Payment_Infl_t_rvsd_c_init/ 
                                                  Accum_Infl_Index_Mid_init),
    
    Incr_Payment_Infl_t_rvsd_c_rvsd =  case_when (Cal_Yr < Cal_Yr_Clm_Chng & Cal_Yr < Cal_Yr_Infl_Chng ~ 
                                                    Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_init, 
                                                  
                                                  Cal_Yr < Cal_Yr_Clm_Chng & Cal_Yr >= Cal_Yr_Infl_Chng ~ 
                                                    Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_rvsd, 
                                                  
                                                  Closed_Yr_rvsd < Cal_Yr_Clm_Chng & Cal_Yr < Cal_Yr_Infl_Chng ~
                                                    Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_init,  
                                                  
                                                  Closed_Yr_rvsd < Cal_Yr_Clm_Chng & Cal_Yr >= Cal_Yr_Infl_Chng ~
                                                    Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_rvsd,  
                                                  
                                                  Closed_Yr_init < Cal_Yr_Clm_Chng & Cal_Yr < Cal_Yr_Infl_Chng ~
                                                    Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_init,
                                                  
                                                  Closed_Yr_init < Cal_Yr_Clm_Chng & Cal_Yr >= Cal_Yr_Infl_Chng ~
                                                    Incr_Payment_init * Accum_Loss_Cost_Trend_Mid_rvsd,
                                                  
                                                  .default =Incr_Payment_rvsd * Accum_Loss_Cost_Trend_Mid_init) ,
    
    
    
    Incr_Payment_Defl_t_rvsd_c_rvsd =  if_else(Cal_Yr >= Cal_Yr_Infl_Chng ,
                                               Incr_Payment_Infl_t_rvsd_c_rvsd/ 
                                                 Accum_Infl_Index_Mid_rvsd,
                                               Incr_Payment_Infl_t_rvsd_c_rvsd/ 
                                                 Accum_Infl_Index_Mid_init),
    
    Incr_Payment_Infl =case_when (
      (Cal_Yr < Cal_Yr_Infl_Chng & Cal_Yr < Cal_Yr_Clm_Chng)  ~ 
        Incr_Payment_Infl_t_init_c_init,
      (Cal_Yr >= Cal_Yr_Infl_Chng & Cal_Yr < Cal_Yr_Clm_Chng)  ~ 
        Incr_Payment_Infl_t_rvsd_c_init, 
      (Cal_Yr < Cal_Yr_Infl_Chng & Cal_Yr >= Cal_Yr_Clm_Chng)  ~ 
        Incr_Payment_Infl_t_init_c_rvsd, 
      (Cal_Yr >= Cal_Yr_Infl_Chng & Cal_Yr >= Cal_Yr_Clm_Chng)  ~ 
        Incr_Payment_Infl_t_rvsd_c_rvsd
    ),
    
    Incr_Payment_Defl =case_when (
      (Cal_Yr < Cal_Yr_Infl_Chng & Cal_Yr < Cal_Yr_Clm_Chng)  ~ 
        Incr_Payment_Defl_t_init_c_init,
      (Cal_Yr >= Cal_Yr_Infl_Chng & Cal_Yr < Cal_Yr_Clm_Chng)  ~ 
        Incr_Payment_Defl_t_rvsd_c_init, 
      (Cal_Yr < Cal_Yr_Infl_Chng & Cal_Yr >= Cal_Yr_Clm_Chng)  ~ 
        Incr_Payment_Defl_t_init_c_rvsd, 
      (Cal_Yr >= Cal_Yr_Infl_Chng & Cal_Yr >= Cal_Yr_Clm_Chng)  ~ 
        Incr_Payment_Defl_t_rvsd_c_rvsd
    ),
    
    Incr_Payment_No_Infl = case_when( Cal_Yr < Cal_Yr_Clm_Chng  ~ 
                                        Incr_Payment_init ,  
                                      Closed_Yr_rvsd < Cal_Yr_Clm_Chng ~
                                        Incr_Payment_init ,  
                                      Closed_Yr_init < Cal_Yr_Clm_Chng ~
                                        Incr_Payment_init ,
                                      .default =Incr_Payment_rvsd 
    ),
    
    
    
  )



## Accumulate incremental claim payments


data_claim_num_tot_pay_Case_Mixture <-  data_counts_incr_amts_infl_Case_Mixture |>
  ungroup() |>
  group_by( claim_num) |>
  mutate(
    Tot_Paid_Infl_t_rvsd_c_rvsd =sum(Incr_Payment_Infl_t_rvsd_c_rvsd),
    Tot_Paid_Infl_t_rvsd_c_init =sum(Incr_Payment_Infl_t_rvsd_c_init),
    Tot_Paid_Infl_t_init_c_rvsd =sum(Incr_Payment_Infl_t_init_c_rvsd),
    Tot_Paid_Infl_t_init_c_init =sum(Incr_Payment_Infl_t_init_c_init),
    Tot_Paid_Infl =   sum(Incr_Payment_Infl),  
    Tot_Paid_Defl =   sum(Incr_Payment_Defl), 
    Tot_Paid_No_Infl_c_init = sum(Incr_Payment_init),
    Tot_Paid_No_Infl_c_rvsd = sum(Incr_Payment_rvsd),
    Tot_Paid_No_Infl        = sum(Incr_Payment_No_Infl)
  ) |>
  select(claim_num,
         Tot_Paid_Infl_t_rvsd_c_rvsd ,
         Tot_Paid_Infl_t_rvsd_c_init ,
         Tot_Paid_Infl_t_init_c_rvsd ,
         Tot_Paid_Infl_t_init_c_init ,
         Tot_Paid_Infl,
         Tot_Paid_Defl,
         Tot_Paid_No_Infl_c_init,
         Tot_Paid_No_Infl_c_rvsd,
         Tot_Paid_No_Infl) |>
  unique()


## Calculate open reserves by claim as the difference between
## ultimate claim payment and incremental claim payments accumulated
## for initial case reserve.

## Note that the Case Reserve amounts are altered to reflect a random deviation
## from the amount that should be posted to exactly reflect future paid to 
## make a more realistic data set.



## set up the aggregate claim statistic accumulated values calculation

set.seed(4663529) # set the seed to allow ability to replicate results

data_counts_amts_infl_Case_Mixture <- left_join( data_counts_incr_amts_infl_Case_Mixture, data_claim_num_tot_pay_Case_Mixture, 
                                        by=(c('claim_num'))) |>
  ungroup()  |>
  group_by(claim_num)  |>
  mutate(
    Acc_Incr_Payment_Infl_t_rvsd_c_rvsd = cumsum(Incr_Payment_Infl_t_rvsd_c_rvsd),
    Acc_Incr_Payment_Infl_t_init_c_rvsd = cumsum(Incr_Payment_Infl_t_init_c_rvsd),
    Acc_Incr_Payment_Infl_t_rvsd_c_init = cumsum(Incr_Payment_Infl_t_rvsd_c_init),
    Acc_Incr_Payment_Infl_t_init_c_init = cumsum(Incr_Payment_Infl_t_init_c_init),
    Acc_Incr_Payment_Infl               = cumsum(Incr_Payment_Infl),
    
    Acc_Incr_Payment_Defl_t_rvsd_c_rvsd = cumsum(Incr_Payment_Defl_t_rvsd_c_rvsd),
    Acc_Incr_Payment_Defl_t_init_c_rvsd = cumsum(Incr_Payment_Defl_t_init_c_rvsd),
    Acc_Incr_Payment_Defl_t_rvsd_c_init = cumsum(Incr_Payment_Defl_t_rvsd_c_init),
    Acc_Incr_Payment_Defl_t_init_c_init = cumsum(Incr_Payment_Defl_t_init_c_init),
    Acc_Incr_Payment_Defl               = cumsum(Incr_Payment_Defl),
    
    Acc_Incr_Payment_No_Infl_c_init = cumsum(Incr_Payment_init),
    Acc_Incr_Payment_No_Infl_c_rvsd = cumsum(Incr_Payment_rvsd),
    
    
    
    Case_Rsv_Infl_Base_t_rvsd_c_rvsd = Tot_Paid_Infl_t_rvsd_c_rvsd - 
      Acc_Incr_Payment_Infl_t_rvsd_c_rvsd,
    Case_Rsv_Infl_Base_t_rvsd_c_init = Tot_Paid_Infl_t_rvsd_c_init -
      Acc_Incr_Payment_Infl_t_rvsd_c_init,
    Case_Rsv_Infl_Base_t_init_c_rvsd = Tot_Paid_Infl_t_init_c_rvsd -
      Acc_Incr_Payment_Infl_t_init_c_rvsd,
    Case_Rsv_Infl_Base_t_init_c_init = Tot_Paid_Infl_t_init_c_init -
      Acc_Incr_Payment_Infl_t_init_c_init,
    
    
    Random_Case_Rsv_Mult_c_init =  rlnorm(1,0,.1),
    Random_Case_Rsv_Mult_c_rvsd =  rlnorm(1,.2,.1),
    
    Random_Case_Rsv_Mult = if_else( Cal_Yr < Cal_Yr_Clm_Chng ,
                                    Random_Case_Rsv_Mult_c_init,
                                    Random_Case_Rsv_Mult_c_rvsd),
    
    
    Case_Rsv_Infl_t_rvsd_c_rvsd = Case_Rsv_Infl_Base_t_rvsd_c_rvsd * 
      Random_Case_Rsv_Mult,
    Case_Rsv_Infl_t_rvsd_c_init = Case_Rsv_Infl_Base_t_rvsd_c_init * 
      Random_Case_Rsv_Mult,
    Case_Rsv_Infl_t_init_c_rvsd = Case_Rsv_Infl_Base_t_init_c_rvsd * 
      Random_Case_Rsv_Mult,
    Case_Rsv_Infl_t_init_c_init = Case_Rsv_Infl_Base_t_init_c_init * 
      Random_Case_Rsv_Mult,
    
    Case_Rsv_No_Inf_Base_c_init = Tot_Paid_No_Infl_c_init - Acc_Incr_Payment_No_Infl_c_init,
    Case_Rsv_No_Inf_Base_c_rvsd = Tot_Paid_No_Infl_c_rvsd - Acc_Incr_Payment_No_Infl_c_rvsd,
    
    Case_Rsv_No_Inf_c_init = Case_Rsv_No_Inf_Base_c_init * Random_Case_Rsv_Mult,
    Case_Rsv_No_Inf_c_rvsd = Case_Rsv_No_Inf_Base_c_init * Random_Case_Rsv_Mult,
    
    
    Case_Rsv_Infl = case_when(
      Cal_Yr < Cal_Yr_Infl_Chng & Cal_Yr < Cal_Yr_Clm_Chng ~ 
        Case_Rsv_Infl_t_init_c_init,
      
      Cal_Yr >= Cal_Yr_Infl_Chng & Cal_Yr < Cal_Yr_Clm_Chng ~
        Case_Rsv_Infl_t_rvsd_c_init,
      
      
      Cal_Yr < Cal_Yr_Infl_Chng & Cal_Yr >= Cal_Yr_Clm_Chng ~
        Case_Rsv_Infl_t_init_c_rvsd, 
      
      Cal_Yr >= Cal_Yr_Infl_Chng & Cal_Yr >= Cal_Yr_Clm_Chng  ~
        Case_Rsv_Infl_t_rvsd_c_rvsd 
      
      
    ),
    
    Mean_Payment = case_when( Cal_Yr < Cal_Yr_Clm_Chng  ~ 
                                Mean_Payment_init,  
                              Closed_Yr_rvsd < Cal_Yr_Clm_Chng ~
                                Mean_Payment_init,  
                              Closed_Yr_init < Cal_Yr_Clm_Chng ~
                                Mean_Payment_init,
                              .default =Mean_Payment_rvsd),
    
    
    
    
    
    Accum_Infl_Index_Mid = if_else(Cal_Yr < Cal_Yr_Infl_Chng,
                                   Accum_Infl_Index_Mid_init,
                                   Accum_Infl_Index_Mid_rvsd),
    
    Accum_Loss_Cost_Trend_Mid =if_else(Cal_Yr < Cal_Yr_Infl_Chng,
                                       Accum_Loss_Cost_Trend_Mid_rvsd,
                                       Accum_Loss_Cost_Trend_Mid_init),
    
    Closed_Yr = case_when( 
      Closed_Yr_rvsd < Cal_Yr_Clm_Chng ~
        Closed_Yr_init,  
      Closed_Yr_init < Cal_Yr_Clm_Chng ~
        Closed_Yr_init,
      .default =  Closed_Yr_rvsd),
    
    open_ind =  case_when( Cal_Yr < Cal_Yr_Clm_Chng  ~ 
                             open_ind_init,  
                           Closed_Yr_rvsd < Cal_Yr_Clm_Chng ~
                             open_ind_init,  
                           Closed_Yr_init < Cal_Yr_Clm_Chng ~
                             open_ind_init,
                           .default = open_ind_rvsd),
    
    
    
    
    Clsd_W_Pay_Cnt = if_else(Closed_Yr == Cal_Yr & Tot_Paid_Infl !=0,1,0),
    
    Clsd_W_O_Pay_Cnt = if_else(Closed_Yr == Cal_Yr & Tot_Paid_Infl ==0,1,0),
    
    Paid_Cnt = case_when( Cal_Yr < Cal_Yr_Clm_Chng  ~ 
                            Paid_Cnt_init,  
                          Closed_Yr_rvsd < Cal_Yr_Clm_Chng ~
                            Paid_Cnt_init,  
                          Closed_Yr_init < Cal_Yr_Clm_Chng ~
                            Paid_Cnt_init,
                          .default = Paid_Cnt_rvsd),
    
    
    
  )



data_claim_detail_Case_Mixture <-data_counts_amts_infl_Case_Mixture  |>
  select( Acc_Yr, Rpt_Yr_Lag, Exp_Cnt,claim_num,ran_grp,
          Dev_Yr, Cal_Yr,
          open_ind, Paid_Cnt, Mean_Payment,
          Accum_Loss_Cost_Trend_Mid,Accum_Infl_Index_Mid,
          Incr_Payment_Infl, Acc_Incr_Payment_Infl,
          Acc_Incr_Payment_Defl, Incr_Payment_Defl,
          Tot_Paid_Infl,
          Case_Rsv_Infl, Clsd_W_Pay_Cnt,Clsd_W_O_Pay_Cnt)



## create an aggregated data set similar to that typically seen for 
## link ratio calculations



## summarize to get an incremental aggregate Accident Year and Development Year data set

data_agg_Acc_Yr_Dev_Yr_Incr_Case_Mixture <- data_claim_detail_Case_Mixture |>
  ungroup() |>
  select(Acc_Yr, Dev_Yr,
         open_ind,
         Paid_Cnt,
         
         Incr_Payment_Infl ,
         Incr_Payment_Defl,
         Accum_Infl_Index_Mid,
         Case_Rsv_Infl ,
         Clsd_W_Pay_Cnt,
         Clsd_W_O_Pay_Cnt,
  ) |>
  
  group_by(Acc_Yr,Dev_Yr) |>
  mutate(Open_Cnt = sum(open_ind),
         Paid_Cnt_Incr= sum(Paid_Cnt),
         Incr_Paid_Infl =sum(Incr_Payment_Infl) ,
         Incr_Paid_Defl =sum(Incr_Payment_Defl),
         Accum_Infl_Index_Mid=mean(Accum_Infl_Index_Mid),
         Case_Rsv =sum(Case_Rsv_Infl),
         Incr_Clsd_W_Pay_Cnt = sum(Clsd_W_Pay_Cnt),
         Incr_Clsd_W_O_Pay_Cnt =sum(Clsd_W_O_Pay_Cnt),
         
  ) |>
  select(Acc_Yr, Dev_Yr,
         Open_Cnt ,
         Paid_Cnt_Incr,
         Incr_Paid_Infl,
         Incr_Paid_Defl ,
         Accum_Infl_Index_Mid,
         Case_Rsv ,
         Incr_Clsd_W_Pay_Cnt ,
         Incr_Clsd_W_O_Pay_Cnt,
         
  )|>
  unique() 


# create another data set for random group



## summarize to get an incremental aggregate Accident Year and Development Year data set

data_agg_Acc_Yr_Dev_Yr_ran_grp_Incr_Case_Mixture <- data_claim_detail_Case_Mixture |>
  ungroup() |>
  select(Acc_Yr, Dev_Yr,ran_grp,
         open_ind,
         Paid_Cnt,
         
         Incr_Payment_Infl ,
         Incr_Payment_Defl,
         Accum_Infl_Index_Mid,
         Case_Rsv_Infl ,
         Clsd_W_Pay_Cnt,
         Clsd_W_O_Pay_Cnt,
  ) |>
  
  group_by(Acc_Yr,Dev_Yr,ran_grp) |>
  mutate(Open_Cnt = sum(open_ind),
         Paid_Cnt_Incr= sum(Paid_Cnt),
         Incr_Paid_Infl =sum(Incr_Payment_Infl) ,
         Incr_Paid_Defl =sum(Incr_Payment_Defl),
         Accum_Infl_Index_Mid=mean(Accum_Infl_Index_Mid),
         Case_Rsv =sum(Case_Rsv_Infl),
         Incr_Clsd_W_Pay_Cnt = sum(Clsd_W_Pay_Cnt),
         Incr_Clsd_W_O_Pay_Cnt =sum(Clsd_W_O_Pay_Cnt),
         
  ) |>
  select(Acc_Yr, Dev_Yr,ran_grp,
         Open_Cnt ,
         Paid_Cnt_Incr,
         Incr_Paid_Infl,
         Incr_Paid_Defl ,
         Accum_Infl_Index_Mid,
         Case_Rsv ,
         Incr_Clsd_W_Pay_Cnt ,
         Incr_Clsd_W_O_Pay_Cnt,
         
  )|>
  unique() 



## summarize to get an incremental & cumulative aggregate Accident Year and Development Year data set

data_agg_Acc_Yr_Dev_Yr_Cum_Case_Mixture <- data_agg_Acc_Yr_Dev_Yr_Incr_Case_Mixture |>
  ungroup()  |>
  group_by(Acc_Yr) |>
  mutate(Cum_Incr_Paid_Infl =cumsum(Incr_Paid_Infl),
         Cum_Incr_Paid_Defl = cumsum(Incr_Paid_Defl),
         Cum_Incr_Clsd_W_Pay_Cnt =cumsum(Incr_Clsd_W_Pay_Cnt),
         Cum_Incr_Clsd_W_O_Pay_Cnt= cumsum(Incr_Clsd_W_O_Pay_Cnt),
         Cum_Rptd_Cnt = Open_Cnt+Cum_Incr_Clsd_W_Pay_Cnt +
           Cum_Incr_Clsd_W_O_Pay_Cnt ,
         Case_Incurred =Cum_Incr_Paid_Infl + Case_Rsv)

# create another summarized data set to get random grouping

data_agg_Acc_Yr_Dev_Yr_ran_grp_Cum_Case_Mixture <- data_agg_Acc_Yr_Dev_Yr_ran_grp_Incr_Case_Mixture |>
  ungroup()  |>
  group_by(Acc_Yr,ran_grp) |>
  mutate(Cum_Incr_Paid_Infl =cumsum(Incr_Paid_Infl),
         Cum_Incr_Paid_Defl = cumsum(Incr_Paid_Defl),
         Cum_Incr_Clsd_W_Pay_Cnt =cumsum(Incr_Clsd_W_Pay_Cnt),
         Cum_Incr_Clsd_W_O_Pay_Cnt= cumsum(Incr_Clsd_W_O_Pay_Cnt),
         Cum_Rptd_Cnt = Open_Cnt+Cum_Incr_Clsd_W_Pay_Cnt +
           Cum_Incr_Clsd_W_O_Pay_Cnt ,
         Case_Incurred =Cum_Incr_Paid_Infl + Case_Rsv)


## Normalize using Reported claim counts at end of first development year
## Calculate averages for dollar amounts


data_aggregated_normalized_Case_Mixture <- left_join(data_agg_Acc_Yr_Dev_Yr_Cum_Case_Mixture ,
                                               data_exposure_Case_Mixture,by=c('Acc_Yr')) |>
  
  ungroup() |>
  select(Acc_Yr,Dev_Yr,
         Rptd_Cnt_Yr_1,
         Open_Cnt,
         Paid_Cnt_Incr,
         Incr_Paid_Infl ,
         Incr_Paid_Defl,
         Accum_Infl_Index_Mid,
         Case_Rsv ,
         Case_Incurred,
         Incr_Clsd_W_Pay_Cnt,
         Incr_Clsd_W_O_Pay_Cnt,
         Cum_Incr_Paid_Infl,
         Cum_Incr_Paid_Defl ,
         Cum_Incr_Clsd_W_Pay_Cnt ,
         Cum_Rptd_Cnt,
         Cum_Incr_Clsd_W_O_Pay_Cnt )  |>
  mutate( 
    Disp_Rate = round(if_else( Acc_Yr == lag(Acc_Yr) & is.na(lag(Acc_Yr)) == FALSE  ,
                         (Incr_Clsd_W_Pay_Cnt + Incr_Clsd_W_O_Pay_Cnt) /
                           (Cum_Rptd_Cnt - lag(Cum_Rptd_Cnt)+ lag(Open_Cnt)),
                         (Incr_Clsd_W_Pay_Cnt + Incr_Clsd_W_O_Pay_Cnt) /
                           Cum_Rptd_Cnt),3)) |>
  group_by(Acc_Yr, Dev_Yr) |>
  
  mutate( Paid_Cnt_Incr_Norm_Rptd = round(Paid_Cnt_Incr/ Rptd_Cnt_Yr_1,3),
                                          
          
          Cum_Rptd_Cnt_Norm = round(Cum_Rptd_Cnt/Rptd_Cnt_Yr_1,3),
                                     
     
          Open_Cnt_Norm = round((Open_Cnt/ Rptd_Cnt_Yr_1),3),
                                 
            
          Cum_Clsd_Cnt_Pct = round((Cum_Incr_Clsd_W_Pay_Cnt+
                                   Cum_Incr_Clsd_W_O_Pay_Cnt ) /
                                           Cum_Rptd_Cnt,3),
                                    
          Cum_Incr_Clsd_W_Pay_Cnt_Norm =round(Cum_Incr_Clsd_W_Pay_Cnt/Rptd_Cnt_Yr_1,3),
                                    
            
           
          Cum_Incr_Clsd_W_O_Pay_Cnt_Norm = round(Cum_Incr_Clsd_W_O_Pay_Cnt/
                                                                     Rptd_Cnt_Yr_1,3),
                                                
            
            
          Avg_Incr_Paid_Infl = round(if_else(Paid_Cnt_Incr > 0,
                                     Incr_Paid_Infl/Paid_Cnt_Incr,
                                     0),0),
                                      
            
            
          Avg_Incr_Paid_Defl = round(if_else(Paid_Cnt_Incr >0,
                                              Incr_Paid_Defl/Paid_Cnt_Incr,
                                                0),0),
                                     
            
          
          Incr_Paid_Infl_Norm_Rptd = round( Incr_Paid_Infl/Rptd_Cnt_Yr_1 ,3),
            
            
          Incr_Paid_Infl_Norm_Rptd_Ln = round( log(Incr_Paid_Infl/Rptd_Cnt_Yr_1),3),
                                               
            
            ,
          Incr_Paid_Defl_Norm_Rptd = round( Incr_Paid_Defl/Rptd_Cnt_Yr_1,0),
                                           
            
          
          Incr_Paid_Defl_Norm_Rptd_Ln = round( log(Incr_Paid_Defl/(Rptd_Cnt_Yr_1)),3),
                                               
            
            
          Cum_Incr_Paid_Infl_Norm_Rptd = round( Cum_Incr_Paid_Infl/Rptd_Cnt_Yr_1,0),
                                                                    
            
          Avg_Case_Rsv = round( if_else(Open_Cnt >0 ,Case_Rsv/Open_Cnt,0),0),
            
          
          
          Cum_Incr_Paid_Defl_Norm_Rptd = round(  Cum_Incr_Paid_Defl/Rptd_Cnt_Yr_1,0),
                                                                     
          
          Case_Incrd_Norm_Rptd = round(Case_Incurred/Rptd_Cnt_Yr_1,0),
          
          
          Cal_Yr = Acc_Yr + Dev_Yr -1, 
          
          Accum_Infl_Index_Mid =round(Accum_Infl_Index_Mid,3)
          
          
          
          )



data_aggregated_normalized_ran_grp_Case_Mixture <- left_join(data_agg_Acc_Yr_Dev_Yr_ran_grp_Cum_Case_Mixture,
                                                         data_exposure_ran_grp_Case_Mixture,
                                                         by=c('Acc_Yr','ran_grp')) |>
  
  ungroup() |>
  select(Acc_Yr,Dev_Yr,ran_grp,
         Rptd_Cnt_Yr_1,
         Open_Cnt,
         Paid_Cnt_Incr,
         Incr_Paid_Infl ,
         Incr_Paid_Defl,
         Accum_Infl_Index_Mid,
         Case_Rsv ,
         Case_Incurred,
         Incr_Clsd_W_Pay_Cnt,
         Incr_Clsd_W_O_Pay_Cnt,
         Cum_Incr_Paid_Infl,
         Cum_Incr_Paid_Defl ,
         Cum_Incr_Clsd_W_Pay_Cnt ,
         Cum_Rptd_Cnt,
         Cum_Incr_Clsd_W_O_Pay_Cnt )  |>
  mutate( 
    Disp_Rate = round(if_else( Acc_Yr == lag(Acc_Yr) & is.na(lag(Acc_Yr)) == FALSE  ,
                               (Incr_Clsd_W_Pay_Cnt + Incr_Clsd_W_O_Pay_Cnt) /
                                 (Cum_Rptd_Cnt - lag(Cum_Rptd_Cnt)+ lag(Open_Cnt)),
                               (Incr_Clsd_W_Pay_Cnt + Incr_Clsd_W_O_Pay_Cnt) /
                                 Cum_Rptd_Cnt),3)) |>
  group_by(Acc_Yr, Dev_Yr,ran_grp) |>
  
  mutate( Paid_Cnt_Incr_Norm_Rptd = round(Paid_Cnt_Incr/ Rptd_Cnt_Yr_1,3),
          
          
          Cum_Rptd_Cnt_Norm = round(Cum_Rptd_Cnt/Rptd_Cnt_Yr_1,3),
          
          
          Open_Cnt_Norm = round((Open_Cnt/ Rptd_Cnt_Yr_1),3),
          
          
          Cum_Clsd_Cnt_Pct = round((Cum_Incr_Clsd_W_Pay_Cnt+
                                      Cum_Incr_Clsd_W_O_Pay_Cnt ) /
                                     Cum_Rptd_Cnt,3),
          
          Cum_Incr_Clsd_W_Pay_Cnt_Norm =round(Cum_Incr_Clsd_W_Pay_Cnt/Rptd_Cnt_Yr_1,3),
          
          
          
          Cum_Incr_Clsd_W_O_Pay_Cnt_Norm = round(Cum_Incr_Clsd_W_O_Pay_Cnt/
                                                   Rptd_Cnt_Yr_1,3),
          
          
          
          Avg_Incr_Paid_Infl = round(if_else(Paid_Cnt_Incr > 0,
                                             Incr_Paid_Infl/Paid_Cnt_Incr,
                                             0),0),
          
          
          
          Avg_Incr_Paid_Defl = round(if_else(Paid_Cnt_Incr >0,
                                             Incr_Paid_Defl/Paid_Cnt_Incr,
                                             0),0),
          
          
          
          Incr_Paid_Infl_Norm_Rptd = round( Incr_Paid_Infl/Rptd_Cnt_Yr_1 ,3),
          
          
          Incr_Paid_Infl_Norm_Rptd_Ln = round( log(Incr_Paid_Infl/Rptd_Cnt_Yr_1),3),
          
          
          ,
          Incr_Paid_Defl_Norm_Rptd = round( Incr_Paid_Defl/Rptd_Cnt_Yr_1,0),
          
          
          
          Incr_Paid_Defl_Norm_Rptd_Ln = round( log(Incr_Paid_Defl/(Rptd_Cnt_Yr_1)),3),
          
          
          
          Cum_Incr_Paid_Infl_Norm_Rptd = round( Cum_Incr_Paid_Infl/Rptd_Cnt_Yr_1,0),
          
          
          Avg_Case_Rsv = round( if_else(Open_Cnt >0 ,Case_Rsv/Open_Cnt,0),0),
          
          
          
          Cum_Incr_Paid_Defl_Norm_Rptd = round(  Cum_Incr_Paid_Defl/Rptd_Cnt_Yr_1,0),
          
          
          Case_Incrd_Norm_Rptd = round(Case_Incurred/Rptd_Cnt_Yr_1,0),
          
          
          Cal_Yr = Acc_Yr + Dev_Yr -1, 
          
          Accum_Infl_Index_Mid =round(Accum_Infl_Index_Mid,3)
          
          
          
  )




#create the grouped data set for random group 

## limit the data to show only results before calendar year 2024

data_aggregated_normalized_train_Case_Mixture  <- data_aggregated_normalized_Case_Mixture |>
  filter(Cal_Yr < 2026)


data_aggregated_normalized_ran_grp_train_Case_Mixture  <- data_aggregated_normalized_ran_grp_Case_Mixture |>
  filter(Cal_Yr < 2026)


data_simple_case_all <- data_aggregated_normalized_Case_Mixture |>
  select(Acc_Yr,Dev_Yr,
         Rptd_Cnt_Yr_1,
         Open_Cnt,
         Paid_Cnt_Incr,
         Incr_Paid_Infl ,
         
         
         Case_Rsv ,
         Case_Incurred,
         Incr_Clsd_W_Pay_Cnt,
         Incr_Clsd_W_O_Pay_Cnt,
         Cum_Incr_Paid_Infl,
         
         Cum_Incr_Clsd_W_Pay_Cnt ,
         Cum_Rptd_Cnt,
         Cum_Incr_Clsd_W_O_Pay_Cnt) |>
  rename( 
          Incr_Paid = Incr_Paid_Infl)

Case_Mixture_train <- data_simple_case_all |>
  mutate( Cal_Yr = Acc_Yr + Dev_Yr -1) |>
  filter( Cal_Yr <= 2025)


data_simple_case_ran_grp_all <- data_aggregated_normalized_ran_grp_Case_Mixture |>
  select(Acc_Yr,Dev_Yr,ran_grp,
         Rptd_Cnt_Yr_1,
         Open_Cnt,
         Paid_Cnt_Incr,
         Incr_Paid_Infl ,
         
         
         Case_Rsv ,
         Case_Incurred,
         Incr_Clsd_W_Pay_Cnt,
         Incr_Clsd_W_O_Pay_Cnt,
         Cum_Incr_Paid_Infl,
         
         Cum_Incr_Clsd_W_Pay_Cnt ,
         Cum_Rptd_Cnt,
         Cum_Incr_Clsd_W_O_Pay_Cnt) |>
  rename( 
    Incr_Paid = Incr_Paid_Infl)

Case_Mixture_ran_grp_train <- data_simple_case_ran_grp_all |>
  mutate( Cal_Yr = Acc_Yr + Dev_Yr -1) |>
  filter( Cal_Yr <= 2025)

# create the Excel files that are imported to start the analysis


write.csv2(Case_Mixture_train, file = "Case_Mixture_train.csv")

write_xlsx(Case_Mixture_train,  "Case_Mixture_train.xlsx")




write.csv2(Case_Mixture_ran_grp_train, file = "Case_Mixture_ran_grp_train.csv")

write_xlsx(Case_Mixture_ran_grp_train,  "Case_Mixture_ran_grp_train.xlsx")


