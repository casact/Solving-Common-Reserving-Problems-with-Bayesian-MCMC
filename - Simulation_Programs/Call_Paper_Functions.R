EDA_Base_Incr <- function(df_train,df_train_non_zero,
                          df_train_non_zero_sum,
                          df_train_non_zero_sum_ran_grp,
                          df_train_zero_count
                          ) {
  
  caption_label <- rlang::englue("Data Source: {{df_train}}")
  
  
  
  p1 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Incr_PP,group=Dev_Yr))+ geom_boxplot()+
    labs(title="Box Plot of  Incremental Pure Premium\n Non_Zero Incremental Count  Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Pure Premium"   ) 
  
 
  p2 <- ggplot(data =df_train, aes(x=Dev_Yr,y=Incr_PP,group=Dev_Yr))+ geom_boxplot()+
    labs(title="Box Plot of  Incremental Pure Premium\n   Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Pure Premium"   ) 
  
  
  p3 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Incr_PP,group=Dev_Yr))+ 
    geom_point(aes(color =Cal_Yr))+
    labs(title="Scatter Plot of  Incremental Pure Premium\n Non_Zero Count  Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Pure Premium"   ) 
  
  
  p4 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Ln_Incr_PP,group=Dev_Yr))+ geom_boxplot()+
    labs(title="Box Plot of Log Incremental Pure Premium \n Non_Zero Count  Across Unit & Accident Year  ",
         x= " Development Year",
         y="Log Incremental Pure Premium" ,
         caption = caption_label, )
  
  
  
  p5 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Ln_Incr_PP_Defl,group=Dev_Yr))+ geom_boxplot()+
    labs(title="Box Plot Log Incremental Pure Premium Deflated Non_Zero Paid\n Across Unit & Accident Year  ",
         x= " Development Year",
         y="Log Incremental Pure Premium Deflated"       ,
         caption = caption_label,) 
  
  
  
  
  p6 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Ln_Incr_PP_Defl, 
                                            ,group =Dev_Yr, color= Cal_Yr))+ 
    geom_point()+
       geom_smooth() +
    labs(title="Log Incremental Pure Premium Deflated Non_Zero Paid\n Across Unit & Accident Year (Loess Curve)",
         x= " Development Year",
         y="Log Incremental Pure Premium Deflated"       ,
         caption = caption_label,) 
  
  
  p7 <-     ggplot(data =df_train_non_zero , aes(sample=Ln_Incr_PP_Defl,group=Dev_Yr))+ geom_qq()+
    labs(title="QQ Plot Log Incremental Deflated Pure Premium \n 
         Non_Zero Count Across Accident Year by Development Year", 
         caption = caption_label,
         
         y="Log Incremental Deflated Pure Premium"         ) +
    facet_wrap(~Dev_Yr)
  
  
  
  p8 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Mean_Payment_Defl,color = Cal_Yr))+
    geom_point ()+
    labs(title="Scatter Plot of  Incremental Deflated Average Payment\n Non_Zero Count  Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Deflated Average Payment"   ) 
  
  p9 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Mean_Payment,color = Cal_Yr))+
    geom_point()+
    labs(title="Scatter Plot of  Incremental Average Payment\n Non_Zero Count  Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Average Payment"   ) 
  
  p10 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Paid_Incr_Cnt_Freq,group=Dev_Yr))+
    geom_boxplot()+
    labs(title="Box Plot Paid Incremental Freq Non_Zero Paid\n Across Unit & Accident Year  ",
         x= " Development Year",
         y="Paid Incremental Count/Reported 12 Months" ,
         caption = caption_label,) 
  
  
  
  p11 <- ggplot(data =df_train, aes(x=Dev_Yr,y=Paid_Incr_Cnt_Freq,color = Cal_Yr))+
    geom_point()+
    labs(title="Box Plot Paid Freq All Observations \n Across Unit & Accident Year  ",
         x= " Development Year",
         y=" Paid Incremental Count/ Reported 12 Months"    ,
         caption = caption_label ) 
  
  p12 <- ggplot(data =df_train, aes(x=Dev_Yr,y=Disp_Rate,color = Cal_Yr))+
    geom_point()+
    labs(title="Box Plot Disposal Rate by Development Year\n Across Unit & Accident Year  ",
         x= " Development Year",
         y=" Closed Count/ Available Count"    ,
         caption = caption_label ) 
  
  
  p13 <- ggplot(data =df_train_zero_count, aes(x=Dev_Yr,y=Prob_Zero,group=Dev_Yr))+ 
    geom_point()+
    labs(title="Probability Zero Paid by Development Year\n Across  Accident Years  ",
         caption = caption_label,
         x="Development Year",
         y= "Percentage Zero Payments") 
  
  p14 <- ggplot(data =df_train_non_zero_sum, aes(x=Dev_Yr,y=Ln_Incr_PP_mu,group=Dev_Yr))+ 
    geom_point()+
    
    labs(title="Mean Natural Log of Incremental Pure Premium \n Across  Accident Years\n Excluding Zero Pay Observations  ",
         caption = caption_label,
         x="Development Year",
         y= "Mean Log  PP") 
  
  
  
  
  
  
  
  
  
  
  p15 <- ggplot(data =df_train_non_zero_sum_ran_grp, aes(x=Dev_Yr,y=Ln_Incr_PP_Defl_sd_adj) )+
    geom_point()+                                                            
    geom_smooth() +
    labs(title="Standard Deviation Incremental PP Deflated\n Natural Log Within Accident Year
         Sample Size Adjusted",
         x ="Development Year",
         y= "Std Dev Log Deflated Incremental Pure Premium",
         caption = caption_label ) 
  
  
  p16 <- ggplot(data =df_train_non_zero_sum_ran_grp, aes(x=Dev_Yr,
                                                         y=Ln_Ln_Incr_PP_Defl_sd_adj
  ))+
    geom_point()+
    geom_smooth() +
    labs(title="Standard Deviation Incremental PP Deflated\n Natural Log of Log Natural Log
         Sample Size Adjusted Within Accident Year",
         x ="Development Year",
         y= "Log Std Dev Log Deflated Incremental Pure Premium",
         caption = caption_label ) 
  
  
  
  p17 <- ggplot(data =df_train_non_zero_sum_ran_grp, aes(x=Dev_Yr,
                                                         y=Incr_PP_Defl_shape_adj ) )+
    
    geom_point()+
    geom_smooth() +
    labs(title="Gamma Shape Incremental PP Deflated\n Method of Moments Within Accident Year
         Size Adjusted",
         x ="Development Year",
         y= "Gamma Shape Incremental Pure Premium",
         caption = caption_label ) 
  
  
  p18 <- ggplot(data =df_train_non_zero_sum_ran_grp, aes(x=Dev_Yr,
                                                         y=Ln_Incr_PP_Defl_shape_adj
  ))+
    geom_point()+
    geom_smooth() +
    labs(title="Natural Log Gamma Shape Incremental PP Deflated\n Method of Moments Within Accident Year
         Sample Size Adjusted",
         x ="Development Year",
         y= "Log Gamma Shape Incremental Pure Premium",
         caption = caption_label ) 
  
  
  
  return (list(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14,
               p15, p16, p17, p18))
  
}


EDA_Acc_Yr_Grp_Incr <- function(df_train,df_train_non_zero,
                                df_train_non_zero_sum,
                                df_train_non_zero_sum_ran_grp,
                                df_train_zero_count) {
  
  caption_label <- rlang::englue("Data Source: {{df_train}}")
  
  
  
  p1 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Incr_PP,group=Dev_Yr  ))+ 
    geom_boxplot()+
    labs(title="Box Plot of  Incremental Pure Premium\n Non_Zero Incremental Count  Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Pure Premium"   ) 
  
  
  p2 <- ggplot(data =df_train, aes(x=Dev_Yr,y=Incr_PP,group=Dev_Yr,
                                   ))+
    
    geom_boxplot()+
    labs(title="Box Plot of  Incremental Pure Premium\n   Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Pure Premium"   ) 
  
  
  p3 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Incr_PP,group=Dev_Yr,color =Acc_Yr_Grp))+ 
    geom_point()+
    labs(title="Scatter Plot of  Incremental Pure Premium\n Non_Zero Count  Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Pure Premium"   ) 
  
  
  p4 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Ln_Incr_PP,group=Dev_Yr      ))+ 
    geom_boxplot()+
    labs(title="Box Plot of Log Incremental Pure Premium \n Non_Zero Count  Across Unit & Accident Year  ",
         x= " Development Year",
         y="Log Incremental Pure Premium" ,
         caption = caption_label, )
  
  
  
  p5 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Ln_Incr_PP_Defl,group=Dev_Yr))+ 
    geom_boxplot()+
    labs(title="Box Plot Log Incremental Pure Premium Deflated Non_Zero Paid\n Across Unit & Accident Year  ",
         x= " Development Year",
         y="Log Incremental Pure Premium Deflated"       ,
         caption = caption_label,) 
  
  
  
  
  p6 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Ln_Incr_PP_Defl, color= Acc_Yr_Grp))+ 
    geom_point()+
    geom_smooth() +
    labs(title="Log Incremental Pure Premium Deflated Non_Zero Paid\n Across Unit & Accident Year (Loess Curve)",
         x= " Development Year",
         y="Log Incremental Pure Premium Deflated"       ,
         caption = caption_label,) 
  
  
  p7 <-     ggplot(data =df_train_non_zero , aes(sample=Ln_Incr_PP_Defl,
                                                 group=Acc_Yr_Grp,color =Acc_Yr_Grp))+ 
    geom_qq()+
    labs(title="QQ Plot Log Incremental Deflated Pure Premium \n 
         Non_Zero Count Across Accident Year by Development Year", 
         caption = caption_label,
         
         y="Log Incremental Deflated Pure Premium"         ) +
    facet_wrap(~Dev_Yr)
  
  
  
  p8 <-   ggplot(data =df_train_non_zero, aes( x =  Acc_Yr, y = Ln_Incr_PP_Defl,
                                               color= Acc_Yr_Grp   )) +
    geom_point( ) +
    labs(title = "Log Incremental Deflated Pure Premium  Development Year by Accident Year",
         
         caption = caption_label,
         x ="Calendar Year ",
         y= "Log Incremental Deflated Pure Premium") +
    facet_wrap(~Dev_Yr)
  
  
  p9 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Mean_Payment_Defl,color = Acc_Yr_Grp))+
    geom_point ()+
    labs(title="Scatter Plot of  Incremental Deflated Average Payment\n Non_Zero Count  Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Deflated Average Payment"   ) 
  
  p10 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Mean_Payment,color = Acc_Yr_Grp))+
    geom_point()+
    labs(title="Scatter Plot of  Incremental Average Payment\n Non_Zero Count  Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Incremental Average Payment"   ) 
  
  p11 <- ggplot(data =df_train_non_zero, aes(x=Dev_Yr,y=Paid_Incr_Cnt_Freq,group=Dev_Yr,
                                             ))+
    geom_boxplot()+
    labs(title="Box Plot Paid Incremental Freq Non_Zero Paid\n Across Unit & Accident Year  ",
         x= " Development Year",
         y="Paid Incremental Count/Reported 12 Months" ,
         caption = caption_label,) 
  
  
  
  p12 <- ggplot(data =df_train, aes(x=Dev_Yr,y=Paid_Incr_Cnt_Freq,color = Acc_Yr_Grp))+
    geom_point()+
    labs(title="Scatter Plot Paid Freq All Observations \n Across Unit & Accident Year  ",
         x= " Development Year",
         y=" Paid Incremental Count/ Reported 12 Months"    ,
         caption = caption_label ) 
  
  p13 <- ggplot(data =df_train, aes(x=Dev_Yr,y=Disp_Rate,color = Acc_Yr_Grp))+
    geom_point()+
    labs(title="Scatter Plot Disposal Rate by Development Year\n Across Unit & Accident Year  ",
         x= " Development Year",
         y="Closed Count/ Available Count"    ,
         caption = caption_label ) 
  
  
  p14 <- ggplot(data =df_train_zero_count, aes(x=Dev_Yr,y=Prob_Zero,group=Dev_Yr,
                                               color = Acc_Yr_Grp ))+ 
    geom_point()+
    labs(title="Probability Zero Paid by Development Year\n Across  Accident Years  ",
         caption = caption_label,
         x="Development Year",
         y= "Percentage Zero Payments") 
  
  p15 <- ggplot(data =df_train_non_zero_sum, 
                aes(x=Dev_Yr,y=Ln_Incr_PP_mu,group=Dev_Yr,
                                                 color = Acc_Yr_Grp ))+
    geom_point()+
    labs(title="Mean Natural Log of Incremental Pure Premium \n Across  Accident Years\n Excluding Zero Pay Observations  ",
         caption = caption_label,
         x="Development Year",
         y= "Mean Log  PP") 
  
 
  
  
  
  
  
  
  
  
  
  p16 <- ggplot(data =df_train_non_zero_sum_ran_grp, aes(x=Dev_Yr,y=Ln_Incr_PP_Defl_sd_adj,
                                                         color = Acc_Yr_Grp) )+
    geom_point()+                                                            
    geom_smooth() +
    labs(title="Standard Deviation Incremental PP Deflated\n Natural Log Within Accident Year
         Sample Size Adjusted",
         x ="Development Year",
         y= "Std Dev Log Deflated Incremental Pure Premium",
         caption = caption_label ) 
  
  
  p17 <- ggplot(data =df_train_non_zero_sum_ran_grp, aes(x=Dev_Yr,
                                                         y=Ln_Ln_Incr_PP_Defl_sd_adj,
                                                         color = Acc_Yr_Grp))+
    geom_point()+
    geom_smooth() +
    labs(title="Standard Deviation Incremental PP Deflated\n Natural Log of Log Natural Log
         Sample Size Adjusted Within Accident Year",
         x ="Development Year",
         y= "Log Std Dev Log Deflated Incremental Pure Premium",
         caption = caption_label ) 
  
  
  
  p18 <- ggplot(data =df_train_non_zero_sum_ran_grp, aes(x=Dev_Yr,
                                                         y=Incr_PP_Defl_shape_adj,
                                                         color = Acc_Yr_Grp) )+
    
    geom_point()+
    geom_smooth() +
    labs(title="Gamma Shape Incremental PP Deflated\n Method of Moments Within Accident Year
         Size Adjusted",
         x ="Development Year",
         y= "Gamma Shape Incremental Pure Premium",
         caption = caption_label ) 
  
  
  p19 <- ggplot(data =df_train_non_zero_sum_ran_grp, aes(x=Dev_Yr,
                                                         y=Ln_Incr_PP_Defl_shape_adj,
                                                         color = Acc_Yr_Grp))+
    geom_point()+
    geom_smooth() +
    labs(title="Natural Log Gamma Shape Incremental PP Deflated\n Method of Moments Within Accident Year
         Sample Size Adjusted",
         x ="Development Year",
         y= "Log Gamma Shape Incremental Pure Premium",
         caption = caption_label ) 
  
  
  
  return (list(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14,
               p15,  p16,
               p17, p18, p19))
  
}




EDA_Base_Cumulative <- function(df_train) {
  
  caption_label <- rlang::englue("Data Source: {{df_train}}")
  
  min_Cal_Yr <- as.numeric(df_train|>
                             select(min_Acc_Yr) |>
                             unique() )
  
  max_Cal_Yr <- as.numeric(df_train|>
                             select(max_Acc_Yr) |>
                             unique() )
  
  
p1 <-  ggplot(data =df_train, aes(x=Dev_Yr,y=Cum_Rptd_Cnt,group=Acc_Yr))+ 
    geom_line(aes(color =Acc_Yr))+
    labs(title=" Cumulative Reported Claim Count\n   Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Cumulative Reported Count"   ) 
  
p2 <-  ggplot(data =df_train, aes(x=Dev_Yr,y=Open_Cnt,group=Acc_Yr))+ 
    geom_line(aes(color =Acc_Yr))+
    labs(title=" Open Claim Count\n   Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Open Claim Count"   ) 
  
p3 <-  ggplot(data =df_train, aes(x=Dev_Yr,y=Cum_Incr_Clsd_W_Pay_Cnt,group=Acc_Yr))+ 
    geom_line(aes(color =Acc_Yr))+
    labs(title=" Cumulative Closed With Payment Count\n   Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Cumulative Closed With Payment Count"   ) 
  
  
p4 <-  ggplot(data =df_train, aes(x=Dev_Yr,y=Cum_Incr_Clsd_W_O_Pay_Cnt,group=Acc_Yr))+ 
    geom_line(aes(color =Acc_Yr))+
    labs(title=" Cumulative Closed Without Payment Count\n   Across Accident Year  ",
         #      subtitle =sub_label_train,
         caption = caption_label,
         y="Cumulative Closed With Payment Count"   ) 
  
  
  
p5 <-  ggplot(data =df_train, aes(x=Dev_Yr,y=Cum_Incr_Paid_Infl,group=Acc_Yr))+ 
    geom_line(aes(color =Acc_Yr))+
    labs(title=" Cumulative Paid Including Inflation\n Across Accident Year  ",
         caption = caption_label,
         x= " Development Year",
         y="Cumulative Paid With Inflation"   ) 
  
p6 <-  ggplot(data =df_train, aes(x=Cal_Yr,y=Accum_Infl_Index_Mid))+ 
    geom_line()+
    labs(title=" Cumulative Mid_Year Inflation Index\n by Calendar Year  ",
         caption = caption_label,
         x= " Calendar Year Year",
         y="Cumulative Mid-Year Inflation Index"   ) +
    scale_x_continuous(breaks = seq(min_Cal_Yr,max_Cal_Yr, by = 1)) +
    guides(x = guide_axis(angle = 90))
  
p7 <-  ggplot(data =df_train, aes(x=Cal_Yr,y=Infl_Index_Pct))+ 
    geom_line()+
    labs(title=" Annual Inflation Index Percentage Change\n by Calendar Year  ",
         caption = caption_label,
         x= " Calendar Year Year",
         y="Annual Inflation Index"   ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(min_Cal_Yr,max_Cal_Yr, by = 1)) +
    guides(x = guide_axis(angle = 90))
  

  
  return (list(p1,p2, p3, p4, p5, p6, p7))
  
}




## Gather summary information after running a model to see results of prior selection. 
## This set of summary information is set up to label the results as 
## being built without data.
## The prior summary gives you all of the priors used by the model, rather
## than just the ones you chose to specify.

model_summary_information_no_data <- function(model_name) {
  model_parameters_print <- rlang::englue("No Data Summary Statistics for: {{model_name}}")
  
  model_priors_print   <- rlang::englue("Prior Distribution for: {{model_name}}")
  
  model_variables_print   <- rlang::englue("List of variables for: {{model_name}}")
  
  model_summary <- summary({{model_name}})
  
  model_prior <- prior_summary({{model_name}})
  
  model_variable <- get_variables({{model_name}})
  
  
  
  return(list(model_parameters_print, 
              model_summary, 
              model_priors_print , 
              model_prior, 
              model_variables_print ,  
              model_variable))
  
}         


model_summary_information <- function(model_name) {
  model_parameters_print <- rlang::englue("Summary Statistics for: {{model_name}}")
  
  model_priors_print   <- rlang::englue("Prior Distribution for: {{model_name}}")
  
  model_variables_print   <- rlang::englue("List of variables for: {{model_name}}")
  
  model_summary <- summary({{model_name}})
  
  model_prior <- prior_summary({{model_name}})
  
  model_variable <- get_variables({{model_name}})
  
  
  
  return(list(model_parameters_print, 
              model_summary, 
              model_priors_print , 
              model_prior, 
              model_variables_print ,  
              model_variable))
  
}         




model_fit_lognormal <- function(model_name,model_data ,
                                summarized_data,
                                summarized_data_ran_grp) {
  
  sub_label_fit <- rlang::englue(" Model: {{model_name}}")
  caption_label <- rlang::englue("Data Source: {{model_data}} ")
  
  min_Cal_Yr <- as.numeric(model_data |>
                             summarize(min_Cal_Yr =min(Cal_Yr)) |>
                             select(min_Cal_Yr) |>
                             unique()
  )
  
  
  max_Cal_Yr <- as.numeric(model_data |>
                             summarize(min_Cal_Yr =max(Cal_Yr)) |>
                             select(min_Cal_Yr) |>
                             unique()
  )
  
  
  min_Acc_Yr <- as.numeric(model_data |>
                             summarize(min_Acc_Yr =min(Acc_Yr)) |>
                             select(min_Acc_Yr) |>
                             unique()
  )
  
  
  max_Acc_Yr <- as.numeric(model_data |>
                             summarize(min_Acc_Yr =max(Acc_Yr)) |>
                             select(min_Acc_Yr) |>
                             unique()
  )
  
  
  
  
  
  p1 <-  model_name|>
    spread_draws(b_Intercept,r_Acc_Yr[Acc_Yr,Intercept]) |>
    median_qi(condition_median_mu = b_Intercept + r_Acc_Yr, .width = c(.95, .66))|>
    ggplot(aes(y = Acc_Yr, x = condition_median_mu, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() +
    labs(title = "Median fitted Adjusted Intercept by Accident Year
           Deflated Incremental Pure Premium Model",
         subtitle = sub_label_fit,
         caption = caption_label,
         y = "Accident Year",
         x = "Overall Intercept + Accident Year Adjustment")
  
  
  model_result <- model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_epred_draws(model_name, 
                    dpar = c("mu", "sigma")) %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    mutate(residuals_defl=Incr_PP_Defl-.epred,
           residuals_ln_defl = Ln_Incr_PP_Defl- log(.epred),
           
           mean_expected_PP_defl = mean(.epred),
           sd_expected_PP_defl = sd(.epred),
           sd_expected_PP_ln_defl = sd(log(.epred)),
           mean_residuals_defl = mean(residuals_defl),
           sd_residuals_defl =sd(residuals_defl),
           sd_residuals_ln_defl =sd(residuals_ln_defl),
           Norm_Resid_All_defl =(residuals_defl)/sd_residuals_defl,
           Norm_Resid_All_Ln_defl= (residuals_ln_defl)/sd_residuals_ln_defl,
           mean_all_residuals_defl =mean(residuals_defl),
           std_all_residuals_defl = sd(residuals_defl),
           residuals=(Incr_PP-.epred* Accum_Infl_Index_Mid),
           residuals_ln = Ln_Incr_PP- log(.epred* Accum_Infl_Index_Mid),
           
           mean_expected_PP = mean(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP = sd(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP_ln = sd(log(.epred * Accum_Infl_Index_Mid)),
           Norm_Resid_All =(residuals)/sd_expected_PP,
           Norm_Resid_All_Ln= (residuals_ln)/sd_expected_PP_ln,
           mean_all_residuals =mean(residuals),
           mean_normalized_residuals_defl = mean(Norm_Resid_All),
           std_all_residuals = sd(residuals)
    ) %>%
    select(Acc_Yr, Dev_Yr, Cal_Yr, residuals,residuals_ln,
           Norm_Resid_All, Norm_Resid_All_Ln,
           mu, sigma, .epred, 
           Incr_PP, Ln_Incr_PP, 
           mean_all_residuals,
           mean_normalized_residuals_defl,
           std_all_residuals, 
           mean_expected_PP,
           sd_expected_PP,
           sd_residuals_defl,
           residuals_defl,
           sd_residuals_ln_defl,
           Norm_Resid_All_defl, Norm_Resid_All_Ln_defl,
           .epred,
           Incr_PP_Defl, Ln_Incr_PP_Defl, 
           mean_all_residuals_defl,
           std_all_residuals_defl, 
           mean_expected_PP_defl,
           sd_expected_PP_defl)
  
  
  
  model_result_mean <- model_result |>
    select(
      Acc_Yr, Dev_Yr, Cal_Yr,
      
      mean_all_residuals_defl,
      mean_normalized_residuals_defl
    ) |>
    unique()
  
  model_result_mu_sigma <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_epred_draws(model_name, 
                    
                    dpar = c("mu", "sigma"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    select(mu,sigma, .epred, Acc_Yr,Dev_Yr, Cal_Yr) |>
    group_by(Dev_Yr) |>
    select(Dev_Yr, mu, sigma, .epred ) |>
    mutate(
           Ln_Incr_PP_Defl_mu = mean(mu),
           
           Ln_Incr_PP_Defl_sd_adj =mean(sigma),
           type ="model") |>
    select(Dev_Yr,
           Ln_Incr_PP_Defl_mu ,
           
           Ln_Incr_PP_Defl_sd_adj,
           type) |>
    unique()
  
  model_result_mu_sigma_linpred <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_linpred_draws(model_name, 
                    
                    dpar = c("mu", "sigma"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    select(mu,sigma, .linpred, Acc_Yr,Dev_Yr, Cal_Yr) |>
    group_by(Dev_Yr) |>
    select(Dev_Yr, mu, sigma, .linpred ) |>
    mutate(
      Ln_Incr_PP_Defl_mu = mean(mu),
      
      Ln_Ln_Incr_PP_Defl_sd_adj =mean(sigma),
      type ="model_linpred") |>
    
    select(Dev_Yr,
           Ln_Incr_PP_Defl_mu ,
           
           Ln_Ln_Incr_PP_Defl_sd_adj,
           type) |>
    unique()
  
  data_mu <- summarized_data |>
    mutate (type ="data") |>
    select(Dev_Yr,
           Ln_Incr_PP_Defl_mu ,
           
           type)
  
  
  data_sigma <- summarized_data_ran_grp |>
    mutate (type ="data") |>
    select(Dev_Yr,
           
           Ln_Incr_PP_Defl_sd_adj,
           Ln_Ln_Incr_PP_Defl_sd_adj,
           type)
  
  data_mu_sigma <- left_join(data_mu,
                             data_sigma,
                              by = join_by(Dev_Yr))|>
                    rename(type = type.y)
 
  
  
  
  compare_mu_sigma <- rbind(data_mu_sigma,
                            model_result_mu_sigma
                            )
  
  compare_ln_sigma <- rbind(data_mu_sigma,
                            model_result_mu_sigma_linpred 
                            )
  
  
  
  ## Graph comparison of model to actual using ggplot and tidybayes data set
  ## not all of the graphs were published in the interest of conserving space in the paper
  
  p2 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p3 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred, color = Dev_Yr)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p4 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Predicted Pure Premium Deflated\n  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  p5 <- model_result %>%
    ggplot(aes(x =  Dev_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Development Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "Residuals Pure Premium Deflated Normalized")
  
  p6 <- model_result %>%
    ggplot(aes(x =  Acc_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Accident Year vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Accident Year",
         y= "Residuals Pure Premium Deflated Normalized") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_continuous(breaks = seq(min_Acc_Yr,max_Acc_Yr, by = 1)) +
    guides(x = guide_axis(angle = 90)) +
    facet_wrap(~Cal_Yr)
  
  p7 <- model_result %>%
    ggplot(aes(x =  Cal_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Calendar Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Calendar Year",
         y= "Residuals Pure Premium Deflated Normalized") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_continuous(breaks = seq(min_Cal_Yr,max_Cal_Yr, by = 1)) +
    facet_wrap(~Dev_Yr)
  
  
  p8 <- model_result %>%
    ggplot(aes(x =  Cal_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Calendar Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Calendar Year",
         y= "Residuals Pure Premium Deflated Normalized") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_continuous(breaks = seq(min_Cal_Yr,max_Cal_Yr, by = 1)) +
    facet_wrap(~Dev_Yr)
  
  
  
  p9 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl,
               color= Dev_Yr)) +
    stat_pointinterval( )+
    labs(title = "Predicted Pure Premium Deflated\ vs. Normalized Residuals (Dev_Yr)",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  
  
  
  
  p10  <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = .epred)) +
    stat_pointinterval()+
    labs(title = "Distribution of Predicted Deflated Pure Premium\n by Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "Predicted Pure Premium Deflated")
  
  p11 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = mu)) +
    stat_pointinterval()+
    labs(title = "Estimated mu vs. Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "mu for lognormal")
  
  p12 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = sigma)) +
    stat_pointinterval()+
    labs(title = "Estimated sigma vs. Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "sigma for lognormal")
  
  
  
  p13 <- compare_mu_sigma %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_mu, color =type)) +
    stat_pointinterval()+
    labs(title = "Estimated mu vs. Data Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "mu for lognormal")
  
  p14 <- compare_mu_sigma %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_sd_adj,color= type)) +
    stat_pointinterval()+
    labs(title = "Estimated sigma vs. Data by Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "sigma for lognormal")
  
  
  p15 <- compare_ln_sigma %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Ln_Incr_PP_Defl_sd_adj,color= type)) +
    stat_pointinterval()+
    labs(title = "Estimated log sigma vs. Data by Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "log sigma for lognormal")
  
  
  
  
  
  return(list(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10,
              p11,p12,p13, p14 , p15))
  
}



model_fit_lognormal_Acc_Yr_Grp <- function(model_name,
                                           model_data , 
                                           summarized_data,
                                           summarized_data_ran_grp) {
  
  sub_label_fit <- rlang::englue(" Model: {{model_name}}")
  caption_label <- rlang::englue("Data Source: {{model_data}} ")
  
  min_Cal_Yr <- as.numeric(model_data |>
                             summarize(min_Cal_Yr =min(Cal_Yr)) |>
                             select(min_Cal_Yr) |>
                             unique()
  )
  
  
  max_Cal_Yr <- as.numeric(model_data |>
                             summarize(min_Cal_Yr =max(Cal_Yr)) |>
                             select(min_Cal_Yr) |>
                             unique()
  )
  
  
  min_Acc_Yr <- as.numeric(model_data |>
                             summarize(min_Acc_Yr =min(Acc_Yr)) |>
                             select(min_Acc_Yr) |>
                             unique()
  )
  
  
  max_Acc_Yr <- as.numeric(model_data |>
                             summarize(min_Acc_Yr =max(Acc_Yr)) |>
                             select(min_Acc_Yr) |>
                             unique()
  )
  
  
  
  
  
  p1 <-  model_name|>
    spread_draws(b_Intercept,r_Acc_Yr[Acc_Yr,Intercept]) |>
    median_qi(condition_median_mu = b_Intercept + r_Acc_Yr, .width = c(.95, .66))|>
    ggplot(aes(y = Acc_Yr, x = condition_median_mu, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() +
    labs(title = "Median fitted Adjusted Intercept by Accident Year
           Deflated Incremental Pure Premium Model",
         subtitle = sub_label_fit,
         caption = caption_label,
         y = "Accident Year",
         x = "Overall Intercept + Accident Year Adjustment")
  
  
  model_result <- model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr,Acc_Yr_Grp) %>%
    add_epred_draws(model_name, 
                    dpar = c("mu", "sigma")) %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr, Acc_Yr_Grp) %>%
    mutate(residuals_defl=Incr_PP_Defl-.epred,
           residuals_ln_defl = Ln_Incr_PP_Defl- log(.epred),
           
           mean_expected_PP_defl = mean(.epred),
           sd_expected_PP_defl = sd(.epred),
           sd_expected_PP_ln_defl = sd(log(.epred)),
           mean_residuals_defl = mean(residuals_defl),
           sd_residuals_defl =sd(residuals_defl),
           sd_residuals_ln_defl =sd(residuals_ln_defl),
           Norm_Resid_All_defl =(residuals_defl)/sd_residuals_defl,
           Norm_Resid_All_Ln_defl= (residuals_ln_defl)/sd_residuals_ln_defl,
           mean_all_residuals_defl =mean(residuals_defl),
           std_all_residuals_defl = sd(residuals_defl),
           residuals=(Incr_PP-.epred* Accum_Infl_Index_Mid),
           residuals_ln = Ln_Incr_PP- log(.epred* Accum_Infl_Index_Mid),
           
           mean_expected_PP = mean(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP = sd(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP_ln = sd(log(.epred * Accum_Infl_Index_Mid)),
           Norm_Resid_All =(residuals)/sd_expected_PP,
           Norm_Resid_All_Ln= (residuals_ln)/sd_expected_PP_ln,
           mean_all_residuals =mean(residuals),
           mean_normalized_residuals_defl = mean(Norm_Resid_All),
           std_all_residuals = sd(residuals)
    ) %>%
    select(Acc_Yr, Dev_Yr, Cal_Yr, Acc_Yr_Grp,
           residuals,residuals_ln,
           Norm_Resid_All, Norm_Resid_All_Ln,
           mu, sigma, .epred, 
           Incr_PP, Ln_Incr_PP, 
           mean_all_residuals,
           mean_normalized_residuals_defl,
           std_all_residuals, 
           mean_expected_PP,
           sd_expected_PP,
           sd_residuals_defl,
           residuals_defl,
           sd_residuals_ln_defl,
           Norm_Resid_All_defl, Norm_Resid_All_Ln_defl,
           .epred,
           Incr_PP_Defl, Ln_Incr_PP_Defl, 
           mean_all_residuals_defl,
           std_all_residuals_defl, 
           mean_expected_PP_defl,
           sd_expected_PP_defl)
  
  
  
  model_result_mean <- model_result |>
    select(
      Acc_Yr, Dev_Yr, Cal_Yr, Acc_Yr_Grp,
      
      mean_all_residuals_defl,
      mean_normalized_residuals_defl
    ) |>
    unique()
  
  model_result_mu_sigma <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr,Acc_Yr_Grp) %>%
    add_epred_draws(model_name, 
                    
                    dpar = c("mu", "sigma"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr,Acc_Yr_Grp) %>%
    select(mu,sigma, .epred, Acc_Yr,Dev_Yr, Cal_Yr,Acc_Yr_Grp) |>
    group_by(Dev_Yr,Acc_Yr_Grp) |>
    select(Dev_Yr, mu, sigma, .epred ,Acc_Yr_Grp) |>
    mutate(
      Ln_Incr_PP_Defl_mu = mean(mu),
      
      Ln_Incr_PP_Defl_sd_adj =mean(sigma),
      type ="model") |>
    select(Dev_Yr,Acc_Yr_Grp,
           Ln_Incr_PP_Defl_mu ,
           
           Ln_Incr_PP_Defl_sd_adj,
           type) |>
    unique()
  
  model_result_mu_sigma_linpred <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr,Acc_Yr_Grp) %>%
    add_linpred_draws(model_name, 
                      
                      dpar = c("mu", "sigma"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr,Acc_Yr_Grp) %>%
    select(mu,sigma, .linpred, Acc_Yr,Dev_Yr, 
           Cal_Yr,Acc_Yr_Grp) |>
    group_by(Dev_Yr,Acc_Yr_Grp) |>
    select(Dev_Yr, mu, sigma, .linpred,Acc_Yr_Grp ) |>
    mutate(
      Ln_Incr_PP_Defl_mu = mean(mu),
      
      Ln_Ln_Incr_PP_Defl_sd_adj =mean(sigma),
      type ="model_linpred") |>
    select(Dev_Yr,Acc_Yr_Grp,
           Ln_Incr_PP_Defl_mu ,
           
           Ln_Ln_Incr_PP_Defl_sd_adj,
           type) |>
    unique()
  
  
  data_mu <- summarized_data |>
    mutate (type ="data") |>
    select(Dev_Yr,Acc_Yr_Grp,
           Ln_Incr_PP_Defl_mu ,
           
           type)
  
  
  data_sigma <- summarized_data_ran_grp |>
    mutate (type ="data") |>
    select(Dev_Yr,Acc_Yr_Grp,
           
           Ln_Incr_PP_Defl_sd_adj,
           Ln_Ln_Incr_PP_Defl_sd_adj,
           type)
  
  data_mu_sigma <- left_join(data_mu,
                             data_sigma,
                             by = join_by(Dev_Yr,Acc_Yr_Grp))|>
    rename(type = type.y         )
  
  
  compare_mu_sigma <- rbind(data_mu_sigma,
                            model_result_mu_sigma,
                            )
  
 
  
  compare_ln_sigma <- rbind(data_mu_sigma,
                            model_result_mu_sigma_linpred 
  )
  
  
  ## Graph comparison of model to actual using ggplot and tidybayes data set
  ## not all of the graphs were published in the interest of conserving space in the paper
  
  p2 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred, color= Acc_Yr_Grp)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p3 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred, color = Dev_Yr)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p4 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl,color = Acc_Yr_Grp)) +
    stat_pointinterval()+
    labs(title = "Predicted Pure Premium Deflated\n  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  p5 <- model_result %>%
    ggplot(aes(x =  Dev_Yr, y = Norm_Resid_All_defl,color= Acc_Yr_Grp)) +
    stat_pointinterval(position = "dodge")+
    labs(title = "Development Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "Residuals Pure Premium Deflated Normalized")
  
  p6 <- model_result %>%
    ggplot(aes(x =  Acc_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Accident Year vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Accident Year",
         y= "Residuals Pure Premium Deflated Normalized") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_continuous(breaks = seq(min_Acc_Yr,max_Acc_Yr, by = 1)) +
    guides(x = guide_axis(angle = 90)) +
    facet_wrap(~Cal_Yr)
  
  p7 <- model_result %>%
    ggplot(aes(x =  Cal_Yr, y = Norm_Resid_All_defl, color= Acc_Yr_Grp)) +
    stat_pointinterval(position ="dodge")+
    labs(title = "Calendar Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Calendar Year",
         y= "Residuals Pure Premium Deflated Normalized") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_continuous(breaks = seq(min_Cal_Yr,max_Cal_Yr, by = 1)) +
    facet_wrap(~Dev_Yr)
  
  
  p8 <- model_result %>%
    ggplot(aes(x =  Cal_Yr, y = Norm_Resid_All_defl , color= Acc_Yr_Grp)) +
    stat_pointinterval(position = "dodge")+
    labs(title = "Calendar Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Calendar Year",
         y= "Residuals Pure Premium Deflated Normalized") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_continuous(breaks = seq(min_Cal_Yr,max_Cal_Yr, by = 1)) +
    facet_wrap(~Dev_Yr)
  
  
  
  p9 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl,
               color= Dev_Yr)) +
    stat_pointinterval( )+
    labs(title = "Predicted Pure Premium Deflated\ vs. Normalized Residuals (Dev_Yr)",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  
  
  
  
  p10 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = .epred, color = Acc_Yr_Grp)) +
    stat_pointinterval(position ="dodge")+
    labs(title = "Distribution of Predicted Deflated Pure Premium\n by Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "Predicted Pure Premium Deflated")
  
  p11 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = mu, color = Acc_Yr_Grp)) +
    stat_pointinterval(position = "dodge")+
    labs(title = "Estimated mu vs. Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "mu for lognormal")
  
  p12 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = sigma, color = Acc_Yr_Grp)) +
    stat_pointinterval(position = "dodge")+
    labs(title = "Estimated sigma vs. Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "sigma for lognormal")
  
  
  
  p13 <- compare_mu_sigma %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_mu, color =type, shape = Acc_Yr_Grp)) +
    stat_pointinterval()+
    labs(title = "Estimated mu vs. Data Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "mu for lognormal")
  
  p14 <- compare_mu_sigma %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_sd_adj,color= type,shape =Acc_Yr_Grp)) +
    stat_pointinterval()+
    labs(title = "Estimated sigma vs. Data by Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "sigma for lognormal")
  
  
 
  
  p15 <- compare_ln_sigma %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Ln_Incr_PP_Defl_sd_adj,
               , shape= Acc_Yr_Grp, color= type)) +
    stat_pointinterval()+
    labs(title = "Estimated log sigma vs. Data by Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "log sigma for lognormal")
  
  
  
  
  return(list(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10,
              p11,p12,p13, p14 , p15))
  
}




model_fit_hurdle_lognormal <- function(model_name,
                                       model_data ,
                                       summarized_data,
                                       summarized_data_ran_grp) {
  
  sub_label_fit <- rlang::englue(" Model: {{model_name}}")
  caption_label <- rlang::englue("Data Source: {{model_data}} ")
  
  min_Cal_Yr <- as.numeric(model_data |>
                             summarize(min_Cal_Yr =min(Cal_Yr)) |>
                             select(min_Cal_Yr) |>
                             unique()
  )
  
  
  max_Cal_Yr <- as.numeric(model_data |>
                             summarize(min_Cal_Yr =max(Cal_Yr)) |>
                             select(min_Cal_Yr) |>
                             unique()
  )
  
  
  min_Acc_Yr <- as.numeric(model_data |>
                             summarize(min_Acc_Yr =min(Acc_Yr)) |>
                             select(min_Acc_Yr) |>
                             unique()
  )
  
  
  max_Acc_Yr <- as.numeric(model_data |>
                             summarize(min_Acc_Yr =max(Acc_Yr)) |>
                             select(min_Acc_Yr) |>
                             unique()
  )
  
  model_result <- model_data %>%
    add_epred_draws(model_name,dpar = c("mu","sigma","hu")) %>%
    
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    mutate(residuals_defl=Incr_PP_Defl-.epred,
           residuals_ln_defl = Ln_Incr_PP_Defl- log(.epred),
           
           mean_expected_PP_defl = mean(.epred),
           sd_expected_PP_defl = sd(.epred),
           sd_expected_PP_ln_defl = sd(log(.epred)),
           mean_residuals_defl = mean(residuals_defl),
           sd_residuals_defl =sd(residuals_defl),
           sd_residuals_ln_defl =sd(residuals_ln_defl),
           Norm_Resid_All_defl =(residuals_defl)/sd_residuals_defl,
           Norm_Resid_All_Ln_defl= (residuals_ln_defl)/sd_residuals_ln_defl,
           mean_all_residuals_defl =mean(residuals_defl),
           std_all_residuals_defl = sd(residuals_defl),
           residuals=(Incr_PP-.epred* Accum_Infl_Index_Mid),
           residuals_ln = Ln_Incr_PP- log(.epred* Accum_Infl_Index_Mid),
           
           mean_expected_PP = mean(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP = sd(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP_ln = sd(log(.epred * Accum_Infl_Index_Mid)),
           Norm_Resid_All =(residuals)/sd_expected_PP,
           Norm_Resid_All_Ln= (residuals_ln)/sd_expected_PP_ln,
           mean_all_residuals =mean(residuals),
           std_all_residuals = sd(residuals)
    ) %>%
    select(Acc_Yr, Dev_Yr, Cal_Yr, residuals,residuals_ln,
           Norm_Resid_All, Norm_Resid_All_Ln,
           mu, sigma, hu, .epred, 
           Incr_PP, Ln_Incr_PP, 
           mean_all_residuals,
           std_all_residuals, 
           mean_expected_PP,
           sd_expected_PP,
           sd_residuals_defl,
           residuals_defl,
           sd_residuals_ln_defl,
           Norm_Resid_All_defl, Norm_Resid_All_Ln_defl,
           .epred,
           Incr_PP_Defl, Ln_Incr_PP_Defl, 
           mean_all_residuals_defl,
           std_all_residuals_defl, 
           mean_expected_PP_defl,
           sd_expected_PP_defl)
  
  ## Graphs for hurdle lognormal model
  
  
  model_result_mu_sigma <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_epred_draws(model_name, 
                    
                    dpar = c("mu", "sigma"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    select(mu,sigma, .epred, Acc_Yr,Dev_Yr, Cal_Yr) |>
    group_by(Dev_Yr) |>
    select(Dev_Yr, mu, sigma, .epred ) |>
    mutate(
      Ln_Incr_PP_Defl_mu = mean(mu),
      
      Ln_Incr_PP_Defl_sd_adj =mean(sigma),
      type ="model") |>
    select(Dev_Yr,
           Ln_Incr_PP_Defl_mu ,
           
           Ln_Incr_PP_Defl_sd_adj,
           type) |>
    unique()
  
  model_result_mu_sigma_linpred <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_linpred_draws(model_name, 
                      
                      dpar = c("mu", "sigma"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    select(mu,sigma, .linpred, Acc_Yr,Dev_Yr, Cal_Yr) |>
    group_by(Dev_Yr) |>
    select(Dev_Yr, mu, sigma, .linpred ) |>
    mutate(
      Ln_Incr_PP_Defl_mu = mean(mu),
      
      Ln_Ln_Incr_PP_Defl_sd_adj =mean(sigma),
      type ="model_linpred") |>
    select(Dev_Yr,
           Ln_Incr_PP_Defl_mu ,
           
           Ln_Ln_Incr_PP_Defl_sd_adj,
           type) |>
    unique()
  
  
  data_mu <- summarized_data |>
    mutate (type ="data") |>
    select(Dev_Yr,
           Ln_Incr_PP_Defl_mu ,
           
           type)
  
  
  data_sigma <- summarized_data_ran_grp |>
    mutate (type ="data") |>
    select(Dev_Yr,
           
           Ln_Incr_PP_Defl_sd_adj,
           Ln_Ln_Incr_PP_Defl_sd_adj,
           type)
  
  data_mu_sigma <- left_join(data_mu,
                             data_sigma,
                             by = join_by(Dev_Yr))|>
    rename(type = type.y)
  
  
  
  
  compare_mu_sigma <- rbind(data_mu_sigma,
                            model_result_mu_sigma
  )
  
  compare_ln_sigma <- rbind(data_mu_sigma,
                            model_result_mu_sigma_linpred 
  )
  
  
  
  
  ## Graph comparison of model to actual using ggplot and tidybayes data set
  ## not all of the graphs were published in the interest of conserving space in the paper
  
  
  p1 <-  model_name|>
    spread_draws(b_Intercept,r_Acc_Yr[Acc_Yr,Intercept]) |>
    median_qi(condition_median_mu = b_Intercept + r_Acc_Yr, .width = c(.95, .66))|>
    ggplot(aes(y = Acc_Yr, x = condition_median_mu, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() +
    labs(title = "Median fitted Adjusted Intercept by Accident Year
           Deflated Incremental Pure Premium Model",
         subtitle = sub_label_fit,
         y = "Accident Year",
         x = "Overall Intercept + Accident Year Adjustment",
         caption = caption_label)
  
  
  p2 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p3 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred, color = Dev_Yr)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p4 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Predicted Pure Premium Deflated\n  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  p5 <- model_result %>%
    ggplot(aes(x =  Dev_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Development Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "Residuals Pure Premium Deflated Normalized")
  
  p6 <- model_result %>%
    ggplot(aes(x =  Acc_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Accident Year vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Accident Year",
         y= "Residuals Pure Premium Deflated Normalized") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_continuous(breaks = seq(min_Acc_Yr,max_Acc_Yr, by = 1)) +
    guides(x = guide_axis(angle = 90)) +
    facet_wrap(~Cal_Yr)
  
  p7 <- model_result %>%
    ggplot(aes(x =  Cal_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Calendar Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Calendar Year",
         y= "Residuals Pure Premium Deflated Normalized") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_continuous(breaks = seq(min_Cal_Yr,max_Cal_Yr, by = 1)) +
    facet_wrap(~Dev_Yr)
  
  
  p8 <- model_result %>%
    ggplot(aes(x =  Cal_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Calendar Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Calendar Year",
         y= "Residuals Pure Premium Deflated Normalized") +
    guides(x = guide_axis(angle = 90)) +
    scale_x_continuous(breaks = seq(min_Cal_Yr,max_Cal_Yr, by = 1)) +
    facet_wrap(~Dev_Yr)
  
  
  
  p9 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl,
               color= Dev_Yr)) +
    stat_pointinterval( )+
    labs(title = "Predicted Pure Premium Deflated\ vs. Normalized Residuals (Dev_Yr)",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  
  
  
  
  p10  <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = .epred)) +
    stat_pointinterval()+
    labs(title = "Distribution of Predicted Deflated Pure Premium\n by Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "Predicted Pure Premium Deflated")
  
  p11 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = mu)) +
    stat_pointinterval()+
    labs(title = "Estimated mu vs. Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "mu for lognormal")
  
  p12 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = sigma)) +
    stat_pointinterval()+
    labs(title = "Estimated sigma vs. Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "sigma for lognormal")
  
  
  
  p13 <- compare_mu_sigma %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_mu, color =type)) +
    stat_pointinterval()+
    labs(title = "Estimated mu vs. Data Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "mu for lognormal")
  
  p14 <- compare_mu_sigma %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_sd_adj,color= type)) +
    stat_pointinterval()+
    labs(title = "Estimated sigma vs. Data by Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "sigma for lognormal")
  
  
  p15 <- compare_ln_sigma %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Ln_Incr_PP_Defl_sd_adj,color= type)) +
    stat_pointinterval()+
    labs(title = "Estimated log sigma vs. Data by Development Year",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "log sigma for lognormal")
  
  
  p16 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = hu,group=Dev_Yr)) +
    geom_boxplot()+
    labs(title = "Development Year vs. Distribution of hu",
         subtitle = sub_label_fit,
         caption = caption_label,
         x ="Development Year",
         y= "Probability of Zero Payment (hu) "
         )
  
  
  
  
  return(list(p1,p2, p3, p4, p5, p6, p7, p8, p9,p10, p11,
              p12,p13,p14, p15, p16))
  
}





model_fit_gamma <- function(model_name,model_data,
                            summarized_data,
                            summarized_data_ran_grp) {
  
  sub_label_fit <- rlang::englue("Data Source: {{model_data}} & Model: {{model_name}}")
  
  
  p1 <-  model_name|>
    spread_draws(b_Intercept,r_Acc_Yr[Acc_Yr,Intercept]) |>
    median_qi(condition_median_mu = b_Intercept + r_Acc_Yr, .width = c(.95, .66))|>
    ggplot(aes(y = Acc_Yr, x = condition_median_mu, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() +
    labs(title = "Median fitted Adjusted Intercept by Accident Year
           Deflated Incremental Pure Premium Model",
         subtitle = sub_label_fit,
         y = "Accident Year",
         x = "Overall Intercept + Accident Year Adjustment")
  
  
  
  model_result <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_epred_draws(model_name, 
                    
                    dpar = c("mu", "shape")) %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    mutate(residuals_defl=Incr_PP_Defl-.epred,
           residuals_ln_defl = Ln_Incr_PP_Defl- log(.epred),
           
           mean_expected_PP_defl = mean(.epred),
           sd_expected_PP_defl = sd(.epred),
           sd_expected_PP_ln_defl = sd(log(.epred)),
           mean_residuals_defl = mean(residuals_defl),
           sd_residuals_defl =sd(residuals_defl),
           sd_residuals_ln_defl =sd(residuals_ln_defl),
           Norm_Resid_All_defl =(residuals_defl)/sd_residuals_defl,
           Norm_Resid_All_Ln_defl= (residuals_ln_defl)/sd_residuals_ln_defl,
           mean_all_residuals_defl =mean(residuals_defl),
           std_all_residuals_defl = sd(residuals_defl),
           residuals=(Incr_PP-.epred* Accum_Infl_Index_Mid),
           residuals_ln = Ln_Incr_PP- log(.epred* Accum_Infl_Index_Mid),
           
           mean_expected_PP = mean(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP = sd(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP_ln = sd(log(.epred * Accum_Infl_Index_Mid)),
           Norm_Resid_All =(residuals)/sd_expected_PP,
           Norm_Resid_All_Ln= (residuals_ln)/sd_expected_PP_ln,
           mean_all_residuals =mean(residuals),
           std_all_residuals = sd(residuals)
    ) %>%
    select(Acc_Yr, Dev_Yr, Cal_Yr, residuals,residuals_ln,
           Norm_Resid_All, Norm_Resid_All_Ln,
           mu, shape, .epred, 
           Incr_PP, Ln_Incr_PP, 
           mean_all_residuals,
           std_all_residuals, 
           mean_expected_PP,
           sd_expected_PP,
           sd_residuals_defl,
           residuals_defl,
           sd_residuals_ln_defl,
           Norm_Resid_All_defl, Norm_Resid_All_Ln_defl,
           .epred,
           Incr_PP_Defl, Ln_Incr_PP_Defl, 
           mean_all_residuals_defl,
           std_all_residuals_defl, 
           mean_expected_PP_defl,
           sd_expected_PP_defl)
  
  
  model_result_mu_shape <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_epred_draws(model_name, 
                    
                    dpar = c("mu", "shape"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    select(mu,shape,.epred, Acc_Yr,Dev_Yr, Cal_Yr) |>
    group_by(Dev_Yr) |>
    select(Dev_Yr, mu, shape, .epred ) |>
    mutate(Incr_PP_Defl_mu = mean(mu),
           Ln_Incr_PP_Defl_mu = mean(log(mu)),
           Incr_PP_Defl_shape = mean(shape),
           Ln_Incr_PP_Defl_shape =mean(log(shape)),
           type ="model") |>
    select(Dev_Yr,Incr_PP_Defl_mu,
           Ln_Incr_PP_Defl_mu ,
           Incr_PP_Defl_shape,
           Ln_Incr_PP_Defl_shape,
           type) |>
    unique()
  
  model_result_mu_shape_linpred <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_linpred_draws(model_name, 
                    
                    dpar = c("mu", "shape"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    select(mu,shape,.linpred, Acc_Yr,Dev_Yr, Cal_Yr) |>
    group_by(Dev_Yr) |>
    select(Dev_Yr, mu, shape, .linpred ) |>
    mutate(
           Ln_Incr_PP_Defl_mu = mean(mu),
           
           Ln_Incr_PP_Defl_shape_adj =mean(shape),
           type ="model_linpred") |>
    select(Dev_Yr,
           Ln_Incr_PP_Defl_mu ,
           
           Ln_Incr_PP_Defl_shape_adj,
           type) |>
    unique()
  
  
  
  data_mu_shape <- summarized_data_ran_grp |>
    mutate (type ="data") |>
   
    select(type, 
           Dev_Yr, 
           Ln_Incr_PP_Defl_mu,
           Ln_Incr_PP_Defl_shape_adj)
  
  
  compare_mu_shape_linpred <- rbind(data_mu_shape,
                            
                            model_result_mu_shape_linpred)
  
  
  
  ## Graph comparison of model to actual using ggplot and tidybayes data set
  ## not all of the graphs were published in the interest of conserving space in the paper
  
  p2 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p3 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred, color = Dev_Yr)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p4 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl , color = Dev_Yr)) +
    stat_pointinterval()+
    labs(title = "Predicted Pure Premium Deflated\n  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  p5 <- model_result %>%
    ggplot(aes(x =  Dev_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Development Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "Residuals Pure Premium Deflated Normalized")
  
  
  
  p6 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl,
               color= Dev_Yr)) +
    stat_pointinterval( )+
    labs(title = "Predicted Pure Premium Deflated\ vs. Normalized Residuals (Dev_Yr)",
         subtitle = sub_label_fit,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  
  
  
  
  p7 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = .epred)) +
    stat_pointinterval()+
    labs(title = "Distribution of Predicted Deflated Pure Premium\n by Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "Predicted Pure Premium Deflated")
  
  p8 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = mu)) +
    stat_pointinterval()+
    labs(title = "Estimated mu vs. Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "mean for gamma")
  
  p9 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = shape)) +
    stat_pointinterval()+
    labs(title = "Estimated shape vs. Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "shape for gamma")
  
  
  p10 <- compare_mu_shape_linpred %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_mu,color =type)) +
    stat_pointinterval()+
    labs(title = "Estimated Log mu vs. Data by Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "Log mean for gamma")
  
  p11 <- compare_mu_shape_linpred %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_shape_adj, color= type)) +
    stat_pointinterval()+
    labs(title = "Estimated Log shape vs. Data by Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "Log shape for gamma")
  
  
  
  
  
  
  return(list(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10, p11))
  
}




model_fit_gamma_acc_yr_grp <- function(model_name,model_data,
                            summarized_data,
                            summarized_data_ran_grp) {
  
  sub_label_fit <- rlang::englue("Data Source: {{model_data}} & Model: {{model_name}}")
  
  
  p1 <-  model_name|>
    spread_draws(b_Intercept,r_Acc_Yr[Acc_Yr,Intercept]) |>
    median_qi(condition_median_mu = b_Intercept + r_Acc_Yr, .width = c(.95, .66))|>
    ggplot(aes(y = Acc_Yr, x = condition_median_mu, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() +
    labs(title = "Median fitted Adjusted Intercept by Accident Year
           Deflated Incremental Pure Premium Model",
         subtitle = sub_label_fit,
         y = "Accident Year",
         x = "Overall Intercept + Accident Year Adjustment")
  
  
  
  model_result <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_epred_draws(model_name, 
                    
                    dpar = c("mu", "shape")) %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    mutate(residuals_defl=Incr_PP_Defl-.epred,
           residuals_ln_defl = Ln_Incr_PP_Defl- log(.epred),
           
           mean_expected_PP_defl = mean(.epred),
           sd_expected_PP_defl = sd(.epred),
           sd_expected_PP_ln_defl = sd(log(.epred)),
           mean_residuals_defl = mean(residuals_defl),
           sd_residuals_defl =sd(residuals_defl),
           sd_residuals_ln_defl =sd(residuals_ln_defl),
           Norm_Resid_All_defl =(residuals_defl)/sd_residuals_defl,
           Norm_Resid_All_Ln_defl= (residuals_ln_defl)/sd_residuals_ln_defl,
           mean_all_residuals_defl =mean(residuals_defl),
           std_all_residuals_defl = sd(residuals_defl),
           residuals=(Incr_PP-.epred* Accum_Infl_Index_Mid),
           residuals_ln = Ln_Incr_PP- log(.epred* Accum_Infl_Index_Mid),
           
           mean_expected_PP = mean(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP = sd(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP_ln = sd(log(.epred * Accum_Infl_Index_Mid)),
           Norm_Resid_All =(residuals)/sd_expected_PP,
           Norm_Resid_All_Ln= (residuals_ln)/sd_expected_PP_ln,
           mean_all_residuals =mean(residuals),
           std_all_residuals = sd(residuals)
    ) %>%
    select(Acc_Yr, Dev_Yr, Cal_Yr, residuals,residuals_ln,
           Norm_Resid_All, Norm_Resid_All_Ln,
           mu, shape, .epred, 
           Incr_PP, Ln_Incr_PP, 
           mean_all_residuals,
           std_all_residuals, 
           mean_expected_PP,
           sd_expected_PP,
           sd_residuals_defl,
           residuals_defl,
           sd_residuals_ln_defl,
           Norm_Resid_All_defl, Norm_Resid_All_Ln_defl,
           .epred,
           Incr_PP_Defl, Ln_Incr_PP_Defl, 
           mean_all_residuals_defl,
           std_all_residuals_defl, 
           mean_expected_PP_defl,
           sd_expected_PP_defl)
  
  
  model_result_mu_shape <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_epred_draws(model_name, 
                    
                    dpar = c("mu", "shape"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    select(mu,shape,.epred, Acc_Yr,Dev_Yr, Cal_Yr) |>
    group_by(Dev_Yr) |>
    select(Dev_Yr, mu, shape, .epred ) |>
    mutate(Incr_PP_Defl_mu = mean(mu),
           Ln_Incr_PP_Defl_mu = mean(log(mu)),
           Incr_PP_Defl_shape = mean(shape),
           Ln_Incr_PP_Defl_shape =mean(log(shape)),
           type ="model") |>
    select(Dev_Yr,Incr_PP_Defl_mu,
           Ln_Incr_PP_Defl_mu ,
           Incr_PP_Defl_shape,
           Ln_Incr_PP_Defl_shape,
           type) |>
    unique()
  
  model_result_mu_shape_linpred <-  model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_linpred_draws(model_name, 
                      
                      dpar = c("mu", "shape"))%>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    select(mu,shape,.linpred, Acc_Yr,Dev_Yr, Cal_Yr) |>
    group_by(Dev_Yr) |>
    select(Dev_Yr, mu, shape, .linpred ) |>
    mutate(
      Ln_Incr_PP_Defl_mu = mean(mu),
      
      Ln_Incr_PP_Defl_shape_adj =mean(shape),
      type ="model_linpred") |>
    select(Dev_Yr,
           Ln_Incr_PP_Defl_mu ,
           
           Ln_Incr_PP_Defl_shape_adj,
           type) |>
    unique()
  
  
  
  data_mu_shape <- summarized_data_ran_grp |>
    mutate (type ="data") |>
    
    select(type, 
           Dev_Yr, 
           Ln_Incr_PP_Defl_mu,
           Ln_Incr_PP_Defl_shape_adj)
  
  
  compare_mu_shape_linpred <- rbind(data_mu_shape,
                                    
                                    model_result_mu_shape_linpred)
  
  
  
  ## Graph comparison of model to actual using ggplot and tidybayes data set
  ## not all of the graphs were published in the interest of conserving space in the paper
  
  p2 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p3 <-  model_result %>%
    ggplot(aes(x = Incr_PP_Defl, y = .epred, color = Dev_Yr)) +
    stat_pointinterval()+
    labs(title = "Historical Pure Premium Deflated\nvs. Distribution of Predicted",
         subtitle = sub_label_fit,
         x ="Historical Pure Premium Deflated",
         y= "Predicted Pure Premium Deflated")
  
  
  p4 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl , color = Dev_Yr)) +
    stat_pointinterval()+
    labs(title = "Predicted Pure Premium Deflated\n  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  p5 <- model_result %>%
    ggplot(aes(x =  Dev_Yr, y = Norm_Resid_All_defl)) +
    stat_pointinterval()+
    labs(title = "Development Year  vs. Normalized Residuals",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "Residuals Pure Premium Deflated Normalized")
  
  
  
  p6 <- model_result%>%
    ggplot(aes(x =  mean_expected_PP_defl, y = Norm_Resid_All_defl,
               color= Dev_Yr)) +
    stat_pointinterval( )+
    labs(title = "Predicted Pure Premium Deflated\ vs. Normalized Residuals (Dev_Yr)",
         subtitle = sub_label_fit,
         x ="Mean Predicted Pure Premium Deflated",
         y= "Residuals Pure Premium Deflated Normalized")
  
  
  
  
  
  p7 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = .epred)) +
    stat_pointinterval()+
    labs(title = "Distribution of Predicted Deflated Pure Premium\n by Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "Predicted Pure Premium Deflated")
  
  p8 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = mu)) +
    stat_pointinterval()+
    labs(title = "Estimated mu vs. Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "mean for gamma")
  
  p9 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = shape)) +
    stat_pointinterval()+
    labs(title = "Estimated shape vs. Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "shape for gamma")
  
  
  p10 <- compare_mu_shape_linpred %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_mu,color =type)) +
    stat_pointinterval()+
    labs(title = "Estimated Log mu vs. Data by Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "Log mean for gamma")
  
  p11 <- compare_mu_shape_linpred %>%
    ggplot(aes(x = Dev_Yr, y = Ln_Incr_PP_Defl_shape_adj, color= type)) +
    stat_pointinterval()+
    labs(title = "Estimated Log shape vs. Data by Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "Log shape for gamma")
  
  
  
  
  
  
  return(list(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10, p11))
  
}



model_fit_lognormal_no_data <- function(model_name,model_data) {
  
  sub_label_fit <- rlang::englue("Data Source: {{model_data}} & Model: {{model_name}}")
  
  
  p1 <-  model_name|>
    spread_draws(b_Intercept,r_Acc_Yr[Acc_Yr,Intercept]) |>
    median_qi(condition_median_mu = b_Intercept + r_Acc_Yr, .width = c(.95, .66))|>
    ggplot(aes(y = Acc_Yr, x = condition_median_mu, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() +
    labs(title = "Median fitted Adjusted Intercept by Accident Year
           Deflated Incremental Pure Premium Model",
         subtitle = sub_label_fit,
         y = "Accident Year",
         x = "Overall Intercept + Accident Year Adjustment")
  
  
  model_result <- model_data %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    add_epred_draws(model_name, 
                    dpar = c("mu", "sigma")) %>%
    group_by(Acc_Yr,Dev_Yr, Cal_Yr) %>%
    mutate(residuals_defl=Incr_PP_Defl-.epred,
           residuals_ln_defl = Ln_Incr_PP_Defl- log(.epred),
           
           mean_expected_PP_defl = mean(.epred),
           sd_expected_PP_defl = sd(.epred),
           sd_expected_PP_ln_defl = sd(log(.epred)),
           mean_residuals_defl = mean(residuals_defl),
           sd_residuals_defl =sd(residuals_defl),
           sd_residuals_ln_defl =sd(residuals_ln_defl),
           Norm_Resid_All_defl =(residuals_defl)/sd_residuals_defl,
           Norm_Resid_All_Ln_defl= (residuals_ln_defl)/sd_residuals_ln_defl,
           mean_all_residuals_defl =mean(residuals_defl),
           std_all_residuals_defl = sd(residuals_defl),
           residuals=(Incr_PP-.epred* Accum_Infl_Index_Mid),
           residuals_ln = Ln_Incr_PP- log(.epred* Accum_Infl_Index_Mid),
           
           mean_expected_PP = mean(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP = sd(.epred * Accum_Infl_Index_Mid),
           sd_expected_PP_ln = sd(log(.epred * Accum_Infl_Index_Mid)),
           Norm_Resid_All =(residuals)/sd_expected_PP,
           Norm_Resid_All_Ln= (residuals_ln)/sd_expected_PP_ln,
           mean_all_residuals =mean(residuals),
           std_all_residuals = sd(residuals)
    ) %>%
    select(Acc_Yr, Dev_Yr, Cal_Yr, residuals,residuals_ln,
           Norm_Resid_All, Norm_Resid_All_Ln,
           mu, sigma, .epred, 
           Incr_PP, Ln_Incr_PP, 
           mean_all_residuals,
           std_all_residuals, 
           mean_expected_PP,
           sd_expected_PP,
           sd_residuals_defl,
           residuals_defl,
           sd_residuals_ln_defl,
           Norm_Resid_All_defl, Norm_Resid_All_Ln_defl,
           .epred,
           Incr_PP_Defl, Ln_Incr_PP_Defl, 
           mean_all_residuals_defl,
           std_all_residuals_defl, 
           mean_expected_PP_defl,
           sd_expected_PP_defl)
  
  
  
  ## Graph comparison of model to actual using ggplot and tidybayes data set
  ## not all of the graphs were published in the interest of conserving space in the paper
  
  
  
  p2 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = .epred)) +
    stat_pointinterval()+
    labs(title = "Distribution of Predicted Deflated Pure Premium\n by Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "Predicted Pure Premium Deflated")
  
  p3 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = mu)) +
    stat_pointinterval()+
    labs(title = "Estimated mu vs. Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "mu for lognormal")
  
  p4 <- model_result %>%
    ggplot(aes(x = Dev_Yr, y = sigma)) +
    stat_pointinterval()+
    labs(title = "Estimated sigma vs. Development Year",
         subtitle = sub_label_fit,
         x ="Development Year",
         y= "sigma for lognormal")
  
  
  
  
  
  
  
  return(list(p1,p2, p3, p4))
  
}





model_comparison_3 <- function(model_1, model_2,model_3,model_data) {
  
  model_1_waic_print <- rlang::englue("waic for: {{model_1}} & data: {{model_data}} ")
  
  model_1_loo_print <- rlang::englue("loo for: {{model_1}} & data: {{model_data}} ")
 
  model_2_loo_print <- rlang::englue("loo for: {{model_2}} & data: {{model_data}} ")
  
  model_2_waic_print <- rlang::englue("waic for: {{model_2}} & data: {{model_data}} ")
  
  model_3_loo_print <- rlang::englue("loo for: {{model_3}} & data: {{model_data}} ")
  
  model_3_waic_print <- rlang::englue("waic for: {{model_3}} & data: {{model_data}} ")
  
  
  compare_waic_print <- rlang::englue("waic compare for: {{model_1}}, {{model_2}} &
                                      {{model_3}} with data: {{model_data}} ")
  
  compare_loo_print <- rlang::englue("loo compare for: {{model_1}}, {{model_2}} & 
                                      {{model_3}} with data: {{model_data}} ")
  
  model_1 <- add_criterion(model_1,criterion =c("waic","loo")) 
  
  model_1_loo <- loo(model_1)
  
  
  model_1_waic <- waic(model_1)
  
  
  model_2 <- add_criterion(model_2,criterion =c("waic","loo")) 
  
  model_2_loo <- loo(model_2)
  
  model_2_waic <- waic(model_2)
  
  
  model_3 <- add_criterion(model_3,criterion =c("waic","loo")) 
  
  model_3_loo <- loo(model_3)
  
  model_3_waic <- waic(model_3)
  
 
  compare_waic_models <-loo_compare(model_1,model_2,model_3,
                                    criterion = "waic")
  
  compare_loo_models <-loo_compare(model_1,model_2, model_3, criterion = "loo")
  
  
  
  return(list(model_1_loo_print ,
              model_1_loo ,
              
              model_1_waic_print,
              model_1_waic, 
              model_2_loo_print,
              model_2_loo ,
             
              model_2_waic_print ,
              model_2_waic ,
              model_3_loo_print,
              model_3_loo ,
              
              model_3_waic_print ,
              model_3_waic ,
              compare_waic_print ,
              compare_waic_models ,
              compare_loo_print ,
              compare_loo_models
              ))
  
}



model_comparison_2 <- function(model_1, model_2,model_data) {
  
  model_1_waic_print <- rlang::englue("waic for: {{model_1}} & data: {{model_data}} ")
  
  model_1_loo_print <- rlang::englue("loo for: {{model_1}} & data: {{model_data}} ")
  
  model_1_loo_mm_print <- rlang::englue("loo moment match for: {{model_1}} & data: {{model_data}} ")
  
  model_2_loo_print <- rlang::englue("loo for: {{model_2}} & data: {{model_data}} ")
  
 
  
  model_2_waic_print <- rlang::englue("waic for: {{model_2}} & data: {{model_data}} ")
  
  compare_waic_print <- rlang::englue("waic compare for: {{model_1}} & 
                                      {{model_2}} with data: {{model_data}} ")
  
  compare_loo_print <- rlang::englue("loo compare for: {{model_1}} & 
                                      {{model_2}} with data: {{model_data}} ")
  
  
  
  model_1 <- add_criterion(model_1,criterion =c("waic","loo")) 
  
  model_1_loo <- loo(model_1)
  
  
  model_1_waic <- waic(model_1)
  
  
  
  model_2 <- add_criterion(model_2,criterion =c("waic","loo")) 
  
  model_2_loo <- loo(model_2)
  
  model_2_waic <- waic(model_2)
  
  
  
  compare_waic_models <-loo_compare(model_1,model_2,criterion = "waic")
  
  compare_loo_models <-loo_compare(model_1,model_2,criterion = "loo")
  
 
  
  return(list(model_1_loo_print ,
              model_1_loo ,
              
             
              model_1_waic_print,
              model_1_waic, 
              model_2_loo_print,
              model_2_loo ,
              
              model_2_waic_print ,
              model_2_waic ,
              compare_waic_print ,
              compare_waic_models ,
              compare_loo_print ,
              compare_loo_models
              ))
  
}


model_comparison_mm_2 <- function(model_1, model_2,model_data) {
  
  model_1_waic_print <- rlang::englue("waic for: {{model_1}} & data: {{model_data}} ")
  
  model_1_loo_print <- rlang::englue("loo for: {{model_1}} & data: {{model_data}} ")
  
  model_1_loo_mm_print <- rlang::englue("loo moment match for: {{model_1}} & data: {{model_data}} ")
  
  model_2_loo_print <- rlang::englue("loo for: {{model_2}} & data: {{model_data}} ")
  
  model_2_loo_mm_print <- rlang::englue("loo moment match for: {{model_2}} & data: {{model_data}} ")
  
  model_2_waic_print <- rlang::englue("waic for: {{model_2}} & data: {{model_data}} ")
  
  compare_waic_print <- rlang::englue("waic compare for: {{model_1}} & 
                                      {{model_2}} with data: {{model_data}} ")
  
  compare_loo_print <- rlang::englue("loo compare for: {{model_1}} & 
                                      {{model_2}} with data: {{model_data}} ")
  
  compare_loo_mm_print <- rlang::englue("loo moment match compare for: {{model_1}} & 
                                      {{model_2}} with data: {{model_data}} ")
  
  model_1 <- add_criterion(model_1,criterion =c("waic","loo")) 
  
  model_1_loo <- loo(model_1)
  
  
  model_1_waic <- waic(model_1)
  
  model_1_loo_mm <- loo_moment_match(model_1,loo=model_1_loo)
  
  model_2 <- add_criterion(model_2,criterion =c("waic","loo")) 
  
  model_2_loo <- loo(model_2)
  
  model_2_waic <- waic(model_2)
  
  model_2_loo_mm <- loo_moment_match(model_2,loo=model_2_loo)
  
  compare_waic_models <-loo_compare(model_1,model_2,criterion = "waic")
  
  compare_loo_models <-loo_compare(model_1,model_2,criterion = "loo")
  
  compare_loo_mm_models <-loo_compare(model_1_loo_mm,model_2_loo_mm)
  
  return(list(model_1_loo_print ,
              model_1_loo ,
              model_1_loo_mm_print ,
              model_1_loo_mm ,
              model_1_waic_print,
              model_1_waic, 
              model_2_loo_print,
              model_2_loo ,
              model_2_loo_mm_print,
              model_2_loo_mm ,
              model_2_waic_print ,
              model_2_waic ,
              compare_waic_print ,
              compare_waic_models ,
              compare_loo_print ,
              compare_loo_models,
              compare_loo_mm_print ,
              compare_loo_mm_models))
  
}


model_comparison_information_mm_3 <- function(model_1, model_2,model_3,model_data) {
  
  model_1_waic_print <- rlang::englue("waic for: {{model_1}} & data: {{model_data}} ")
  
  model_1_loo_print <- rlang::englue("loo for: {{model_1}} & data: {{model_data}} ")
  
  model_1_loo_mm_print <- rlang::englue("loo moment match for: {{model_1}} & data: {{model_data}} ")
  
  model_2_loo_print <- rlang::englue("loo for: {{model_2}} & data: {{model_data}} ")
  
  model_2_loo_mm_print <- rlang::englue("loo moment match for: {{model_2}} & data: {{model_data}} ")
  
  model_2_waic_print <- rlang::englue("waic for: {{model_2}} & data: {{model_data}} ")
  
  model_3_loo_print <- rlang::englue("loo for: {{model_3}} & data: {{model_data}} ")
  
  model_3_loo_mm_print <- rlang::englue("loo moment match for: {{model_3}} & data: {{model_data}} ")
  
  model_3_waic_print <- rlang::englue("waic for: {{model_3}} & data: {{model_data}} ")
  
  model_1_name <- rlang::englue("Model 1 is {{model_1}} ")
  
  model_2_name <- rlang::englue("Model 2 is {{model_2}} ")
  
  model_3_name <- rlang::englue("Model 3 is {{model_3}} ")
  
  model_names <- paste(model_1_name, model_2_name, model_3_name, sep ="\n")
  
  
  compare_waic_print <- rlang::englue("waic compare for: {{model_1}}, {{model_2}} &
                                      {{model_3}} with data: {{model_data}} " )
  
  
  
  compare_loo_print <- rlang::englue("loo compare for: {{model_1}}, {{model_2}} & 
                                      {{model_3}} with data: {{model_data}} ")
  
  
  
  compare_loo_mm_print <- rlang::englue("loo moment match compare for: {{model_1}},{{model_2}} & 
                                      {{model_3}} with data: {{model_data}} ")
  
  
  
  
  model_1 <- add_criterion(model_1,criterion =c("waic","loo")) 
  
  model_1_loo <- loo(model_1)
  
  
  model_1_waic <- waic(model_1)
  
  model_1_loo_mm <- loo_moment_match(model_1,loo=model_1_loo)
  
  model_2 <- add_criterion(model_2,criterion =c("waic","loo")) 
  
  model_2_loo <- loo(model_2)
  
  model_2_waic <- waic(model_2)
  
  model_2_loo_mm <- loo_moment_match(model_2,loo=model_2_loo)
  
  model_3 <- add_criterion(model_3,criterion =c("waic","loo")) 
  
  model_3_loo <- loo(model_3)
  
  model_3_waic <- waic(model_3)
  
  model_3_loo_mm <- loo_moment_match(model_3,loo=model_3_loo)
  
  compare_waic_models <-loo_compare(model_1,model_2,model_3,
                                    criterion = "waic")
  
  compare_loo_models <-loo_compare(model_1,model_2, model_3, criterion = "loo")
  
  compare_loo_mm_models <-loo_compare(model_1_loo_mm, model_2_loo_mm, model_3_loo_mm)
  
  
  return(list(model_1_loo_label = model_1_loo_print ,
              
              model_1_loo_mm_label = model_1_loo_mm_print ,
              
              model_1_waic_label =model_1_waic_print,
              
              model_2_loo_label = model_2_loo_print,
              
              model_2_loo_mm_label = model_2_loo_mm_print,
              
              model_2_waic_label = model_2_waic_print ,
              
              model_3_loo_label = model_3_loo_print,
              
              model_3_loo_mm_label = model_3_loo_mm_print,
              
              model_3_waic_loo = model_3_waic_print ,
              
              model_name_label = model_names,
              
              compare_waic_label = compare_waic_print ,
              
              compare_loo_label =compare_loo_print ,
              
              compare_loo_mm_label = compare_loo_mm_print ,
              
              
              
              model_1_loo_result = model_1_loo,
              
              
              model_1_waic_result = model_1_waic ,
              
              model_1_loo_mm_result =model_1_loo_mm ,
              
              
              
              model_2_loo_result = model_2_loo ,
              
              model_2_waic_result = model_2_waic,
              
              model_2_loo_mm_result = model_2_loo_mm ,
              
              
              model_3_loo_result = model_3_loo ,
              
              model_3_waic_result = model_3_waic ,
              
              model_3_loo_mm_result = model_3_loo_mm ,
              
              compare_waic_models_result = compare_waic_models ,
              
              compare_loo_models_result = compare_loo_models ,
              
              compare_loo_mm_models_result = compare_loo_mm_models 
              
  ))
  
}



compare_titles_3 <-function(model_1,model_2,model_3,model_data) {
  Model_1_name <- rlang::englue(" {{model_1}}")
  Model_2_name <- rlang::englue(" {{model_2}}")
  Model_3_name <- rlang::englue(" {{model_3}}")
  Train_Data_Name <- rlang::englue(" {{model_data}}")
  model_id_vector <- c("model_1","model_2","model_3", "train_data")
  model_name_vector <- c(Model_1_name,
                         Model_2_name,
                         Model_3_name,
                         Train_Data_Name)
  model_match_df <-data.frame(model_id = model_id_vector,
                              model_name = model_name_vector)
  return(model_match_df)
  
}


Generate_Expected_Inflation_Forecast <- function(num_draws, Case_Inflation_Index) {
  
  Expected_Inflation_Index <- Case_Inflation_Index |>
    select(Accum_Infl_Index_Mid,Cal_Yr)%>%
    expand_grid(.draw =1: num_draws) %>%
    rename(Accum_Infl_Index_Mid_Exp =Accum_Infl_Index_Mid)
  
  return(Expected_Inflation_Index)
}



Generate_Predicted_Inflation_Forecast <- function(num_draws,
                                                  Case_Inflation_Index,
                                                  seed_num,
                                                  infl_mu,
                                                  infl_sigma,
                                                  start_cal_yr,
                                                  end_cal_yr) {
  
  
  set.seed(8603529)
  
  Case_Predicted_Trend <- expand_grid(.draw =1:num_draws,
                                      Cal_Yr = start_cal_yr:end_cal_yr) %>%
    group_by(.draw, Cal_Yr) %>%
    mutate(Infl_Index = rlnorm(1,infl_mu,infl_sigma),
           
           
           Infl_Index_Mid =Infl_Index **.5)
  
  
  Case_Predicted_Trend_Accumulated <- Case_Predicted_Trend %>%
    ungroup() %>%
    group_by(.draw) %>%
    mutate( Accum_Infl_Index =cumprod(Infl_Index),
            
            Accum_Infl_Index_Mid =if_else(Cal_Yr == min(Cal_Yr),
                                          Infl_Index_Mid,
                                          Infl_Index_Mid* lag(Accum_Infl_Index)))
  
  
  Case_Inflation_Index_Train_Period <- Case_Inflation_Index %>%
    filter( Cal_Yr ==start_cal_yr -1) %>%
    select(Accum_Infl_Index_Mid)%>%
    expand_grid(.draw =1: num_draws) %>%
    rename(Accum_Infl_Index_Mid_Start =Accum_Infl_Index_Mid)
  
  
  Predicted_Trend_Accumulated <- left_join(Case_Predicted_Trend_Accumulated ,
                                           Case_Inflation_Index_Train_Period, 
                                           by=c(".draw") ) %>%
    mutate(Accum_Infl_Index_Mid_Adj =Accum_Infl_Index_Mid_Start* Accum_Infl_Index_Mid ) |>
    select(.draw, Cal_Yr,Accum_Infl_Index_Mid_Adj )
  
  return(Predicted_Trend_Accumulated)
  
  
}




create_model_predicted_forecasts <- function(model_1,model_2,model_3, 
                                             future_data,
                                             future_inflation_forecast,
                                             titles) {
  
  caption_label <- rlang::englue("Data Source: {{future_data}} & 
                                              Inflation Source: {{future_inflation_forecast}}")
  
  # Model 1 data prep and graphs
  
  
  model_1_pred <- add_predicted_draws(future_data,
                                      model_1) %>%
    mutate(pred_paid =Rptd_Cnt_Yr_1*.prediction,
           model_id = "model_1") %>%
    group_by(.draw,Cal_Yr) %>%
    
    select(.draw,Cal_Yr, .prediction, pred_paid,Acc_Yr, Dev_Yr,model_id) 
  
  
  model_1_pred_intermediate <-  left_join(model_1_pred, 
                                          future_inflation_forecast,
                                          by = join_by(.draw, Cal_Yr)) |>
    mutate(pred_paid_infl = pred_paid * Accum_Infl_Index_Mid_Adj) 
  
  
  
  model_1_pred_name <-left_join( model_1_pred_intermediate, 
                                 title_df, by = join_by( model_id))
  
  model_1_pred_tot_rsv <- model_1_pred_name |>
    ungroup() |>
    select(.draw, pred_paid_infl,model_name) |>
    group_by( .draw,model_name) |>
    summarise(tot_rsv = sum(pred_paid_infl),
              tot_rsv_m = sum(pred_paid_infl/1000000),
              .groups = "keep")
  
  
  
p1 <-  ggplot(model_1_pred_tot_rsv , aes(x=tot_rsv_m, color =model_name)) +
    stat_ecdf(geom = "step")+
    labs(title="Total Reserve Estimate\n Simulated Future Inflation",
         x="Total Reserve Estimate in Millions",
         y="Cumulative Distribution",
         caption = caption_label)+
    scale_y_continuous(labels = scales::percent) +
    guides(x = guide_axis(angle = 90)) +
    theme(legend.position="bottom")
  
  
  model_1_pred_Acc_Yr_rsv <- model_1_pred_name |>
    ungroup() |>
    select(.draw, pred_paid_infl,model_name, Acc_Yr) |>
    group_by( .draw,model_name, Acc_Yr) |>
    summarise(tot_rsv = sum(pred_paid_infl),
              tot_rsv_m = sum(pred_paid_infl/1000000),
              .groups = "keep")
  
 p2 <- model_1_pred_Acc_Yr_rsv %>%
    ggplot(aes(x = Acc_Yr, y = tot_rsv_m,colour = model_name)) +
    stat_pointinterval(position = "dodge")+
    
    labs(title = "Accident Year Ultimate Reserve Estimates\n Simulated Future Inflation ",
         x ="Accident Year",
         y= "Reserve Estimate (000's) ",
         caption = caption_label)+
    guides(x = guide_axis(angle = 90)) +
    theme(legend.position="bottom")
 
 
 
  
 # Model 2 data prep and graphs
 
 
 model_2_pred <- add_predicted_draws(future_data,
                                     model_2) %>%
   mutate(pred_paid =Rptd_Cnt_Yr_1*.prediction,
          model_id = "model_2") %>%
   group_by(.draw,Cal_Yr) %>%
   
   select(.draw,Cal_Yr, .prediction, pred_paid,Acc_Yr, Dev_Yr,model_id) 
 
 
 model_2_pred_intermediate <-  left_join(model_2_pred, 
                                         future_inflation_forecast,
                                         by = join_by(.draw, Cal_Yr)) |>
   mutate(pred_paid_infl = pred_paid * Accum_Infl_Index_Mid_Adj) 
 
 
 
 model_2_pred_name <-left_join( model_2_pred_intermediate, 
                                title_df, by = join_by( model_id))
 
 model_2_pred_tot_rsv <- model_2_pred_name |>
   ungroup() |>
   select(.draw, pred_paid_infl,model_name) |>
   group_by( .draw,model_name) |>
   summarise(tot_rsv = sum(pred_paid_infl),
             tot_rsv_m = sum(pred_paid_infl/1000000),
             .groups = "keep")
 
 
 
 p3 <-  ggplot(model_2_pred_tot_rsv , aes(x=tot_rsv_m, color =model_name)) +
   stat_ecdf(geom = "step")+
   labs(title="Total Reserve Estimate\n Simulated Future Inflation",
        x="Total Reserve Estimate in Millions",
        y="Cumulative Distribution",
        caption = caption_label)+
   scale_y_continuous(labels = scales::percent) +
   guides(x = guide_axis(angle = 90)) +
   theme(legend.position="bottom")
 
 
 model_2_pred_Acc_Yr_rsv <- model_2_pred_name |>
   ungroup() |>
   select(.draw, pred_paid_infl,model_name, Acc_Yr) |>
   group_by( .draw,model_name, Acc_Yr) |>
   summarise(tot_rsv = sum(pred_paid_infl),
             tot_rsv_m = sum(pred_paid_infl/1000000),
             .groups = "keep")
 
 p4 <- model_2_pred_Acc_Yr_rsv %>%
   ggplot(aes(x = Acc_Yr, y = tot_rsv_m,colour = model_name)) +
   stat_pointinterval(position = "dodge")+
   
   labs(title = "Accident Year Ultimate Reserve Estimates\n Simulated Future Inflation ",
        x ="Accident Year",
        y= "Reserve Estimate (000's) ",
        caption = caption_label)+
   guides(x = guide_axis(angle = 90)) +
   theme(legend.position="bottom")
 
 
 
 # Model 3 data prep and graphs
 
 
 model_3_pred <- add_predicted_draws(future_data,
                                     model_3) %>%
   mutate(pred_paid =Rptd_Cnt_Yr_1*.prediction,
          model_id = "model_3") %>%
   group_by(.draw,Cal_Yr) %>%
   
   select(.draw,Cal_Yr, .prediction, pred_paid,Acc_Yr, Dev_Yr,model_id) 
 
 
 model_3_pred_intermediate <-  left_join(model_3_pred, 
                                         future_inflation_forecast,
                                         by = join_by(.draw, Cal_Yr)) |>
   mutate(pred_paid_infl = pred_paid * Accum_Infl_Index_Mid_Adj) 
 
 
 
 model_3_pred_name <-left_join( model_3_pred_intermediate, 
                                title_df, by = join_by( model_id))
 
 model_3_pred_tot_rsv <- model_3_pred_name |>
   ungroup() |>
   select(.draw, pred_paid_infl,model_name) |>
   group_by( .draw,model_name) |>
   summarise(tot_rsv = sum(pred_paid_infl),
             tot_rsv_m = sum(pred_paid_infl/1000000),
             .groups = "keep")
 
 
 
 p5 <-  ggplot(model_3_pred_tot_rsv , aes(x=tot_rsv_m, color =model_name)) +
   stat_ecdf(geom = "step")+
   labs(title="Total Reserve Estimate\n Simulated Future Inflation",
        x="Total Reserve Estimate in Millions",
        y="Cumulative Distribution",
        caption = caption_label)+
   scale_y_continuous(labels = scales::percent) +
   guides(x = guide_axis(angle = 90)) +
   theme(legend.position="bottom")
 
 
 model_3_pred_Acc_Yr_rsv <- model_3_pred_name |>
   ungroup() |>
   select(.draw, pred_paid_infl,model_name, Acc_Yr) |>
   group_by( .draw,model_name, Acc_Yr) |>
   summarise(tot_rsv = sum(pred_paid_infl),
             tot_rsv_m = sum(pred_paid_infl/1000000),
             .groups = "keep")
 
 p6 <- model_3_pred_Acc_Yr_rsv %>%
   ggplot(aes(x = Acc_Yr, y = tot_rsv_m,colour = model_name)) +
   stat_pointinterval(position = "dodge")+
   
   labs(title = "Accident Year Ultimate Reserve Estimates\n Simulated Future Inflation ",
        x ="Accident Year",
        y= "Reserve Estimate (000's) ",
        caption = caption_label)+
   guides(x = guide_axis(angle = 90)) +
   theme(legend.position="bottom")
 
 
 
 model_acc_yr_rsv <- rbind(model_1_pred_Acc_Yr_rsv,
                           model_2_pred_Acc_Yr_rsv,
                           model_3_pred_Acc_Yr_rsv)
 
 model_tot_rsv <- rbind(model_1_pred_tot_rsv,
                        model_2_pred_tot_rsv,
                        model_3_pred_tot_rsv)
 
 
 p7 <-  ggplot( model_tot_rsv , aes(x=tot_rsv_m, color =model_name)) +
   stat_ecdf(geom = "step")+
   labs(title="Total Reserve Estimate\n Simulated Future Inflation",
        x="Total Reserve Estimate in Millions",
        y="Cumulative Distribution",
        caption = caption_label)+
   scale_y_continuous(labels = scales::percent) +
   guides(x = guide_axis(angle = 90)) +
   theme(legend.position="bottom")
 
 p8 <- model_acc_yr_rsv %>%
   ggplot(aes(x = Acc_Yr, y = tot_rsv_m,colour = model_name)) +
   stat_pointinterval(position = "dodge")+
   
   labs(title = "Accident Year Ultimate Reserve Estimates\n Simulated Future Inflation ",
        x ="Accident Year",
        y= "Reserve Estimate (000's) ",
        caption = caption_label)+
   guides(x = guide_axis(angle = 90)) +
   theme(legend.position="bottom")
 
 return(list(p1, p2, p3, p4, p5, p6, p7, p8) )
 
  
}


