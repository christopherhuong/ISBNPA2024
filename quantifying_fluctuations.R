library(tidyverse)
library(rpart)
library(purrr)


setwd("G:/My Drive/INCH Projects/[2023] Apple Watch Study")


df <- read.csv("clean_step_count_2-13-24.csv")
# qual <- read.csv("clean_baseline_qualtrics.csv")


df$date <- as.Date(df$date)



# assign an id# for every email, then remove email variable 
df <- df %>% 
  arrange(email) %>%
  group_by(email) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>% select(-email)



# create variable for consecutive days, median steps, and NAs
df1 <- df %>%
  select(id, date, steps) %>%
  group_by(id) %>%
  mutate(prog_day = 1:n(),
         median_steps = median(steps),
         na_group = if_else(is.na(steps), 1, 0))  #for any NA in steps
  

# cap at 28 days






# summary(df1$prog_day)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   10.00   19.00   20.11   28.00   66.00 

# summary(df1$steps)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 26    3735    6171    6768    9108   37856 



dfs_2 <- df1 %>% 
  mutate(minjump_2 = median_steps * 0.20,
         minjump = median_steps * 0.30,
         minjump_4 = median_steps *0.40) %>%
  group_by(id, median_steps) %>%
  nest() %>%
  mutate(tree = as.list(NA),
         data_comp = as.list(NA))


# save(dfs_2, file="dfs_2.RData")

load("dfs_2.RData")

mingap  = 7 # set minimum gap between the shift (n) and shift (n + 1)  
minlag = 7 # # set minimum lag between the shift (n) and shift (n - 1) 

# computing tree and prediction using the function developed by Dario Baretta & Guillaume Chevance (https://osf.io/64cmv/)

for (i in seq_along(dfs_2$id)) {
  
  dfs_2$tree[[i]] <- rpart(steps ~ prog_day, data = dfs_2$data[[i]]) 
  
  new <- data.frame(prog_day = dfs_2$data[[i]]$prog_day)
  
  predicted_value <- enframe(predict(dfs_2$tree[[i]], new))
  
  dfs_2$data_comp[[i]] <- dfs_2$data[[i]] %>% 
    add_column(pred_value = predicted_value$value)  %>% 
    mutate(lag_pred_value = lag(pred_value),
           shift = pred_value - lag_pred_value,
           shift = if_else(!is.na(shift), shift, 0)) %>%
    select(-lag_pred_value) %>%
    group_by(pred_value) %>% 
    mutate(gap_duration = n(), # time before the shift
           imputed = sum(na_group),
           imputed_ratio = imputed/gap_duration) %>% 
    ungroup() %>% 
    mutate(lag_duration = lag(gap_duration)) %>%   
    mutate(shift_check = if_else(abs(shift) >= minjump & gap_duration >= mingap & lag_duration >= minlag, 'TRUE', 'FALSE'),
           lossgain = case_when(shift > 0 & shift_check == TRUE ~ 'Gain',
                                shift < 0 & shift_check == TRUE ~ 'Loss',
                                TRUE ~ NA_character_),
           sensitivity_2 = if_else(abs(shift) >= minjump_2 & gap_duration >= mingap & lag_duration >= minlag, 'TRUE', 'FALSE'),
           lossgain_2 = case_when(shift > 0 & sensitivity_2 == TRUE ~ 'Gain',
                                  shift < 0 & sensitivity_2 == TRUE ~ 'Loss',
                                  TRUE ~ NA_character_),
           sensitivity_4 = if_else(abs(shift) >= minjump_4 & gap_duration >= mingap & lag_duration >= minlag, 'TRUE', 'FALSE'),
           lossgain_4 = case_when(shift > 0 & sensitivity_4 == TRUE ~ 'Gain',
                                  shift < 0 & sensitivity_4 == TRUE ~ 'Loss',
                                  TRUE ~ NA_character_))
  
}


# sensitivy for smaller and bigger shift

dfs_3 <- dfs_2 %>% 
  select(-data) %>% 
  mutate(sens_2 = NA_integer_,
         main_3 = NA_integer_,
         sens_4 = NA_integer_)


for (i in seq_along(dfs_3$id)) {
  
  
  dfs_3$sens_2[i] <- dfs_3$data_comp[[i]] %>% 
    filter(!is.na(lossgain_2)) %>% 
    summarise(n = n()) %>% 
    deframe()
  
  dfs_3$main_3[i] <- dfs_3$data_comp[[i]] %>% 
    filter(!is.na(lossgain)) %>% 
    summarise(n = n()) %>% 
    deframe()
  
  dfs_3$sens_4[[i]] <- dfs_3$data_comp[[i]] %>% 
    filter(!is.na(lossgain_4)) %>% 
    summarise(n = n()) %>% 
    deframe()
  
}




# plots for each id    

dfs_4 <- dfs_3 %>% 
  mutate(gl_plot = as.list(NA))


for (i in seq_along(dfs_4$id)){
  
  carats <- deframe(dfs_3$data_comp[[i]] %>%
                      filter(!is.na(lossgain)) %>% 
                      select(prog_day))
  
  
  dfs_4$gl_plot[[i]] <- dfs_4$data_comp[[i]] %>% 
    
    ggplot(aes(x = prog_day)) +
    
    geom_line(aes(y = steps), colour = "grey70") +
    
    geom_point(aes(y = steps), alpha = .7) +
    
    geom_line(aes(y = pred_value), colour = "black", size = 1) +
    
    geom_vline(xintercept = carats, colour = "grey5", size = 1, alpha = .5) +
    
    geom_point(aes(y = pred_value, fill = lossgain, shape = lossgain), colour = "snow", size = 4) +
    
    scale_fill_manual("Shift type", values = c("dodgerblue2", "orange2"), breaks = c("Gain", "Loss")) + 
    
    scale_shape_manual("Shift type", values = c("Gain" = 24,"Loss" = 25), breaks = c("Gain", "Loss")) +
    
    scale_y_continuous("Steps", breaks = seq(0, 20000, by = 4000)) +
    
    ylim(0, 20000) +
    
    xlim(0, 30) +
    
    ggtitle(paste("Participant #", dfs_4$id[[i]], "|", "Median steps =", as.integer(dfs_4$median_steps[[i]]), "|", "Individual shift threshold =", as.integer(dfs_4$data_comp[[i]]$minjump))) +
    
    theme(plot.title = element_text(size=15)) +
    
    # theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)) +
    
    xlab("Days") +
    
    theme_classic() +
    
    theme(panel.grid.minor.y = element_blank())
  
}



dfs_4$gl_plot[[89]] #specify participant id (1-143) for their plot



# create variable for counts of gains & losses 
dfs_4 <- dfs_4 %>%
  mutate(gains = map_int(data_comp, ~ sum(.x$lossgain == "Gain", na.rm = T)),
         losses = map_int(data_comp, ~ sum(.x$lossgain == "Loss", na.rm=T)))


dfs_4 <- dfs_4 %>% 
  mutate(changes = gains + losses) %>%  #absolute number of sudden changes
  mutate(maintain = if_else(changes == 0, 1, 0)) # 1=no changes, 0=any changes
  

















# next --------------------------------------------------------------------


#pivot longer
full_data_1 <- dfs_4 %>% 
  ungroup() %>% 
  unnest(cols = c(data_comp)) %>% 
  pivot_longer(cols = c(lossgain_2, lossgain, lossgain_4), names_to = "jump_size", values_to = "lossgain") %>% 
  mutate(gain_loss_dv = if_else(!is.na(lossgain), 1, 0)) %>%
  mutate(jump_size = case_when(jump_size == "lossgain_2" ~ "20 %",
                               jump_size == "lossgain" ~ "30 %", 
                               jump_size == "lossgain_4" ~ "40 %")) %>% 
  select(jump_size, id, date, prog_day, median_steps, steps, pred_value, gap_duration, lag_duration, shift, lossgain, gain_loss_dv)


full_data_2 <- full_data_1 %>% 
  group_by(jump_size, id) %>% 
  nest() %>% 
  group_by(jump_size) %>% 
  nest()




Heaviside <- function(value){
  
  if (value>0){
    h=1
  }
  else {h=0}
  return(h)
}


dc_d <- function (df, win=NROW(df), scale_min, scale_max, doPlot = FALSE, useVarNames = TRUE, colOrder = TRUE, useTimeVector = NA, timeStamp = "01-01-1999"){
  
  if(any(stats::is.ts(df),xts::is.xts(df),zoo::is.zoo(df))){
    time_vec <- stats::time(df)
  } else {
    time_vec <- NA
  }
  
  if(is.null(dim(df))){
    if(is.numeric(df)){
      df <- data.frame(df)
    } else {
      df <- data.frame(as.numeric_discrete(df))
    }
  }
  
  if(any(df<scale_min,df>scale_max)){
    stop("Range of values in df is outside [scale_min,scale_max]!")
  }
  
  if(win<=0){stop("Need a window > 0")}
  
  tsNames <- colnames(df)
  
  ew_data_D <- matrix(NA, nrow=(nrow(df)+2), ncol=NCOL(df))
  ew_data_D <- data.frame(ew_data_D)
  for (column in 1:NCOL(df)){
    tsy <- as.numeric(df[,column])
    dens <- NA
    x <- NA
    y <- NA
    s <- scale_max-scale_min
    y <- pracma::linspace(scale_min, scale_max, win)
    for (i in (1:(length(tsy)-(win-1)))){
      x <- tsy[i:(i+win-1)]
      x <- sort(x)
      r=0
      g=0
      for (e in (1:(win-1))){
        for (d in ((e+1):win)){
          for (a in (e:(d-1))){
            for (b in (((a+1):d))){
              h <- Heaviside((y[b]-y[a])-(x[b]-x[a]))
              if(h==1){
                r <- r + (((y[b]-y[a]) - (x[b]-x[a])))
                g <- g + (y[b]-y[a])
              } else {
                r <- r
                g <- g + (y[b]-y[a])
              }
            }
          }
        }
      }
      ew_data_D[(i+win-1),(column)] <- 1-(r/g)
    }
  }
  ew_data_D <- ew_data_D[(1:nrow(df)),]
  attr(ew_data_D,"time") <- time_vec
  if(is.null(dim(ew_data_D))){
    ew_data_D <- data.frame(ew_data_D)
  }
  
  colnames(ew_data_D) <- tsNames
  g <- NULL
  if(doPlot){
    g <- plotDC_res(df_win = ew_data_D, win = win, useVarNames = useVarNames, colOrder = colOrder, timeStamp = timeStamp, doPlot = doPlot, title = "Distribution Uniformity Diagram")
    return(list(data = ew_data_D, graph = g))
  } else {
    return(ew_data_D)
  }
  
  
  
}









dc_f <- function(df, win=NROW(df), scale_min, scale_max, doPlot = FALSE, useVarNames = TRUE, colOrder = TRUE, useTimeVector = NA, timeStamp = "01-01-1999"){
  
  if(any(stats::is.ts(df),xts::is.xts(df),zoo::is.zoo(df))){
    time_vec <- stats::time(df)
  } else {
    time_vec <- NA
  }
  
  if(is.null(dim(df))){
    if(is.numeric(df)){
      df <- data.frame(df)
    } else {
      df <- data.frame(as.numeric_discrete(df))
    }
  }
  
  if(any(df<scale_min,df>scale_max)){
    stop("Range of values in df is outside [scale_min,scale_max]!")
  }
  
  
  if(win<=0){stop("Need a window > 0")}
  
  tsNames <- colnames(df)
  
  ew_data_F <- matrix(NA, nrow=NROW(df), ncol=NCOL(df))
  ew_data_F <- data.frame(ew_data_F)
  data <- rbind(df, matrix(0,nrow=2,ncol=NCOL(df), dimnames = list(NULL,colnames(df))))
  
  s <- scale_max-scale_min
  length_ts <- nrow(data)
  
  for (column in 1:NCOL(data)){
    
    distance    <- 1
    fluctuation <- NA
    tsy         <- as.numeric(data[,column])
    
    # start sliding window
    for(i in (1:(nrow(data)-win-1))){
      
      y <- NA
      fluct <- NA
      dist_next <- 1
      k <- NA
      
      # yd2 <- diff(diff(tsy))
      # k <- rep(0,length(yd2))
      # k[yd2!=0] <- 1
      
      for (j in (0:(win-2))){
        
        # Start 2nd sliding window
        # k[j+1] <- eval_f(ts[i+j],ts[i+j+1],ts[i+j+2])
        
        if((tsy[i+j+1] >= tsy[i+j]) & (tsy[i+j+1] > tsy[i+j+2])){
          (k[j+1]<- 1)
        }  else if((tsy[i+j+1] <= tsy[i+j]) & (tsy[i+j+1]<tsy[i+j+2])){
          (k[j+1]<- 1)
        }  else if ((tsy[i+j+1]>tsy[i+j]) & (tsy[i+j+1] == tsy[i+j+2])){
          (k[j+1]<- 1)
        }  else if ((tsy[i+j+1]<tsy[i+j]) & (tsy[i+j+1] == tsy[i+j+2])){
          (k[j+1] <-1)
        }  else if ((tsy[i+j+1]==tsy[i+j]) & (tsy[i+j+1] > tsy[i+j+2])){
          (k[j+1] <-1)
        }  else if ((tsy[i+j+1]==tsy[i+j]) & (tsy[i+j+1] < tsy[i+j+2])){
          (k[j+1] <-1)
        }  else {
          (k[j+1] <- 0)}
      }
      
      k[win-1] <- 1
      k <- k[1:(win-1)]
      
      for (g in (1:length(k))){
        if(k[g]==1){
          y[g] <- abs(tsy[i+g]-tsy[i+g-dist_next])
          fluct[g] = (y[g]/((i+g)-(i+g-dist_next)))
          dist_next <- distance
        } else if(k[g]==0){
          y[g]=0
          fluct[g]=0
          dist_next <- dist_next+1}
      }
      ew_data_F[(i+win-1),(column)]<-sum(fluct/(s*(win-1)), na.rm=TRUE)
    }
  }
  ew_data_F <- ew_data_F[1:nrow(df),]
  attr(ew_data_F,"time") <- time_vec
  if(is.null(dim(ew_data_F))){
    ew_data_F <- data.frame(ew_data_F)
  }
  
  colnames(ew_data_F) <- tsNames
  g <- NULL
  if(doPlot){
    g <- plotDC_res(df_win = ew_data_F, win = win, useVarNames = useVarNames, colOrder = colOrder, timeStamp = timeStamp, doPlot = doPlot, title = "Fluctuation Intensity Diagram")
    return(list(data = ew_data_F, graph = g))
  } else {
    return(ew_data_F)
  }
}






dc_win <- function(df, win=NROW(df), scale_min, scale_max, doPlot = FALSE, doPlotF = FALSE, doPlotD = FALSE, returnFandD = FALSE, useVarNames = TRUE, colOrder = TRUE, useTimeVector = NA, timeStamp = "01-01-1999"){
  
  if(any(stats::is.ts(df),xts::is.xts(df),zoo::is.zoo(df))){
    time_vec <- stats::time(df)
  } else {
    time_vec <- NA
  }
  
  if(is.null(dim(df))){
    if(is.numeric(df)){
      df <- data.frame(df)
    } else {
      df <- data.frame(as.numeric_discrete(df))
    }
  }
  
  
  if(any(df<scale_min,df>scale_max)){
    stop("Range of values in df is outside [scale_min,scale_max]!")
  }
  
  if(win<=0){stop("Need a window > 0")}
  
  tsNames <- colnames(df)
  
  data_f <- dc_f(df = df, win = win, scale_min = scale_min, scale_max = scale_max, doPlot = doPlotF, useVarNames = useVarNames, colOrder = colOrder, useTimeVector = useTimeVector, timeStamp = timeStamp)
  data_d <- dc_d(df = df, win = win, scale_min = scale_min, scale_max = scale_max, doPlot = doPlotD, useVarNames = useVarNames, colOrder = colOrder, useTimeVector = useTimeVector, timeStamp = timeStamp)
  
  if(doPlotF){
    graph_f <- data_f$data
    data_f  <- data_f$data
    #graphics::plot(graph_f)
  }
  
  if(doPlotD){
    graph_d <- data_d$data
    data_d  <- data_d$data
    # graphics::plot(graph_d)
  }
  
  df_win <- data_f*data_d
  
  # if(logDC){
  #   for(c in 1:NCOL(df_win)){
  #     idNA <- is.na(df_win[,c])
  #     id0  <- df_win[,c]<=0
  #     if(sum(!idNA&id0,na.rm = TRUE)>0){
  #    df_win[!idNA&id0,c] <- df_win[!idNA&id0,c] + .Machine$double.eps
  #    }
  #   }
  #   df_win <- log(df_win)
  # }
  
  
  colnames(df_win) <- tsNames
  attr(df_win, "time")      <- attr(data_f,"time")
  attr(df_win, "scale_min") <- scale_min
  attr(df_win, "scale_max") <- scale_max
  attr(df_win, "win")       <- win
  attr(df_win, "dataType")  <- "dc_win"
  
  g <- NULL
  if(doPlot){
    g <- plotDC_res(df_win = df_win, win = win, useVarNames = useVarNames, colOrder = colOrder, timeStamp = timeStamp, doPlot = doPlot)
  }
  
  if(returnFandD){
    return(list(dynamic_complexity =  df_win, F_data = data_f, D_data = data_d, plot = g))
  } else {
    return(df_win)
  }
  
}





# apply dc_win to our data ------------------------------------------------


for (j in seq_along(full_data_2$jump_size)){
  
  for (i in seq_along(full_data_2$data[[j]]$id)){
    
    dyn_comp <- dc_win(full_data_2$data[[j]]$data[[i]]$steps, win = 7, scale_min = min(full_data_2$data[[j]]$data[[i]]$steps), scale_max = max(full_data_2$data[[j]]$data[[i]]$steps))
    
    full_data_2$data[[j]]$data[[i]] <- full_data_2$data[[j]]$data[[i]] %>% 
      add_column(dyn_comp = dyn_comp$df) %>%
      mutate(dyn_comp_lag1 = lag(dyn_comp),
             dyn_comp_lag2 = lag(dyn_comp_lag1),
             dyn_comp_lag3 = lag(dyn_comp_lag2),
             dyn_comp_lag4 = lag(dyn_comp_lag3)) %>%
      rowwise() %>% 
      mutate(dyn_col_top_2 = max(c(dyn_comp_lag1, dyn_comp_lag2), na.rm = TRUE),
             dyn_col_top_3 = max(c(dyn_comp_lag1, dyn_comp_lag2, dyn_comp_lag3), na.rm = TRUE),
             dyn_col_top_4 = max(c(dyn_comp_lag1, dyn_comp_lag2, dyn_comp_lag3, dyn_comp_lag4), na.rm = TRUE)) %>% 
      select(-c(dyn_comp, dyn_comp_lag1, dyn_comp_lag2, dyn_comp_lag3, dyn_comp_lag4))
    
  }
  
}



# compute duration between 2 shifts ---------------------------------------


full_data_3 <- full_data_2 %>% 
  unnest(cols = c(data)) %>% 
  unnest(cols = c(data)) %>% 
  group_by(jump_size, id) %>% 
  mutate(ref_date_shift = if_else(!is.na(lossgain) & gain_loss_dv == 1, date, NA_Date_),
         ref_date_loss = if_else(lossgain == 'Loss' & gain_loss_dv == 1, date, NA_Date_),
         ref_date_gain = if_else(lossgain == 'Gain' & gain_loss_dv == 1, date, NA_Date_)) %>% # added after having added duration
  fill(c(ref_date_shift, ref_date_loss, ref_date_gain), .direction = "up") %>% 
  ungroup()




full_data_3.1 <- full_data_3 %>%   
  group_by(jump_size, id, ref_date_shift) %>% 
  mutate(duration_shift = row_number()) %>% 
  ungroup() %>%
  group_by(jump_size, id, ref_date_loss) %>% 
  mutate(duration_loss = row_number()) %>% 
  ungroup() %>%
  group_by(jump_size, id, ref_date_gain) %>% 
  mutate(duration_gain = row_number()) %>% 
  ungroup()


# unnest and remove 7 days before & after ---------------------------------

full_data_4 <- full_data_3.1 %>%
  filter(dyn_col_top_2 > 0) %>% 
  group_by(jump_size, id) %>% 
  nest() %>% 
  mutate(row_id = as.list(NA),
         row_rem = as.list(NA),
         data_clean = as.list(NA))



for (i in seq_along(full_data_4$id)){
  
  full_data_4$row_id[[i]] <- c(which(!is.na(full_data_4$data[[i]]$lossgain)))
  
  full_data_4$row_rem[[i]] <- sort(c(outer(full_data_4$row_id[[i]], c(1:7), FUN = '+')))
  
  full_data_4$data_clean[[i]] <- full_data_4$data[[i]][-full_data_4$row_rem[[i]], ] 
  
}


full_data_4$row_id[[1]]
full_data_4$row_rem[[1]]
full_data_4$data_clean[[1]]


# - create 3 databases, one for each jump size
# - preprocess each database before modelling


mod_data_20 <- full_data_4 %>% 
  select(jump_size, id, data_clean) %>% 
  ungroup() %>% 
  unnest(cols = c(data_clean)) %>% 
  filter(jump_size == "20 %") %>% 
  group_by(id) %>% 
  mutate(id = as.character(id),
         gain_loss_dv = as.factor(gain_loss_dv),
         across(where(is.numeric), ~scale(.))) %>% 
  ungroup()


mod_data_30 <- full_data_4 %>% 
  select(jump_size, id, data_clean) %>% 
  ungroup() %>% 
  unnest(cols = c(data_clean)) %>% 
  filter(jump_size == "30 %") %>% 
  group_by(id) %>% 
  mutate(id = as.character(id),
         gain_loss_dv = as.factor(gain_loss_dv),
         across(where(is.numeric), ~scale(.))) %>% 
  ungroup()


mod_data_40 <- full_data_4 %>% 
  select(jump_size, id, data_clean) %>% 
  ungroup() %>% 
  unnest(cols = c(data_clean)) %>%
  filter(jump_size == "40 %") %>% 
  group_by(id) %>% 
  mutate(id = as.character(id),
         gain_loss_dv = as.factor(gain_loss_dv),
         across(where(is.numeric), ~scale(.))) %>%
  ungroup()


# running main models (shifts defined as >30% median) with full database

mod1_30 <- glmer(gain_loss_dv ~ prog_day + dyn_col_top_3 + (1 | id), data = mod_data_30, family = binomial)

mod2_30 <- glmer(gain_loss_dv ~ prog_day + dyn_col_top_3 + (prog_day + dyn_col_top_3 || id), data = mod_data_30, family = binomial)

mod3_30 <- glmer(gain_loss_dv ~ duration_shift + prog_day+ dyn_col_top_3 + (duration_shift + prog_day + dyn_col_top_3 || id), data = mod_data_30, family = binomial)


broom::tidy(anova(mod1_30, mod2_30)) %>% knitr::kable()

broom::tidy(anova(mod2_30, mod3_30))  %>% knitr::kable()


summary(mod3_30)

# - preprocess the data before running the model on gains and losses separately 

































