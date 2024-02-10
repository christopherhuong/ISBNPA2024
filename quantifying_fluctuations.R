
library(dplyr)

setwd("G:/My Drive/INCH Projects/[2023] Apple Watch Study")




depr <- read.csv("depr_participants.csv")

df <- read.csv("clean_step_count.csv")
df$date <- as.Date(df$date)


id_keys <- depr %>%
  distinct(email) %>%
  mutate(Case.Number = 1:n())



df1 <- df %>%
  filter(email %in% depr$email) %>%
  select(email, date, steps) %>%
  group_by(email) %>%
  mutate(prog_day = 1:n(),
         median_steps = median(steps),
         na_group = 0)
  




dfs_2 <- df1 %>% 
  left_join(id_keys, by = ('email')) %>%
  mutate(minjump_2 = median_steps * 0.20,
         minjump = median_steps * 0.30,
         minjump_4 = median_steps *0.40) %>%
  group_by(Case.Number, median_steps) %>%
  nest() %>%
  mutate(tree = as.list(NA),
         data_comp = as.list(NA))


dfs_2 <- dfs_2 %>%
  arrange(Case.Number)


mingap  = 7 # set minimum gap between the shift (n) and shift (n + 1)  
minlag = 7 # # set minimum lag between the shift (n) and shift (n - 1) 

# computing tree and prediction using the function developed by the authors (the function has been decomposed)

for (i in seq_along(dfs_2$Case.Number)) {
  
  dfs_2$tree[[i]] <- rpart::rpart(steps ~ prog_day, data = dfs_2$data[[i]]) 
  
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


for (i in seq_along(dfs_3$Case.Number)) {
  
  
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


for (i in seq_along(dfs_4$Case.Number)){
  
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
    
    scale_y_continuous("Steps", breaks = seq(0, 35000, by = 5000)) +
    
    ylim(0, 30000) +
    
    xlim(0, 30) +
    
    ggtitle(paste("Participant #", dfs_4$Case.Number[[i]], "|", "Median steps =", as.integer(dfs_4$median_steps[[i]]), "|", "Individual shift threshold =", as.integer(dfs_4$data_comp[[i]]$minjump))) +
    
    theme(plot.title = element_text(size=15)) +
    
    # theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)) +
    
    xlab("Days") +
    
    theme_classic() +
    
    theme(panel.grid.minor.y = element_blank())
  
}


# participants <- left_join(id_keys, select(depr, c(email, k10_change)), by = "email")
# 

dfs_4$gl_plot[[4]]
# to plot sudden gains and losses for each participant - change the number in the double squared brackets [[]] to switch from one participant to another



pdf("depr_steps.pdf", width = 8, height = 4)
dfs_4$gl_plot[[1]] # 1 gain, 1 loss
dfs_4$gl_plot[[2]] # 1 loss
dfs_4$gl_plot[[3]] # 1 gain
dfs_4$gl_plot[[4]] # no change
dfs_4$gl_plot[[5]] # 1 gain
dfs_4$gl_plot[[6]] # 1 loss

dev.off()























