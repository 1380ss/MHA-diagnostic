#1.Currently, you need to have data locally. 
#Download data from here (see link below), and put it into your WORKING DIRECTORY
#https://drive.google.com/drive/folders/16_s05KSGh26cAYsoslC9C1hcW2JNagWk

#2.Instruction: just run everything until the last row. 
#study_index and contextual_index are things you can cahnge

#3. make sure you update R and all packages! 
#check here ('Can’t load/update package correctly') if you have issue:
#https://gossamer-plier-c2f.notion.site/Debug-25a94ae79ec7458dbc0f295f5ab66176?pvs=4


#update.packages(ask = FALSE)
#.libPaths()   
# packages needed
########################## setup your work directory#################
library(stringr)
library(DT)
library(gt)
library(paletteer)
#library(usethis)
library(devtools)
library(rlang)
library(websocket)
library(webshot2)
library(googlesheets4)
library(httpuv)
library(gtExtras)
library(dplyr)
# limit only 50 rows at the right terminal
options(max.print = 50)
# read the master sheet
link='https://docs.google.com/spreadsheets/d/1nUi1JxSStUqHOiMoDXfIW-UBAKDRDdVcWWtehIz_3r4/edit#gid=0'
link='https://docs.google.com/spreadsheets/d/15tIyOb0Z1GLWeQNwnmYgCmpAU4oAlUzleN2IjuZZv8M/edit#gid=0'
data_info <- read_sheet(link)
# context_info
context_info <- read_sheet(link,sheet=2)
# arm_info
arm_info <- read_sheet(link,sheet=3)
# column_info
column_info <- read_sheet(link,sheet=4)
column_info_columns <- column_info$columns
column_info_explain <- column_info$explanation
# check file in data_info and context_info, which are documented in Gsheet, make sure uploaded those file to working directory
study_names <- data_info$`Study name`
############################################## change the path where you put the colors_function.R file #############
# import the conditional formatting color functions
#source("~/Desktop/MHA/function/color_functions.R")


###########################
########################### color function
# function pal
pal <- function(x,threshold1=1.5,threshold2=3) {
  f_neg1 <- scales::col_numeric(
    palette = c('#FEBEB1', '#ffffff'),
    domain = c(-threshold1, 0)
  )
  f_neg2 <- scales::col_numeric(
    palette = c('#AE123A','#FEBEB1'),
    domain = c(-threshold2,-threshold1)
  )
  
  f_pos1 <- scales::col_numeric(
    palette = c('#ffffff', '#93C190'),
    domain = c(0, threshold1)
  )
  f_pos2 <- scales::col_numeric(
    palette = c('#93C190','#24693D'),
    domain = c(threshold1,threshold2)
  )
  
  dplyr::case_when(
    x < -threshold2 ~ '#AE123A',
    x <= -threshold1 ~ f_neg2(x),
    x <= 0 ~ f_neg1(x),
    x <=threshold1 ~ f_pos1(x),
    x <=threshold2 ~ f_pos2(x),
    x >threshold2 ~ '#24693D',
    .default = "#000000"
  )
}

# function pal_p_assigned
pal_p_assigned <- function(x) {
  f <- scales::col_numeric(
    palette = c('#ffffff', 'royalblue'),
    domain = c(0, 7)
  )
  ifelse(x>5,f(log(x*0.18)),f(0))
}


# function pal_p_rewards
pal_p_rewards <- function(x) {
  f <- scales::col_numeric(
    palette = c('#ffffff', 'royalblue'),
    domain = c(0, 7)
  )
  f(log(x))
}






############################

# function load_and_clean_data
load_and_clean_data <- function(study_index, context_index, starting_time='2023-01-16'){
  # input
  # output
  # potential extension
  # function description for functions.
  df <- read.csv(data_info$`File name`[study_index])
  df$arm_time <- as.Date(df$arm_time)

  #TODO:need to cahnge later
  if (!is.na(starting_time)){
    df <- df[df$arm_time>=starting_time,]
    time_comment = paste0('since ',starting_time)
  } else {
    time_comment = 'all time'
  }
  
  contextual_column_raw_names <- context_info[study_index,][!is.na(context_info[study_index,])][-1]
  contextual_column_converted_names <- colnames(context_info)[!is.na(context_info[study_index,])][-1]
  context_raw = contextual_column_raw_names[context_index]
  context_new = contextual_column_converted_names[context_index]
  df[context_raw] <- round(df[context_raw],2)
  
  # change name for arm
  for (k in unique(df$arm)){
    df$arm[df$arm==k] <- arm_info$`Ideal names`[which(arm_info$`Raw arm names`==k)]  
  }
  
  append_context <- function(x){return(paste0(context_new,'=',x))}
  df[context_raw] <- lapply(df[context_raw],append_context)
  
  return(list(df=df, context_raw=context_raw, context_new=context_new, time_comment=time_comment))
}

# function prepare_tables
prepare_tables <- function(df, context_raw){
  table_seperated <- df %>% group_by(df[context_raw],arm, .drop=F) %>%
    summarise(mean_reward = round(mean(reward, na.rm=T),3), 
              MSE_reward = round(sd(reward, na.rm=T)/sqrt(sum(!is.na(reward))),3),
              assigned = n(),
              rewards = sum(!is.na(reward)),
              reward_rate = round(sum(!is.na(reward))/n(),2),
              participants_assigned = length(unique(learner_id)), 
              participants_rewards = length(unique(learner_id[!is.na(reward)])))
  
  table_overall <- df %>% group_by(arm, .drop=F) %>% 
    summarise(mean_reward = round(mean(reward, na.rm=T),3), 
              MSE_reward = round(sd(reward, na.rm=T)/sqrt(sum(!is.na(reward))),3),
              assigned = n(),
              rewards = sum(!is.na(reward)),
              reward_rate = round(sum(!is.na(reward))/n(),2),
              participants_assigned = length(unique(learner_id)), 
              participants_rewards = length(unique(learner_id[!is.na(reward)])))
  
  table_overall[context_raw] <- 'Total'
  
  return(list(table_seperated=table_seperated, table_overall=table_overall))
}


# function create_final_table
create_final_table <- function(table_seperated, table_overall, df){
  final_table <- rbind(table_seperated,table_overall)
  reward_mean_total <- mean(df$reward,na.rm=T)
  final_table['dist'] <- round((final_table$mean_reward-reward_mean_total)/final_table$MSE_reward,2)
  return(final_table)
}

# function Add footnotes for GT table
add_footnotes <- function(gt_obj) {
  for (i in seq_along(column_info$columns)) {
    gt_obj <- gt_obj %>%
      tab_footnote(
        footnote = column_info$explanation[i],
        locations = cells_column_labels(
          columns = column_info$columns[i]
        )
      )
  }
  return(gt_obj)
}


# function generate_gt_table
generate_gt_table <- function(final_table, study_index, time_comment, context_new, context_raw){
  final_table <- 
    gt(final_table, rowname_col = 'arm') |>
    tab_header(
      title = paste0(study_names[study_index],', ',time_comment),
      subtitle = paste0("Contextual variable: ",context_new)
    ) %>% 
    tab_options(row_group.background.color = '#ffffcc',
                column_labels.background.color = '#edf8fb',
                stub.background.color = '#edf8fb')%>%
    cols_align(
      align = c( "center"),
      columns = everything()
    ) %>% 
    data_color(columns = 'dist', target_columns = "mean_reward",
               fn=pal) %>%
    data_color(columns = c('participants_rewards'),
               fn=pal_p_rewards) %>%
    data_color(columns = c('participants_assigned'),
               fn=pal_p_assigned)%>%
    tab_footnote(
      footnote = paste0('Reward design: ', data_info[study_index,'Reward']),
      locations = cells_title(groups = "title")
    )%>%
    tab_footnote(
      footnote = context_info[which(context_info[,1]=='Explanation/description'),context_new],
      locations = cells_title(groups = "subtitle")
    )
  
  # Loop through each column in column_info and add a footnote for it
  final_table <- add_footnotes(final_table)
  
  # Customize the image file name
  file_name = paste0(study_names[study_index],'-',time_comment,'-',context_new,'.png')
  gtsave_extra(final_table,filename = file_name)
  ##################### specify the path to set location you wanna put the images
  ##################### In the example, it is saved in the Desktop
}


# final function make_plot
make_plot <- function(study_index, context_index, starting_time='2023-01-16'){
  
  data <- load_and_clean_data(study_index, context_index, starting_time)
  
  tables <- prepare_tables(data$df, data$context_raw)
  
  final_table <- create_final_table(tables$table_seperated, tables$table_overall, data$df)
  
  generate_gt_table(final_table, study_index, data$time_comment, data$context_new, data$context_raw)
}




##############don't run this loop yet.
#for (i in 1:8){
#  for (j in 1:length(context_info[i,][!is.na(context_info[i,])][-1])){
#    make_plot(i,j,starting_time='2023-01-16')
#    make_plot(i,j,starting_time=NA)
#  }
#}





study_index <- 1
contextual_index <- 1

data_info$`File name`[study_index] #here's the data/mooclet we are looking at
names(context_info)[contextual_index+1] #here's the contextual variable we are looking at now
make_plot(study_index,contextual_index)
