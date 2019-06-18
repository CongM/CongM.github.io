setwd('~/Downloads/JHU/RA/Roman/wd/tax')
setwd('~/Downloads/JHU/RA/Roman/wd/examiners/')

library(lubridate)
library(openxlsx)
library(data.table)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(rJava)
library(NLP)
library(openNLP)
library(gender)
library(genderizeR)
library(httr)
library(jsonlite)
library(rvest)

library(RMySQL)
library(Rmpi)


#################################################################################

dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home/jre/lib/server/libjvm.dylib')

dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/jre/lib/server/libjvm.dylib'))





#################################################################################


testdata <- read.csv('testoutput.csv', stringsAsFactors = F)


temp1 <- filter(testdata, grepl('-', page_count)) %>%
  mutate(file_name = page_count, page_count = category, category = '')


newdata <- filter(testdata, !grepl('-', page_count)) %>%
  bind_rows(temp1)


temp2 <- filter(testdata, grepl('[0-9]', category)) %>%
  mutate(file_name = page_count, page_count = category, category = '')


newdata <- filter(testdata, !grepl('[0-9]', category)) %>%
  bind_rows(temp2) %>% arrange(id)

rnames <- names(testdata)




rawdata <- fread('application_metadata.csv', header = F, nrows = 100)

temp1 <- filter(rawdata, grepl('[0-9]', category)) %>%
  mutate(file_name = page_count, page_count = category, category = '')

newdata <- filter(rawdata, !grepl('[0-9]', category)) %>%
  bind_rows(temp1) %>% arrange(id)




# 149554672 rows in total

splitnumber <- 10000000

splitdata <- c(0, seq(splitnumber, 149554672, splitnumber))

for(i in 1:2) {
  cat(i, '\n')
  tempdata <- fread('application_metadata.csv', header = F, nrows = splitnumber, skip = splitdata[i])
  names(tempdata) <- rnames
  temp1 <- filter(tempdata, grepl('[0-9]', category)) %>%
    mutate(file_name = page_count, page_count = category, category = '')
  newdata <- filter(tempdata, !grepl('[0-9]', category)) %>%
    bind_rows(temp1) %>% arrange(as.integer(id))
  filename <- paste0('application_metadata/application_metadata_', i, '.csv')
  write.csv(newdata, file = filename, row.names = F, na = '')
  rm(tempdata, newdata)
  gc()
}





write.csv(newdata, file = 'application_metadata_new.csv', row.names = F, na = '')




#################################################################################


all_spe <- read.csv('all_spe_gender.csv', stringsAsFactors = F)
all_spe <- mutate(all_spe, SPE_Name = str_trim(SPE_Name, side = 'both'), SPE_First_Name_1 = str_trim(SPE_First_Name_1, side = 'both'))
all_spe <- mutate(all_spe, SPE_Name = str_replace_all(SPE_Name, ' {2,}', ' '))
write.csv(all_spe, file = 'all_spe.csv', row.names = F, na = '')
all_examiners <- read.csv('all_examiners.csv', stringsAsFactors = F)
all_examiners <- mutate(all_examiners, Examiner_Name = str_trim(Examiner_Name, side = 'both'))
all_examiners <- mutate(all_examiners, Examiner_Name = str_replace_all(Examiner_Name, ' {2,}', ' '))
write.csv(all_examiners, file = 'all_examiners.csv', row.names = F, na = '')
all_examiners_gender <- read.csv('all_examiner_gender.csv', stringsAsFactors = F)
all_examiners_gender <- mutate(all_examiners_gender, Examiner_Name = str_trim(Examiner_Name, side = 'both'), 
                               Examiner_Last_Name = str_trim(Examiner_Last_Name, side = 'both'), 
                               Examiner_First_Name = str_trim(Examiner_First_Name, side = 'both'), 
                               Examiner_First_Name_1 = str_trim(Examiner_First_Name_1, side = 'both'))
all_examiners_gender <- mutate(all_examiners_gender, Examiner_Name = str_replace_all(Examiner_Name, ' {2,}', ' '))
write.csv(all_examiners_gender, file = 'all_examiner_gender.csv', row.names = F, na = '')


examiners_gs <- fread('examiners_gs.txt')
examiners_gs <- mutate(examiners_gs, examiner_name = str_trim(examiner_name, side = 'both'))
promotion <- read.csv('16-00252_Responsive Records.csv', stringsAsFactors = F)
promotion <- mutate(promotion, Examiner.Name = str_trim(Examiner.Name, side = 'both'))
examiners_gau <- fread('examiners_gau.txt')
examiners_gau <- mutate(examiners_gau, examiner = str_trim(examiner, side = 'both'))
examiners_gau <- mutate(examiners_gau, examiner = str_replace_all(examiner, ' {2,}', ' '))
all_examiners <- read.csv('all_examiners.csv', stringsAsFactors = F)
link <- select(all_examiners, -PatEx_ID) %>% distinct()

promotion <- left_join(promotion, link, by = c('Examiner.Name' = 'Examiner_Name'))

# get_id_from_name <- function(name) {
#   #name <- str_remove_all(name, ',')
#   #splitname <- str_split(name, ' ', simplify = T)
#   ids <- sapply(name, grep, link$Examiner_Name)
#   ids[sapply(ids, function(x) length(x) == 0)] <- NA
#   return(link$Internal_ID[unlist(ids)])
# }

# get_id_from_name <- function(name) {
#   results <- vector()
#   for (i in 1:length(name)) {
#     temp <- filter(link, Examiner_Name == name[i])
#     if(nrow(temp) != 0) {
#       results[i] <- temp$Internal_ID
#     } else {
#       results[i] <- ifelse(sum(agrepl(name[i], link$Examiner_Name)), agrep(name[i], link$Examiner_Name), NA)
#     }
#   }
#   return(results)
# }


# get_id_from_name <- function(name) {
#   results <- vector()
#   for (i in 1:length(name)) {
#     temp <- filter(link, Examiner_Name == name[i])
#     if(nrow(temp) != 0) {
#       results[i] <- temp$Internal_ID
#     } else {
#       results[i] <- ifelse(sum(agrepl(name[i], link$Examiner_Name)), agrep(name[i], link$Examiner_Name), NA)
#       if(is.na(results[i])) {
#         splitname <- str_split(str_remove_all(name[i], ','), ' ', simplify = T)
#         allindex <- unlist(sapply(splitname, grep, link$Examiner_Name))
#         freq <- data.frame(table(allindex))
#         results[i] <- ifelse(max(freq$Freq)==length(splitname), 
#                              as.numeric(as.character(freq$allindex[freq$Freq==length(splitname)])), 
#                              NA)
#       }
#     }
#   }
#   return(results)
# }




# get_id_from_name <- function(name) {
#   results <- vector()
#   for (i in 1:length(name)) {
#     cat(i, '\n')
#     temp <- filter(link, Examiner_Name == name[i])
#     if(nrow(temp) != 0) {
#       results[i] <- temp$Internal_ID
#     } else {
#       results[i] <- ifelse(sum(grepl(name[i], link$Examiner_Name))==1, grep(name[i], link$Examiner_Name), NA)
#       if(is.na(results[i])) {
#         if(nchar(name[i])<=10) {
#           splitname <- str_split(str_replace_all(str_remove_all(name[i], '\\.'), '-', ' '), ' ', simplify = T)
#         } else {
#           splitname <- str_split(str_replace_all(str_remove_all(name[i], ',|\\.'), '-', ' '), ' ', simplify = T)
#         }
#         allindex <- unlist(sapply(splitname, grep, link$Examiner_Name))
#         freq <- data.frame(table(allindex))
#         results[i] <- ifelse(max(freq$Freq)==length(splitname), 
#                              as.numeric(as.character(freq$allindex[freq$Freq==length(splitname)])), 
#                              ifelse(max(freq$Freq)==length(splitname)-1 & max(freq$Freq) >= 3, 
#                                     as.numeric(as.character(freq$allindex[freq$Freq==length(splitname)-1])),
#                                     NA))
#       }
#     }
#   }
#   return(results)
# }



get_id_from_name <- function(name) {
  results <- vector()
  for (i in 1:length(name)) {
    cat(i, '\n')
    temp <- filter(link, Examiner_Name == name[i])
    if(nrow(temp) != 0) {
      results[i] <- temp$Internal_ID
    } else {
      results[i] <- ifelse(sum(grepl(name[i], link$Examiner_Name))==1, link$Internal_ID[grep(name[i], link$Examiner_Name)], NA)
      if(is.na(results[i])) {
        if(nchar(name[i])<=10) {
          splitname <- str_split(str_replace_all(str_remove_all(name[i], '\\.'), '-', ' '), ' ', simplify = T)
        } else {
          splitname <- str_split(str_replace_all(str_remove_all(name[i], ',|\\.'), '-', ' '), ' ', simplify = T)
        }
        allindex <- unlist(sapply(splitname, grep, link$Examiner_Name))
        freq <- data.frame(table(allindex))
        results[i] <- ifelse(max(freq$Freq)==length(splitname), 
                             link$Internal_ID[as.numeric(as.character(freq$allindex[freq$Freq==length(splitname)]))], 
                             NA)
      }
    }
  }
  return(results)
}

link <- select(all_examiners, -PatEx_ID) %>% distinct()

all_examiners_gender <- read.csv('all_examiner_gender.csv', stringsAsFactors = F)
#all_examiners_gender <- filter(all_examiners_gender, Internal_ID < 17000)

examiners_gau <- read.csv('examiners_gau.csv', stringsAsFactors = F)
gau <- select(examiners_gau, examiner) %>% distinct() %>% filter(examiner != '')
gau <- mutate(gau, Internal_ID = get_id_from_name(examiner))
#check <- filter(link, !(Internal_ID %in% gau$Internal_ID))
#gau$Internal_ID[c(2647,6322,8862,9868,16038)] <- c(rep(15060, 3), 2433, 7563)
#gau$Internal_ID[c(3893, 3694, 9175, 3570, 2259, 5142, 5420, 11896, 176, 6344, 5255, 2618, 6668, 3734, 11314, 7820, 509, 6643, 8092, 3033, 3677)] <- NA
#check <- left_join(gau, link)
new_gau <- filter(gau, is.na(Internal_ID))
new_gau <- mutate(new_gau, Internal_ID = 30000:(29999+nrow(new_gau)))
gau <- filter(gau, !is.na(Internal_ID)) %>% bind_rows(new_gau)
examiners_gau <- select(examiners_gau, -Internal_ID)
examiners_gau <- left_join(examiners_gau, gau)
write.csv(examiners_gau, file = 'examiners_gau.csv', row.names = F, na = '')

new_examiners_gau <- mutate(new_gau, PatEx_ID = NA)
new_examiners_gau <- rename(new_examiners_gau, Examiner_Name = examiner) %>%
  select(Internal_ID, PatEx_ID, Examiner_Name)
all_examiners <- bind_rows(all_examiners, new_examiners_gau)
write.csv(all_examiners, file = 'all_examiners.csv', row.names = F, na = '')


all_examiners_gender <- bind_rows(all_examiners_gender, new_examiners_gender_gau)
write.csv(all_examiners_gender, file = 'all_examiner_gender.csv', row.names = F, na = '')


examiners_gs <- read.csv('examiners_gs.csv', stringsAsFactors = F)
gs <- select(examiners_gs, examiner_name) %>% distinct()
gs <- mutate(gs, Internal_ID = get_id_from_name(examiner_name))
#check <- filter(link, !(Internal_ID %in% gs$Internal_ID))
#gs$Internal_ID[c(7,123,808)] <- c(15060, 2433, 7563)
new_gs <- filter(gs, is.na(Internal_ID))
new_gs <- mutate(new_gs, Internal_ID = 40000:(39999+nrow(new_gs)))
gs <- filter(gs, !is.na(Internal_ID)) %>% bind_rows(new_gs)
examiners_gs <- select(examiners_gs, -Internal_ID)
examiners_gs <- left_join(examiners_gs, gs)
write.csv(examiners_gs, file = 'examiners_gs.csv', row.names = F, na = '')

new_examiners_gs <- mutate(new_gs, PatEx_ID = NA)
new_examiners_gs <- rename(new_examiners_gs, Examiner_Name = examiner_name) %>%
  select(Internal_ID, PatEx_ID, Examiner_Name)
all_examiners <- bind_rows(all_examiners, new_examiners_gs)
write.csv(all_examiners, file = 'all_examiners.csv', row.names = F, na = '')


all_examiners_gender <- bind_rows(all_examiners_gender, new_examiners_gender_gs)
write.csv(all_examiners_gender, file = 'all_examiner_gender.csv', row.names = F, na = '')


get_grade_start_date <- function(id, grade) {
  results <- vector()
  for(i in 1:length(id)) {
    if(!is.na(id[i])) {
      if(grade[i] != 0) {
        temp <- filter(promotion, Internal_ID == id[i]) %>%
          select(paste0('X', grade[i]))
        results[i] <- ifelse(nrow(temp) != 0, unlist(temp), NA)
      } else {
        results[i] <- NA
      }
    } else {
      results[i] <- NA
    }
    
  }
  return(results)
}

get_grade_end_date <- function(id, grade) {
  results <- vector()
  for(i in 1:length(id)) {
    if(!is.na(id[i])) {
      if(grade[i] != 0 & grade[i] != 15) {
        temp <- filter(promotion, Internal_ID == id[i])
        results[i] <- ifelse(nrow(temp) != 0, unlist(temp[which(colnames(temp) == paste0('X', grade[i]))+1]), NA)
      } else {
        results[i] <- NA
      }
    } else {
      results[i] <- NA
    }
    
  }
  return(results)
}

examiners_gs <- mutate(examiners_gs, start_date = get_grade_start_date(Internal_ID, grade))
examiners_gs <- mutate(examiners_gs, end_date = get_grade_end_date(Internal_ID, grade))
write.csv(examiners_gs, file = 'examiners_gs.csv', row.names = F, na = '')




examiners_gs <- read.csv('examiners_gs.csv', stringsAsFactors = F)
examiners_gau <- read.csv('examiners_gau.csv', stringsAsFactors = F)
examiners_gs <- mutate(examiners_gs, examiner_name = str_replace_all(examiner_name, ' {2,}', ' '))
examiners_gau <- mutate(examiners_gau, examiner = str_replace_all(examiner, ' {2,}', ' '))
write.csv(examiners_gs, file = 'examiners_gs.csv', row.names = F, na = '')
write.csv(examiners_gau, file = 'examiners_gau.csv', row.names = F, na = '')

gs <- select(examiners_gs, examiner_name, Internal_ID) %>% distinct()
gau <- select(examiners_gau, examiner, Internal_ID) %>% distinct()

get_last_name <- function(name) {
  str_trim(str_to_title(str_split(name, ',', simplify = T)[1]), side = 'both')
}

get_first_name <- function(name) {
  str_trim(str_to_title(str_split(name, ',', simplify = T)[2]), side = 'both')
}


all_examiners <- read.csv('all_examiners.csv', stringsAsFactors = F)
link <- select(all_examiners, -PatEx_ID) %>% distinct()
link <- mutate(link, last_name = sapply(Examiner_Name, get_last_name, simplify = T), first_name = sapply(Examiner_Name, get_first_name, simplify = T))
link <- mutate(link, full_name = ifelse(is.na(last_name), first_name, 
                                        ifelse(is.na(first_name), last_name, paste(first_name, last_name))))
all_spe <- read.csv('all_spe.csv', stringsAsFactors = F)
spe <- select(all_spe, SPE_Name) %>% distinct()

# get_id_from_name2 <- function(name) {
#   results <- vector()
#   for (i in 1:length(name)) {
#     cat(i, '\n')
#     temp <- filter(link, full_name == name[i])
#     if(nrow(temp) != 0) {
#       results[i] <- temp$Internal_ID
#     } else {
#       results[i] <- ifelse(sum(grepl(name[i], link$full_name))==1, grep(name[i], link$full_name), NA)
#       if(is.na(results[i])) {
#         if(nchar(name[i])<=10) {
#           splitname <- str_split(str_replace_all(str_remove_all(name[i], '\\.'), '-', ' '), ' ', simplify = T)
#         } else {
#           splitname <- str_split(str_replace_all(str_remove_all(name[i], ',|\\.'), '-', ' '), ' ', simplify = T)
#         }
#         allindex <- unlist(sapply(splitname, grep, link$full_name))
#         freq <- data.frame(table(allindex))
#         results[i] <- ifelse(max(freq$Freq)==length(splitname), 
#                              as.numeric(as.character(freq$allindex[freq$Freq==length(splitname)])), 
#                              NA)
#       }
#     }
#   }
#   return(results)
# }

results <- data.frame()

for(i in 1:nrow(spe)) {
  cat(i, '\n')
  name <- spe$SPE_Name[i]
  index <- c()
  for(j in 1:nrow(link)) {
    last_name <- link$last_name[j]
    first_name <- str_split(link$first_name[j], ' ', simplify = T) %>% str_c()
    first_name <- first_name[first_name!='' & nchar(first_name)>1]
    if(str_detect(name, last_name) & sum(str_detect(name, first_name))>0 & min(unlist(str_locate_all(name, last_name))) > max(unlist(str_locate_all(name, first_name)))) {
      index <- c(index, j)
    }
  }
  if(length(index)>0) {
    matched_name <- link$Examiner_Name[index]
    Internal_ID <- link$Internal_ID[index]
  } else {
    matched_name <- NA
    Internal_ID <- NA
  }
  
  results <- bind_rows(results, data.frame(name, matched_name, Internal_ID, stringsAsFactors = F))
}

results <- rename(results, spe_name = name)

write.csv(results, file = 'matched_spe_name_id.csv', row.names = F, na = '')

#spe <- left_join(spe, results, by = c('SPE_Name' = 'spe_name'))
matched <- read.csv('matched_spe_name_id.csv', stringsAsFactors = F)
all_spe <- select(all_spe, year:note)
all_spe <- left_join(all_spe, matched, by = c('SPE_Name' = 'spe_name'))
all_spe <- distinct(all_spe)
write.csv(all_spe, file = 'all_spe.csv', row.names = F, na = '')









#################################################################################


preprocess_string <- function(string) {
  stri_enc_toutf8(string) %>%
    str_to_title() %>% 
    str_replace_all(' {2,}', ' ')
}


all_examiners <- read.csv('all_examiners.csv', stringsAsFactors = F)
all_names <- select(all_examiners, -PatEx_ID) %>% distinct()
all_names <- mutate(all_names, Examiner_Name = str_to_title(Examiner_Name))

all_spe <- read.csv('all_spe.csv', stringsAsFactors = F)
all_AU <- unique(all_spe$art_unit)
all_AU <- all_AU[all_AU!='']

srfw <- read.csv('srfw_data_test.csv', stringsAsFactors = F)
srfw <- mutate(srfw, raw = preprocess_string(search_note))

for(i in 1:nrow(srfw)) {
  cat(i, '\n')
  t <- preprocess_string(srfw$search_note[i])
}

allmonth <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  
results <- data.frame()

results <- read.csv('demo_consultant.csv', stringsAsFactors = F)
results <- mutate(results, examiner_to_AU_from_raw = as.character(examiner_to_AU_from_raw))

ptm <- proc.time()
for (i in 11706:11800) {
  cat(i, '\n')
  application_id <- srfw$application_id[i]
  file_date <- srfw$file_date[i]
  year <- srfw$year[i]
  date <- srfw$date[i]
  exclusion_flag <- srfw$exclusion_flag[i]
  raw <- srfw$raw[i]
  #date_from_raw <- ifelse(str_detect(raw, '[0-9]{1,2}/[0-9]{2,4} |[0-9]{2,4}\\.[0-9]{2}\\.[0-9]{2}|[0-9]{2}[a-zA-Z]{3,}[0-9]{2}'), str_extract_all(raw, '[0-9]{1,2}/[0-9]{4}|[0-9]{2,4}\\.[0-9]{2}\\.[0-9]{2}|[0-9]{2}[a-zA-Z]{3,}[0-9]{2}', simplify = T), NA)
  date_from_raw <- ifelse(str_detect(raw, ' [0-9]{1,2}/[0-9]{2,4} |[a-zA-Z]{3,} [0-9]{1,2}/[0-9]{2,4} |[0-9]{2,4}\\.[0-9]{2}\\.[0-9]{2}|[0-9]{2} ?[a-zA-Z]{3,} ?[0-9]{2,4}'), str_extract_all(raw, ' [0-9]{1,2}/[0-9]{4} |[a-zA-Z]{3,} [0-9]{1,2}/[0-9]{2,4} |[0-9]{2,4}\\.[0-9]{2}\\.[0-9]{2}|[0-9]{2} ?[a-zA-Z]{3,} ?[0-9]{2,4}', simplify = T), NA)
  if(!is.na(date_from_raw)) {
    if(str_detect(date_from_raw, '[a-zA-Z]')) {
      date_from_raw <- ifelse(sum(str_detect(allmonth, str_extract_all(date_from_raw, '[a-zA-Z]{1,}', simplify = T)))>0, date_from_raw, NA)
    }
  }
  
  #examiner_to_AU <- ifelse(str_detect(raw, 'Au ?[0-9]{4}|Art Unit:? [0-9]{4}'), str_extract_all(raw, 'Au ?[0-9]{4}|Art Unit:? [0-9]{4}', simplify = T), NA)
  #examiner_to_AU <- str_remove_all(examiner_to_AU, ' |:|Au|Art Unit')
  examiner_to_AU_from_raw <- ifelse(str_detect(raw, ' ?[0-9]{4,} ?'), str_extract_all(raw, ' ?[0-9]{4,} ?', simplify = T), NA)
  examiner_to_AU_from_raw <- str_trim(examiner_to_AU_from_raw, side = 'both')
  examiner_to_AU_from_raw <- ifelse(examiner_to_AU_from_raw %in% all_AU, examiner_to_AU_from_raw, NA)
  examiner_to_role_from_raw <- ifelse(str_detect(raw, 'Pri |Pe |Pe\\.'), 'Primary', 
                                      ifelse(str_detect(raw, 'Primary') & !str_detect(raw, 'Supervisory Primary'), 'Primary', NA))
  examiner_to_position_from_raw <- ifelse(str_detect(raw, 'Spe|Supervisor'), 'SPE', NA)
  if(str_detect(raw, 'Primary.*Spe|Pe.*Spe|Primary.*Supervisor')) {
    examiner_to_role_from_raw <- c('Primary', NA)
    examiner_to_position_from_raw <- c(NA, 'SPE')
  } else if(str_detect(raw, 'Spe.*Primary|Spe.*Pe|Supervisor.*Primary')) {
    examiner_to_role_from_raw <- c(NA, 'Primary')
    examiner_to_position_from_raw <- c('SPE', NA)
  }
  
  index <- c()
  temp_role <- c()
  temp_position <- c()
  for (j in 1:nrow(all_names)) {
    temp <- str_split(all_names$Examiner_Name[j], ',', simplify = T)
    last_name <- temp[1]
    first_name <- temp[2]
    first_name <- str_split(first_name, ' ', simplify = T) %>% str_c()
    #temp <- str_split(all_names$Examiner_Name[j], ',', simplify = T) %>% str_split(' ', simplify = T) %>% str_c()
    first_name <- first_name[first_name!='' & nchar(first_name)>1]
    #temp <- paste0(temp, ' ')
    #temp[2] <- paste0(temp[2], ' ')
    if(str_detect(raw, last_name) & sum(str_detect(raw, first_name), na.rm = T)>0 & min(unlist(str_locate_all(raw, last_name)), na.rm = T) > max(unlist(str_locate_all(raw, first_name)), na.rm = T)) {
      temp_role <- c(temp_role, ifelse(str_detect(raw, 'Pri |Pe |Pe\\.') & min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Pri |Pe |Pe\\.'))) <= 3, 'Primary', 
                                       ifelse(str_detect(raw, 'Primary') & !str_detect(raw, 'Supervisory Primary') & abs(min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Primary')))) <= 3, 'Primary', 
                                              ifelse(str_detect(raw, 'Primary') & !str_detect(raw, 'Supervisory Primary') & str_detect(raw, 'Examiner') & abs(min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Primary')))) <= 12, 'Primary', NA))))
      temp_position <- c(temp_position, ifelse(str_detect(raw, 'Spe|Supervisor') & abs(min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Spe|Supervisor')))) <= 3, 'SPE', 
                                               ifelse(str_detect(raw, 'Spe|Supervisor') & abs(max(unlist(str_locate_all(raw, last_name))) - min(unlist(str_locate_all(raw, 'Spe|Supervisor')))) <= 3, 'SPE',
                                                      ifelse(str_detect(raw, 'Supervisory Patent Examiner') & abs(min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Supervisory Patent Examiner')))) <= 3, 'SPE', NA))))
      index <- c(index, j)
    }
  }
  if(length(index)>0) {
    examiner_to_name_from_raw <- all_names$Examiner_Name[index]
    examiner_to_Internal_ID_from_raw <- all_names$Internal_ID[index]
    examiner_to_role_from_raw <- temp_role
    examiner_to_position_from_raw <- temp_position
  } else {
    examiner_to_name_from_raw <- NA
    examiner_to_Internal_ID_from_raw <- NA
  }

  results <- bind_rows(results, data.frame(application_id, exclusion_flag, year, file_date, date, date_from_raw, examiner_to_name_from_raw, examiner_to_Internal_ID_from_raw, examiner_to_AU_from_raw, examiner_to_role_from_raw, examiner_to_position_from_raw, raw, stringsAsFactors = F))
}
print(proc.time() - ptm)


write.csv(results, file = 'demo_consultant.csv', row.names = F, na = '')


examiners_gau <- read.csv('examiners_gau.csv', stringsAsFactors = F)
all_spe <- read.csv('all_spe.csv', stringsAsFactors = F)

demo_consultant <- read.csv('demo_consultant.csv', stringsAsFactors = F)
demo_consultant <- mutate(demo_consultant, examiner_to_AU_from_raw = as.character(examiner_to_AU_from_raw))


get_au <- function(y, id) {
  temp <- filter(examiners_gau, year == y & Internal_ID == id)
  return(ifelse(nrow(temp)>0, temp$gau, NA))
}

demo_consultant <- mutate(demo_consultant, examiner_to_AU_from_examiners_gau = mapply(get_au, year, examiner_to_Internal_ID_from_raw))

get_position <- function(y, id) {
  temp <- filter(all_spe, year == y & Internal_ID == id)
  return(ifelse(nrow(temp)>0, 'SPE', NA))
}

demo_consultant <- mutate(demo_consultant, examiner_to_position_from_all_spe = mapply(get_position, year, examiner_to_Internal_ID_from_raw))


write.csv(demo_consultant, file = 'demo_consultant.csv', row.names = F, na = '') 






#application_data <- fread('application_data.csv', stringsAsFactors = F)
#application_data <- select(application_data, application_number:filing_date, examiner_name_last:examiner_art_unit, patent_number:patent_issue_date)
#save(application_data, file = 'application_data.rda')

load('application_data.rda')
all_examiners <- read.csv('all_examiners.csv', stringsAsFactors = F)

app <- filter(application_data, !is.na(examiner_id))
app <- left_join(app, all_examiners, by = c('examiner_id'='PatEx_ID'))
demo_application <- filter(application_data, is.na(examiner_id)) %>% bind_rows(app)


write.csv(demo_application, file = 'demo_application.csv', row.names = F, na = '') 


for (i in 1:nrow(examiners)) {
  cat(i, '\n')
  t <- ifelse(examiners$filing_date[i]=='', NA, year(examiners$filing_date[i]))
}



demo_application <- fread('demo_application.csv', stringsAsFactors = F)
examiners <- select(demo_application, filing_date:examiner_art_unit)
examiners <- filter(examiners, !is.na(examiner_id))
examiners <- distinct(examiners)
examiners <- mutate(examiners, year = year(as.Date(filing_date)), 
                    examiner_name = paste(paste0(examiner_name_last, ','), examiner_name_first, examiner_name_middle))
write.csv(examiners, file = 'examiners_app.csv', row.names = F, na = '')

new_names_app <- select(examiners, examiner_id, examiner_name) %>% distinct()

preprocess_string <- function(string) {
  stri_enc_toutf8(string) %>%
    str_trim(side = 'both') %>% 
    str_replace_all(' {2,}', ' ')
}

new_names_app <- mutate(new_names_app, examiner_name = preprocess_string(examiner_name))
all_examiners <- left_join(all_examiners, new_names_app, by = c('Examiner_Name' = 'examiner_name'))
all_examiners <- mutate(all_examiners, PatEx_ID = examiner_id) %>%
  select(-examiner_id) %>% distinct()
temp1 <- left_join(new_names_app, all_examiners, by = c('examiner_id'='PatEx_ID'))
temp2 <- left_join(new_names_app, all_examiners, by = c('examiner_name' = 'Examiner_Name'))
newnames <- filter(temp2, is.na(Internal_ID)) %>% 
  select(examiner_name) %>%
  distinct()
newnames <- mutate(newnames, Internal_ID = 17000:(16999+nrow(newnames)))
new_app <- filter(temp2, is.na(Internal_ID)) %>%
  select(examiner_id:examiner_name) %>%
  left_join(newnames) %>%
  rename(PatEx_ID = examiner_id, Examiner_Name = examiner_name) %>%
  select(Internal_ID, PatEx_ID, Examiner_Name)
all_examiners <- filter(all_examiners, Internal_ID < 17000) %>%
  bind_rows(new_app)
write.csv(all_examiners, file = 'all_examiners.csv', row.names = F, na = '')

all_examiners_gender <- read.csv('all_examiner_gender.csv', stringsAsFactors = F)

all_examiners_gender <- bind_rows(all_examiners_gender, new_examiners_gender_app)
write.csv(all_examiners_gender, file = 'all_examiner_gender.csv', row.names = F, na = '')




test <- as.String(srfw$search_note[6])


sent_ann <- Maxent_Sent_Token_Annotator()
word_ann <- Maxent_Word_Token_Annotator()
person_ann <- Maxent_Entity_Annotator(kind = 'person')
date_ann <- Maxent_Entity_Annotator(kind = 'date')
pipeline <- list(sent_ann, word_ann, person_ann, date_ann)
test_annotations <- annotate(test, pipeline)
test_doc <- AnnotatedPlainTextDocument(test, test_annotations)

entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}


entities(test_doc, kind = 'person')
entities(test_doc, kind = 'date')


#################################################################################

preprocess_string <- function(string) {
  stri_enc_toutf8(string) %>%
    str_to_title() %>% 
    str_replace_all(' {2,}', ' ')
}


all_examiners <- read.csv('all_examiners.csv', stringsAsFactors = F)
all_names <- select(all_examiners, -PatEx_ID) %>% distinct()
all_names <- mutate(all_names, Examiner_Name = str_to_title(Examiner_Name))

all_spe <- read.csv('all_spe.csv', stringsAsFactors = F)
all_AU <- unique(all_spe$art_unit)
all_AU <- all_AU[all_AU!='']

#srfw <- read.csv('srfw_data_test.csv', stringsAsFactors = F)
#srfw <- mutate(srfw, raw = preprocess_string(search_note))


con <- dbConnect(MySQL(),
                 user = 'root', password = '',
                 dbname = 'patents', 
                 host = '127.0.0.1', ## if db is hosted locally else enter your server address instead of localhost
                 port = 3333  ## Check your port for MySQL
)

dbListTables(con)

rs <- dbSendQuery(con, 'select * from srfw_data')
srfw <- fetch(rs, n=-1)

#save(srfw, file = 'srfw.rda')

dbDisconnect(con)

load('srfw.rda')
srfw <- mutate(srfw, raw = preprocess_string(search_note))

allmonth <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')


construct_consultant <- function(srfw) {
  
  results <- data.frame()
  
  for (i in 1:nrow(srfw)) {
    cat(i, '\n')
    application_id <- srfw$application_id[i]
    file_date <- srfw$file_date[i]
    year <- srfw$year[i]
    date <- srfw$date[i]
    exclusion_flag <- srfw$exclusion_flag[i]
    raw <- srfw$raw[i]
    #date_from_raw <- ifelse(str_detect(raw, '[0-9]{1,2}/[0-9]{2,4} |[0-9]{2,4}\\.[0-9]{2}\\.[0-9]{2}|[0-9]{2}[a-zA-Z]{3,}[0-9]{2}'), str_extract_all(raw, '[0-9]{1,2}/[0-9]{4}|[0-9]{2,4}\\.[0-9]{2}\\.[0-9]{2}|[0-9]{2}[a-zA-Z]{3,}[0-9]{2}', simplify = T), NA)
    date_from_raw <- ifelse(str_detect(raw, ' [0-9]{1,2}/[0-9]{2,4} |[a-zA-Z]{3,} [0-9]{1,2}/[0-9]{2,4} |[0-9]{2,4}\\.[0-9]{2}\\.[0-9]{2}|[0-9]{2} ?[a-zA-Z]{3,} ?[0-9]{2,4}'), str_extract_all(raw, ' [0-9]{1,2}/[0-9]{4} |[a-zA-Z]{3,} [0-9]{1,2}/[0-9]{2,4} |[0-9]{2,4}\\.[0-9]{2}\\.[0-9]{2}|[0-9]{2} ?[a-zA-Z]{3,} ?[0-9]{2,4}', simplify = T), NA)
    if(!is.na(date_from_raw)) {
      if(str_detect(date_from_raw, '[a-zA-Z]')) {
        date_from_raw <- ifelse(sum(str_detect(allmonth, str_extract_all(date_from_raw, '[a-zA-Z]{1,}', simplify = T)))>0, date_from_raw, NA)
      }
    }
    
    #examiner_to_AU <- ifelse(str_detect(raw, 'Au ?[0-9]{4}|Art Unit:? [0-9]{4}'), str_extract_all(raw, 'Au ?[0-9]{4}|Art Unit:? [0-9]{4}', simplify = T), NA)
    #examiner_to_AU <- str_remove_all(examiner_to_AU, ' |:|Au|Art Unit')
    examiner_to_AU_from_raw <- ifelse(str_detect(raw, ' ?[0-9]{4,} ?'), str_extract_all(raw, ' ?[0-9]{4,} ?', simplify = T), NA)
    examiner_to_AU_from_raw <- str_trim(examiner_to_AU_from_raw, side = 'both')
    examiner_to_AU_from_raw <- ifelse(examiner_to_AU_from_raw %in% all_AU, examiner_to_AU_from_raw, NA)
    examiner_to_role_from_raw <- ifelse(str_detect(raw, 'Pri |Pe |Pe\\.'), 'Primary', 
                                        ifelse(str_detect(raw, 'Primary') & !str_detect(raw, 'Supervisory Primary'), 'Primary', NA))
    examiner_to_position_from_raw <- ifelse(str_detect(raw, 'Spe|Supervisor'), 'SPE', NA)
    if(str_detect(raw, 'Primary.*Spe|Pe.*Spe|Primary.*Supervisor')) {
      examiner_to_role_from_raw <- c('Primary', NA)
      examiner_to_position_from_raw <- c(NA, 'SPE')
    } else if(str_detect(raw, 'Spe.*Primary|Spe.*Pe|Supervisor.*Primary')) {
      examiner_to_role_from_raw <- c(NA, 'Primary')
      examiner_to_position_from_raw <- c('SPE', NA)
    }
    
    index <- c()
    temp_role <- c()
    temp_position <- c()
    for (j in 1:nrow(all_names)) {
      temp <- str_split(all_names$Examiner_Name[j], ',', simplify = T)
      last_name <- temp[1]
      first_name <- temp[2]
      first_name <- str_split(first_name, ' ', simplify = T) %>% str_c()
      #temp <- str_split(all_names$Examiner_Name[j], ',', simplify = T) %>% str_split(' ', simplify = T) %>% str_c()
      first_name <- first_name[first_name!='' & nchar(first_name)>1]
      #temp <- paste0(temp, ' ')
      #temp[2] <- paste0(temp[2], ' ')
      if(str_detect(raw, last_name) & sum(str_detect(raw, first_name), na.rm = T)>0 & min(unlist(str_locate_all(raw, last_name)), na.rm = T) > max(unlist(str_locate_all(raw, first_name)), na.rm = T)) {
        temp_role <- c(temp_role, ifelse(str_detect(raw, 'Pri |Pe |Pe\\.') & min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Pri |Pe |Pe\\.'))) <= 3, 'Primary', 
                                         ifelse(str_detect(raw, 'Primary') & !str_detect(raw, 'Supervisory Primary') & abs(min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Primary')))) <= 3, 'Primary', 
                                                ifelse(str_detect(raw, 'Primary') & !str_detect(raw, 'Supervisory Primary') & str_detect(raw, 'Examiner') & abs(min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Primary')))) <= 12, 'Primary', NA))))
        temp_position <- c(temp_position, ifelse(str_detect(raw, 'Spe|Supervisor') & abs(min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Spe|Supervisor')))) <= 3, 'SPE', 
                                                 ifelse(str_detect(raw, 'Spe|Supervisor') & abs(max(unlist(str_locate_all(raw, last_name))) - min(unlist(str_locate_all(raw, 'Spe|Supervisor')))) <= 3, 'SPE',
                                                        ifelse(str_detect(raw, 'Supervisory Patent Examiner') & abs(min(unlist(str_locate_all(raw, first_name))) - max(unlist(str_locate_all(raw, 'Supervisory Patent Examiner')))) <= 3, 'SPE', NA))))
        index <- c(index, j)
      }
    }
    if(length(index)>0) {
      examiner_to_name_from_raw <- all_names$Examiner_Name[index]
      examiner_to_Internal_ID_from_raw <- all_names$Internal_ID[index]
      examiner_to_role_from_raw <- temp_role
      examiner_to_position_from_raw <- temp_position
    } else {
      examiner_to_name_from_raw <- NA
      examiner_to_Internal_ID_from_raw <- NA
    }
    
    results <- bind_rows(results, data.frame(application_id, exclusion_flag, year, file_date, date, date_from_raw, examiner_to_name_from_raw, examiner_to_Internal_ID_from_raw, examiner_to_AU_from_raw, examiner_to_role_from_raw, examiner_to_position_from_raw, raw, stringsAsFactors = F))
  }
  
  return(results)
  
}


mpi.spawn.Rslaves(nslaves = 3)

mpi.remote.exec(library(stringr))
mpi.remote.exec(library(dplyr))

mpi.bcast.cmd(allmonth <- mpi.bcast.Robj())
mpi.bcast.Robj(allmonth)
mpi.bcast.cmd(all_AU <- mpi.bcast.Robj())
mpi.bcast.Robj(all_AU)
mpi.bcast.cmd(all_names <- mpi.bcast.Robj())
mpi.bcast.Robj(all_names)

mpi.bcast.Robj2slave(construct_consultant)

srfw_list <- list(srfw[52001:53530,], srfw[53531:55060,], srfw[55061:56588,])
mpi.scatter.Robj2slave(srfw_list)

ptm <- proc.time()
temp <- mpi.remote.exec(construct_consultant(srfw_list))
print(proc.time() - ptm)


mpi.close.Rslaves()

#results <- data.frame()
#results <- read.csv('demo_consultant.csv', stringsAsFactors = F)
#results <- mutate(results, examiner_to_AU_from_raw = as.character(examiner_to_AU_from_raw))

load('demo_consultant.rda')

tempresult <- bind_rows(temp[[1]], temp[[2]], temp[[3]])
results <- bind_rows(results, tempresult)

save(results, file = 'demo_consultant.rda')

#write.csv(results, file = 'demo_consultant.csv', row.names = F, na = '')


examiners_gau <- read.csv('examiners_gau.csv', stringsAsFactors = F)
all_spe <- read.csv('all_spe.csv', stringsAsFactors = F)

demo_consultant <- read.csv('demo_consultant.csv', stringsAsFactors = F)
demo_consultant <- mutate(demo_consultant, examiner_to_AU_from_raw = as.character(examiner_to_AU_from_raw))


get_au <- function(y, id) {
  temp <- filter(examiners_gau, year == y & Internal_ID == id)
  return(ifelse(nrow(temp)>0, temp$gau, NA))
}

demo_consultant <- mutate(demo_consultant, examiner_to_AU_from_examiners_gau = mapply(get_au, year, examiner_to_Internal_ID_from_raw))

get_position <- function(y, id) {
  temp <- filter(all_spe, year == y & Internal_ID == id)
  return(ifelse(nrow(temp)>0, 'SPE', NA))
}

demo_consultant <- mutate(demo_consultant, examiner_to_position_from_all_spe = mapply(get_position, year, examiner_to_Internal_ID_from_raw))


write.csv(demo_consultant, file = 'demo_consultant.csv', row.names = F, na = '') 







#application_data <- fread('application_data.csv', stringsAsFactors = F)
#application_data <- select(application_data, application_number:filing_date, examiner_name_last:examiner_art_unit, patent_number:patent_issue_date)
#save(application_data, file = 'application_data.rda')

load('application_data.rda')
all_examiners <- read.csv('all_examiners.csv', stringsAsFactors = F)

app <- filter(application_data, !is.na(examiner_id))
app <- left_join(app, all_examiners, by = c('examiner_id'='PatEx_ID'))
demo_application <- filter(application_data, is.na(examiner_id)) %>% bind_rows(app) %>% distinct()

write.csv(demo_application, file = 'demo_application.csv', row.names = F, na = '') 



rawexaminers <- read.delim('rawexaminer.tsv', stringsAsFactors = F)
rawexaminers <- select(rawexaminers, -uuid)
demo_application2 <- select(demo_application, application_number:filing_date, patent_number:patent_issue_date)
demo_application2 <- left_join(demo_application2, rawexaminers, by = c('patent_number'='patent_id'))

preprocess_string <- function(string) {
  str_trim(string, side = 'both') %>%
    str_replace_all(' {2,}', ' ')
}

demo_application2 <- mutate(demo_application2, name_first = preprocess_string(name_first), name_last = preprocess_string(name_last))

link <- select(demo_application2, name_first:name_last) %>% distinct()
link <- filter(link, !(is.na(name_first) & is.na(name_last)))
  
get_last_name <- function(name) {
  str_trim(str_to_title(str_split(name, ',', simplify = T)[1]), side = 'both')
}

get_first_name <- function(name) {
  str_trim(str_to_title(str_split(name, ',', simplify = T)[2]), side = 'both')
}


all_examiners <- read.csv('all_examiners.csv', stringsAsFactors = F)
all_names <- select(all_examiners, -PatEx_ID) %>% distinct()
all_names <- mutate(all_names, last_name = sapply(Examiner_Name, get_last_name, simplify = T), first_name = sapply(Examiner_Name, get_first_name, simplify = T))
all_names <- mutate(all_names, full_name = ifelse(is.na(last_name), first_name, 
                                        ifelse(is.na(first_name), last_name, paste(first_name, last_name))))

results <- data.frame()

for(i in 20001:21000) {
  cat(i, '\n')
  name_first <- link$name_first[i]
  name_last <- link$name_last[i]
  index <- c()
  for(j in 1:nrow(all_names)) {
    last_name <- all_names$last_name[j]
    first_name <- str_split(all_names$first_name[j], ' ', simplify = T) %>% str_c()
    first_name <- first_name[first_name!='' & nchar(first_name)>1]
    if(str_detect(name_last, last_name) & sum(str_detect(name_first, first_name), na.rm = T)>0) {
      index <- c(index, j)
    }
  }
  if(length(index)>0) {
    matched_name <- all_names$Examiner_Name[index]
    Internal_ID <- all_names$Internal_ID[index]
  } else {
    matched_name <- NA
    Internal_ID <- NA
  }
  
  results <- bind_rows(results, data.frame(name_first, name_last, matched_name, Internal_ID, stringsAsFactors = F))
}

save(results, file = 'temp_app_link.rda')


write.csv(demo_application2, file = 'demo_application2.csv', row.names = F, na = '') 





demo_application2 <- fread('demo_application2.csv', stringsAsFactors = F)
link <- select(demo_application2, name_first:name_last) %>% distinct()
link <- filter(link, !(is.na(name_first) & is.na(name_last))) %>%
  filter(!(name_first=='' & name_last==''))

rm(demo_application2)
gc()

get_last_name <- function(name) {
  str_trim(str_to_title(str_split(name, ',', simplify = T)[1]), side = 'both')
}

get_first_name <- function(name) {
  str_trim(str_to_title(str_split(name, ',', simplify = T)[2]), side = 'both')
}


all_examiners <- read.csv('all_examiners.csv', stringsAsFactors = F)
all_names <- select(all_examiners, -PatEx_ID) %>% distinct()
all_names <- mutate(all_names, last_name = sapply(Examiner_Name, get_last_name, simplify = T), first_name = sapply(Examiner_Name, get_first_name, simplify = T))
all_names <- mutate(all_names, full_name = ifelse(is.na(last_name), first_name, 
                                                  ifelse(is.na(first_name), last_name, paste(first_name, last_name))))


construct_application <- function(link) {
  
  results <- data.frame()
  
  for(i in 1:nrow(link)) {
    cat(i, '\n')
    name_first <- link$name_first[i]
    name_last <- link$name_last[i]
    index <- c()
    for(j in 1:nrow(all_names)) {
      last_name <- all_names$last_name[j]
      first_name <- str_split(all_names$first_name[j], ' ', simplify = T) %>% str_c()
      first_name <- first_name[first_name!='' & nchar(first_name)>1]
      if(str_detect(name_last, last_name) & sum(str_detect(name_first, first_name), na.rm = T)>0) {
        index <- c(index, j)
      }
    }
    if(length(index)>0) {
      matched_name <- all_names$Examiner_Name[index]
      Internal_ID <- all_names$Internal_ID[index]
    } else {
      matched_name <- NA
      Internal_ID <- NA
    }
    
    results <- bind_rows(results, data.frame(name_first, name_last, matched_name, Internal_ID, stringsAsFactors = F))
  }
  
  return(results)
  
}



mpi.spawn.Rslaves(nslaves = 3)

mpi.remote.exec(library(stringr))
mpi.remote.exec(library(dplyr))

mpi.bcast.cmd(all_names <- mpi.bcast.Robj())
mpi.bcast.Robj(all_names)

mpi.bcast.Robj2slave(construct_application)

link_list <- list(link[34001:35000,], link[35001:36000,], link[36001:37000,])
mpi.scatter.Robj2slave(link_list)

ptm <- proc.time()
temp <- mpi.remote.exec(construct_application(link_list))
print(proc.time() - ptm)


mpi.close.Rslaves()


#results <- data.frame()
load('temp_app_link.rda')

tempresult <- bind_rows(temp[[1]], temp[[2]], temp[[3]])
results <- bind_rows(results, tempresult)

save(results, file = 'temp_app_link.rda')


load('temp_app_link_sciserver.rda')
tempresults <- results
load('temp_app_link.rda')
results <- bind_rows(results, tempresults)
save(results, file = 'temp_app_link.rda')



demo_application2 <- fread('demo_application2.csv', stringsAsFactors = F)
demo_application2 <- left_join(demo_application2, results)

write.csv(demo_application2, file = 'demo_application2.csv', row.names = F, na = '') 



check <- select(demo_application, application_number, examiner_name_last:examiner_name_middle)
check <- filter(check, examiner_name_last != 'None')
temp <- select(demo_application2, application_number, name_first:role)
check <- left_join(check, temp)
check <- filter(check, role != '')





#################################################################################

library(data.table)
library(tidyr)
library(dplyr)
library(dbplyr)
library(DBI)
library(RMySQL)
library(RPostgreSQL)
library(stringr)
library(httr)
library(jsonlite)
library(wru)
library(gender)
library(genderizeR)
library(namesexdata)
library(zipzcta)
library(zcta)
library(gaze)
library(tidycensus)
library(tidyverse)

setwd('/mnt/data2/census_project')


drv <- dbDriver('PostgreSQL')  
db <- 'cbsgaltaxdb1'  
host_db <- '10.175.198.54'  
db_port <- '5432'  
db_user <- 'cong'  
conn <- dbConnect(drv, dbname=db, host=host_db, port=db_port, user=db_user, password=rstudioapi::askForPassword("Password"))


dtab <- dbGetQuery(conn, "select * from zcta")

ptin_panel_data <- dbGetQuery(conn, "select * from ptin_panel_data")

tbl_age_2012_04 <- dbGetQuery(conn, "select * from tbl_age_2012_04")
tbl_age_2012_12 <- dbGetQuery(conn, "select * from tbl_age_2012_12")
tbl_age_2013_01 <- dbGetQuery(conn, "select * from tbl_age_2013_01")
tbl_age_2014_04 <- dbGetQuery(conn, "select * from tbl_age_2014_04")
tbl_age_2015_12 <- dbGetQuery(conn, "select * from tbl_age_2015_12")

tbl_irs_foia_ero_contact_2015_12 <- dbGetQuery(conn, "select * from tbl_irs_foia_ero_contact_2015_12")
tbl_irs_foia_ero_contact_2016_08 <- dbGetQuery(conn, "select * from tbl_irs_foia_ero_contact_2016_08")
tbl_irs_foia_ero_contact_2016_12 <- dbGetQuery(conn, "select * from tbl_irs_foia_ero_contact_2016_12")
tbl_irs_foia_ero_contact_2017_09 <- dbGetQuery(conn, "select * from tbl_irs_foia_ero_contact_2017_09")

tbl_irs_foia_ero_partner_2015_12 <- dbGetQuery(conn, "select * from tbl_irs_foia_ero_partner_2015_12")
tbl_irs_foia_ero_partner_2016_08 <- dbGetQuery(conn, "select * from tbl_irs_foia_ero_partner_2016_08")
tbl_irs_foia_ero_partner_2016_12 <- dbGetQuery(conn, "select * from tbl_irs_foia_ero_partner_2016_12")
tbl_irs_foia_ero_partner_2017_09 <- dbGetQuery(conn, "select * from tbl_irs_foia_ero_partner_2017_09")

tbl_irs_foia_ptin_2010 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2010")
tbl_irs_foia_ptin_2011 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2011")
tbl_irs_foia_ptin_2012 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2012")
tbl_irs_foia_ptin_2012_04 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2012_04")
tbl_irs_foia_ptin_2013_01 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2013_01")
tbl_irs_foia_ptin_2014_04 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2014_04")
tbl_irs_foia_ptin_2015_12 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2015_12")
tbl_irs_foia_ptin_2016_10 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2016_10")
tbl_irs_foia_ptin_2017_03 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2017_03")
tbl_irs_foia_ptin_2017_08 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2017_08")
tbl_irs_foia_ptin_2017_09 <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_2017_09")
tbl_irs_foia_ptin_legacy <- dbGetQuery(conn, "select * from tbl_irs_foia_ptin_legacy")


table_names <- dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE table_schema='public'")


preprocess_name <- function(name) {
  temp <- name %>% str_squish() %>% str_split(' ') %>% unlist()
  name <- ifelse(length(temp) == 2 & (grepl('\\.', temp[1]) | nchar(temp[1]) == 1), temp[2], 
                 ifelse(length(temp) > 2 & nchar(temp[length(temp)]) > nchar(temp[1]), temp[length(temp)], temp[1])) %>%
    str_squish() %>%
    str_replace_all("\\\\|^-|[0-9]-|--+|;|,|\\.|\\?|'|\"|#|`|\\(|\\)|]|[0-9]", "") %>%
    str_to_title()
  return(name)
}


predict_race_by_wru <- function(p1, p2, p3, p4, p5) {
  race <- c('white', 'black', 'hispanic', 'asian', 'other')
  return(race[which.max(c(p1,p2,p3,p4,p5))])
}




#### Extract all names

tables_contain_names <- table_names %>% 
  filter(grepl('ptin_panel|age|ero_contact|ero_partner|tbl_irs_foia_ptin', table_name)) %>%
  arrange(table_name)


## All first names
allfnames <- data.frame()

for (i in 1:nrow(tables_contain_names)) {
  cat(i, '\n')
  temp <- tbl(conn, tables_contain_names$table_name[i]) %>%
    select(fname) %>%
    distinct() %>%
    collect()
  allfnames <- rbind(allfnames, temp)
}

allfnames <- allfnames %>% 
  distinct() %>%
  filter(!is.na(fname) & fname != '')

allfnames <- allfnames %>%
  mutate(first_name = sapply(fname, preprocess_name))

allfnames <- allfnames %>%
  arrange(first_name)

save(allfnames, file = 'allfnames.rda')


## All last names
alllnames <- data.frame()

for (i in 1:nrow(tables_contain_names)) {
  cat(i, '\n')
  temp <- tbl(conn, tables_contain_names$table_name[i]) %>%
    select(lname) %>%
    distinct() %>%
    collect()
  alllnames <- rbind(alllnames, temp)
}

alllnames <- alllnames %>% 
  distinct() %>%
  filter(!is.na(lname) & lname != '')

alllnames <- alllnames %>%
  mutate(last_name = sapply(lname, preprocess_name))

alllnames <- alllnames %>%
  arrange(last_name)

save(alllnames, file = 'alllnames.rda')




#### Race/ethnicity based on last name using package `wru`

surname_to_race <- alllnames %>% 
  select(last_name) %>%
  distinct() %>%
  mutate(surname = last_name)

surname_to_race <- predict_race(voter.file = surname_to_race, surname.only = T)

surname_to_race <- surname_to_race %>%
  mutate(race_by_wru = mapply(predict_race_by_wru, pred.whi, pred.bla, pred.his, pred.asi, pred.oth)) %>%
  rename(prob_white_by_wru = pred.whi, 
         prob_black_by_wru = pred.bla, 
         prob_hispanic_by_wru = pred.his, 
         prob_asian_by_wru = pred.asi, 
         prob_other_by_wru = pred.oth)

save(surname_to_race, file = 'surname_to_race.rda')



#### Gender based on first name using packages `gender`, `genderizeR`, `namesexdata`

fnames <- unique(allfnames$first_name)
allgender <- data.frame()

## SSA, IPUMS, NAPP method
methods <- c('ssa', 'ipums', 'napp')

for(m in methods) {
  tempgender <- gender(fnames, method = m)
  tempgender <- select(tempgender, -(year_min:year_max)) %>%
    rename(first_name = name) %>%
    mutate(method = m)
  # Extract the first name that have not been gendered
  fnames <- setdiff(fnames, tempgender$first_name)
  allgender <- bind_rows(allgender, tempgender)
}

## Kantrowitz method
tempgender <- gender(fnames, method = 'kantrowitz') %>%
  filter(!is.na(gender)) %>%
  mutate(method = 'kantrowitz') %>%
  rename(first_name = name)

allgender <- bind_rows(allgender, tempgender)

# Extract the first name that have not been gendered
fnames <- setdiff(fnames, tempgender$first_name) 

## Genderize method using GET (could give probability)
tempgender <- data.frame()

for(i in 1:length(fnames)) {
  tempresult <- paste0('https://api.genderize.io/?name=', fnames[i]) %>%
    GET() %>% 
    content('text', encoding = 'UTF-8') %>%
    fromJSON() %>%
    unlist() %>%
    data.frame() %>%
    t() %>%
    data.frame()
  
  if(ncol(tempresult) > 1) {
    if(tempresult$gender == 'male') {
      tempresult <- rename(tempresult, first_name = name, proportion_male = probability) %>%
        mutate(proportion_male = as.numeric(as.character(proportion_male)), proportion_female = 1 - proportion_male, method = 'genderize') %>%
        select(first_name, proportion_male, proportion_female, gender, count, method)
    } else {
      tempresult <- rename(tempresult, first_name = name, proportion_female = probability) %>%
        mutate(proportion_female = as.numeric(as.character(proportion_female)), proportion_male = 1 - proportion_female, method = 'genderize') %>%
        select(first_name, proportion_male, proportion_female, gender, count, method)
    }
  } else {
    tempresult <- rename(tempresult, first_name = name)
  }
  
  tempgender <- bind_rows(tempgender, tempresult)
  
}

tempgender <- filter(tempgender, !is.na(gender))
allgender <- bind_rows(allgender, tempgender)

# Extract the first name that have not been gendered
fnames <- setdiff(fnames, tempgender$first_name) 


#### Zip to ZCTA to County

zip_to_zcta <- zipzcta::zipzcta

zcta_to_county <- zcta::zcta_county_rel_10 %>%
  select(zcta5:poppt, zpop, copop, zpoppct, copoppct) %>%
  mutate(geoid = as.integer(geoid)) %>%
  left_join(gaze::county10, by = "geoid") %>%
  select(zcta5:usps, name) %>%
  rename(zcta_2010 = zcta5, 
         state_code_2010 = state, 
         state_abb = usps,
         county_code_2010 = county,
         county_name = name,
         county_geoid = geoid,
         poppt_2010 = poppt,
         zpop_2010 = zpop,
         zpoppt_2010 = zpoppct,
         copop_2010 = copop,
         copoppt_2010 = copoppct)

zip_to_zcta_to_county <- zip_to_zcta %>% 
  left_join(zcta_to_county, by = c("zcta" = "zcta_2010")) %>%
  select(zip:po_name, 
         zip_type, 
         zcta_2010 = zcta, 
         state_code_2010, 
         state_abb,
         county_code_2010,
         county_name,
         county_geoid,
         poppt_2010:copoppt_2010) %>%
  arrange(zip)

save(zip_to_zcta_to_county, file = 'zip_to_zcta_to_county.rda')



#### Add race and ethnicity data to all ZCTAs, counties, and states based on 2010 Census

state <- data.frame(state.name, state.abb) %>%
  bind_rows(data.frame(state.name = c('District of Columbia', 'Puerto Rico'), state.abb = c('DC', 'PR'))) %>%
  rename(state_name = state.name, state_abb = state.abb)

census_api_key("98ec67d94b5ddb2f2d6a09989fd4e70587428fb2")

v10_1 <- load_variables(2010, 'sf1')


temp <- get_decennial(geography = "state", variables = "H011001", year = 2010)
state_pop <- temp %>% 
  select(state_geoid = GEOID, state_name = NAME, state_pop_2010 = value) %>%
  left_join(state) %>%
  select(state_geoid:state_name, state_abb, state_pop_2010)

save(state_pop, file = 'state_pop.rda')


temp <- get_decennial(geography = "county", variables = "H011001", year = 2010)
county_pop <- temp %>% 
  select(county_geoid = GEOID, county_name_withstate = NAME, county_pop_2010 = value) %>%
  mutate(county_geoid = as.integer(county_geoid))

save(county_pop, file = 'county_pop.rda')



state_race <- read.csv('reference_race_state.csv')
state_race <- state_race %>%
  select(state_geoid = Id,
         state_name = Geography,
         state_race_not_hispanic_2010 = Not.Hispanic.or.Latino.,
         state_race_not_hispanic_white_2010 = Not.Hispanic.or.Latino....White.alone,
         state_race_not_hispanic_black_2010 = Not.Hispanic.or.Latino....Black.or.African.American.alone,
         state_race_not_hispanic_AmericanIndianAndAlaskaNative_2010 = Not.Hispanic.or.Latino....American.Indian.and.Alaska.Native.alone,
         state_race_not_hispanic_asian_2010 = Not.Hispanic.or.Latino....Asian.alone,
         state_race_not_hispanic_NativeHawaiianAndOtherPacificIslander_2010 = Not.Hispanic.or.Latino....Native.Hawaiian.and.Other.Pacific.Islander.alone,
         state_race_not_hispanic_other_2010 = Not.Hispanic.or.Latino....Some.Other.Race.alone,
         state_race_not_hispanic_TwoOrMore_2010 = Not.Hispanic.or.Latino....Two.or.More.Races,
         state_race_hispanic_2010 = Hispanic.or.Latino.,
         state_race_hispanic_white_2010 = Hispanic.or.Latino....White.alone,
         state_race_hispanic_black_2010 = Hispanic.or.Latino....Black.or.African.American.alone,
         state_race_hispanic_AmericanIndianAndAlaskaNative_2010 = Hispanic.or.Latino....American.Indian.and.Alaska.Native.alone,
         state_race_hispanic_asian_2010 = Hispanic.or.Latino....Asian.alone,
         state_race_hispanic_NativeHawaiianAndOtherPacificIslander_2010 = Hispanic.or.Latino....Native.Hawaiian.and.Other.Pacific.Islander.alone,
         state_race_hispanic_other_2010 = Hispanic.or.Latino....Some.Other.Race.alone,
         state_race_hispanic_TwoOrMore_2010 = Hispanic.or.Latino....Two.or.More.Races)
state_race <- state_race %>%
  mutate(state_geoid = str_sub(state_geoid, 10, 11),
         state_total_2010 = state_race_not_hispanic_2010 + state_race_hispanic_2010)

save(state_race, file = 'state_race.rda')



county_race <- read.csv('reference_race_county.csv')
county_race <- county_race %>%
  select(county_geoid = Id2,
         county_name_withstate = Geography,
         county_race_not_hispanic_2010 = Not.Hispanic.or.Latino.,
         county_race_not_hispanic_white_2010 = Not.Hispanic.or.Latino....White.alone,
         county_race_not_hispanic_black_2010 = Not.Hispanic.or.Latino....Black.or.African.American.alone,
         county_race_not_hispanic_AmericanIndianAndAlaskaNative_2010 = Not.Hispanic.or.Latino....American.Indian.and.Alaska.Native.alone,
         county_race_not_hispanic_asian_2010 = Not.Hispanic.or.Latino....Asian.alone,
         county_race_not_hispanic_NativeHawaiianAndOtherPacificIslander_2010 = Not.Hispanic.or.Latino....Native.Hawaiian.and.Other.Pacific.Islander.alone,
         county_race_not_hispanic_other_2010 = Not.Hispanic.or.Latino....Some.Other.Race.alone,
         county_race_not_hispanic_TwoOrMore_2010 = Not.Hispanic.or.Latino....Two.or.More.Races,
         county_race_hispanic_2010 = Hispanic.or.Latino.,
         county_race_hispanic_white_2010 = Hispanic.or.Latino....White.alone,
         county_race_hispanic_black_2010 = Hispanic.or.Latino....Black.or.African.American.alone,
         county_race_hispanic_AmericanIndianAndAlaskaNative_2010 = Hispanic.or.Latino....American.Indian.and.Alaska.Native.alone,
         county_race_hispanic_asian_2010 = Hispanic.or.Latino....Asian.alone,
         county_race_hispanic_NativeHawaiianAndOtherPacificIslander_2010 = Hispanic.or.Latino....Native.Hawaiian.and.Other.Pacific.Islander.alone,
         county_race_hispanic_other_2010 = Hispanic.or.Latino....Some.Other.Race.alone,
         county_race_hispanic_TwoOrMore_2010 = Hispanic.or.Latino....Two.or.More.Races)
county_race <- county_race %>%
  mutate(county_total_2010 = county_race_not_hispanic_2010 + county_race_hispanic_2010)

save(county_race, file = 'county_race.rda')



zcta_race <- read.csv('reference_race_zcta.csv')
zcta_race <- zcta_race %>%
  select(zcta_2010 = Geography,
         zcta_race_not_hispanic_2010 = Not.Hispanic.or.Latino.,
         zcta_race_not_hispanic_white_2010 = Not.Hispanic.or.Latino....White.alone,
         zcta_race_not_hispanic_black_2010 = Not.Hispanic.or.Latino....Black.or.African.American.alone,
         zcta_race_not_hispanic_AmericanIndianAndAlaskaNative_2010 = Not.Hispanic.or.Latino....American.Indian.and.Alaska.Native.alone,
         zcta_race_not_hispanic_asian_2010 = Not.Hispanic.or.Latino....Asian.alone,
         zcta_race_not_hispanic_NativeHawaiianAndOtherPacificIslander_2010 = Not.Hispanic.or.Latino....Native.Hawaiian.and.Other.Pacific.Islander.alone,
         zcta_race_not_hispanic_other_2010 = Not.Hispanic.or.Latino....Some.Other.Race.alone,
         zcta_race_not_hispanic_TwoOrMore_2010 = Not.Hispanic.or.Latino....Two.or.More.Races,
         zcta_race_hispanic_2010 = Hispanic.or.Latino.,
         zcta_race_hispanic_white_2010 = Hispanic.or.Latino....White.alone,
         zcta_race_hispanic_black_2010 = Hispanic.or.Latino....Black.or.African.American.alone,
         zcta_race_hispanic_AmericanIndianAndAlaskaNative_2010 = Hispanic.or.Latino....American.Indian.and.Alaska.Native.alone,
         zcta_race_hispanic_asian_2010 = Hispanic.or.Latino....Asian.alone,
         zcta_race_hispanic_NativeHawaiianAndOtherPacificIslander_2010 = Hispanic.or.Latino....Native.Hawaiian.and.Other.Pacific.Islander.alone,
         zcta_race_hispanic_other_2010 = Hispanic.or.Latino....Some.Other.Race.alone,
         zcta_race_hispanic_TwoOrMore_2010 = Hispanic.or.Latino....Two.or.More.Races)
zcta_race <- zcta_race %>%
  mutate(zcta_2010 = str_remove(zcta_2010, 'ZCTA5 '),
         zcta_total_2010 = zcta_race_not_hispanic_2010 + zcta_race_hispanic_2010)

save(zcta_race, file = 'zcta_race.rda')


state_info <- left_join(state_pop, state_race)
state_info <- state_info %>%
  mutate(state_race_white_2010 = state_race_not_hispanic_white_2010,
         state_race_black_2010 = state_race_not_hispanic_black_2010,
         state_race_asian_2010 = state_race_not_hispanic_asian_2010,
         state_race_other_2010 = state_race_not_hispanic_AmericanIndianAndAlaskaNative_2010 + state_race_not_hispanic_NativeHawaiianAndOtherPacificIslander_2010 + state_race_not_hispanic_other_2010 + state_race_not_hispanic_TwoOrMore_2010) %>%
  select(state_geoid:state_pop_2010, 
         state_total_2010,
         state_race_white_2010:state_race_black_2010,
         state_race_hispanic_2010,
         state_race_asian_2010:state_race_other_2010)

county_info <- left_join(county_pop, county_race)
county_info <- county_info %>%
  mutate(county_race_white_2010 = county_race_not_hispanic_white_2010,
         county_race_black_2010 = county_race_not_hispanic_black_2010,
         county_race_asian_2010 = county_race_not_hispanic_asian_2010,
         county_race_other_2010 = county_race_not_hispanic_AmericanIndianAndAlaskaNative_2010 + county_race_not_hispanic_NativeHawaiianAndOtherPacificIslander_2010 + county_race_not_hispanic_other_2010 + county_race_not_hispanic_TwoOrMore_2010) %>%
  select(county_geoid:county_pop_2010, 
         county_total_2010,
         county_race_white_2010:county_race_black_2010,
         county_race_hispanic_2010,
         county_race_asian_2010:county_race_other_2010) %>%
  arrange(county_geoid)

zcta_info <- zcta_race %>%
  mutate(zcta_race_white_2010 = zcta_race_not_hispanic_white_2010,
         zcta_race_black_2010 = zcta_race_not_hispanic_black_2010,
         zcta_race_asian_2010 = zcta_race_not_hispanic_asian_2010,
         zcta_race_other_2010 = zcta_race_not_hispanic_AmericanIndianAndAlaskaNative_2010 + zcta_race_not_hispanic_NativeHawaiianAndOtherPacificIslander_2010 + zcta_race_not_hispanic_other_2010 + zcta_race_not_hispanic_TwoOrMore_2010) %>%
  select(zcta_2010, 
         zcta_total_2010,
         zcta_race_white_2010:zcta_race_black_2010,
         zcta_race_hispanic_2010,
         zcta_race_asian_2010:zcta_race_other_2010)


zip_zcta_county_pop_race <- zip_to_zcta_to_county %>%
  select(zip, zcta_2010:county_geoid, zpop_2010:copop_2010)
zip_zcta_county_pop_race <- zip_zcta_county_pop_race %>%
  left_join(zcta_info) %>%
  left_join(state_info) %>%
  left_join(county_info)
zip_zcta_county_pop_race <- zip_zcta_county_pop_race %>% 
  select(zip:zcta_2010,
         state_code_2010, state_geoid:state_name, state_abb,
         county_code_2010, county_geoid, county_name,
         zcta_total_2010:zcta_race_other_2010,
         state_total_2010:state_race_other_2010,
         county_total_2010:county_race_other_2010)

save(zip_zcta_county_pop_race, file = 'zip_zcta_county_pop_race.rda')
write.csv(zip_zcta_county_pop_race, file = 'zip_zcta_county_pop_race.csv', row.names = FALSE)


check <- zip_zcta_county_pop_race %>%
  select(zcta_total_2010:county_race_other_2010) %>%
  mutate(count_zcta = zcta_race_white_2010 + zcta_race_black_2010 + zcta_race_hispanic_2010 + zcta_race_asian_2010 + zcta_race_other_2010,
         count_state = state_race_white_2010 + state_race_black_2010 + state_race_hispanic_2010 + state_race_asian_2010 + state_race_other_2010,
         count_county = county_race_white_2010 + county_race_black_2010 + county_race_hispanic_2010 + county_race_asian_2010 + county_race_other_2010) %>%
  select(zcta_total_2010, count_zcta, state_total_2010, count_state, county_total_2010, count_county) %>%
  mutate(diff_zcta = zcta_total_2010 - count_zcta,
         diff_state = state_total_2010 - count_state,
         diff_county = county_total_2010 - count_county)

write.csv(check, file = 'check_zip_zcta_county_pop_race.csv', row.names = FALSE)







setwd('/mnt/data2/examiners/')

drv <- dbDriver('MySQL')  
db <- 'examiners'
host_db <- '10.175.198.54'  
db_port <- 3306
db_user <- 'cong'  
conn <- dbConnect(drv, dbname=db, host=host_db, port=db_port, user=db_user, password=rstudioapi::askForPassword("Password"))

table_names <- dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE table_schema='examiners'")

all_examiners <- tbl(conn, table_names$table_name[2]) %>%
  collect() %>%
  select(-PatEx_ID)

examiners_gs_old <- tbl(conn, table_names$table_name[5]) %>%
  collect()

copy_to(conn, examiners_gs_old, 'examiners_gs_old', temporary = FALSE, indexes = list('Internal_ID'))

check_temp <- examiners_gs_old %>%
  group_by(examiner_name) %>%
  summarise(count = n())

raw <- read.csv('16-00252_Responsive_Records.csv') %>%
  mutate(Examiner.Name = str_squish(Examiner.Name)) %>%
  arrange(Examiner.Name)

examiners_gs <- raw %>%
  gather(grade, start_date, X5:X15) %>%
  filter(start_date != '') %>%
  rename(Examiner_Name = Examiner.Name) %>%
  mutate(grade = str_remove(grade, 'X'), Examiner_Name = str_squish(Examiner_Name)) %>%
  arrange(Examiner_Name)

end_date <- c()
for (name in unique(examiners_gs$Examiner_Name)) {
  t <- examiners_gs %>%
    filter(Examiner_Name == name)
  end_date <- c(end_date, lead(t$start_date))
}

examiners_gs <- examiners_gs %>%
  cbind(end_date) %>%
  left_join(all_examiners) %>%
  distinct()

save(examiners_gs, file = 'examiners_gs.rda')
write.csv(examiners_gs, file = 'examiners_gs.csv', row.names = FALSE, na = '')


check <- examiners_gs %>%
  group_by(Examiner_Name) %>%
  summarise(count1 = n())

check2 <- data.frame()
for (i in 1:nrow(raw)) {
  cat(i, '\n')
  Examiner_Name <- raw$Examiner.Name[i]
  count2 <- sum(raw[i,]!='')-1
  check2 <- rbind(check2, data.frame(Examiner_Name, count2))
}

check <- check %>%
  left_join(check2) %>%
  mutate(diff = abs(count1-count2))

write.csv(check, file = 'check.csv', row.names = FALSE)



copy_to(conn, examiners_gs, 'examiners_gs', overwrite = TRUE, temporary = FALSE, indexes = list('Internal_ID'))




preprocess_name <- function(name) {
  name <- name %>% str_squish()
  return(name)
}


all_examiners <- tbl(conn, table_names$table_name[2]) %>%
  collect()



# all_examiners <- all_examiners %>%
#   mutate(Examiner_Name = preprocess_name(Examiner_Name), n_char = nchar(Examiner_Name)) %>%
#   arrange(n_char)
# 
# results <- data.frame()
# allnames <- all_examiners$Examiner_Name
# for (i in 1:nrow(all_examiners)) {
#   cat(i, '\n')
#   
#   if (all_examiners$n_char[i] > 9) {
#     ind <- agrep(all_examiners$Examiner_Name[i], allnames, max.distance = 0.1, costs = list(insert = 0.1, delet = 0, sub = 0))
#     Internal_ID <- all_examiners$Internal_ID[ind]
#     PatEx_ID <- all_examiners$PatEx_ID[ind]
#     Examiner_Name <- all_examiners$Examiner_Name[ind]
#   } else {
#     Internal_ID <- all_examiners$Internal_ID[i]
#     PatEx_ID <- all_examiners$PatEx_ID[i]
#     Examiner_Name <- all_examiners$Examiner_Name[i]
#   }
#   Internal_ID_2 <- i
#   results <- bind_rows(results, data.frame(Internal_ID, Internal_ID_2, PatEx_ID, Examiner_Name))
#   allnames <- setdiff(allnames, Examiner_Name)
#   
#   if (length(allnames) < 1) {
#     break
#   }
# }


all_examiners <- all_examiners %>%
  mutate(Examiner_Name = preprocess_name(Examiner_Name)) %>%
  arrange(Examiner_Name)

allnames <- unique(all_examiners$Examiner_Name)
link <- data.frame(Internal_ID_2 = 1:length(allnames), Examiner_Name = allnames)

all_examiners <- left_join(all_examiners, link) %>%
  select(Internal_ID, Internal_ID_2, PatEx_ID, Examiner_Name)


save(all_examiners, file = 'all_examiners.rda')
write.csv(all_examiners, file = 'all_examiners.csv', row.names = F, na = '')




load('examiners_gs.rda')
all_examiners <- read.csv('all_examiners.csv')
application_data <- fread('application_data.csv')

all_spe <- tbl(conn, table_names$table_name[3]) %>%
  collect()

examiners_gau <- tbl(conn, table_names$table_name[4]) %>%
  collect()

link1 <- examiners_gs %>%
  mutate(sdate = as.Date(start_date, format = '%m/%d/%y')) %>%
  group_by(Internal_ID) %>%
  summarise(earliest_date_from_gs = min(sdate))

link2 <- application_data %>%
  select(PatEx_ID = examiner_id, filing_date) %>%
  filter(!is.na(PatEx_ID) & filing_date != '') %>% 
  group_by(PatEx_ID) %>%
  summarise(earliest_date_from_patex = min(filing_date))

link3 <- all_spe %>%
  select(year:month, Internal_ID) %>%
  mutate(date = as.Date(paste0(year, '-', month, '-', '1'))) %>%
  group_by(Internal_ID) %>%
  summarise(earliest_date_from_spe = min(date))

link4 <- examiners_gau %>%
  select(year, Internal_ID) %>%
  group_by(Internal_ID) %>%
  summarise(earliest_year_from_gau = min(year))


similarnames <- all_examiners %>%
  left_join(link1) %>%
  left_join(link2) %>%
  left_join(link3) %>%
  left_join(link4)

save(similarnames, file = 'similarnames.rda')


diff_days <- function(dat, row1, row2) {
  return(difftime(dat$earliest_date_from_gs[row1], dat$earliest_date_from_patex[row2]))
}

temp1 <- similarnames %>% filter(Manually == 0)
t <- temp1 %>% 
  group_by(Internal_ID_2) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
temp1 <- temp1 %>% filter(Internal_ID_2 %in% t$Internal_ID_2)

temp2 <- similarnames %>% filter(Manually != 0)


similarnames_long <- read.csv('similarnames_long.csv')

similarnames_wide <- similarnames_long %>%
  spread(Internal_ID_2, Diff_Days)

diff_days <- unique(similarnames_long$Diff_Days)
diff_days <- diff_days[!is.na(diff_days)]

dat <- as_tibble(diff_days)
dat %>% filter(value <= 1000) %>% ggplot(aes(x = value)) + geom_histogram()

ggplot(data.frame(diff_days)) + geom_histogram(aes(x = diff_days), bin = 40)




all_examiner_gender_old <- tbl(conn, table_names$table_name[1]) %>% collect()
save(all_examiner_gender_old, file = 'all_examiner_gender_old.rda')  
write.csv(all_examiner_gender_old, file = 'all_examiner_gender_old.csv', row.names = F, na = '')

all_examiners_old <- tbl(conn, table_names$table_name[2]) %>% collect()
save(all_examiners_old, file = 'all_examiners_old.rda')  
write.csv(all_examiners_old, file = 'all_examiners_old.csv', row.names = F, na = '')

all_spe_old <- tbl(conn, table_names$table_name[3]) %>% collect()
save(all_spe_old, file = 'all_spe_old.rda')  
write.csv(all_spe_old, file = 'all_spe_old.csv', row.names = F, na = '')

examiners_gau_old <- tbl(conn, table_names$table_name[4]) %>% collect()
save(examiners_gau_old, file = 'examiners_gau_old.rda')  
write.csv(examiners_gau_old, file = 'examiners_gau_old.csv', row.names = F, na = '')

examiners_gs_old <- tbl(conn, table_names$table_name[5]) %>% collect()
save(examiners_gs_old, file = 'examiners_gs_old.rda')  
write.csv(examiners_gs_old, file = 'examiners_gs_old.csv', row.names = F, na = '')



all_examiners <- read.csv('all_examiners.csv')
all_examiners <- all_examiners %>%
  rename(Internal_ID_old = Internal_ID, 
         Internal_ID = Internal_ID_2,
         Possible_Paired_Internal_ID = Possible_Paired_Internal_ID_2)
save(all_examiners, file = 'all_examiners.rda')
copy_to(conn, all_examiners, 'all_examiners', overwrite = TRUE, temporary = FALSE, indexes = list('Internal_ID'))


link <- all_examiners %>%
  select(Internal_ID_old:Internal_ID) %>%
  distinct()

all_examiner_gender <- tbl(conn, table_names$table_name[1]) %>%
  collect() %>% 
  rename(Internal_ID_old = Internal_ID) %>%
  left_join(link) %>%
  select(Internal_ID_old, Internal_ID, everything()) 

all_examiner_gender <- tbl(conn, table_names$table_name[1]) %>%
  collect() %>%
  distinct()
save(all_examiner_gender, file = 'all_examiner_gender.rda')  
write.csv(all_examiner_gender, file = 'all_examiner_gender.csv', row.names = F, na = '')
copy_to(conn, all_examiner_gender, 'all_examiner_gender', overwrite = TRUE, temporary = FALSE, indexes = list('Internal_ID'))


all_spe <- tbl(conn, table_names$table_name[3]) %>%
  collect() %>%
  rename(Internal_ID_old = Internal_ID) %>%
  left_join(link)
all_spe <- tbl(conn, table_names$table_name[3]) %>%
  collect() %>%
  distinct()
save(all_spe, file = 'all_spe.rda')  
write.csv(all_spe, file = 'all_spe.csv', row.names = F, na = '')
copy_to(conn, all_spe, 'all_spe', overwrite = TRUE, temporary = FALSE, indexes = list('Internal_ID'))


examiners_gau <- tbl(conn, table_names$table_name[4]) %>%
  collect() %>%
  rename(Internal_ID_old = Internal_ID) %>%
  left_join(link) %>%
  distinct()
save(examiners_gau, file = 'examiners_gau.rda')  
write.csv(examiners_gau, file = 'examiners_gau.csv', row.names = F, na = '')
copy_to(conn, examiners_gau, 'examiners_gau', overwrite = TRUE, temporary = FALSE, indexes = list('Internal_ID'))


examiners_gs <- tbl(conn, table_names$table_name[5]) %>%
  collect() %>%
  rename(Internal_ID_old = Internal_ID) %>%
  left_join(link) %>%
  distinct()
save(examiners_gs, file = 'examiners_gs.rda')
write.csv(examiners_gs, file = 'examiners_gs.csv', row.names = F, na = '')
copy_to(conn, examiners_gs, 'examiners_gs', overwrite = TRUE, temporary = FALSE, indexes = list('Internal_ID'))


examiners_gs_old <- tbl(conn, table_names$table_name[6]) %>%
  collect() %>%
  rename(Internal_ID_old = Internal_ID) %>%
  left_join(link) %>%
  distinct()
# save(examiners_gs_old, file = 'examiners_gs_old.rda')  
# write.csv(examiners_gs_old, file = 'examiners_gs_old.csv', row.names = F, na = '')
copy_to(conn, examiners_gs_old, 'examiners_gs_old', overwrite = TRUE, temporary = FALSE, indexes = list('Internal_ID'))



#################################################################################











