library(data.table)


salary <- list.files("E:/Projects/NFL Projections DB/data/salaries", full.names = T,
                     pattern = "2020.*ms|2020.*turkey")

milly <- list.files("E:/Projects/Mass Extract Zips/data/extracted csvs/", 
                    pattern = "NFL.*202.*fant.*mill", full.names = T)
nick <- list.files("E:/Projects/Mass Extract Zips/data/extracted csvs/", 
                   pattern = "NFL.*202.*nick", full.names = T)

## move to github folder
for(f in c(nick,milly)){
  
  file.copy(f, to = "F:/R Development/tidyDK/data/contests")
  
}

## now prep to change file names
dates <- sort(unique(gsub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", c(nick,milly))))

wks <- c('w02', 'w03', 'w04', 'w05', 'w06', 'w07', 'w08', 'w09','w10', 'w11', 
         'turkeyday', 'w12', 'w14', 'w15', 'w16', 'w17') 

date_wk_sub <- data.table(sch_date = as.Date(dates), wk_str = wks)


## get all file names in directory, start changing
tidyDK_files <- list.files("F:/R Development/tidyDK/data/contests", full.names = T)


## easy part
for(i in 1:length(tidyDK_files)){
  
  f <- tidyDK_files[i]
  
  date <- as.Date(gsub("(.*)(\\d{4}-\\d{2}-\\d{2})(.*)", "\\2", f))
  
  str_sub <- date_wk_sub[sch_date == date, wk_str]
  
  fn <- paste0(gsub("(.*)(\\d{4}-\\d{2}-\\d{2})(.*)", "\\1", f), 
               str_sub,
               gsub("(.*)(\\d{4}-\\d{2}-\\d{2})(.*)", "\\3", f))
  
  fn <- gsub("NFL", "NFL2020", fn)
  
  file.rename(f, fn)
  
}

## salary renaming

for( f in salary){
  
  
  file.copy(f, "F:/R Development/tidyDK/data/salaries/")
  
}

## do some renaming
tidyDK_sals <- list.files("F:/R Development/tidyDK/data/salaries/", full.names = T)
sals_renamed <- tidyDK_sals

## ez apply
sals_renamed <- gsub('2020', 'NFL2020', sals_renamed)
sals_renamed <- gsub('Week_12_thurs_', '', sals_renamed)
sals_renamed <- gsub('(.*)_(\\d{1})_(.*)', '\\1_0\\2_\\3', sals_renamed)

## now rename

for(i in 1:length(tidyDK_sals)){
  
  file.rename(tidyDK_sals[i], sals_renamed[i])
  
}
