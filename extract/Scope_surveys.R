setwd('G://My Drive/DHS New/')

library(foreign)
library(dplyr)

fs <- list.files(pattern='^..(WI|wi|GE|ge|BR|br).....(DTA|dta|SHP|shp)$')

df <- data.frame()

for (f in fs){
  num <- substr(f, 5, 5)
  cc <- toupper(substr(f, 1, 2))
  subversion <- ifelse(toupper(substr(f, 6, 6)) %in% as.character(seq(0, 9)), 1,
                          ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[1:8], 2, 
                                 ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[9:17], 3, 
                                        ifelse(toupper(substr(f, 6, 6)) %in% LETTERS[18:26], 4, "X"))))
  if (nrow(df[df$num==num & df$cc==cc & df$subversion==subversion, ]) > 0){
    df[df$num==num & df$cc==cc & df$subversion==subversion, toupper(substr(f, 3, 4))] <- f
  }else{
    temp <- data.frame(num=num, cc=cc, subversion=subversion, WI='', GE='', BR='', stringsAsFactors = F)
    df <- bind_rows(df, temp)
    df[df$num==num & df$cc==cc & df$subversion==subversion, toupper(substr(f, 3, 4))] <- f
  }
  print(f)
}

df

write.csv(df, 'C://Users/matt/mortality/scope/UseFiles.csv', row.names=F)
