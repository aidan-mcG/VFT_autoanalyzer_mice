converter <- function(){
   require(plyr)
   require(dplyr)
   require(ggplot2)
   require(readxl)
   require(tidyverse)
   require(broom)
      fname <- file.choose()
      data <- read_excel(fname)
      data_processing <- sapply(data, tolower)
      data_processing <- as.data.frame(data_processing)
      data_done <- data_processing %>%
         mutate_all(funs(recode(.,
             oooooo = "2.56",
             ooooox = "2.56",
             ooooxo = "2.23",
             ooooxx = "1.30", 
             oooxoo = "1.56",
             oooxox = "0.98",
             oooxxo = "0.65",
             oooxxx = "0.44",
             ooxooo = "1.16",
             ooxoox = "0.75",
             ooxoxo = "0.51",
             ooxoxx = "0.36",
             ooxxoo = "0.40",
             ooxxox = "0.29",
             ooxxxo = "0.20",
             ooxxxx = "0.14",
             oxoooo = "0.91",
             oxooox = "0.59",
             oxooxo = "0.40",
             oxooxx = "0.29",
             oxoxoo = "0.32",
             oxoxox = "0.23",
             oxoxxo = "0.17",
             oxoxxx = "0.12",
             oxxooo = "0.27",
             oxxoox = "0.19",
             oxxoxo = "0.13",
             oxxoxx = "0.10",
             oxxxoo = "0.11",
             oxxxox = "0.08",
             oxxxxo = "0.05",
             oxxxxx = "0.03",
             xxxxox = "0.02",
             xxxxoo = "0.02",
             xxxoxx = "0.02",
             xxxoxo = "0.03",
             xxxoox = "0.04",
             xxxooo = "0.06",
             xxoxxx = "0.02",
             xxoxxo = "0.03",
             xxoxox = "0.05",
             xxoxoo = "0.07",
             xxooxx = "0.06",
             xxooxo = "0.09",
             xxooox = "0.13",
             xxoooo = "0.18",
             xoxxxx = "0.03",
             xoxxxo = "0.04",
             xoxxox = "0.06",
             xoxxoo = "0.09",
             xoxoxx = "0.08",
             xoxoxo = "0.11",
             xoxoox = "0.15",
             xoxooo = "0.22",
             xooxxx = "0.10",
             xooxxo = "0.13",
             xooxox = "0.19",
             xooxoo = "0.27",
             xoooxx = "0.23",
             xoooxo = "0.33",
             xoooox = "0.47",
             xooooo = "0.69",
             xxxxxx = "0.02",
             xxxxxo = "0.02")
         ))
      path_user <- dirname(fname)
      newname_inter <- gsub(".xlsx", "", basename(fname))
      newname <- paste(newname_inter, "_processed", ".csv", sep = "")
      newfilepath <- file.path(path_user, newname)
      write.csv(data_done, newfilepath)
}
