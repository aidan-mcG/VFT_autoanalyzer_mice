library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(broom)
#data <- ##read in data here
data <- read_excel("fake_forcode/forcode.xlsx")
#define groups. do not use dashes (underscores are okay)
groups <- c("wt", "ko", "three")

updown <- function(input){
   vft_tq <- data$vft_t
   vft_tq <- tolower(vft_tq)
   vft_tq <- gsub("0", "o", vft_tq)
   vft_tq <- vft_tq %>%
      recode(oooooo = "2.56",
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
   vft_tq <- as.numeric(vft_tq)
   data_done <- cbind(data, vft_tq)
   data_done$group <- factor(data_done$group, levels = groups)
   assign("data_done", data_done, envir = .GlobalEnv)
}

makegraphs <- function(input){
      updown(input)
      height <- max(data_done$vft_tq) + .2
      groupnames <- c(unique(data_done$group))
      forbars <- tibble(aggregate(data_done[, 4], list(data_done$group), mean))
      forbars <- forbars %>%
         rename(group = Group.1) %>%
         mutate(g_index = seq_along(group))
      vf_t <- 
         ggplot(data = data_done, aes(x = group, y = vft_tq)) + 
            geom_bar(stat = "summary", fill = "white", size = 0.75, width = .9, aes(color = group)) +
            geom_dotplot(binaxis = "y", dotsize = 0.65, aes(fill = group)) +
            stat_summary(geom = "errorbar", fun.data = mean_se, width=.1, size = .9) +
            scale_fill_manual(values=c("deepskyblue", "brown1", "darkgreen", "darkgoldenrod1", "darkviolet")) +
            scale_color_manual(values=c("deepskyblue", "brown1", "darkgreen", "darkgoldenrod1", "darkviolet")) +
            guides(fill = FALSE, color = FALSE) +
            scale_y_continuous(limits = c(0, height), expand = c(0, 0)) +
            theme_minimal() +
            theme(
               axis.text = element_text(size = 8),
               plot.caption = element_text(size = 12, 
                                        face = "italic",
                                        color = "#606F7B",
                                        margin = margin(t = 15)))+
            labs(
               x = "Group",
               y = "Threshold (g)",
               title = "Von Frey Threshold"
         ) 
      assign("vf_t", vf_t, envir = .GlobalEnv)
 if(length(groupnames) >=3 && length(groupnames) < 6) {
         sweepme <- data_done %>% 
               do(Model = TukeyHSD(aov(vft_tq ~ group, data=.)))
         tukey <- as.data.frame(tidy(sweepme, Model)) %>%
            select(comparison, adj.p.value) %>%
            mutate(index = seq_along(adj.p.value))
         tukey$adj.p.value <- round(tukey$adj.p.value, digits = 3)
         tukey_signifs <- tukey %>%
            filter(adj.p.value <= 0.05) %>%
            separate(comparison, into = c("group", "g2"), sep = "-", remove = FALSE)
         subs <- forbars %>%
            select(group, g_index)
         tukey_signifs <- join(tukey_signifs, subs, by = "group") %>%
            rename(g_index1 = g_index)%>%
            rename(g1 = group) %>%
            rename(group = g2)
         tukey_signifs <- join(tukey_signifs, subs, by = "group") %>%
            rename(g2 = group) %>%
            rename(g_index2 = g_index)
         tukey_signifs$g1 <- as.factor(tukey_signifs$g1)
         tukey_signifs$g2 <- as.factor(tukey_signifs$g2)
         tukey_signifs <- tukey_signifs %>%
            mutate(master_index = seq_along(comparison))
         
         if(length(tukey_signifs$master_index >= 1)) {
            barheight <- max(forbars[,2])
            p <- max(tukey_signifs$master_index)
            for (i in 1:p) {
               vft_t_p <- tibble(
                  x = c(tukey_signifs$g_index2[i], tukey_signifs$g_index2[i], tukey_signifs$g_index1[i], tukey_signifs$g_index1[i]),
                  y = c(barheight+i*0.1, barheight + .2+i*0.1, barheight + .2+i*0.1, barheight+i*0.1)
               )
               vf_t <- vf_t +
                  geom_line(data = vft_t_p, aes(x = x, y = y, group = 1)) +
                  annotate("text", x = (tukey_signifs$g_index2[i] + tukey_signifs$g_index1[i])/2, y = barheight +i*0.1 + 0.25, 
                        label = "*",
                        size = 6, color = "#22292F"
                  )+
                  labs(
                     caption = "Tukey hsd post hoc. * denotes p <= 0.05"
                  )   
             
         }
         }   
   }else if(length(groupnames == 2)) {
      p <- t.test(vft_tq ~ group, data = data_done, paired = FALSE, equal.var = TRUE)$p.value
      if(p <= 0.05){
         barheight <- max(forbars[,2])
         vft_t_p <- tibble(
            x = c(1, 1, 2, 2),
            y = c(barheight, barheight + .2, barheight + .2, barheight)
         )
         vf_t <- vf_t +
            geom_line(data = vft_t_p, aes(x = x, y = y, group = 1)) +
            annotate("text", x = 1.5, y = barheight + 0.5, 
                  label = "*",
                  size = 6, color = "#22292F"
                  )+
            labs(
               caption = "Unpaired t test. * denotes p <= 0.05"
            )   
               assign("vf_t", vf_t, envir = .GlobalEnv)  
            }      
   
   }else {
      message("Error: more than 5 groups")
   }
assign("graph", vf_t, envir = .GlobalEnv)
}



