---
title: "BCH_crosswalk_study"
author: "Sorcha Ashe & Arnav Lal"
date: "2025-09-25"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## BCH walkers

```{r echo=F,warning=F,error=F}
walkers <- read.csv("C:/Users/ashes/OneDrive/Desktop/School/Miscellaneous/BCH_crosswalk_study_walkers.csv")

#proportion of successful crossers per light cycle, sampled over 2 days
ggsave("C:/Users/ashes/OneDrive/Desktop/School/Miscellaneous/BCH_crossing_proportions.svg",device="svg",width=6,height=4)
walkers %>% pivot_longer(cols=c(8,9),names_to="crossers") %>% ggplot(aes(x=light_cycle,y=value,fill=factor(crossers,levels=c("prop_across","prop_diag"),labels=c("across","diag")))) + geom_col(position="dodge",alpha=0.5) + facet_grid(~date) + theme_minimal() + ylab("proportion successful crosses\n") + xlab("\nlight cycle") + guides(fill = guide_legend(title = "crossing \ndirection"))
dev.off()


#piechart of successful vs unsuccessful crosses
across_success_mean <- mean(walkers$prop_across)
diag_success_mean <- mean(walkers$prop_diag)
across_fail_mean <- 1 - across_success_mean
diag_fail_mean <- 1 - diag_success_mean

ggsave("C:/Users/ashes/OneDrive/Desktop/School/Miscellaneous/BCH_crossing_piecharts.svg",device="svg",width=6,height=4)
pie <- data.frame(status=c("success","fail"),across=c(across_success_mean,across_fail_mean),diag=c(diag_success_mean,diag_fail_mean))
pie %>% pivot_longer(cols=c(2,3),names_to="direction") %>% ggplot(aes(x="", y=value, fill=factor(status,levels=c("success","fail"),labels=c("successful","unsuccessful"))),alpha=0.5) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0) + facet_grid(~direction) + theme_void() + theme(strip.text = element_text(size = 12)) + guides(fill=guide_legend(title="outcome")) + scale_fill_manual(values=c("chartreuse3","indianred2"))
dev.off()
```

## BCH light cycles

```{r echo=FALSE,warning=F,error=F}
cycles <- read.csv("C:/Users/ashes/OneDrive/Desktop/School/Miscellaneous/BCH_crosswalk_study_cycles.csv")

#walking speed analysis, 3 crosswalks
ggsave("C:/Users/ashes/OneDrive/Desktop/School/Miscellaneous/BCH_walking_speeds.svg",device="svg",width=6,height=4)
cycles %>% pivot_longer(cols=c(2:4),names_to="sites") %>% filter(X=="st1_vel" | X=="st2_vel" | X=="diag_vel") %>% ggplot(aes(x=factor(sites,levels=c("DFCI","Galleria","BCH")),y=value,fill=value)) + geom_col() + facet_grid(~factor(X,levels=c("diag_vel","st1_vel","st2_vel"),labels=c("diagnoal","horizontal","vertical"))) + scale_fill_gradient(name="required\nwalking\nspeed (m/s)",low="green",high="red") + xlab("\ncrosswalk site") + ylab("required crossing speed (m/s)\n") + geom_hline(yintercept=1.3) + geom_hline(yintercept=0.9,linetype="dashed") + geom_hline(yintercept=0.6,linetype="dotted",linewidth=1.5)
dev.off()

#alternative presentation
ggsave("C:/Users/ashes/OneDrive/Desktop/School/Miscellaneous/BCH_walking_speeds.svg",device="svg",width=6,height=4)
cycles %>% pivot_longer(cols=c(2:4),names_to="sites") %>% filter(X=="st1_vel" | X=="st2_vel" | X=="diag_vel") %>% ggplot(aes(x=factor(sites,levels=c("DFCI","Galleria","BCH")),y=value,fill=sites)) + geom_col(alpha=0.6) + facet_grid(~factor(X,levels=c("diag_vel","st1_vel","st2_vel"),labels=c("diagonal","horizontal","vertical"))) + scale_fill_manual(name="intersection",values=c("navy","orange","maroon")) + xlab("\ncrosswalk site") + ylab("required crossing speed (m/s)\n") + geom_hline(yintercept=1.3) + geom_hline(yintercept=0.9,linetype="dashed") + geom_hline(yintercept=0.6,linetype="dotted",linewidth=1.5) + theme(strip.text = element_text(size = 12))
dev.off()



```
