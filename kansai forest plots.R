rm(list = ls())
library(tidyverse)
library(RColorBrewer)
setwd("C:/Users/Regional_5/Desktop/For-Sorting/_EngD/Projects/TRB 2020/kansai")
var.names <- read.csv("Kansai Economic Census Variable Names.csv", stringsAsFactors = F, header = T)

## company
setwd("F:/TRB 2020/oem results/lasso-elast-mcp-scad/c")
b <- read.csv("oem-kansai-beta-c-2016.csv", header = T)
b <- b[-1,] ## remove intercept
var.names.b <- cbind(var.names[1:168,5:6],b)
b.gather <- 
  var.names.b%>%
  gather(3:6,key = "penalty", value = "coef.std")
forest1 <- ggplot(b.gather, aes(x = factor(detail_en, levels = rev(levels(factor(detail_en)))), y = coef.std))
plot <- forest1 + geom_point(aes(color = penalty), size = 3, position = "jitter") + 
  coord_flip(ylim = c(-0.5,0.5)) +
  scale_color_brewer(palette = "Set1") +
  xlab(NULL)
ggsave("treeplot-c-2016.png-jitter.png", plot, width = 40, height = 50, units = "cm", dpi = 300)
#plot

## workers
setwd("F:/TRB 2020/oem results/lasso-elast-mcp-scad/w")
b <- read.csv("oem-kansai-beta-w-2016.csv", header = T)
b <- b[-1,] ## remove intercept
var.names.b <- cbind(var.names[1:168,5:6],b)
b.gather <- 
  var.names.b%>%
  gather(3:6,key = "penalty", value = "coef.std")
forest1 <- ggplot(b.gather, aes(x = factor(detail_en, levels = rev(levels(factor(detail_en)))), y = coef.std))
plot <- forest1 + geom_point(aes(color = penalty), size = 3, position = "jitter") + 
  coord_flip(ylim = c(-0.5,0.5)) +
  scale_color_brewer(palette = "Set1") +
  xlab(NULL)
ggsave("treeplot-w-2016-jitter.png", plot, width = 40, height = 50, units = "cm", dpi = 300)
#plot

## company and workers
setwd("E:/TRB 2020/oem results/lasso-elast-mcp-scad/cw")
b <- read.csv("oem-kansai-beta-cw-2016.csv", header = T)
b <- b[-1,] ## remove intercept
var.names.b <- cbind(var.names[c(1:168,214:381),5:6],b)
var.names.b$category_en <- as.factor(var.names.b$category_en)
levels(var.names.b$category_en)
levels(var.names.b$category_en) <- c("workers", "company")
b.gather <- 
  var.names.b%>%
  gather(3:6,key = "penalty", value = "coef.std")
forest1 <- ggplot(b.gather, aes(x = factor(detail_en, levels = rev(levels(factor(detail_en)))), y = coef.std))
plot <- forest1 + geom_point(aes(color = penalty), size = 3, position = "jitter") + 
  coord_flip(ylim = c(-0.5,0.5)) +
  scale_color_brewer(palette = "Set1") +
  xlab(NULL) + facet_wrap(~category_en)
ggsave("treeplot-cw-2016-jitter.png", plot, width = 40, height = 50, units = "cm", dpi = 300)
plot
