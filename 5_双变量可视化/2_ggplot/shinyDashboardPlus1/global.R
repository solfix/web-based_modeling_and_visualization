library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(rlang)
#library(ggthemes)
library(ggmosaic)
library(showtext)
library(Cairo)

showtext_auto(enable=T)

# 加载R脚本
source("R/utils.R")
source("R/themes.R")   # 主题
source("R/palettes.R") # 调色版
source("R/vis.R")      # 可视化图像选项
source("R/vis1.R")     # 单变量可视化函数

# 加载内置数据集
m_datasets <- readRDS(file="../../../data/m_datasets.rds")

# 内置数据集名称
built_in_dataset_names <- names(m_datasets)

# 数据变换的类型
m_transforms <- c("对数变换"="log_transform", "标准化"="std_transform")



