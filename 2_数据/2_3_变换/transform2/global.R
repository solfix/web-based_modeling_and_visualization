library(shiny)
library(tidyverse)
library(rlang)

is_empty <- function (x, empty = "\\s*")
{
  length(x) == 0 || (length(x) == 1 && is.na(x)) ||
    (length(x) == 1 && grepl(paste0("^", empty, "$"), x))
}

# 加载内置数据集
m_datasets <- readRDS(file="../../../data/m_datasets.rds")

# 数据变换的类型
m_transforms <- c("对数变换"="log_transform")

