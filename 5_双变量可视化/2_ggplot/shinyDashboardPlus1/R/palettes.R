library(RColorBrewer)
library(viridis)
library(wesanderson)
library(ggsci)
library(ggthemes)

# 参考：
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/

palette_family_d <- c(
  "base", "RColorBrewer", "viridis", "wesanderson", "ggsci"
)

palette_family_c <- c(
  "base", "viridis", "wesanderson", "ggsci"
)

##########
# R base #
##########

# rainbow(n), heat.colors(n), terrain.colors(n), topo.colors(n), and cm.colors(n)
base_palettes <- c("rainbow", "heat", "terrain", "topo", "cm", "grey")

#
# 用法：
#   scale_fill_manual(values=terrain.colors(3))
#   scale_color_gradientn(colors=terrain.colors(100))

# 灰阶：
# scale_fill_grey(start = 0.8, end = 0.2)
# scale_color_grey(start = 0.8, end = 0.2)

#########################
# RColorBrewer 调色方案 #
#########################

# 调色板分类：
#   (1) 单色系：YlOrRd, YlOrBr, YlGnBu, YlGn, Reds, RdPu, Purples, PuRd, PuBuGn, PuBu, OrRd, Oranges, Greys, Greens, GnBu, BuPu, BuGn, Blues
#   (2) 双色渐变系：Spectral, RdYlGn, RdYlBu, RdGy, RdBu, PuOr, PRGn, PiYG, BrBG
#   (3) 多色系：Set3, Set2, Set1, Pastel2, Pastel1, Paired, Dark2, Accent
brewer_palettes_seq <- brewer.pal.info %>% filter(category=="seq") %>% rownames(.) %>% rev()
brewer_palettes_div <- brewer.pal.info %>% filter(category=="div") %>% rownames(.) %>% rev()
brewer_palettes_qual <- brewer.pal.info %>% filter(category=="qual") %>% rownames(.) %>% rev()

brewer_palettes <- c(brewer_palettes_seq, brewer_palettes_div, brewer_palettes_qual)

# 调用函数：
#   scale_color_brewer()
#   scale_fill_brewer()
#
# 例如：scale_color_brewer(palette="YlOrRd")
#
# 注：RColorBrewer调色板只能用于离散值

####################
# viridis 调色方案 #
####################

viridis_palettes <- c(
  "magma"   = "A",
  "inferno" = "B",
  "plasma"  = "C",
  "viridis" = "D",
  "cividis" = "E",
  "rocket"  = "F",
  "mako"    = "G",
  "turbo"   = "H")

# 调用函数：
#   scale_color_viridis()
#   scale_fill_viridis()
#
# 例如：scale_fill_viridis(direction=1, discrete = FALSE, option="magma")
#
# 注：viridis 调色板既能用于离散值，也能用于连续值，通过参数discrete控制。

########################
# wesanderson 调色方案 #
########################

# 19种调色板，每种调色板5种颜色：
#    BottleRocket1       7
#    BottleRocket2       5
#    Rushmore1           5
#    Rushmore            5
#    Royal1              4
#    Royal2              5
#    Zissou1             5
#    Darjeeling1         5
#    Darjeeling2         5
#    Chevalier1          4
#    FantasticFox1       5
#    Moonrise1           4
#    Moonrise2           4
#    Moonrise3           5
#    Cavalcanti1         5
#    GrandBudapest1      4
#    GrandBudapest2      4
#    IsleofDogs1         6
#    IsleofDogs2         5

wesanderson_palettes <- names(wes_palettes)

# 使用：
#
# 离散值：scale_fill_manual(scale_fill_manual(values = wes_palette("GrandBudapest1", n=3)))
# 连续值可以通过对离散值进行差值获得：
#    pal <- wes_palette("Zissou1", 100, type = "continuous")
#    p + scale_fill_gradientn(colors = pal)

##################
# ggsci 调色方案 #
##################

ggsci_palettes_discrete <- c(
  "aaas",         # AAAS Journal Color Scales
  "d3",           # D3.js Color Scales
  "futurama",     # The Futurama Color Scales
  "igv",          # Integrative Genomics Viewer (IGV) Color Scales
  "jama",         # Journal of the American Medical Association Color Scales
  "jco",          # Journal of Clinical Oncology Color Scales
  "lancet",       # Lancet Journal Color Scales
  "locuszoom",    # LocusZoom Color Scales
  "nejm",         # NEJM Color Scales
  "npg",          # NPG Journal Color Scales
  "rickandmorty", # Rick and Morty Color Scales
  "simpsons",     # The Simpsons Color Scales
  "startrek",     # Star Trek Color Scales
  "tron",         # Tron Legacy Color Scales
  "uchicago",     # The University of Chicago Color Scales
  "ucscgb"        # UCSC Genome Browser Color Scales
)

ggsci_palettes_continuous <- c(
  "gsea" = "gesa", # The GSEA GenePattern Color Scales
  "material-red" = "red",  # Material Design Color Palettes
  "material-pink" = "pink",
  "material-purple" = "purple",
  "material-deep-purple" = "deep-purple",
  "material-indigo" = "indigo",
  "material-blue" = "blue",
  "material-light-blue" = "light-blue",
  "material-cyan" = "cyan",
  "material-teal" = "teal",
  "material-green" = "green",
  "material-light-green" = "light-green",
  "material-lime" = "lime",
  "material-yellow" = "yellow",
  "material-amber" = "amber",
  "material-orange" = "orange",
  "material-deep-orange" = "deep-orange",
  "material-brown" = "brown",
  "material-grey" = "grey",
  "material-blue-grey" = "blue-grey"
)

# 使用：
#    scale_color_aaas()
#    scale_fill_aaas()

get_rev_vector <- function(v, reverse=FALSE) {
  if (reverse) {
    rev(v)
  } else {
    v
  }
}

add_fill_discrete <- function(p, palatte_class, palatte_name, n, reverse=FALSE) {

  pal_d <- if(reverse) -1 else 1

  switch (palatte_class,
          base = p <- switch (palatte_name,
                              rainbow = p + scale_fill_manual(values=get_rev_vector(rainbow(n),        reverse)),
                              heat    = p + scale_fill_manual(values=get_rev_vector(heat.colors(n),    reverse)),
                              terrain = p + scale_fill_manual(values=get_rev_vector(terrain.colors(n), reverse)),
                              topo    = p + scale_fill_manual(values=get_rev_vector(topo.colors(n),    reverse)),
                              cm      = p + scale_fill_manual(values=get_rev_vector(cm.colors(n),      reverse)),
                              grey    = p + scale_fill_grey(start=ifelse(reverse, 0.8, 0.2), end=ifelse(reverse, 0.2, 0.8))
          ),

          RColorBrewer = p <- p + scale_fill_brewer(palette=palatte_name, direction=pal_d),

          viridis = p <- p + scale_fill_viridis(direction=pal_d,
                                                discrete=TRUE,
                                                option=palatte_name),

          wesanderson = p <- p + scale_fill_manual(values=get_rev_vector(wes_palette(palatte_name, n, type="continuous"), reverse)),

          # 不能反序
          ggsci = p <- switch (palatte_name,
                               aaas         = p + scale_fill_aaas(),
                               d3           = p + scale_fill_d3(),
                               futurama     = p + scale_fill_futurama(),
                               igv          = p + scale_fill_igv(),
                               jama         = p + scale_fill_jama(),
                               jco          = p + scale_fill_jco(),
                               lancet       = p + scale_fill_lancet(),
                               locuszoom    = p + scale_fill_locuszoom(),
                               nejm         = p + scale_fill_nejm(),
                               npg          = p + scale_fill_npg(),
                               rickandmorty = p + scale_fill_rickandmorty(),
                               simpsons     = p + scale_fill_simpsons(),
                               startrek     = p + scale_fill_startrek(),
                               tron         = p + scale_fill_tron(),
                               uchicago     = p + scale_fill_uchicago(),
                               ucscgb       = p + scale_fill_ucscgb()
          ),
  )

  p
}

add_color_discrete <- function(p, palatte_class, palatte_name, n, reverse=FALSE) {

  pal_d <- if(reverse) -1 else 1

  switch (palatte_class,
          base = p <- switch (palatte_name,
                              rainbow = p + scale_color_manual(values=get_rev_vector(rainbow(n),        reverse)),
                              heat    = p + scale_color_manual(values=get_rev_vector(heat.colors(n),    reverse)),
                              terrain = p + scale_color_manual(values=get_rev_vector(terrain.colors(n), reverse)),
                              topo    = p + scale_color_manual(values=get_rev_vector(topo.colors(n),    reverse)),
                              cm      = p + scale_color_manual(values=get_rev_vector(cm.colors(n),      reverse)),
                              grey    = p + scale_color_grey(start=ifelse(reverse, 0.8, 0.2),
                                                             end=ifelse(reverse, 0.2, 0.8))
          ),

          RColorBrewer = p <- p + scale_color_brewer(palette=palatte_name, direction=pal_d),

          viridis = p <- p + scale_color_viridis(direction=pal_d,
                                                 discrete=TRUE,
                                                 option=palatte_name),

          wesanderson = p <- p + scale_color_manual(values=get_rev_vector(wes_palette(palatte_name, n, type="continuous"), reverse)),

          # 不能反序
          ggsci = p <- switch (palatte_name,
                               aaas         = p + scale_color_aaas(),
                               d3           = p + scale_color_d3(),
                               futurama     = p + scale_color_futurama(),
                               igv          = p + scale_color_igv(),
                               jama         = p + scale_color_jama(),
                               jco          = p + scale_color_jco(),
                               lancet       = p + scale_color_lancet(),
                               locuszoom    = p + scale_color_locuszoom(),
                               nejm         = p + scale_color_nejm(),
                               npg          = p + scale_color_npg(),
                               rickandmorty = p + scale_color_rickandmorty(),
                               simpsons     = p + scale_color_simpsons(),
                               startrek     = p + scale_color_startrek(),
                               tron         = p + scale_color_tron(),
                               uchicago     = p + scale_color_uchicago(),
                               ucscgb       = p + scale_color_ucscgb()
          ),
  )

  p
}

add_fill_continuous <- function(p, palatte_class, palatte_name, n=100, reverse=FALSE) {

  pal_d <- if(reverse) -1 else 1

  switch (palatte_class,
          base = p <- switch (palatte_name,
                              rainbow = p + scale_fill_gradientn(colors=get_rev_vector(rainbow(n),        reverse)),
                              heat    = p + scale_fill_gradientn(colors=get_rev_vector(heat.colors(n),    reverse)),
                              terrain = p + scale_fill_gradientn(colors=get_rev_vector(terrain.colors(n), reverse)),
                              topo    = p + scale_fill_gradientn(colors=get_rev_vector(topo.colors(n),    reverse)),
                              cm      = p + scale_fill_gradientn(colors=get_rev_vector(cm.colors(n),      reverse)),
                              grey    = p + scale_fill_gradient(low=ifelse(reverse, "gray80", "gray20"),
                                                                high=ifelse(reverse, "gray20", "gray80"))
          ),

          viridis = p <- p + scale_fill_viridis(direction=pal_d,
                                                discrete=FALSE,
                                                option=palatte_name),

          wesanderson = p <- p + scale_fill_gradientn(colors=get_rev_vector(wes_palette(palatte_name, n, type="continuous"), reverse)),

          ggsci = p <- switch (palatte_name,
                               gsea = p + scale_fill_gsea(reverse=reverse),
                               p + scale_fill_material(palette=palatte_name, reverse=reverse)
          ),
  )

  p
}

add_color_continuous <- function(p, palatte_class, palatte_name, n=100, reverse=FALSE) {

  pal_d <- if(reverse) -1 else 1

  switch (palatte_class,
          base = p <- switch (palatte_name,
                              rainbow = p + scale_color_gradientn(colors=get_rev_vector(rainbow(n),        reverse)),
                              heat    = p + scale_color_gradientn(colors=get_rev_vector(heat.colors(n),    reverse)),
                              terrain = p + scale_color_gradientn(colors=get_rev_vector(terrain.colors(n), reverse)),
                              topo    = p + scale_color_gradientn(colors=get_rev_vector(topo.colors(n),    reverse)),
                              cm      = p + scale_color_gradientn(colors=get_rev_vector(cm.colors(n),      reverse)),
                              grey    = p + scale_color_gradient(low=ifelse(reverse, "gray80", "gray20"),
                                                                 high=ifelse(reverse, "gray20", "gray80"))
          ),

          viridis = p <- p + scale_color_viridis(direction=pal_d,
                                                 discrete=FALSE,
                                                 option=palatte_name),

          wesanderson = p <- p + scale_color_gradientn(colors=get_rev_vector(wes_palette(palatte_name, n, type="continuous"), reverse)),

          ggsci = p <- switch (palatte_name,
                               gsea = p + scale_color_gsea(reverse=reverse),
                               p + scale_color_material(palette=palatte_name, reverse=reverse)
          ),
  )

  p
}
