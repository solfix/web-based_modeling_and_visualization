# 定义用户界面

# (1) 标题
header <- dashboardHeader(title = span(icon("horse-head"), "可视化"), titleWidth = 300)

# (2) 侧栏
sidebar <- dashboardSidebar(

  collapsed = TRUE,

  sidebarMenu(id = "sidebar",
              menuItem("数据", tabName = "Data", icon = icon("table")),
              menuItem("可视化", tabName = "Vis", selected = TRUE, icon = icon("chart-bar"))
  )
)

# 主体
body <- dashboardBody(

  shinyFeedback::useShinyFeedback(),

  tabItems(

    tabItem(tabName = "Data",
            fluidPage(
              DT::dataTableOutput("table")
            )
    ),

    tabItem(tabName = "Vis",
            fluidPage(
              verbatimTextOutput("message"),
              tabsetPanel(id = "vis_tab",
                          tabPanel(title="单变量可视化", value="Vis1", plotOutput("ggplot_vis1")),
                          tabPanel(title="双变量可视化", value="Vis2", plotOutput("ggplot_vis2")),
              ),

              checkboxInput("options_vis", "绘图选项", value=FALSE),
              conditionalPanel("input.options_vis==true",
                               column(width=4,
                                      textInput("vis_title", label="标题"),
                                      textInput("vis_subtitle", label="副标题"),
                                      textInput("vis_xlab", label="x-轴标签"),
                                      textInput("vis_ylab", label="y-轴标签"),
                                      actionButton(inputId="vis_params_ok", label="提交", class = "btn-success", icon = icon("sync")),
                                      actionButton(inputId="vis_params_clear", label="清除", class = "btn-warning", icon = icon("redo-alt")),
                               ),
                               column(width=4,
                                      checkboxInput("show_char1", "符号选项（组1）", value=FALSE),
                                      conditionalPanel("input.show_char1==true",
                                                       numericInput("vis_char_shape", label="符号形状", min=0, max=25, value=16, step=1),
                                                       numericInput("vis_char_size",  label="符号大小", min=0, max=30, value=2),
                                                       selectInput("vis_char_color", label="符号颜色", choices=colors(), selected="black"),
                                                       selectInput("vis_char_fill" , label="符号填充色", choices=colors(), selected="gray50"),
                                                       numericInput("vis_char_alpha", label="符号透明度", min=0, max=1, value=.5, step=.1)
                                      ),

                                      checkboxInput("show_line1", "线条选项（组1）", value=FALSE),
                                      conditionalPanel("input.show_line1==true",
                                                       numericInput("vis_line_type", label="线条形状", min=0, max=6, value=1, step=1),
                                                       numericInput("vis_line_size", label="线条宽度", min=0, max=20, value=1, step=1),
                                                       selectInput("vis_line_color", label="线条颜色", choices=colors(), selected="black"),
                                                       numericInput("vis_line_alpha", label="线条透明度", min=0, max=1, value=.5, step=.1)
                                      ),

                                      checkboxInput("show_area1", "区域选项（组1）", value=FALSE),
                                      conditionalPanel("input.show_area1==true",
                                                       numericInput("vis_area_linetype", label="区域边界线形状", min=0, max=6, value=1, step=1),
                                                       numericInput("vis_area_linesize", label="区域边界线宽度", min=0, max=20, value=0.5, step=0.5),
                                                       selectInput("vis_area_linecolor", label="区域边界线颜色", choices=colors(), selected="black"),
                                                       selectInput("vis_area_fill", label="区域填充色", choices=colors(), selected="lightgray"),
                                                       numericInput("vis_area_alpha", label="区域透明度", min=0, max=1, value=.5, step=.1)
                                      ),
                                      checkboxInput("show_char2", "符号选项（组2）", value=FALSE),
                                      conditionalPanel("input.show_char2==true",
                                                       numericInput("vis_char2_shape", label="符号形状", min=0, max=25, value=16, step=1),
                                                       numericInput("vis_char2_size",  label="符号大小", min=0, max=30, value=2),
                                                       selectInput("vis_char2_color", label="符号颜色", choices=colors(), selected="black"),
                                                       selectInput("vis_char2_fill" , label="符号填充色", choices=colors(), selected="gray50"),
                                                       numericInput("vis_char2_alpha", label="符号透明度", min=0, max=1, value=.5, step=.1)
                                      ),

                                      checkboxInput("show_line2", "线条选项（组2）", value=FALSE),
                                      conditionalPanel("input.show_line2==true",
                                                       numericInput("vis_line2_type", label="线条形状", min=0, max=6, value=1, step=1),
                                                       numericInput("vis_line2_size", label="线条宽度", min=0, max=20, value=1, step=1),
                                                       selectInput("vis_line2_color", label="线条颜色", choices=colors(), selected="black"),
                                                       numericInput("vis_line2_alpha", label="线条透明度", min=0, max=1, value=.5, step=.1)
                                      ),

                                      checkboxInput("show_area2", "区域选项（组2）", value=FALSE),
                                      conditionalPanel("input.show_area2==true",
                                                       numericInput("vis_area2_linetype", label="区域边界线形状", min=0, max=6, value=1, step=1),
                                                       numericInput("vis_area2_linesize", label="区域边界线宽度", min=0, max=20, value=0.5, step=0.5),
                                                       selectInput("vis_area2_linecolor", label="区域边界线颜色", choices=colors(), selected="black"),
                                                       selectInput("vis_area2_fill", label="区域填充色", choices=colors(), selected="lightgray"),
                                                       numericInput("vis_area2_alpha", label="区域透明度", min=0, max=1, value=.5, step=.1)
                                      )

                               ),
                               column(width=4,
                                      conditionalPanel("input.vis1_var_type == 'numeric' & input.vis1_numeric_plot_type == 'dotplot_t'",
                                                       numericInput("vis1_binwidth_r", label="箱宽倒数", min=20, max=100, value=30, step=5),
                                                       numericInput("vis1_dotsize", label="相对于箱宽的直径", min=.25, max=5, value=1, step=.25),
                                                       numericInput("vis1_stackratio", label="堆叠点的距离", min=.2, max=2, value=1, step=.1)
                                      ),

                                      conditionalPanel("input.vis1_var_type == 'numeric' & input.vis1_numeric_plot_type == 'histogram_t'",
                                                       numericInput("vis1_bins", label="分箱数", min=1, max=30, value=6, step=1),
                                                       checkboxInput("vis1_freq", "频率密度", value=FALSE),
                                                       conditionalPanel("input.vis1_freq == true",
                                                                        checkboxInput("vis1_add_density", "添加核密度曲线", value=FALSE))
                                      ),
                                      conditionalPanel("input.vis1_var_type == 'numeric' & (input.vis1_numeric_plot_type == 'histogram_t' | input.vis1_numeric_plot_type == 'density_t')",
                                                       checkboxInput("vis1_add_rug", "地毯", value=FALSE),
                                      ),
                                      conditionalPanel("(input.vis1_var_type == 'numeric' & (input.vis1_numeric_plot_type == 'boxplot_t' | input.vis1_numeric_plot_type == 'violin_t')) |
                                                        (input.vis1_var_type == 'factor' & input.vis1_factor_plot_type == 'barplot_t')",
                                                       numericInput("vis1_width", label="箱宽", min=0, max=1, value=.7, step=.1),
                                                       checkboxInput("vis1_horizontal", "水平方向", value=FALSE),
                                      ),

                                      conditionalPanel("input.vis1_var_type == 'numeric' & (input.vis1_numeric_plot_type == 'boxplot_t' | input.vis1_numeric_plot_type == 'violin_t')",
                                                       checkboxInput("vis1_add_jitter", "添加抖动散点", value=FALSE),
                                                       conditionalPanel("input.vis1_add_jitter == true",
                                                                        numericInput("vis1_jitter_width", label="抖动散点范围", min=.05, max=.5, value=.3, step=.05)
                                                       )
                                      ),

                                      conditionalPanel("input.vis1_var_type == 'numeric' & input.vis1_numeric_plot_type == 'boxplot_t'",
                                                       checkboxInput("vis1_notch", "缺口", value=FALSE),
                                      ),

                                      conditionalPanel("input.vis1_var_type == 'numeric' & input.vis1_numeric_plot_type == 'violin_t'",
                                                       checkboxInput("vis1_add_boxplot", "添加箱线图", value=FALSE),
                                      ),

                                      conditionalPanel("input.vis1_var_type == 'factor' & input.vis1_factor_plot_type == 'pie_t'",
                                                       numericInput("vis1_start", label="初始角度", min=0, max=360, value=0, step=10),
                                                       checkboxInput("vis1_clockwise", "顺时针", value=FALSE),
                                                       #checkboxInput("vis1_add_theme", "添加主题", value=FALSE),
                                      ),
                               )
              )
            )

    )
  )
)

controlbar <- dashboardControlbar(
  overlay = FALSE,
  div(
    style="text-align:left; padding: 15px",
    selectInput("dataset", "选择数据集：", names(m_datasets), selected="SurgicalUnit"),

    conditionalPanel("input.sidebar == 'Data'",
                     # (2-1-a) 数据筛选
                     checkboxInput("show_filter", "筛选数据", value=FALSE),
                     conditionalPanel("input.show_filter == true",
                                      textAreaInput("data_filter", NULL,
                                                    placeholder="输入筛选条件（例如，Age>50）并按返回键",
                                                    resize="vertical", rows=3),
                                      actionButton(inputId="data_filter_ok", label="完成"),
                                      actionButton(inputId="data_filter_clear", label="清除")
                     ),

                     # (2-1-b) 变量选择
                     checkboxInput("show_select", "选择变量", value=FALSE),
                     conditionalPanel("input.show_select == true",
                                      selectInput("columns", NULL, multiple=TRUE,
                                                  choices=names(m_datasets[["SurgicalUnit"]]),
                                                  selected=names(m_datasets[["SurgicalUnit"]]),
                                                  selectize=F)
                     ),

                     # (2-1-c) 数据变换
                     checkboxInput("show_transform", "数据变换", value=FALSE),
                     conditionalPanel("input.show_transform == true",
                                      selectInput("transform", "选择变换类型：", choices=m_transforms),
                                      selectInput("transform_vars", "选择作变换的变量：", multiple=TRUE, choices=NULL)
                     ),

                     # (2-1-d) 数据保存和删除
                     checkboxInput("show_store", "保存 / 删除数据", value=FALSE),
                     conditionalPanel("input.show_store == true",
                                      textInput("dataset_name", "输入数据集名称:", "NewData"),
                                      actionButton("add_dataset", "保存", class = "btn-success"),
                                      hr(),
                                      selectInput("del_dataset_names", "选择删除的变量：",
                                                  multiple=TRUE, choices=NULL, selected=NULL),
                                      actionButton("del_dataset", "删除", class = "btn-danger")
                     ),

                     # (2-1-e) 数据上传和下载
                     checkboxInput("show_upload", "上传 / 下载数据", value=FALSE),
                     conditionalPanel("input.show_upload == true",
                                      div(
                                        style="text-align:left; padding: 5px",
                                        textInput("upload_dataset_name", "输入上传数据集名称:", "UserData"),
                                        helpText('上传文件(.csv、.tsv、xls、xlsx, sav)：'),
                                        fileInput("upload", NULL, accept = c(".csv", ".tsv"),
                                                  buttonLabel = list(icon = icon("upload"), "上传..."),
                                                  multiple=FALSE, placeholder = "未选择任何文件"),
                                        actionButton("import_dataset", "导入", class = "btn-success")),
                                      #hr(),
                                      div(
                                        style="text-align:left; padding: 5px",
                                        downloadButton("download_dataset", "下载数据集",
                                                       style = "color: #fff; background-color: #00c0ef; border-color: #00c0ef;"))
                     ),
    ),

    conditionalPanel("input.sidebar == 'Vis'",
                     selectInput("vis_theme", label="主题", choices=ggplot2_themes, selected="theme_gray"),
                     numericInput("vis_font_size", label="字体大小", min=1, max=40, value=16),
                     checkboxInput("show_palette", "调色板", value=FALSE),
                     conditionalPanel("input.show_palette == true",
                                      selectInput("vis_palette_family_d", label="调色板族(离散)", choices=palette_family_d, selected="base"),
                                      selectInput("vis_palette_d", label="调色板(离散)", choices=c("none", base_palettes), selected="none"),
                                      checkboxInput("vis_palette_rev_d", "反序(离散)", value=FALSE),
                                      selectInput("vis_palette_family_c", label="调色板族(连续)", choices=palette_family_d, selected="base"),
                                      selectInput("vis_palette_c", label="调色板(连续)", choices=c("none", base_palettes), selected="none"),
                                      checkboxInput("vis_palette_rev_c", "反序(连续)", value=FALSE)
                     ),

                     hr()
    ),

    conditionalPanel("input.vis_tab == 'Vis1'",
                     radioButtons("vis1_var_type", NULL,
                                  choices=c("数值"="numeric", "因子"="factor"),
                                  selected="numeric"),
                     selectInput("vis1_var", "选择变量：", multiple=FALSE,
                                 choices=names(m_datasets[["SurgicalUnit"]])),
                     conditionalPanel("input.vis1_var_type == 'numeric'",
                                      selectInput("vis1_numeric_plot_type",
                                                  "选择绘图类型：",
                                                  choices=numeric_vis1_types,
                                                  selected="histogram_t")),
                     conditionalPanel("input.vis1_var_type == 'factor'",
                                      selectInput("vis1_factor_plot_type",
                                                  "选择绘图类型：",
                                                  choices=factor_vis1_types,
                                                  selected="barplot_t")),
    ),

    conditionalPanel("input.vis_tab == 'Vis2'",
                     selectInput("vis2_var1", "选择 x 变量：", choices=NULL),
                     selectInput("vis2_var2", "选择 y 变量：", choices=NULL),
                     selectInput("vis2_plot_type", "选择绘图类型：", choices=NULL)
                     #uiOutput("vis2_type")
    ),

    conditionalPanel("input.sidebar == 'Vis'",
                     downloadButton(outputId="vis_download", label="下载图片", style = "color: #fff; background-color: #00c0ef; border-color: #00c0ef;"),
                     checkboxInput("show_fig_options", "下载图片选项", value=FALSE),
                     conditionalPanel("input.show_fig_options == true",
                                      selectInput("vis_fig_type", label="图片类型", choices=c("png", "pdf", "svg"), selected="png"),
                                      numericInput("vis_fig_wd", label="图片宽度", min=1, max=30, value=5),
                                      numericInput("vis_fig_ht", label="图片高度", min=1, max=30, value=4),
                     )
    ),
  )
)

footer = dashboardFooter(
  left = "Developed By Capas",
  right = "Copyright © 2021-2022, All Rights Reserved."
)

# 将标题、侧栏和主体组合在一起
dashboardPage(
  header,
  sidebar,
  body,
  controlbar = controlbar,
  footer = footer)
