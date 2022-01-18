# 定义用户界面

# (1) 标题
header <- dashboardHeader(title = span(icon("paw"), "shinydashboardPlus布局"), titleWidth = 300)

# (2) 侧栏
sidebar <- dashboardSidebar(

  collapsed = TRUE,

  sidebarMenu(id = "sidebar",
              menuItem("数据", tabName = "Data", icon = icon("table")),
              menuItem("单变量可视化", tabName = "Vis1", selected = TRUE, icon = icon("chart-bar"))
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
    tabItem(tabName = "Vis1",
            fluidPage(
              #verbatimTextOutput("message"),
              plotlyOutput("plotly_vis1"),
              hr(),
              conditionalPanel("input.vis1_var_type != 'factor' | input.vis1_factor_plot_type != 'pie_t'",
                               fluidRow(
                                 column(width=4, selectInput("vis1_theme", label="主题", choices=ggplot2_themes, selected="theme_gray")),
                                 column(width=4, numericInput("vis1_fontsize", label="字体大小", min=1, max=40, value=16)),
                               ),

                               checkboxInput("options_vis1", "绘图选项", value=FALSE),
                               conditionalPanel("input.options_vis1==true",
                                                column(width=4,
                                                       textInput("vis1_title", label="标题"),
                                                       textInput("vis1_subtitle", label="副标题"),
                                                       conditionalPanel("input.vis1_var_type != 'factor' | input.vis1_factor_plot_type != 'pie_t'",
                                                                        textInput("vis1_xlab", label="x-轴标签"),
                                                                        textInput("vis1_ylab", label="y-轴标签"),
                                                       )
                                                ),

                                                column(width=4,

                                                       conditionalPanel("input.vis1_var_type == 'numeric' & input.vis1_num_plot_type == 'scatter_t'",
                                                                        numericInput("vis1_shape", label="符号形状", min=0, max=25, value=1, step=1),
                                                                        numericInput("vis1_size",  label="符号大小", min=0, max=30, value=2)
                                                       ),

                                                       conditionalPanel("input.vis1_var_type != 'factor' | input.vis1_factor_plot_type != 'pie_t'",
                                                                        selectInput("vis1_color", label="颜色", choices=colors(), selected="black"),
                                                                        selectInput("vis1_fill" , label="填充颜色", choices=colors(), selected="lightgray")
                                                       ),
                                                       numericInput("vis1_alpha", label="透明度", min=0, max=1, value=1, step=.1),

                                                       conditionalPanel("input.vis1_var_type == 'numeric' & input.vis1_num_plot_type == 'histogram_t'",
                                                                        numericInput("vis1_bins", label="分箱数", min=1, max=30, value=5, step=1),
                                                                        checkboxInput("vis1_freq", "频率密度", value=FALSE),
                                                                        conditionalPanel("input.vis1_freq == true",
                                                                                         checkboxInput("vis1_density", "添加核密度曲线", value=FALSE))
                                                       ),

                                                       conditionalPanel("input.vis1_var_type == 'numeric' & input.vis1_num_plot_type == 'boxplot_t'",
                                                                        #     checkboxInput("vis1_notch", "缺口", value=FALSE),
                                                                        checkboxInput("vis1_horizontal", "水平方向", value=TRUE)
                                                       ),

                                                       conditionalPanel("input.vis1_var_type == 'factor' & input.vis1_factor_plot_type == 'barplot_t'",
                                                                        numericInput("vis1_width", label="箱宽", min=0, max=1, value=.7, step=.1),
                                                                        checkboxInput("vis1_horiz", "水平方向", value=FALSE)
                                                       ),

                                                       fluidRow(
                                                         actionButton(inputId="vis1_params_ok", label="提交", class = "btn-success", icon = icon("sync")),
                                                         actionButton(inputId="vis1_params_clear", label="清除", class = "btn-warning", icon = icon("redo-alt")),
                                                       ),
                                                ),



                                                column(width=4,
                                                       selectInput("vis1_fig_type", label="图片类型", choices=c("png", "pdf", "svg"), selected="png"),
                                                       numericInput("vis1_fig_wd", label="图片宽度", min=1, max=30, value=5),
                                                       numericInput("vis1_fig_ht", label="图片高度", min=1, max=30, value=4),
                                                       downloadButton(outputId="vis1_download", label="下载图片", style = "color: #fff; background-color: #00c0ef; border-color: #00c0ef;"),
                                                )
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

    hr(),

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

    conditionalPanel("input.sidebar == 'Vis1'",
                     radioButtons("vis1_var_type", NULL,
                                  choices=c("数值"="numeric", "因子"="factor"),
                                  selected="numeric"),
                     selectInput("vis1_var", "选择变量：", multiple=FALSE,
                                 choices=names(m_datasets[["SurgicalUnit"]])),
                     conditionalPanel("input.vis1_var_type == 'numeric'",
                                      selectInput("vis1_num_plot_type",
                                                  "选择绘图类型：",
                                                  choices=num_vis1_types,
                                                  selected="scatter_t")),
                     conditionalPanel("input.vis1_var_type == 'factor'",
                                      selectInput("vis1_factor_plot_type",
                                                  "选择绘图类型：",
                                                  choices=factor_vis1_types,
                                                  selected="barplot_t")),
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
