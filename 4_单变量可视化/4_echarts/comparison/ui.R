# 定义用户界面

# (1) 标题
header <- dashboardHeader(title = "单变量可视化-数值型")

# (2) 侧栏
sidebar <- dashboardSidebar(

  selectInput("dataset", "选择数据集：", names(m_datasets), selected="SurgicalUnit"),

  hr(),

  div(
    sidebarMenu(id = "sidebar",
                menuItem("数据", tabName = "Data", icon = icon("table")),
                menuItem("单变量可视化", tabName = "Vis1", selected = TRUE, icon = icon("chart-bar"))
    )
  ),

  hr(),

  conditionalPanel("input.sidebar == 'Data'",
                   # (2-1-a) 数据筛选
                   checkboxInput("show_filter", "筛选数据", value=FALSE),
                   conditionalPanel("input.show_filter == true",
                                    textAreaInput("data_filter", NULL,
                                                  placeholder="输入筛选条件（例如，Age>50）并按返回键",
                                                  resize="vertical", rows=2),
                                    tags$table(
                                      tags$td(actionButton(inputId="data_filter_ok", label="完成")),
                                      tags$td(actionButton(inputId="data_filter_clear", label="清除"))
                                    )
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
                                    selectInput("transform", "选择变换类型：", multiple=FALSE,
                                                choices=m_transforms),
                                    conditionalPanel("input.transform == '对数变换'",
                                                     uiOutput("log_transform_ui")),
                                    conditionalPanel("input.transform == '标准化'",
                                                     uiOutput("standardize_ui")),
                   ),

                   # (2-1-d) 数据保存和删除
                   checkboxInput("show_store", "保存/删除数据", value=FALSE),
                   conditionalPanel("input.show_store == true",
                                    textInput("dataset_name", "输入数据集名称:", "NewData"),
                                    actionButton("add_dataset", "保存", class = "btn-success"),

                                    selectInput("del_dataset_names", "选择删除的变量：", multiple=TRUE,
                                                choices=NULL, selected=NULL),
                                    actionButton("del_dataset", "删除", class = "btn-danger")
                   ),

                   # (2-1-e) 数据上传
                   checkboxInput("show_upload", "上传/下载数据", value=FALSE),
                   conditionalPanel("input.show_upload == true",
                                    div(
                                      style="text-align:center; padding: 5px",
                                    textInput("upload_dataset_name", "输入上传数据集名称:", "UserData"),
                                    helpText('上传文件(.csv、.tsv、xls、xlsx, sav)：'),
                                    fileInput("upload", NULL, accept = c(".csv", ".tsv"),
                                              buttonLabel = list(icon = icon("upload"), "上传..."),
                                              multiple=FALSE, placeholder = "未选择任何文件"),
                                    actionButton("import_dataset", "导入", class = "btn-success"),

                                    hr(),
                                    downloadButton("download_dataset", "下载数据集",
                                                   style = "color: #fff; background-color: #00c0ef; border-color: #00c0ef"))
                   ),
  ),

  conditionalPanel("input.sidebar == 'Vis1'",
                   radioButtons("vis1_var_type", NULL, choices=c("数值"="numeric", "因子"="factor"), selected="numeric"),
                   selectInput("vis1_var", "选择变量：", multiple=FALSE, choices=NULL),

                   conditionalPanel("input.vis1_var_type == 'numeric'",
                                    selectInput("vis1_num_plot_type", "选择绘图类型：", multiple=FALSE,
                                                choices=num_vis1_types, selected="histogram_t")),

                   conditionalPanel("input.vis1_var_type == 'factor'",
                                    selectInput("vis1_factor_plot_type", "选择绘图类型：", multiple=FALSE,
                                                choices=factor_vis1_types, selected="barplot_t")),
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
              tabBox(width = 12,
                     tabPanel(title = "graphics",
                              plotOutput("graphics_vis1")),

                     tabPanel(title = "ggplot2",
                              plotOutput("ggplot2_vis1")),

                     tabPanel(title = "Plotly",
                              plotlyOutput("plotly_vis1")),

                     tabPanel(title = "ECharts",
                              echarts4rOutput("echarts_vis1")),
              )
            )
    )
  )
)

# 将标题、侧栏和主体组合在一起
dashboardPage(
  header,
  sidebar,
  body)
