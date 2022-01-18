# 定义用户界面
ui <- fluidPage(

  shinyFeedback::useShinyFeedback(),

  # (1) App标题
  titlePanel("保存数据"),

  # (2) 侧栏布局
  sidebarLayout(

    # (2-1) 侧栏面板：
    sidebarPanel(

      selectInput("dataset", "选择数据集：", names(m_datasets), selected="SurgicalUnit"),

      # (2-1-a) 数据筛选
      checkboxInput("show_filter", "筛选数据", value=FALSE),
      conditionalPanel("input.show_filter == true",
                       wellPanel(
                         textAreaInput("data_filter", NULL,
                                       placeholder="输入筛选条件（例如，Age>50）并按返回键",
                                       resize="vertical", rows=3),
                         actionButton(inputId="data_filter_ok", label="完成"),
                         actionButton(inputId="data_filter_clear", label="清除")
                         )),

      # (2-1-b) 变量选择
      checkboxInput("show_select", "选择变量", value=FALSE),
      conditionalPanel("input.show_select == true",
                       wellPanel(
                         selectInput("columns", NULL, multiple=TRUE,
                                     choices=names(m_datasets[["SurgicalUnit"]]),
                                     selected=names(m_datasets[["SurgicalUnit"]]),
                                     selectize=F)
                         )),

      # (2-1-c) 数据变换
      checkboxInput("show_transform", "数据变换", value=FALSE),
      conditionalPanel("input.show_transform == true",
                       wellPanel(
                         selectInput("transform", "选择变换类型：", choices=m_transforms),
                         selectInput("transform_vars", "选择作变换的变量：", multiple=TRUE, choices=NULL)
                       )),

      # (2-1-d) 数据保存
      checkboxInput("show_store", "保存数据", value=FALSE),
      conditionalPanel("input.show_store == true",
                       wellPanel(
                         textInput("dataset_name", "输入数据集名称:", "NewData"),
                         actionButton("add_dataset", "保存", class = "btn-success")
                       )),
    ),

    # (2-2) 主面版
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
)
