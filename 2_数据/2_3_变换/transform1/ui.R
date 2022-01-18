# 定义用户界面
ui <- fluidPage(

  shinyFeedback::useShinyFeedback(),

  # (1) App标题
  titlePanel("数据变换1"),

  # (2) 侧栏布局
  sidebarLayout(

    # (2-1) 侧栏面板：
    sidebarPanel(

      selectInput("dataset", "选择数据集：", names(m_datasets), selected="SurgicalUnit"),
      checkboxInput("show_filter", "筛选数据", value=FALSE),

      # 数据筛选
      conditionalPanel("input.show_filter == true",
                       wellPanel(
                         textAreaInput("data_filter", NULL,
                                       placeholder="输入筛选条件（例如，Age>50）并按返回键",
                                       resize="vertical", rows=3),
                         actionButton(inputId="data_filter_ok", label="完成"),
                         actionButton(inputId="data_filter_clear", label="清除")
                       )),

      # 变量选择
      checkboxInput("show_select", "选择变量", value=FALSE),
      conditionalPanel("input.show_select == true",
                       wellPanel(
                         selectInput("columns", NULL, multiple=TRUE,
                                     choices=names(m_datasets[["SurgicalUnit"]]),
                                     selected=names(m_datasets[["SurgicalUnit"]]),
                                     selectize=F)
                       )),
    ),

    # (2-2) 主面版
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
)
