library(shiny)
library(tidyverse)
library(datamods)

# 所有数据框形式的内置数据名称
df_names <- ls("package:datasets") %>%
  .[purrr::map_lgl(., ~ "data.frame" %in% class(get(., "package:datasets")))]

# 定义用户界面
ui <- fluidPage(

  # (1) App标题
  titlePanel("数据筛选2"),

  # (2) 侧栏布局
  sidebarLayout(

    # (2-1) 侧栏面板：
    sidebarPanel(
      # (2-1a) wellPanel将相关控件圈在一起
      #   (2-1a-1) 单选selectInput()创建的下拉菜单选择内置数据集；
      #   (2-1a-2) checkboxInput() 设置是否应用数据筛选功能
      wellPanel(
        selectInput("dataset", "选择数据集：", df_names, selected="mtcars"),
        checkboxInput("show_filter", "筛选数据", value = FALSE),

        #  (2-1a-3) conditionalPanel()动态显示UI：如果应用数据筛选，
        #           显示确定筛选条件的构件，否则隐藏构件。
        #    (2-1a-3a) 用filter_data_ui()生成所有变量的筛选构件。
        conditionalPanel("input.show_filter == true",
                         filter_data_ui("filtering", show_nrow=FALSE, max_height="360px")),
      ),

      # (2-1b) 多选selectInput()从选定的数据集中选择要显示的变量（列）。
      selectInput("columns", "选择变量：", multiple=TRUE, choices=names(mtcars), selected=names(mtcars))
    ),

    # (2-2) 主面版---输出控件DT::dataTableOutput以表格显示数据
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
)
# 定义服务器逻辑
server <- function(input, output) {

  # 以反应值形式保存选取的数据
  dataset <- reactive(get(input$dataset, "package:datasets"))

  # 数据集发生切换后，更新变量（列）的选择项
  observeEvent(dataset(), {

    # “冻结”输入：确保任何使用该输入的反应值或输出都不会被更新。
    # 这是因为本例中，当切换数据集，变量（列）若未更新，则这些变量在新数据集中不可用。
    freezeReactiveValue(input, "columns")

    # 以新数据集的变量名更新变量（列）的选择项
    updateSelectInput(inputId = "columns", choices = names(dataset()), selected = names(dataset()))
  })

  res_filter <- filter_data_server(
    id = "filtering",
    data = dataset,
    name = reactive(input$dataset),
    widget_num = "slider",
    widget_date = "slider",
    label_na = "Missing"
  )

  # DT::renderDataTable分页显示数据集中已选取的变量，缺省一页显示5行
  output$table <- DT::renderDataTable({
    if(input$show_filter) res_filter$filtered()[, input$columns] else dataset()[, input$columns]
  }, options = list(
    # 表格中显示的中文标签
    language = list(
      info = "显示第 _START_ 至 _END_ 项结果，共 _TOTAL_ 项",
      search = "搜索：",
      paginate = list(previous = "上一页", `next` = "下一页"),
      lengthMenu = "显示 _MENU_ 项结果"
    ),
    pageLength = 5,
    scrollX = TRUE # 水平方向滑动条
  ))
}

# 创建并运行Shiny应用
shinyApp(ui = ui, server = server)
