library(shiny)

# 定义用户界面
ui <- fluidPage(

    # (1) App标题
    titlePanel("表格显示2"),

    # (2) 侧栏布局
    sidebarLayout(

        # (2-1) 侧栏面板：
        #   (2-1a)  单选selectInput()创建的下拉菜单选择内置数据集；
        #   (2-1b)  多选selectInput()从选定的数据集中选择要显示的变量（列）。
        sidebarPanel(
            selectInput("dataset", "选择数据集：", choices=ls("package:datasets"), selected="mtcars"),
            selectInput("columns", "选择变量：", multiple=TRUE, choices=names(mtcars), selected=names(mtcars))
        ),

        # (2-2) 主面版---输出控件tableOutput()在表格中显示数据
        mainPanel(
            tableOutput("table")
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

    # 在表格中显示数据集中已选取的变量
    output$table <- renderTable({
        dataset()[, input$columns]
    })
}

# 创建并运行Shiny应用
shinyApp(ui = ui, server = server)
