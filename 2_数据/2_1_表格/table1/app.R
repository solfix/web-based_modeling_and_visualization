library(shiny)

# 定义用户界面
ui <- fluidPage(

    # (1) App标题
    titlePanel("表格显示1"),

    # (2) 侧栏布局
    sidebarLayout(

        # (2-1) 侧栏面板：输入控件selectInput()创建的下拉菜单选择内置数据集
        sidebarPanel(
            selectInput("dataset", "选择数据集：", ls("package:datasets"))
        ),

        # (2-2) 主面版：输出控件tableOutput()在表格中显示数据
        mainPanel(
            tableOutput("table")
        )
    )
)

# 定义服务器逻辑
server <- function(input, output) {

    # 用反应表达式保存选取的数据
    dataset <- reactive(get(input$dataset, "package:datasets"))

    # 以表格显示数据集
    output$table <- renderTable({
        dataset()
    })
}

# 运行App
shinyApp(ui = ui, server = server)
