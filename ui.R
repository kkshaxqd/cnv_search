# ui.R
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
dashboardPage(
  dashboardHeader(title="CNV结果可视化"
                  ),
  dashboardSidebar( 
    #决定标签显示的名称，和变量
    
    sidebarMenu(
      menuItem("上传文件", tabName = "uploadfile", icon = icon("upload")),
      menuItem("数据表", tabName = "datatable", icon = icon("th")),
      menuItem("结果统计", tabName = "picture", icon = icon("map")),
      menuItem("circos图", tabName = "circos", icon = icon("circle-o")),
      menuItem("UCSC展示", tabName = "ucsc", icon = icon("universal-access"))
    )
    
                   ),
  dashboardBody(
    tabItems(
      #新的第一个标签下内容
      tabItem(
          tabName = "uploadfile",
          h2("  UPload CNVfile"),
          sidebarPanel(
          fileInput('file1', '提交整合后三软件CNV结果文件',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain')),
          tags$hr(),
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',
                     c(Tab='\t',
                       Comma=',',
                       Semicolon=';'
                     ),
                     selected="Tab"
                    # inline=TRUE
          ),
          radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     selected="None"
                     #inline=TRUE
                     )
          #numericInput("obs", "Number of observations to view:", 10)
        )
      ),
      
      # First tab content  第一个标签下内容
      tabItem(tabName = "datatable",
              fluidRow(
                h2("  CNV结果表"),
                DT::dataTableOutput("table")
                
              )
      ),
      
      # Second tab content 第二个标签下内容
      tabItem(tabName = "picture",
              #box(plotOutput("plot1", height = 250)),
              #box(
                #title = "Controls",
                #sliderInput("slider", "Number of observations:", 1, 100, 50)
              #)
              #sidebarPanel(
                #selectInput("softvar","软件",
                          #  c("all"="software",
                         #     "CNVKIT"="CNVKIT",
                        #      "XHMM"="XHMM",
                         #     "CONTRA"="CONTRA",
                         #     "CONTRA-LARGECNV"="CONTRA-LARGECNV"
                        #    ),
                        #    selected="all"
 
              #  )
    #  ),
      # rownames(ba)<-c("chromosome",	"start",	"end",	"SIZE",	"gene",	"log2",	"cn",	"TYPE",	"depth",	"probes",	"weight",	"software")
      sidebarPanel(
        selectInput("var","列统计",
                    c("software"="software",
                      "chr"="chromsome",
                      "start"="start",
                      "end"="end",
                      "SIZE"="SIZE",
                      "gene"="gene",
                      "log2"="log2",
                      "cn"="cn",
                      "TYPE"="TYPE",
                      "depth"="depth",
                      "probes"="probes"
                    ) 
          )
      ),
      tableOutput("summary"),
      plotOutput("summaryplot",height = "1000px")
    ),
    tabItem(tabName = "circos", 
      h2("整体Circos图展示"),
      plotOutput('plotcircos',width = "85%", height = "1000px" )
      
    ),
    tabItem(tabName = "ucsc", 
            h2("UCSC显示CNV具体情况"),
            textInput("CNV_reigon", "CNV input","chr1:17380437-20915212"),
            h5("想要显示的cnv输入时使用如上格式"),
            shinyjs::useShinyjs(),
            actionButton("submit", "Submit")
    )
    #plotOutput("splom"),
    #plotOutput("wireframe")

  )
  
)
)
