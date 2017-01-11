shinyUI(
  fluidPage(theme = shinytheme("journal"),
            titlePanel("Medicare Part D Drug Price Trends"),
            div("An interactive App, created by", tags$a(href= "https://twitter.com/DrJMLuther","J.M. Luther"),
                "with ", tags$a(href= "http://shiny.rstudio.com/", "RStudio/Shiny")),
            p("Code and relevant files can be found on ", tags$a(href="https://github.com/JMLuther/CMS-Drug-Price-Shiny-App", "GitHub")),
            br(),
            p("This dataset contains average drug prices paid by Medicare Part D and Part B claims, which account for ~75% of medications paid for by Medicaid. This does not include any data from private insurers, and therefore it is not representative of the general population. These data also do not account for manufacturer rebates, which reduce the actual cost paid by Medicare/Medicaid"),
            p("Part B data is summarised by drug when possible, and all possible drugs are contained in the datasets. A subset of drug data is available at ", tags$a(href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Dashboard/2015-Medicaid-Drug-Spending/2015-Medicaid-Drug-Spending.html", "CMS Medicaid dashboard"),". Some results differ for reasons that are not clear to me. The rebates may also explain the difference in cost"),
            p("In general, the Total Cost for the highest cost drugs are approximately twice the actual CMS Total Drug cost, although there is a highly variable relationship between the two. Similarly, the trends in unit prices and total costs generally reflect the actual CMS costs."),
            
            sidebarLayout(
              
              sidebarPanel(
                h3("Drug Name Search"),
                textInput("search1", "Select from the dropdown, or type in a drug name"),
                actionButton("mybutton", "Click to Search"), tags$hr(""),
                h4("Select one or more Drug Brands to Plot"),
                p(("hold down 'ctrl' while selecting multiple drugs")),
                uiOutput("my_output_UI"),
                p("Data Source:",
                  tags$a(href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Information-on-Prescription-Drugs/2015MedicareData.html",
                         "CMS.gov (Part B & D Data)")),
                br()
              ),
              
              mainPanel(
                div(em("mouse over the data points for details", color="red"), align = "center"),
                tabsetPanel(
                  tabPanel("Cost by Unit Price", ggiraphOutput("plot1")),
                  tabPanel("Total Cost", ggiraphOutput("plot2")),
                  tabPanel("Beneficiary Count", ggiraphOutput("plot3")),
                  tabPanel("Top 25 Cost per Beneficiary", ggiraphOutput("plot4")))
              
              )
            )
  )
)
