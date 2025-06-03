
ns <- function(x)return(x)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

qtext <- read_csv("nbjsq_question_text.csv") |> dplyr::pull(qtext)

overall_value_mapping_tabpanel <- 
  tabPanel(
    title = "一括設定", 
    value = "bulk_settings", 
    div(
      style = paste(
        "margin-top:20px;",
        "height: 600px;",
        "overflow-y: auto;",
        "border: 1px solid #eeeeee;",
        "padding: 10px;"
      ),
      pmap(list(
        list("A","B","C","D","E-H"), #セクションアルファベット
        list(
          "A)あなたの仕事についてうかがいます",
          "B)最近1か月間のあなたの状態についてうかがいます",
          "C)あなたの周りの方々についてうかがいます",
          "D)満足度についてうかがいます",
          "E-H)仕事・職場・会社について"
        ), #設問の名前
        list(
          c("そうだ","まあそうだ","ややちがう","ちがう"),
          c("ほとんどなかった","ときどきあった","しばしばあった","ほとんどいつもあった"),
          c("非常に","かなり","多少","まったくない"),
          c("満足","まあ満足","やや不満足","不満足"),
          c("そうだ","まあそうだ","ややちがう","ちがう")
        ) #選択のベクトル
      ), ~{
        div(
          fluidRow(
            style = "margin-top:20pt;",
            column(width = 2, ..2),
            column(width = 2, selectInput(inputId = ns(paste0("oavmap_",..1,"1")), label = ..3[1], choices = c())),
            column(width = 2, selectInput(inputId = ns(paste0("oavmap_",..1,"2")), label = ..3[2], choices = c())),
            column(width = 2, selectInput(inputId = ns(paste0("oavmap_",..1,"3")), label = ..3[3], choices = c())),
            column(width = 2, selectInput(inputId = ns(paste0("oavmap_",..1,"4")), label = ..3[4], choices = c())),
            column(width = 2, actionButton(inputId = ns(paste0("oavmap_",..1,"_admit")), label = "上書き設定"))
          ),
          hr()  
        )
      })
    )
  )

value_mapping_tabpanels <- pmap(
  list(
    list("A","B","C","D","E-H"), #panel title
    list(c(1:17),c(18:46),c(47:55),c(56:57),c(58:80)), #qnumbers
    list(
      c("そうだ","まあそうだ","ややちがう","ちがう"),
      c("ほとんどなかった","ときどきあった","しばしばあった","ほとんどいつもあった"),
      c("非常に","かなり","多少","まったくない"),
      c("満足","まあ満足","やや不満足","不満足"),
      c("そうだ","まあそうだ","ややちがう","ちがう")
    ) #選択のベクトル
  ), 
  ~{
    choice_of_qs <- ..3
    tabPanel(
      title = ..1, 
      div(
        style = paste(
          "margin-top:20px;",
          "height: 600px;",
          "overflow-y: auto;",
          "border: 1px solid #eeeeee;",
          "padding: 10px;"
        ),
        map(..2, ~{
          fluidRow(
            column(width=3,paste0(.,")",qtext[.])),
            column(width=2,selectInput(label=choice_of_qs[1], inputId = ns(paste0("vmap_q",.,"_1")), choices = c())),
            column(width=2,selectInput(label=choice_of_qs[2], inputId = ns(paste0("vmap_q",.,"_2")), choices = c())),
            column(width=2,selectInput(label=choice_of_qs[3], inputId = ns(paste0("vmap_q",.,"_3")), choices = c())),
            column(width=2,selectInput(label=choice_of_qs[4], inputId = ns(paste0("vmap_q",.,"_4")), choices = c()))
          )  
        })
      )
    ) 
  }
)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  h4("NBJSQの値マッピング"),
  wellPanel(
    tabsetPanel(
      id = ns("nbjsq_val_map_tabs"),
      overall_value_mapping_tabpanel,
      value_mapping_tabpanels[[1]],
      value_mapping_tabpanels[[2]],
      value_mapping_tabpanels[[3]],
      value_mapping_tabpanels[[4]],
      value_mapping_tabpanels[[5]]
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
