# --- 1. 必要ライブラリ ---
# reactable を追加
library(shiny)
library(shinydashboard)
library(dplyr)
library(reactable) 
library(shinycssloaders)

source("modules/analysis_table_module.R")

source("modules/calculate_hensati.R")
source("modules/calculate_sougoukrisk.R")
source("modules/calculate_hensati_hyou.R")
source("modules/setting_hensati_hyou.R")

# --- 4. モジュールをテストするためのスタンドアロンアプリ ---
# ダミーデータの作成
dummy_current_data <- read_csv("demodata/processed_nbjsq_dummy_data1_alpha.csv")
dummy_previous_data <- read_csv("demodata/processed_nbjsq_dummy_data2_alpha.csv")

# ---- アプリUI ----
ui <- dashboardPage(
  dashboardHeader(title = "表描画モジュール テスト (reactable版)"),
  dashboardSidebar(),
  dashboardBody(
    analysis_table_module_ui("analysis_table")
  )
)

# ---- アプリサーバー ----
server <- function(input, output, session) {
  
  current_data_reactive <- reactive({ dummy_current_data })
  previous_data_reactive <- reactive({ dummy_previous_data })
  
  analysis_table_module_server(
    id = "analysis_table",
    processed_current_year_data = current_data_reactive,
    processed_previous_year_data = previous_data_reactive
  )
  
}

# --- 5. アプリケーションの実行 ---
shinyApp(ui, server)
