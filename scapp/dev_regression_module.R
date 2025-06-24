# --- 1. 必要ライブラリ ---
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(reactable)
library(shinycssloaders)
library(broom)
library(ggplot2) # ヒストグラム描画のために追加

source("modules/logistic_regression_module.R")

# --- 4. スタンドアロンアプリ ---
dummy_current_data <- NULL#read_csv("demodata/processed_nbjsq_dummy_data1_alpha.csv")
dummy_previous_data <- read_csv("demodata/processed_nbjsq_dummy_data2_alpha.csv")

ui <- dashboardPage(
  dashboardHeader(title = "回帰分析モジュール テスト"),
  dashboardSidebar(),
  dashboardBody(
    analysis_regression_module_ui("regression_module")
  )
)

server <- function(input, output, session) {
  current_data_reactive <- reactive({ dummy_current_data })
  previous_data_reactive <- reactive({ dummy_previous_data })
  
  analysis_regression_module_server(
    id = "regression_module",
    processed_current_year_data = current_data_reactive,
    processed_previous_year_data = previous_data_reactive
  )
}

# --- 5. アプリケーションの実行 ---
shinyApp(ui, server)