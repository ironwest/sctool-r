

# --- 1. 必要ライブラリ ---
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(reactable)
library(shinycssloaders)
library(ggplot2)  
library(openxlsx2)
library(zip)

source("modules/dept_comparison_module.R")

source("modules/calculate_hensati.R")
source("modules/calculate_sougoukrisk.R")
source("modules/calculate_hensati_hyou.R")
source("modules/setting_hensati_hyou.R")
source("modules/setting_gh_analysis.R") #ghsetting
source("modules/setting_bench_mapper.R")
source("modules/make_xx_result.R")
source("modules/make_excel_report.R")


# スタンドアロンアプリ ---
# ダミーデータ
dummy_current_data <- read_csv("demodata/processed_nbjsq_dummy_data1_alpha.csv")
dummy_previous_data <- NULL#read_csv("demodata/processed_nbjsq_dummy_data2_alpha.csv")



# ---- アプリUI ----
ui <- dashboardPage(
  dashboardHeader(title = "部署比較モジュール テスト"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    dept_comparison_module_ui("dept_comparison")
  )
)

# ---- アプリサーバー ----
server <- function(input, output, session) {
  data_reactive_now <- reactive({ dummy_current_data })
  data_reactive_past <- reactive({ dummy_previous_data })
  
  dept_comparison_module_server(
    id = "dept_comparison",
    processed_data_now = data_reactive_now,
    processed_data_past = data_reactive_past
  )
}


# --- 6. アプリケーションの実行 ---
shinyApp(ui, server)