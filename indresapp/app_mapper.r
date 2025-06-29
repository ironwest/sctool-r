# 必要ライブラリ (ファイルの先頭にまとめて記述することを推奨)
library(shiny)
library(shinydashboard)
library(jsonlite) # 設定ファイルのJSON処理のため
library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(reactable)

source("modules/wizard_module.R")
source("modules/calculate_scores.R")


# --- アプリケーション実行のためのUIとサーバー ---
ui <- fluidPage(
  wizard_module_ui("current_year_wizard")
)


server <- function(input, output, session) {
  # 今年度ウィザードモジュールのサーバーロジック呼び出し
  current_year_results <- wizard_module_server("current_year_wizard", year_label = "今年度")
  
  # 昨年度ウィザードモジュールのサーバーロジック呼び出し
  previous_year_results <- wizard_module_server("previous_year_wizard", year_label = "昨年度")
  
  # モジュールの返り値を利用する例 (デバッグ用)
  observe({
    req(current_year_results$is_setup_complete()) # setupが完了したら
    
    if (current_year_results$is_setup_complete()) {
      cat("今年度の設定が完了しました。\n")
      current_year_results$get_csv_data()
      current_year_results$get_column_map()
      current_year_results$get_value_map()
      current_year_results$is_setup_complete()
      print(str(current_year_results$get_column_map()))
      
      # print(str(current_year_results$get_value_map()))
    }
  })
  observe({
    req(previous_year_results$is_setup_complete())
    if (previous_year_results$is_setup_complete()) {
      cat("昨年度の設定が完了しました。\n")
    }
  })
}

# アプリケーションの実行
shinyApp(ui, server)
