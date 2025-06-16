# --- 1. 必要ライブラリ ---
# reactable を追加
library(shiny)
library(shinydashboard)
library(dplyr)
library(reactable) # DT の代わりに reactable を使用
library(DT) # もし他の部分でDTを使っている場合は残す（このスクリプトでは不要）

source("calculate_hensati.R")
source("calculate_sougoukrisk.R")
hensati_data <- read_csv("table11.csv")
nbjsq <- read_csv("nbjsq_question_text.csv")
nbjsqlabs <- read_csv("nbjsq_label_hensati.csv")
skrisk_gyousyu <- c(
  "全産業",
  "医療・福祉",
  "運輸・郵便業",
  "卸売・小売業",
  "教育・学習支援業",
  "金融・保険業",
  "建設業",
  "公務",
  "サービス業",
  "情報通信業",
  "製造業"
)


# --- 2. 表描画モジュールUI (`analysis_table_module_ui`) ---
analysis_table_module_ui <- function(id) {
  ns <- NS(id) # 名前空間を取得
  
  tagList(
    fluidRow(
      box(
        title = "表示設定",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        fluidRow(
          column(width = 4,
                 selectInput(ns("grouping_var"), "集計単位の選択",
                             choices = c("部署（大分類）" = "dept1",
                                         "部署（大分類 - 中分類）" = "dept1_dept2",
                                         "年齢区分" = "age_kubun",
                                         "性別" = "gender"))
          ),
          column(width = 4,
                 selectInput(ns("display_mode"), "表示モードの選択",
                             choices = c("偏差値(今回)" = "hensati",
                                         "前回との差" = "diff",
                                         "偏差値(前回)" = "hensati_prev"))
          ),
          column(width = 4,
                 selectInput(ns("gyousyu"), "総合健康リスク計算の業種の選択",
                             choices = skrisk_gyousyu),
                 selectInput(ns("long_or_cross"), "総合健康リスク計算の種類",
                             choices = c("縦断(推奨)" = "long","横断" = "cross")),
                 br(), # 見た目調整
                 actionButton(ns("update_table_button"), "表を更新", icon = icon("sync"))
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "集団分析結果",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        # dataTableOutput から reactableOutput に変更
        reactableOutput(ns("summary_table"))
      )
    )
  )
}


# --- 3. 表描画モジュールサーバー (`analysis_table_module_server`) ---
analysis_table_module_server <- function(id,
                                         processed_current_year_data, # reactive({ tibble })
                                         processed_previous_year_data) { # reactive({ tibble | NULL })
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    nbjsq <- read_csv("nbjsq_question_text.csv")
    
    # グループ化する変数
    group_vars <- eventReactive(input$update_table_button,{
      switch(input$grouping_var,
             "dept1" = "dept1",
             "dept1_dept2" = c("dept1", "dept2"),
             "age_kubun" = "age_kubun",
             "gender" = "gender",
             "dept1"
      )
    })
    
    display_data <- eventReactive(input$update_table_button, {
      req(processed_current_year_data())
      req(processed_previous_year_data())
      
      if(input$display_mode == "hensati"){
        #今年度のデータを読み込む
        current_data <- processed_current_year_data()  
      }else if(input$display_mode == "diff"){
        browser()
        
        datanow <- processed_current_year_data()  
        datapast <- processed_previous_year_data()
        
      }else if(input$display_mode == "hensati_prev"){
        current_data <- processed_previous_year_data()
      }
      
      
      
      #どの基準値を利用するか選択
      target_sheet <- switch(input$grouping_var,
                             "dept1" = "全体",
                             "dept1_dept2" = "全体",
                             "age_kubun" = "age_kubun",
                             "gender" = "gender",
                             "全体"
      )
      
      #偏差値表を作成する
      hyou <- calculate_hensati(
        d = current_data, 
        hensati_data = hensati_data, 
        tgtsheet=target_sheet, 
        grp_vars = group_vars(), 
        nbjsq = nbjsq, 
        nbjsqlabs=nbjsqlabs, 
        ret = "hensati"
      )
      
      hyou <- hyou |>  #<<- を最後に<-に戻すこと（デバッグ中）
        select(hensatigrp, all_of(group_vars()), syakudo_japanese, hensati) |> 
        pivot_wider(id_cols = all_of(group_vars()), names_from = syakudo_japanese, values_from = hensati)
     

      #高ストレス者の人数と割合を計算する
      hyouhs <- current_data |> 
        group_by(across(all_of(group_vars()))) |> 
        summarise(
          `受検人数` = n(),
          `不完全回答人数` = sum(is.na(is_hs)),
          `高ストレス者人数` = sum(is_hs, na.rm=TRUE),
          `高ストレス者割合` = `高ストレス者人数`/`受検人数`
        )
      
      hyou <- hyou |> left_join(hyouhs, by=group_vars())
      
      #総合健康リスクの計算
      hyouskrisk <- calculate_sougoukrisk(
        d = current_data,
        grp_vars = group_vars(),
        tgtgyousyu = input$gyousyu
      )
      
      hyouskrisk <- hyouskrisk |> select(all_of(group_vars()), matches(input$long_or_cross))
      
      if(input$long_or_cross == "long"){
        hyouskrisk <- hyouskrisk |> rename("総合健康リスク" = total_risk_long)
      }else if(input$long_or_cross == "cross"){
        hyouskrisk <- hyouskrisk |> rename("総合健康リスク" = total_risk_cross)
      }
      
      hyou <- hyou |> left_join(hyouskrisk, by=group_vars())
      
    return(hyou)
      
    }, ignoreNULL = FALSE)
    
    # テーブルのレンダリング (renderDataTable から renderReactable に変更)
    output$summary_table <- renderReactable({
      req(display_data())
      req(group_vars())
  
      
      # 表のスタイル定義------------------
      color_pal <- list(
        light_green_bg = "#e6f5e6", # 背景の淡い緑
        dark_green_text = "#006400", # 文字の濃い緑
        light_red_bg = "#fde8e8",   # 背景の淡い赤
        dark_red_text = "#990000"    # 文字の濃い赤
      )
      
      # 縦書きヘッダーのためのスタイル定義
      vertical_header_style <- list(
        writingMode = "vertical-rl",
        transformOrigin = "bottom left",
        whiteSpace = "nowrap",
        padding = "3px 1px", # 上下左右のパディングを調整
        height = "auto",    # 列名の長さに応じて調整
        #width="auto",
        fontSize = "0.9em"   # フォントサイズを少し小さくする例
      )
      
      
      # 全ての列に適用するデフォルトの列定義
      default_col_def <- colDef(
        headerStyle = vertical_header_style,
        minWidth = 40, # 回転後のヘッダー幅（元の高さ）とデータ内容を考慮して調整
        align = "center", # データセルの中央揃え
        format = colFormat(digits=0),
        style = function(value) {
          # 値が数値でない、またはNAの場合はスタイルを適用しない
          if (!is.numeric(value) || is.na(value)) {
            return()
          }
          
          # スタイル変数を初期化
          bgcolor <- NULL
          fontcolor <- NULL
          fontweight <- NULL
          
          # 条件分岐（条件の厳しい順に並べ替え）
          if (value >= 60) {          # 60以上: 背景淡い緑、文字濃い緑、Bold
            bgcolor <- color_pal$light_green_bg
            fontcolor <- color_pal$dark_green_text
            fontweight <- "bold"
          } else if (value >= 55) {   # 55以上60未満: 背景淡い緑
            bgcolor <- color_pal$light_green_bg
          } else if (value <= 40) {   # 40以下: 背景淡い赤、文字濃い赤、Bold
            bgcolor <- color_pal$light_red_bg
            fontcolor <- color_pal$dark_red_text
            fontweight <- "bold"
          } else if (value <= 45) {   # 40より大きく45以下: 背景淡い赤
            bgcolor <- color_pal$light_red_bg
          }
          
          # 適用するスタイルをリストで返す
          list(background = bgcolor, color = fontcolor, fontWeight = fontweight)
        }
      )
      
      # 横書き列の列定義
      make_horizontal_col_def <- function(name){
        #横書きヘッダーのためのスタイル定義
        horizontal_header_style <- list(
          writingMode = "horizontal-tb",
          transformOrigin = "bottom left",
          whiteSpace = "nowrap",
          padding = "5px 2px", # 上下左右のパディングを調整
          #width = "auto",    
          fontSize = "0.9em",   # フォントサイズを少し小さくする例
          backgroundColor = "#FFFFFF"
        )
        
        colDef(
          name = name,
          sticky = "left",
          headerStyle = horizontal_header_style,
          align = "left",
          minWidth = 100,
          vAlign = "bottom"
        )
      }
      
     
      col_setting <- list(
        group = group_vars(),
        basic = c("受検人数", "不完全回答人数","高ストレス者人数","高ストレス者割合","総合健康リスク"),
        outcome = c("ソーシャル・キャピタル", "ワークエンゲイジメント","職場のハラスメント","心理的ストレス反応合計","仕事の負担合計"),
        sigen = c("作業レベル資源合計", "部署レベル資源合計","事業場レベル資源"),
        stress = c("活気","イライラ感","疲労感","不安感","抑うつ感"),
        hutan = c("仕事の量的負担","仕事の質的負担","身体的負担度","職場での対人関係","職場環境","情緒的負担","役割葛藤","WSB（－）"),
        sagyou = c("仕事のコントロール","技能の活用","仕事の適正","仕事の意義","役割明確さ","成長の機会"),
        busyo = c("上司の支援","同僚の支援","経済・地位報酬","尊重報酬","安定報酬","上司のリーダーシップ","上司の公正な態度","ほめてもらえる職場","失敗を認める職場"),
        jigyouba = c("経営層との信頼関係","変化への対応","個人の尊重","公正な人事評価","多様な労働者への対応","キャリア形成","WSB（＋）")
      )
      
      group_colors <- list(
        outcome  = "#dbeafe",
        sigen    = "#cffafe",
        stress   = "#ede9fe",
        hutan    = "#ffedd5",
        sagyou   = "#dcfce7",
        busyo    = "#fce7f3",  
        jigyouba = "#e0f2fe"
      )
      
     #コラムグループの設定
      col_group_list <- list(
        colGroup(name = "アウトカム"            , columns = col_setting$outcome , headerStyle = list(backgroundColor = group_colors$outcome , fontWeight = "bold"), sticky="left"),
        colGroup(name = "資源"                  , columns = col_setting$sigen   , headerStyle = list(backgroundColor = group_colors$sigen   , fontWeight = "bold"), sticky="left"),
        colGroup(name = "心理的ストレス反応合計", columns = col_setting$stress  , headerStyle = list(backgroundColor = group_colors$stress  , fontWeight = "bold")),
        colGroup(name = "仕事の負担合計"        , columns = col_setting$hutan   , headerStyle = list(backgroundColor = group_colors$hutan   , fontWeight = "bold")),
        colGroup(name = "作業レベル資源"        , columns = col_setting$sagyou  , headerStyle = list(backgroundColor = group_colors$sagyou  , fontWeight = "bold")),
        colGroup(name = "部署レベル資源"        , columns = col_setting$busyo   , headerStyle = list(backgroundColor = group_colors$busyo   , fontWeight = "bold")),
        colGroup(name = "事業場レベル資源"      , columns = col_setting$jigyouba, headerStyle = list(backgroundColor = group_colors$jigyouba, fontWeight = "bold"))
      )
      
      column_order <- c(col_setting$group,
                        col_setting$basic,
                        col_setting$outcome,
                        col_setting$sigen,
                        col_setting$stress,
                        col_setting$hutan,
                        col_setting$sagyou,
                        col_setting$busyo,
                        col_setting$jigyouba)
      
      column_setting_list <- map(set_names(column_order), ~ default_col_def)
      column_setting_list$dept1 <- make_horizontal_col_def("部署1")
      column_setting_list$dept2 <- make_horizontal_col_def("部署2")
      column_setting_list$age_kubun <- make_horizontal_col_def("年齢区分")
      column_setting_list$gender <- make_horizontal_col_def("性別")
      
      
      
      #列の色を設定する
      column_setting_list[["心理的ストレス反応合計"]]$headerStyle$backgroundColor <- group_colors$stress
      column_setting_list[["仕事の負担合計"]]$headerStyle$backgroundColor <- group_colors$hutan
      column_setting_list[["作業レベル資源合計"]]$headerStyle$backgroundColor <- group_colors$sagyou
      column_setting_list[["部署レベル資源合計"]]$headerStyle$backgroundColor <- group_colors$busyo
      column_setting_list[["事業場レベル資源"]]$headerStyle$backgroundColor <- group_colors$jigyouba
      
      for(tgtgrp in c("stress","hutan","sagyou","busyo","jigyouba")){
        for(tgtcol in col_setting[[tgtgrp]]){
          column_setting_list[[tgtcol]]$headerStyle$backgroundColor <- group_colors[[tgtgrp]]  
        } 
      }
      
      #表の順番を整える-------------------
      
      
      hyou <- display_data() |> relocate(!!!rlang::syms(column_order))
      #reactableでの描画
      
      reactable(
        hyou,#display_data(),# hyou,
        defaultColDef = default_col_def,
        columns = column_setting_list,
        columnGroups = col_group_list,
        filterable = FALSE,     # 各列のフィルターを有効化 (DTのfilter='top'に相当)
        searchable = TRUE,      # 全体検索ボックスを有効化
        highlight = TRUE,       # 行をホバーした際にハイライト
        bordered = TRUE,        # 境界線を表示
        striped = TRUE,         # 縞模様
        compact = TRUE,         # コンパクト表示
        wrap = FALSE,           # 横スクロールを有効化 (DTのscrollX=TRUEに相当)
        defaultPageSize = 15,   # 1ページの表示行数 (DTのpageLengthに相当)
        minRows = 15,
        resizable = TRUE,
        showPageSizeOptions = TRUE, # ページサイズ変更オプションを表示
        pageSizeOptions = c(10, 15, 30, 60), # ページサイズの選択肢
        theme = reactableTheme( # 簡単なテーマ設定
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5ff",
          cellPadding = "8px 12px"
        )
      )
      
    })
    
  })
}


# --- 4. モジュールをテストするためのスタンドアロンアプリ ---
# ダミーデータの作成
dummy_current_data <- read_csv("../demodata/processed_nbjsq_dummy_data1_alpha.csv")
dummy_previous_data <- read_csv("../demodata/processed_nbjsq_dummy_data2_alpha.csv")



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
