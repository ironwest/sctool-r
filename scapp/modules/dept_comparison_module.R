# --- 1. 必要ライブラリ ---
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(reactable)
library(shinycssloaders)
library(ggplot2)     # グラフ描画に使用
library(rmarkdown)   # PDF生成用


# --- 2. 外部ファイルとグローバル変数の読み込み ---
source("calculate_hensati.R")
source("calculate_sougoukrisk.R")
source("calculate_hensati_hyou.R")
source("setting_hensati_hyou.R")
source("setting_gh_analysis.R") #ghsetting
source("setting_bench_mapper.R")
source("make_xx_result.R")

hensati_data      <- read_csv("table11.csv")
nbjsq             <- read_csv("nbjsq_question_text.csv")
nbjsq_answerlabs  <- read_csv("nbjsq_answer_labels.csv")
nbjsqlabs         <- read_csv("nbjsq_label_hensati.csv")
risk_calc_setting <- read_csv("../modules/risk_coefficients.csv")


#hensati_dataをマッピングできるように名前を変更をする
hensati_data <- hensati_data |> 
  filter(qtype == "NBJSQ") |> 
  mutate(`尺度名bench` = factor(`尺度名`, levels = names(benchmapper), labels = benchmapper)) |> 
  mutate(`尺度名bench` = as.character(`尺度名bench`))

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

nbjsq_gyousyu <- c(
  "全体",
  "運輸・郵便業",
  "営業 販売 接客職",
  "卸売・小売業",
  "教育・学習支援業",
  "金融・保険業",
  "建設業","情報通信業",
  "鉱業・採石・砂利採取業",
  "公務","医療・福祉",
  "製造業",
  "製造 運輸 通信 生産 サービス職",
  "電気・ガス・熱供給・水道業",
  "農林水産業",
  "不動産・物品賃貸業"
)


# --- 3. モジュールUI --------

analysis_target_choices <- list(
  "基本" = list("基本" = "basic"),
  "BJSQ" = list("総合健康リスク" = "skrisk",
                "ストレス判定図:負担" = "hantei_hutan",
                "ストレス判定図:支援" = "hantei_sien"),
  "アウトカム" = list("ワークエンゲイジメント" = "outcome_we", 
                 "職場の一体感" = "outcome_sc", 
                 "ハラスメント" = "outcome_harass", 
                 "心理的ストレス反応" = "outcome_stress_reaction", 
                 "仕事の負担" = "outcome_work_hutan"),
  "資源" = list("作業レベル" = "sigen_sagyou",
              "部署レベル" = "sigen_busyo",
              "事業場レベル" = "sigen_jigyouba"),
  "その他" = list("その他" = "na_else")
)

analysis_target_tibble <- enframe(analysis_target_choices) |> 
  rename(bunrui = name) |> 
  mutate(dfr = map(value, ~{
    enframe(.) |> unnest(value)
  })) |> 
  select(bunrui,dfr) |> 
  unnest(dfr)

dept_comparison_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "比較設定", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        fluidRow(
          column(width = 4, 
                 h4("対象部門の選択"),
                 selectInput(ns("level_select"), "比較レベル選択", choices = c("部署（大分類）" = "dept1", "部署（大分類 - 中分類）" = "dept1_dept2")),
                 selectInput(ns("target_dept1"), "部署選択(大分類)", choices = NULL),
                 selectInput(ns("target_dept2"), "部署選択(中分類)", choices = NULL)
          ),
          column(width = 4, 
                 h4("総合健康リスク計算の設定"),
                 selectInput(ns("gyousyu"), "総合健康リスク計算の業種の選択",
                             choices = skrisk_gyousyu),
                 selectInput(ns("long_or_cross"), "総合健康リスク計算の種類",
                             choices = c("縦断(推奨)" = "long","横断" = "cross"))
          ),
          column(width = 4, 
                 h4("ベンチマーク職種の設定"),
                 selectInput(ns("bench_gyousyu"), "NBJSQのベンチマーク用の業種の選択",
                             choices = nbjsq_gyousyu),
                 h4("表示形式の設定"),
                 selectInput(ns("analysis_displaytype"),"偏差値/平均値の切り替え", choices = c("偏差値" = "hensati", "平均値" = "average")),
                 actionButton(ns("run_comparison"), "比較を実行", icon = icon("search-plus"), class = "btn-success", style="margin-top: 25px;"))
        ),
      )
    ),
    fluidRow(
      box(
        title = "分析結果", width = 12, status = "info", solidHeader = TRUE,
        fluidRow(
          column(width=6,
                 selectInput(ns("analysis_target"),"分析結果の選択", choices = analysis_target_choices)       
                 )
        ),
        fluidRow(
          column(width = 4,
                 h4("グラフ"),
                 plotOutput(ns("graph")) |> withSpinner()
          ),
          column(width = 8,
                 h4("尺度別の前回・今回の比較"),
                 reactableOutput(ns("hyou")) |> withSpinner()
          )
        ),
        fluidRow(
          column(width = 12,style = "margin-top:10px;",
                 h4("設問の回答状況"),
                 reactableOutput(ns("hyouq1")) |> withSpinner(),
                 reactableOutput(ns("hyouq2")) |> withSpinner(),
                 reactableOutput(ns("hyouq3")) |> withSpinner()
          )
        ),
        hr(),
        p("以下のボタンから、分析結果をまとめたExcelのレポートとしてダウンロードできます。"),
        downloadButton(ns("download_report_button"), "Excelレポートをダウンロード", icon = icon("file-pdf"))
      )
    )
  )
}

# --- 4. モジュールサーバー (変更あり) ---
dept_comparison_module_server <- function(id, processed_data_now, processed_data_past = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- UIの動的更新 ---
    observe({
      req(processed_data_now())
      data <- processed_data_now()
      level <- input$level_select
      
      dept1_choice <- unique(data$dept1) |> sort()
      updateSelectInput(session, "target_dept1", choices = dept1_choice, selected = input$target_dept1)
      
      if(level == "dept1_dept2"){
        dept2_choice <- data |> 
          filter(dept1 == input$target_dept1) |> 
          pull(dept2) |> 
          unique() |> 
          sort()
        
        updateSelectInput(session, "target_dept2", choices = dept2_choice, selected = input$target_detp2)  
      }else{
        updateSelectInput(session, "target_dept2", choices = "---", selected = "---")  
      }
      
    })
    
    # --- 分析実行 (変更なし) ---
    analysis_results <- eventReactive(input$run_comparison, {
      req(processed_data_now(), input$target_dept1)
      
      #dept1かdept2かでロジックを分けるのは手間なので、grpという変数を作成する
      level <- input$level_select
      data_now <- processed_data_now()  
      data_past <- processed_data_past()  
      
      if(level == "dept1"){
        data_now <- data_now |> mutate(grp = dept1)
        data_past <- data_past |> mutate(grp = dept1)
        target_grp_name <- input$target_dept1
      }else if(level == "dept1_dept2"){
        data_now <- data_now |> mutate(grp = str_c(dept1,"-",dept2))
        data_past <- data_past |> mutate(grp = str_c(dept1,"-",dept2))
        target_grp_name <- str_c(input$target_dept1,"-",input$target_dept2)
      }
      
      #データの加工-----------------------------------------
      hyou_now <- calculate_hensati_hyou(
        current_data = data_now, 
        hensati_data = hensati_data, 
        target_sheet = "全体", #全体以外は現時点ではなし 
        group_vars = "grp", 
        nbjsq = nbjsq, 
        nbjsqlabs = nbjsqlabs,
        target_gyousyu = input$gyousyu,
        target_longorcross = input$long_or_cross,
        precise=TRUE
      )
      
      hyou_oa_now <- data_now |> 
        mutate(grp = "全体") |> 
        calculate_hensati_hyou(
          hensati_data = hensati_data, 
          target_sheet = "全体", #全体以外は現時点ではなし 
          group_vars = "grp", 
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = input$gyousyu,
          target_longorcross = input$long_or_cross,
          precise=TRUE
        )
      
      hyou_past <- calculate_hensati_hyou(
        current_data = data_past, 
        hensati_data = hensati_data, 
        target_sheet = "全体", #全体以外は現時点ではなし 
        group_vars = "grp", 
        nbjsq = nbjsq, 
        nbjsqlabs = nbjsqlabs,
        target_gyousyu = input$gyousyu,
        target_longorcross = input$long_or_cross,
        precise=TRUE
      )
      
      hyou_oa_past <- data_past |> 
        mutate(grp = "全体") |> 
        calculate_hensati_hyou(
          hensati_data = hensati_data, 
          target_sheet = "全体", #全体以外は現時点ではなし 
          group_vars = "grp", 
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = input$gyousyu,
          target_longorcross = input$long_or_cross,
          precise=TRUE
        )
      
      hyou_oa_now  <- hyou_oa_now  |> 
        mutate(`時期` = "今回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1))
        
      hyou_oa_past <- hyou_oa_past |> 
        mutate(`時期` = "前回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1))
      
      hyou_now     <- hyou_now     |> 
        mutate(`時期` = "今回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1)) |> 
        mutate(color_this = grp == target_grp_name)
      
      hyou_past    <- hyou_past    |> 
        mutate(`時期` = "前回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1)) |> 
        mutate(color_this = grp == target_grp_name)
      
      
      #グラフとテーブルを設定データをもとに作成する------------------------------------
      ghres <- tibble()
      for(i in 1:length(ghsetting)){

        asetting <- enframe(ghsetting)$value[[i]]
        set_type <- asetting$type
        
        if(set_type == "ghbase"){
          ares <- make_ghbase_result(hyou_oa_now, hyou_now,hyou_oa_past, hyou_past, target_grp_name, asetting)   
        }else if(set_type == "hanteizu"){
          tgtsyokusyu <- input$gyousyu
          ares <- make_hanteizu_result(hyou_oa_now, hyou_now,hyou_oa_past, hyou_past, asetting, target_grp_name,risk_calc_setting, tgtsyokusyu)
        }else if(set_type == "gh"){
          tgtgyousyu <- input$bench_gyousyu
          ares <- make_gh_result(hyou_oa_now, hyou_now,hyou_oa_past, hyou_past, hensati_data, tgtgyousyu, target_grp_name, asetting, mode="gh", display_type = input$analysis_displaytype)
          qqres <- make_qq_result(data_now, data_past, target_grp_name, asetting, nbjsq, nbjsq_answerlabs)
          
          ares <- ares |> left_join(qqres, by=c("bunrui","name"))
        }else if(set_type == "h"){
          ares <- make_gh_result(hyou_oa_now, hyou_now,hyou_oa_past, hyou_past, hensati_data, tgtgyousyu, target_grp_name, asetting, mode="h", display_type = input$analysis_displaytype)
          qqres <- make_qq_result(data_now, data_past, target_grp_name, asetting, nbjsq, nbjsq_answerlabs)
          
          ares <- ares |> left_join(qqres, by=c("bunrui","name"))
        }
        
        ghres <- bind_rows(ghres, ares)
      }
      
      return(ghres)
    })
    
    #--- 基本テーブルのレンダリング------------------------
    output$graph <- renderPlot({
      req(analysis_results())
      req(input$analysis_target)
      aset <- ghsetting[[input$analysis_target]]
      
      gg <- analysis_results() |> 
        filter(bunrui == aset$bunrui, name == aset$name) |> 
        pull(g)
      
      return(gg[[1]])
      
    })
    
    # 詳細テーブルのレンダリング
    output$hyou <- renderReactable({
      req(analysis_results())
      req(input$analysis_target)
      
      aset <- ghsetting[[input$analysis_target]]
      
      hh <- analysis_results() |> 
        filter(bunrui == aset$bunrui, name == aset$name) |> 
        pull(h)
      
      hh[[1]]
    })
  
    question_hyous <- reactive({
      req(analysis_results())
      req(input$analysis_target)
      
      aset <- ghsetting[[input$analysis_target]]
      if("questions" %in% names(aset)){
        q <- analysis_results() |> 
          filter(bunrui == aset$bunrui, name == aset$name) |> 
          pull(q)
        
        q <- q[[1]]
      }else{
        q <- NULL
      }
      
      return(q)
    })  
  
    output$hyouq1 <- renderReactable({
      req(question_hyous())
      qhyous <- question_hyous()
      
      if(is.null(qhyous)){
        qres <- reactable(tibble(" " = ""))
      }else{
        if(nrow(qhyous)>=1){
          qres <- qhyous |> slice(1) |> pull(qhyou)
          qres <- qres[[1]]
        }else{
          qres <- reactable(tibble(" " = ""))
        }
      }
      return(qres)
    })
    
    output$hyouq2 <- renderReactable({
      req(question_hyous())
      qhyous <- question_hyous()
      if(is.null(qhyous)){
        qres <- reactable(tibble(" " = ""))
      }else{
        if(nrow(qhyous)>=2){
          qres <- qhyous |> slice(2) |> pull(qhyou)
          qres <- qres[[1]]
        }else{
          qres <- reactable(tibble(" " = ""))
        }
      }
      return(qres)
    })
    
    output$hyouq3 <- renderReactable({
      req(question_hyous())
      
      qhyous <- question_hyous()
      if(is.null(qhyous)){
        qres <- reactable(tibble(" " = ""))
      }else{
        if(nrow(qhyous)>=3){
          print(str_c("NROW QHYOU: ",nrow(qhyous)))
          qres <- qhyous |> slice(3) |> pull(qhyou)
          qres <- qres[[1]]
        }else{
          qres <- reactable(tibble(" " = ""))
        }
      }
      return(qres)
    })
    
    
    
    # --- ★ PDFダウンロード  ---
    output$download_report_button <- downloadHandler(
      filename = function() {
        paste0("department_report_", input$target_dept, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(analysis_results())

        if(TRUE){ #デバッグ用
          browser()
          
          ardat <- analysis_results()
          ardat <<- ardat
          # パラメータを準備
          report_params <<- list(
            dept1 = input$target_dept1,
            dept2 = input$target_dept2,
            skr_gyousyu = input$gyousyu,
            skr_longcross = input$long_or_cross,
            bench_gyousyu = input$bench_gyousyu,
            display_type = input$analysis_displaytype,
            rendering_data = ardat
          )
          
          write_rds(ardat, "temp.rds", compress="gz")
          write_rds(report_params,"repparam.rds", compress="gz")
        }
        
        #openxlsx2でエクセルファイルを作成する
        # パラメータを準備
        report_params <- list(
          dept1 = input$target_dept1,
          dept2 = input$target_dept2,
          skr_gyousyu = input$gyousyu,
          skr_longcross = input$long_or_cross,
          bench_gyousyu = input$bench_gyousyu,
          display_type = input$analysis_displaytype,
          rendering_data = analysis_results()
        )
        
        # Rmdファイルのコピーとrenderの呼び出し
        temp_report_path <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", temp_report_path, overwrite = TRUE)
        
        showNotification("エクセルレポートを生成中です...", duration = 5, type = "message")
        
        rmarkdown::render(
          input = temp_report_path,
          output_file = file,
          params = report_params,
          envir = new.env(parent = globalenv())
        )
      }
    )
    
  })
}


# --- 4. スタンドアロンアプリ ---
# ダミーデータ
dummy_current_data <- read_csv("../demodata/processed_nbjsq_dummy_data1_alpha.csv")
dummy_previous_data <- read_csv("../demodata/processed_nbjsq_dummy_data2_alpha.csv")



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