# TODO: 集団解析で、10人未満部署を一つにまとめる方法

# モジュールUI-------------------------
dept_comparison_module_ui <- function(id) {
  ns <- NS(id)
  
  skrisk_gyousyu <- c(
    "全産業","医療・福祉","運輸・郵便業","卸売・小売業","教育・学習支援業",
    "金融・保険業","建設業","公務","サービス業","情報通信業","製造業"
  )
  
  nbjsq_gyousyu <- c(
    "全体","運輸・郵便業","営業 販売 接客職","卸売・小売業","教育・学習支援業",
    "金融・保険業","建設業","情報通信業","鉱業・採石・砂利採取業","公務",
    "医療・福祉","製造業","製造 運輸 通信 生産 サービス職",
    "電気・ガス・熱供給・水道業","農林水産業","不動産・物品賃貸業"
  )
  
  
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
    mutate(dfr = map(value, ~{enframe(.) |> unnest(value)})) |> 
    select(bunrui,dfr) |> 
    unnest(dfr)
  
  tagList(
    fluidRow(
      box(
        title = "比較設定", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        fluidRow(
          column(width = 4, 
                 h4("対象部門の選択"),
                 selectInput(ns("level_select"), "比較レベル選択", choices = c("部署（大分類）" = "dept1", "部署（大分類 - 中分類）" = "dept1_dept2")),
                 selectInput(ns("target_dept1"), "部署選択(大分類)", choices = NULL),
                 conditionalPanel(
                   condition = "input.level_select == 'dept1_dept2'",
                   ns = ns,
                   selectInput(ns("target_dept2"), "部署選択(中分類)", choices = NULL)
                 )
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
        p("以下のボタンから、分析結果をまとめたExcelのレポートとしてダウンロードできます。（20－30秒レポート作成にかかります）"),
        downloadButton(ns("download_report_button"), "Excelレポートをダウンロード", icon = icon("file-excel")),
        hr(),
        p("以下のボタンで、全部署のレポートを作成します実行時間が長くなる場合があるため、注意してください。"),
        downloadButton(ns("download_all_report_button"), "全部署のExcelレポートをダウンロード", icon = icon("folder"))
      )
    )
  )
}

# モジュールサーバー------
dept_comparison_module_server <- function(id, processed_data_now, processed_data_past = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      is_data_now = TRUE,
      is_data_past = TRUE
    )
    
    observe({
      rv$is_data_now <- !is.null(processed_data_now())
      rv$is_data_past <- !is.null(processed_data_past())
    })
    
    hensati_data      <- read_csv("modules/table11.csv")
    nbjsq             <- read_csv("modules/nbjsq_question_text.csv")
    nbjsq_answerlabs  <- read_csv("modules/nbjsq_answer_labels.csv")
    nbjsqlabs         <- read_csv("modules/nbjsq_label_hensati.csv")
    risk_calc_setting <- read_csv("modules/risk_coefficients.csv")
    
    
    #hensati_dataをマッピングできるように名前を変更をする
    hensati_data <- hensati_data |> 
      filter(qtype == "NBJSQ") |> 
      mutate(`尺度名bench` = factor(`尺度名`, levels = names(benchmapper), labels = benchmapper)) |> 
      mutate(`尺度名bench` = as.character(`尺度名bench`))
    
    
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
    gen_analysis_results <- function(data_now, data_past,level, targetdept1, targetdept2, gyousyu, long_or_cross,bench_gyousyu,analysis_displaytype){
      
      #datanowの処理--------------------
      
      if(level == "dept1"){
        data_now <- data_now |> mutate(grp = dept1)
        target_grp_name <- targetdept1
      }else if(level == "dept1_dept2"){
        data_now <- data_now |> mutate(grp = str_c(dept1,"-",dept2))
        target_grp_name <- str_c(targetdept1,"-",targetdept2)
      }
      
      hyou_now <- calculate_hensati_hyou(
        current_data = data_now, 
        hensati_data = hensati_data, 
        target_sheet = "全体", #全体以外は現時点ではなし 
        group_vars = "grp", 
        nbjsq = nbjsq, 
        nbjsqlabs = nbjsqlabs,
        target_gyousyu = gyousyu,
        target_longorcross = long_or_cross,
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
          target_gyousyu = gyousyu,
          target_longorcross = long_or_cross,
          precise=TRUE
        )
      
      hyou_oa_now  <- hyou_oa_now  |> 
        mutate(`時期` = "今回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1))
      
      hyou_now     <- hyou_now     |> 
        mutate(`時期` = "今回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1)) |> 
        mutate(color_this = grp == target_grp_name)
      
      #datapastの処理--------------------------
      if(is.null(data_past)){
        data_past <- data_now |> filter(FALSE)
        hyou_oa_past <- hyou_oa_now |> filter(FALSE)
        hyou_past <- hyou_now |> filter(FALSE)
      }else{
        if(level == "dept1"){
          data_past <- data_past |> mutate(grp = dept1)
          target_grp_name <- targetdept1
        }else if(level == "dept1_dept2"){
          data_past <- data_past |> mutate(grp = str_c(dept1,"-",dept2))
          target_grp_name <- str_c(targetdept1,"-",targetdept2)
        }
        
        hyou_past <- calculate_hensati_hyou(
          current_data = data_past, 
          hensati_data = hensati_data, 
          target_sheet = "全体", #全体以外は現時点ではなし 
          group_vars = "grp", 
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = gyousyu,
          target_longorcross = long_or_cross,
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
            target_gyousyu = gyousyu,
            target_longorcross = long_or_cross,
            precise=TRUE
          )
        
        hyou_oa_past <- hyou_oa_past |> 
          mutate(`時期` = "前回") |> 
          mutate(`対象` = grp) |> 
          mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1))
        
        hyou_past    <- hyou_past    |> 
          mutate(`時期` = "前回") |> 
          mutate(`対象` = grp) |> 
          mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1)) |> 
          mutate(color_this = grp == target_grp_name)
      }
       
      #グラフとテーブルを設定データをもとに作成する------------------------------------
      ghres <- tibble()
      for(i in 1:length(ghsetting)){
        
        asetting <- enframe(ghsetting)$value[[i]]
        set_type <- asetting$type
        
        if(set_type == "ghbase"){
          ares <- make_ghbase_result(hyou_oa_now, hyou_now,hyou_oa_past, hyou_past, target_grp_name, asetting)   
        }else if(set_type == "hanteizu"){
          tgtsyokusyu <- gyousyu
          ares <- make_hanteizu_result(hyou_oa_now, hyou_now,hyou_oa_past, hyou_past, asetting, target_grp_name,risk_calc_setting, tgtsyokusyu)
        }else if(set_type == "gh"){
          tgtgyousyu <- bench_gyousyu
          ares <- make_gh_result(hyou_oa_now, hyou_now,hyou_oa_past, hyou_past, hensati_data, tgtgyousyu, target_grp_name, asetting, mode="gh", display_type = analysis_displaytype)
          qqres <- make_qq_result(data_now, data_past, target_grp_name, asetting, nbjsq, nbjsq_answerlabs)
          
          ares <- ares |> left_join(qqres, by=c("bunrui","name"))
        }else if(set_type == "h"){
          ares <- make_gh_result(hyou_oa_now, hyou_now,hyou_oa_past, hyou_past, hensati_data, tgtgyousyu, target_grp_name, asetting, mode="h", display_type = analysis_displaytype)
          qqres <- make_qq_result(data_now, data_past, target_grp_name, asetting, nbjsq, nbjsq_answerlabs)
          
          ares <- ares |> left_join(qqres, by=c("bunrui","name"))
        }
        
        ghres <- bind_rows(ghres, ares)
      }
      
      return(ghres)
    }
    
    analysis_results <- eventReactive(input$run_comparison, {
      req(processed_data_now(), input$target_dept1)
      
      res <- gen_analysis_results(data_now = processed_data_now(),
                           data_past = processed_data_past(),
                           level = input$level_select,
                           targetdept1 = input$target_dept1,
                           targetdept2 = input$target_dept2,
                           gyousyu = input$gyousyu,
                           long_or_cross = input$long_or_cross,
                           bench_gyousyu = input$bench_gyousyu,
                           analysis_displaytype = input$analysis_displaytype)
      
      
      
      return(res)
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
    
    
    # レポートダウンロードハンドラー-----------------------
    output$download_report_button <- downloadHandler(
      filename = function() {
        if(input$level_select=="dept1"){
          filename <- paste0(input$target_dept1, "_", Sys.Date(), ".xlsx")
        }else if(input$level_select == "dept1_dept2"){
          filename <- paste0(input$target_dept1, "_", input$target_dept2,"_",Sys.Date(), ".xlsx")
        }
        
        invalid_pattern <- "[<>:\"/\\\\|?*]|[\\x00-\\x1F]"
        filename <- str_remove_all(filename, invalid_pattern)
        
        return(filename)
        
      },
      content = function(file) {
        req(analysis_results())
        
        #プログレスバーを表示する
        progress <- shiny::Progress$new()
        progress$set(message = "Excelレポートを作成中", value = 0)
        on.exit(progress$close())
        
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
        
        #TODO: Add info panel for rendering report
        make_excel_report(report_params, file, progress)
        
      }
    )
    
    output$download_all_report_button <- downloadHandler(
      filename = function(){
        if(input$level_select=="dept1"){
          filename <- "大分類.zip"
        }else if(input$level_select == "dept1_dept2"){
          filename <- "大分類-中分類.zip"
        }
        
        return(filename)
      },
      content = function(file){
        temp_report_dir <- file.path(tempdir(), paste0("reports_", as.integer(Sys.time())))
        dir.create(temp_report_dir)

        progress1 <- shiny::Progress$new()
        progress1$set(message = "全部署レポートを作成中", value = 0)
        on.exit(progress1$close())
        
        #選択されているものを固定する(念のため
        gyousyu <- input$gyousyu
        long_or_cross <- input$long_or_cross
        bench_gyousyu <- input$bench_gyousyu
        analysis_displaytype <- input$analysis_displaytype
        
        #choiceを再度取得
        data <- processed_data_now()
        level <- input$level_select
      
        dept1_choice <- unique(data$dept1) |> sort()
        dept2_choice <- data |> 
          filter(dept1 == input$target_dept1) |> 
          pull(dept2) |> 
          unique() |> 
          sort()
        
        if(level == "dept1_dept2"){
          #10人以上だけ出力する
          choices <- data |> count(dept1, dept2) |> filter(n >= 10)
        }else if(level == "dept1"){
          choices <- data |> count(dept1) |> filter(n >= 10)
        }
        
        totchoices <- nrow(choices)
        for(i in 1:totchoices){
          
          progress1$set(message = str_c(i,"/",totchoices,"のレポートを作成中"), value = i*(1/totchoices))
          adept1 <- choices$dept1[i]
          
          if(level == "dept1_dept2"){
            adept2 <- choices$dept2[i]  
          }else{
            adept2 <- NULL
          }
          
          progress2 <- shiny::Progress$new()
          progress2$set(message = "Excelレポートを作成中", value = 0)
          
          res <- gen_analysis_results(data_now = processed_data_now(),
                                      data_past = processed_data_past(),
                                      level = level,
                                      targetdept1 = adept1,
                                      targetdept2 = adept2,
                                      gyousyu = gyousyu,
                                      long_or_cross = long_or_cross,
                                      bench_gyousyu = bench_gyousyu,
                                      analysis_displaytype = analysis_displaytype)
          
          report_params <- list(
            dept1 = adept1,
            dept2 = adept2,
            skr_gyousyu = gyousyu,
            skr_longcross = long_or_cross,
            bench_gyousyu = bench_gyousyu,
            display_type = analysis_displaytype,
            rendering_data = res
          )
          
          filename_xlsx <- str_c(adept1,"_",adept2,".xlsx")
          invalid_pattern <- "[<>:\"/\\\\|?*]|[\\x00-\\x1F]"
          output_path <- file.path(temp_report_dir, str_remove_all(filename_xlsx, invalid_pattern))
          
          
          #TODO: Add info panel for rendering report
          make_excel_report(report_params, output_path, progress2)
          
          progress2$close()
        }
     
        
        # 元の作業ディレクトリを記憶
        owd <- getwd()
        # 関数を抜け出すときに必ず元のディレクトリに戻るように設定
        on.exit(setwd(owd), add = TRUE) # add = TRUEで既存のon.exit(progress$close())を上書きしない
        
        # 専用の一時フォルダに作業ディレクトリを移動
        setwd(temp_report_dir)
        
        # フォルダ内にあるすべてのファイルを取得
        files_to_zip <- list.files(full.name=TRUE)
        
        # zipコマンドを実行
        # zipfileにはShinyから渡された絶対パス`file`を指定
        # filesにはカレントディレクトリのファイルリストを指定
        zip::zipr(zipfile = file, files = files_to_zip)
      }
    )
  })
}

