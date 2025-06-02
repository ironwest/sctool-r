# test-wizard_module.R (例)
library(shiny)
library(testthat)
library(jsonlite) # 設定ファイルの読み書きをテストする場合に必要
library(DT) # dataTableOutputのテストで必要になる場合がある

# --- モジュール定義を読み込むか、ここに直接記述 ---
source("./scapp/modules/wizard_module.R")

# (前の回答で提供された wizard_module_ui と wizard_module_server のコードがある前提)


# モジュールIDとラベル
# --- テストの記述 ---
test_that("ウィザードモジュールが期待通りに動作する", {
  
  # モジュールIDとラベル
  test_module_id <- "test_wizard"
  test_year_label <- "テスト年度"
  
  testServer(wizard_module_server, args = list(id = test_module_id, year_label = test_year_label), {
    # `session` オブジェクトは testServer によって提供されます
    # `input`, `output` も同様です
    
    # --- 初期状態の確認 ---
    expect_equal(rv$wizard_step, 1, info = "初期ステップは1であるべき")
    expect_true(output$wizard_show_step1, info = "初期状態でステップ1が表示されるべき")
    expect_false(output$wizard_show_step2, info = "初期状態でステップ2は非表示であるべき")
    expect_false(output$wizard_show_step3, info = "初期状態でステップ3は非表示であるべき")
    
    # --- ステップ1: CSVアップロードのシミュレーション ---
    # ダミーのCSVファイルを作成
    dummy_csv_path <- tempfile(fileext = ".csv")
    dummy_df <- data.frame(
      ID = 1:3,
      年齢 = c(20, 30, 40),
      性別 = c("男性", "女性", "男性"),
      Q1 = c(1, 2, 3),
      Q2 = c(4, 3, 2)
    )
    write.csv(dummy_df, dummy_csv_path, row.names = FALSE, fileEncoding="CP932") # fileEncodingを合わせる
    
    # fileInputをシミュレート
    session$setInputs(csv_file_input = list(
      name = "dummy.csv",
      size = file.size(dummy_csv_path),
      type = "text/csv",
      datapath = dummy_csv_path
    ))
    
    # ファイル読み込み後の状態を確認
    expect_equal(nrow(rv$csv_data), 3, info = "CSVデータの行数が正しいべき")
    expect_equal(rv$csv_headers, colnames(dummy_df), info = "CSVヘッダーが正しいべき")
    expect_true(grepl("ファイル読み込み成功", output$csv_upload_status_text), info = "アップロード成功メッセージが表示されるべき")
    
    # 「次へ」ボタンクリックをシミュレート
    session$setInputs(goto_step2_button = 1) # ボタンクリックは数値でカウントアップ
    expect_equal(rv$wizard_step, 2, info = "ステップ2に遷移するべき")
    expect_false(output$wizard_show_step1, info = "ステップ1が非表示になるべき")
    expect_true(output$wizard_show_step2, info = "ステップ2が表示されるべき")
    
    
    # --- ステップ2: 列名マッピングのシミュレーション ---
    # map_age_columnなどのselectInputの選択をシミュレート
    # (これらのselectInputのchoicesは、CSV読み込み時にupdateSelectInputで設定される前提)
    Sys.sleep(0.1) # updateSelectInputが反映されるのを少し待つ (状況による)
    session$setInputs(map_age_column = "年齢")
    session$setInputs(map_gender_column = "性別")
    # (同様に他のマッピングも設定)
    # NBJSQの列マッピング (Q1のみテスト)
    session$setInputs(map_nbjsq_q1 = "Q1")
    
    
    # 設定保存ボタンのテスト (downloadHandler)
    # downloadHandlerのテストは、直接ファイルが生成されるわけではなく、
    # ハンドラ関数が正しいデータで呼び出されるかを確認するのが一般的。
    # もしくは、downloadHandler内でrvに保存するロジックがあれば、そのrvの値を検証。
    # ここでは、downloadHandlerのテストは省略し、次のステップへの遷移にフォーカス。
    
    # 列マッピング設定のrvへの保存と「次へ」ボタンクリックをシミュレート
    session$setInputs(goto_step3_button = 1)
    expect_equal(rv$wizard_step, 3, info = "ステップ3に遷移するべき")
    expect_equal(rv$column_map_age, "年齢", info = "年齢の列マッピングがrvに保存されるべき")
    expect_equal(rv$column_map_gender, "性別", info = "性別の列マッピングがrvに保存されるべき")
    # expect_equal(rv$column_map_nbjsq[["q1"]], "Q1", info = "Q1の列マッピングがrvに保存されるべき")
    
    
    # --- ステップ3: 値マッピングのシミュレーション ---
    # gender_val_map_main_ui が表示され、その中のselectInputを操作
    # (このUIはrv$column_map_genderが設定された後に正しく表示される)
    Sys.sleep(0.1) # renderUIの反映を待つ
    
    # (このテストケースでは、get_unique_values_from_mapped_columnが正しく"男性","女性"を返す想定)
    # (そのため、gender_val_map_main_ui内のselectInputのchoicesにこれらが含まれる)
    session$setInputs(val_map_gender_male = "男性")
    session_val_map_gender_female = "女性"
    
    
    # NBJSQ一括設定のシミュレーション
    # (bulk_ui内のselectInputを操作)
    # (このテストケースでは、q1_col_nameから取得したユニーク値に "1", "2", "3", "4" が含まれる想定)
    session$setInputs(val_map_nbjsq_bulk_aefgh_1 = "4") # "そうだ" に対応するCSV値を "4" とする
    session$setInputs(val_map_nbjsq_bulk_aefgh_2 = "3") # "まあそうだ" に対応するCSV値を "3" とする
    session$setInputs(val_map_nbjsq_bulk_aefgh_3 = "2") # "ややちがう" に対応するCSV値を "2" とする
    session$setInputs(val_map_nbjsq_bulk_aefgh_4 = "1") # "ちがう" に対応するCSV値を "1" とする
    session$setInputs(bulk_set_aefgh_button = 1) # 一括設定ボタンクリック
    
    # 一括設定の結果がrvに反映されているか確認 (例: Q1に対して)
    # expect_equal(rv$value_map_nbjsq_individual[["q1"]], as.list(c("4", "3", "2", "1")), info = "Q1の値マッピング(AEFGH)が一括設定されるべき")
    # ↑ as.list() の挙動とrv$value_map_nbjsq_individualの構造を合わせる必要あり。
    # rv$value_map_nbjsq_individual[["q1"]] が list("4", "3", "2", "1") となることを期待。
    
    # 「完了」ボタンクリック
    session$setInputs(finish_setup_button = 1)
    # (完了時のモーダル表示やrv$wizard_stepの変更などをテスト)
    
    
    # --- モジュールの返り値のテスト ---
    # (wizard_module_serverが返すリスト内のリアクティブな値をテスト)
    # final_col_map <- module_return_values$get_column_map() # module_return_values は testServer の返り値ではない
    # wizard_module_server内で定義した return() の内容を取得する方法が必要
    # testServerのexpr内で直接アクセスするか、session$getReturned() を使う (後述)
    
    # testServerは最後に評価された式を返す。モジュールの返り値をテストするには、
    # それをtestServerのexprの最後に置くか、session$getReturned() を使用する。
    # session$getReturned() は Shiny 1.6.0 以降で利用可能。
    
    # 例: モジュールの返り値を期待通りに取得できるか (Shiny 1.6.0+ の場合)
    # returned_values <- session$getReturned()
    # expect_equal(returned_values$get_column_map()$age, "年齢", info = "モジュールの返り値(年齢列)が正しい")
    # expect_equal(returned_values$get_value_map()$gender$male, "男性", info = "モジュールの返り値(男性の値)が正しい")
    
    
    # 一時ファイルを削除
    unlink(dummy_csv_path)
  })
})
