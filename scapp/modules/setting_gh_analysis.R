ghsetting <- list(
  basic = list( #1
    bunrui = "基本",
    name = "基本",
    type = "ghbase",
    select_these = c("受検人数","不完全回答人数","高ストレス者人数","高ストレス者割合"),
    plot_this    = c("高ストレス者割合"),
    percent_this = c("高ストレス者割合")
  ),
  skrisk = list( #2
    bunrui = "基本",
    name = "総合健康リスク",
    type = "ghbase",
    select_these = c("総合健康リスク","total_risk_long", "total_risk_cross", "total_risk_old"),
    name_mapper = c("総合健康リスク"="総合健康リスク","総合健康リスク(縦断)"="total_risk_long","総合健康リスク(横断)" = "total_risk_cross","総合健康リスク(旧)" = "total_risk_old"),
    plot_this    = c("総合健康リスク"),
    percent_this = NULL
  ),
  hantei_hutan = list( #3
    bunrui = "基本",
    name = "ストレス判定図(仕事の負担とコントロール)",
    type = "hanteizu",
    select_these = c("demand","control"),
    graph_labels = c("良い←仕事の負担→悪い" = "demand", "悪い←仕事のコントロール→良い" = "control"),
    name_mapper = c("仕事の負担" = "demand", "仕事のコントロール" = "control"),
    plot_this    = NULL,
    percent_this = NULL
  ),
  hantei_sien = list( #4
    bunrui = "基本",
    name = "ストレス判定図(上司と同僚の支援)",
    type = "hanteizu",
    select_these = c("boss_support","fellow_support"),
    graph_labels = c("悪い←上司の支援→良い" = "boss_support", "悪い←同僚の支援→良い" = "fellow_support"),
    name_mapper = c("上司の支援" = "boss_support", "同僚の支援" = "fellow_support"),
    plot_this    = NULL,
    percent_this = NULL
  ),
  outcome_we = list( #5
    bunrui = "アウトカム",
    name = "ワークエンゲージメント",
    type = "gh",
    select_these = c("ワークエンゲイジメント"),
    plot_this    = c("ワークエンゲイジメント"),
    percent_this = NULL,
    questions = c(79, 80)
  ), 
  outcome_sc = list(
    bunrui = "アウトカム",
    name = "ソーシャルキャピタル",
    type = "gh",
    select_these = c("ソーシャル・キャピタル"),
    plot_this    = c("ソーシャル・キャピタル"),
    percent_this = NULL,
    questions = c(78)
  ),
  outcome_harass = list(
    bunrui = "アウトカム",
    name = "職場のハラスメント",
    type = "gh",
    select_these = c("職場のハラスメント"),
    plot_this    = c("職場のハラスメント"),
    percent_this = NULL,
    questions = c(77)
  ),
  outcome_stress_reaction =list(
    bunrui = "アウトカム",
    name = "心理的ストレス反応",
    type = "gh",
    select_these = c("心理的ストレス反応合計", "活気","イライラ感","疲労感","不安感","抑うつ感"),
    plot_this    = c("心理的ストレス反応合計"),
    percent_this = NULL,
    questions = c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35)
  ),
  outcome_work_hutan = list(
    bunrui = "アウトカム",
    name = "仕事の負担",
    type = "gh",
    select_these = c("仕事の負担合計","仕事の量的負担","仕事の質的負担","身体的負担度","職場での対人関係","職場環境","情緒的負担","役割葛藤","WSB（－）"),
    plot_this    = c("仕事の負担合計"),
    percent_this = NULL,
    questions = c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35)
  ),
  sigen_sagyou = list(
    bunrui = "資源",
    name = "作業レベル資源",
    type = "gh",
    select_these = c("作業レベル資源合計","仕事のコントロール","技能の活用","仕事の適正","仕事の意義","役割明確さ","成長の機会"),
    plot_this    = c("作業レベル資源合計"),
    percent_this = NULL,
    questions = c(8, 9, 10, 11, 16, 17, 60, 61)
  ),
  sigen_busyo = list(
    bunrui = "資源",
    name = "部署レベル資源",
    type = "gh",
    select_these = c("部署レベル資源合計","上司の支援","同僚の支援","経済・地位報酬","尊重報酬","安定報酬","上司のリーダーシップ","上司の公正な態度","ほめてもらえる職場","失敗を認める職場"),
    plot_this    = c("部署レベル資源合計"),
    percent_this = NULL,
    questions = c(47, 48, 50, 51, 53, 54, 62, 63, 64, 65, 66, 67, 68)
  ),
  sigen_jigyouba = list(
    bunrui = "資源",
    name = "事業場レベル資源",
    type = "gh",
    select_these = c("事業場レベル資源","経営層との信頼関係","変化への対応","個人の尊重","公正な人事評価","多様な労働者への対応","キャリア形成","WSB（＋）"),
    plot_this    = c("事業場レベル資源"),
    percent_this = NULL,
    questions = c(69, 70, 71, 72, 73, 74, 76)
  
  ),
  na_else = list(
    bunrui = "その他",
    name = "その他",
    type = "h",
    select_these = c("家族・友人の支援","家庭の満足度"),
    plot_this    = NULL,
    percent_this = NULL,
    questions = c(36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 49, 52, 55, 56, 57)
  )
)
