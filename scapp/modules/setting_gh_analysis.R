#type: gh
ghsetting <- list(
  basic = list(
    type = "gh",
    select_these = c("受検人数","不完全回答人数","高ストレス者人数","高ストレス者割合"),
    plot_this    = c("高ストレス者割合"),
    percent_this = c("高ストレス者割合")
  ),
  skrisk = list(
    type = "gh",
    select_these = c("総合健康リスク","total_risk_long", "total_risk_cross", "total_risk_old"),
    plot_this    = c("総合健康リスク"),
    percent_this = NULL
  ),
  hantei_hutan = list(
    type = "hanteizu",
    select_these = c("demand","control"),
    name_mapper = c("仕事の負担" = "demand", "仕事のコントロール" = "control"),
    plot_this    = NULL,
    percent_this = NULL
  ),
  hantei_sien = list(
    type = "hanteizu",
    select_these = c("boss_support","fellow_support"),
    name_mapper = c("上司の支援" = "boss_support","同僚の支援" = "fellow_support"),
    plot_this    = NULL,
    percent_this = NULL
  ),
  outcome_we = list(
    type = "gh_prec",
    select_these = c("ワークエンゲイジメント"),
    plot_this    = c("ワークエンゲイジメント"),
    percent_this = NULL
  ), 
  outcome_sc = list(
    type = "gh_prec",
    select_these = c("ソーシャル・キャピタル"),
    plot_this    = c("ソーシャル・キャピタル"),
    percent_this = NULL
  ),
  outcome_harass = list(
    type = "gh_prec",
    select_these = c("職場のハラスメント"),
    plot_this    = c("職場のハラスメント"),
    percent_this = NULL
  ),
  outcome_stress_reaction =list(
    type = "gh",
    select_these = c("心理的ストレス反応合計", "活気","イライラ感","疲労感","不安感","抑うつ感"),
    plot_this    = c("心理的ストレス反応合計"),
    percent_this = NULL
  ),
  outcome_work_hutan = list(
    type = "gh",
    select_these = c("仕事の負担合計","仕事の量的負担","仕事の質的負担","身体的負担度","職場での対人関係","職場環境","情緒的負担","役割葛藤","WSB（－）"),
    plot_this    = c("仕事の負担合計"),
    percent_this = NULL
  ),
  sigen_sagyou = list(
    type = "gh",
    select_these = c("作業レベル資源合計","仕事のコントロール","技能の活用","仕事の適正","仕事の意義","役割明確さ","成長の機会"),
    plot_this    = c("作業レベル資源合計"),
    percent_this = NULL
  ),
  sigen_busyo = list(
    type = "gh",
    select_these = c("部署レベル資源合計","上司の支援","同僚の支援","経済・地位報酬","尊重報酬","安定報酬","上司のリーダーシップ","上司の公正な態度","ほめてもらえる職場","失敗を認める職場"),
    plot_this    = c("部署レベル資源合計"),
    percent_this = NULL
  ),
  sigen_jigyouba = list(
    type = "gh",
    select_these = c("事業場レベル資源","経営層との信頼関係","変化への対応","個人の尊重","公正な人事評価","多様な労働者への対応","キャリア形成","WSB（＋）"),
    plot_this    = c("事業場レベル資源"),
    percent_this = NULL
  ),
  na_else = list(
    type = "h",
    select_these = c("家族・友人の支援","家庭の満足度"),
    plot_this    = NULL,
    percent_this = NULL
  )
)
