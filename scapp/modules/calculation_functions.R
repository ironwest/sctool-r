library(tidyverse)

calculate_scores <- function(d){
  
  qmapper <- read_csv("nbjsq_question_text.csv")
  
  nbjsqscore <- qmapper |> 
    select(qnum, is_reverse)
  
  #一時的なIDを付与
  d <- d |> 
    mutate(tempid = 1:n(), .before=1)
  
  #選択した結果を得点に反映
  tempd_score <- d |> 
    select(tempid, matches("^q")) |> 
    pivot_longer(!tempid) |> 
    mutate(qnum = as.numeric(str_extract(name,"\\d+"))) |> 
    left_join(nbjsqscore, by=c("qnum")) |> 
    mutate(score = case_when(
      is_reverse == 0 ~ value, 
      is_reverse == 1 ~ 5 - value
    )) |> 
    mutate(name = str_c(name,"_score")) |> 
    select(tempid, name, qnum, score) 
  
  #尺度を計算
  syakudo_minor_group <- qmapper |> select(qnum, syakudo_minor, syakudo_minor_eng)
  syakudo_min_score <- tempd_score |> 
    left_join(syakudo_minor_group, by="qnum") |> 
    group_by(tempid, syakudo_minor_eng) |> 
    summarise(score = mean(score, na.rm=FALSE), .groups = "drop")
  
  
  syakudo_major_group <- qmapper |> select(syakudo_minor_eng, syakudo_major_eng) |> 
    filter(syakudo_major_eng %in% c("w_total","s_total","j_total","b_total","p_total")) |> 
    distinct()
  
  syakudo_maj_score <- syakudo_min_score |> 
    left_join(syakudo_major_group, by=c("syakudo_minor_eng")) |> 
    filter(!is.na(syakudo_major_eng)) |> 
    group_by(tempid, syakudo_major_eng) |> 
    summarise(score = mean(score, na.rm=FALSE), .groups = "drop")
  
  #ここまで計算したスコアや尺度を縦持ちから横持ちに直して、元のデータに結合する
  scoredata <- bind_rows(
    tempd_score |> select(tempid, name, score),
    syakudo_min_score |> select(tempid, name = syakudo_minor_eng, score),
    syakudo_maj_score |> select(tempid, name = syakudo_major_eng, score)
  ) |> 
   pivot_wider(id_cols = tempid, names_from = name, values_from  =score)
  
  dfin <- d |> 
    left_join(scoredata, by="tempid") |> 
    mutate(age_kubun = case_when(
      between(age,0 ,19) ~ "10代",
      between(age,20,29) ~ "20代",
      between(age,30,39) ~ "30代",
      between(age,40,49) ~ "40代",
      between(age,50,59) ~ "50代",
      between(age,60,999) ~ "60代以上",
      TRUE ~ NA_character_
    ), .after = age)
  
  
  return(dfin)
}

# calculate_scores(d)
#' 
#'   
#'   nbjsq2 <- d |> 
#'     left_join(nbjsqscore, by="nbjsq") |> 
#'     mutate(score = case_when(
#'       good_nbjsq == 4 ~ value,
#'       good_nbjsq == 1 ~ 5 - value
#'     )) |> 
#'     select(empid, nbjsq, score)
#'   
#'   #尺度の計算を行うための設定データncalc1, ncalc2を利用して尺度計算を行う 
#'   syakudo <- nbjsq2 |>
#'     left_join(ncalc1, by=c("nbjsq"="q"))
#'   
#'   
#'   syakudo <- syakudo |> 
#'     group_by(empid, name1) |>
#'     summarise(syakudo_score = mean(score, na.rm=TRUE)) |> 
#'     ungroup()
#'   
#'   
#'   syakudo_grp <- syakudo |> 
#'     left_join(ncalc2,by=c("name1")) |> 
#'     filter(!is.na(name2)) |> 
#'     group_by(empid, name2) |> 
#'     summarise(syakudo_score = mean(syakudo_score, na.rm=TRUE)) |> 
#'     rename(name1 = name2)
#'   
#'   #尺度をもとに偏差値を計算する
#'   hensati_data <- read_csv("./table11.csv")
#'   
#'   hensati_data |> 
#'     filter(sheet == "全体", qtype == "NBJSQ") |> 
#'     pull(`尺度名`) |> 
#'     clipr::write_clip()
#'   
#'   syakudo_grp |> ungroup() |> 
#'     select(name1) |> distinct() |> pull(name1) |> clipr::write_clip()
#'   
#'   syakudo |> ungroup() |> 
#'     select(name1) |> distinct() |> pull(name1) |> clipr::write_clip()
#'   
#'   mapping_table <- tribble(
#'     ~table11, ~syakudoname,
#'     "仕事の負担" , "仕事の負担合計",
#'     "心理的な仕事の負担（量）" , "仕事の量的負担",
#'     "心理的な仕事の負担（質）" , "仕事の質的負担",
#'     "自覚的な身体的負担度" , "身体的負担度",
#'     "職場の対人関係でのストレス" , "職場での対人関係",
#'     "職場環境によるストレス" , "職場環境",
#'     "情緒的負担*" , "情緒的負担",
#'     "役割葛藤*" , "役割葛藤",
#'     "ワーク・セルフ・バランス（N）*" , "WSB（－）",
#'     "仕事の資源（作業レベル）" , "作業レベル資源合計",
#'     "仕事のコントロール度" , "仕事のコントロール",
#'     "技能の活用度" , "技能の活用",
#'     "仕事の適性度" , "仕事の適正",
#'     "働きがい（仕事の意義）" , "仕事の意義",
#'     "役割明確さ*" , "役割明確さ",
#'     "成長の機会*" , "成長の機会",
#'     "仕事の資源（部署レベル）†" , "部署レベル資源合計",
#'     "仕事の資源（部署レベル）‡" , "部署レベル資源合計-家族サポート含む",
#'     "上司からのサポート" , "上司の支援",
#'     "同僚からのサポート" , "同僚の支援",
#'     "家族・友人からのサポート" , "家族・友人の支援",
#'     "経済・地位報酬*" , "経済・地位報酬",
#'     "尊重報酬*" , "尊重報酬",
#'     "安定報酬*" , "安定報酬",
#'     "上司のリーダーシップ*" , "上司のリーダーシップ",
#'     "上司の公正な態度*" , "上司の公正な態度",
#'     "ほめてもらえる職場*" , "ほめてもらえる職場",
#'     "失敗を認める職場*" , "失敗を認める職場",
#'     "仕事の資源（事業場レベル）" , "事業場レベル資源",
#'     "経営層との信頼関係*" , "経営層との信頼関係",
#'     "変化への対応*" , "変化への対応",
#'     "個人の尊重*" , "個人の尊重",
#'     "公正な人事評価*" , "公正な人事評価",
#'     "多様な労働者への対応*" , "多様な労働者への対応",
#'     "キャリア形成*" , "キャリア形成",
#'     "ワーク・セルフ・バランス（P）*" , "WSB（＋）",
#'     "アウトカム（心理的ストレス反応）§" , "心理的ストレス反応合計",
#'     "活気" , "活気",
#'     "イライラ感" , "イライラ感",
#'     "疲労感" , "疲労感",
#'     "不安感" , "不安感",
#'     "抑うつ感" , "抑うつ感",
#'     "身体愁訴" , "身体愁訴",
#'     "仕事の満足度" , "仕事の満足度",
#'     "家庭の満足度" , "家庭の満足度",
#'     "職場のハラスメント*" , "職場のハラスメント",
#'     "職場の一体感*" , "ソーシャル・キャピタル",
#'     "ワーク・エンゲイジメント*" , "ワークエンゲージメント"
#'   )
#'   
#'   
#'   
#'   
#'   hensaticalcdata <- hensati_data |> 
#'     filter(sheet == "全体", qtype == "NBJSQ") |> 
#'     select(table11 = `尺度名`,mean =`平均値`,sd=`標準偏差`) |> 
#'     left_join(mapping_table,by="table11") |> 
#'     select(syakudoname, mean, sd)
#'   
#'   syakudo_scores <- bind_rows(syakudo_grp, syakudo)
#'   
#'   hensati_group_data <- base |> 
#'     left_join(syakudo_scores,by="empid") |> 
#'     filter(!is.na(syakudo_score)) |> 
#'     group_by(grp,name1) |> 
#'     summarise(score = mean(syakudo_score)) |> 
#'     ungroup() |> 
#'     left_join(hensaticalcdata, by=c("name1" = "syakudoname")) |> 
#'     mutate(hensati = 50 + 10*(score - mean)/sd) |> 
#'     select(grp, name1, hensati)
#'   
#'   hensati_overall_data　<- base |> 
#'     left_join(syakudo_scores,by="empid") |> 
#'     filter(!is.na(syakudo_score)) |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp,name1) |> 
#'     summarise(score = mean(syakudo_score)) |> 
#'     ungroup() |> 
#'     left_join(hensaticalcdata, by=c("name1" = "syakudoname")) |> 
#'     mutate(hensati = 50 + 10*(score - mean)/sd) |> 
#'     select(grp, name1, hensati)
#'   
#'   
#'   hensati_hyou_data <- bind_rows(hensati_overall_data,hensati_group_data) |> 
#'     rename(value = hensati)
#'   
#'   
#'   #ハラスメントの設問を入手
#'   haras <- aton |> 
#'     filter(str_detect(`CSV列名`,"ハラスメント")) |> 
#'     select(csv = `CSV列名`, cat = Cat3)
#'   
#'   haras_data <- arm2 |>  select(empid, matches("ハラスメント")) |> 
#'     pivot_longer(!empid) |> 
#'     left_join(haras,by=c("name"="csv")) |> 
#'     select(empid, name1 = cat, syakudo_score = value)
#'   
#'   harasment_group_data <- base |> 
#'     left_join(haras_data, by="empid") |> 
#'     group_by(grp, name1) |> 
#'     count(syakudo_score) |> 
#'     filter(!is.na(syakudo_score)) |> 
#'     pivot_wider(id_cols = c(grp,name1), names_from = syakudo_score, values_from = n, values_fill = 0) |> 
#'     ungroup() |> 
#'     mutate(present = (`3`+`4`)/(`1`+`2`+`3`+`4`)) |> 
#'     select(grp, name1, value = present)
#'   
#'   harasment_oa_data <- base |> 
#'     left_join(haras_data, by="empid") |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp, name1) |> 
#'     count(syakudo_score) |> 
#'     filter(!is.na(syakudo_score)) |> 
#'     pivot_wider(id_cols = c(grp,name1), names_from = syakudo_score, values_from = n, values_fill = 0) |> 
#'     ungroup() |> 
#'     mutate(present = (`3`+`4`)/(`1`+`2`+`3`+`4`)) |> 
#'     select(grp, name1, value = present)
#'   
#'   harasment_hyou_data <- bind_rows(harasment_oa_data, harasment_group_data)
#'   
#'   #resでアウトカム以降は全部計算可能
#'   #resから高ストレス者、総合健康リスクを計算する必要がある。
#'   
#'   #高ストレス者計算
#'   areadat <- tibble(
#'     area = c(rep("A",length(1:17)), rep("B",length(18:46)), rep("C",length(47:55))),
#'     q    = c(1:17                 , 18:46                 , 47:55)
#'   )
#'   
#'   #ここで、高ストレス者計算は悪いほど得点が高い。24年度公開の表は
#'   #良好程得点が高い形になっているため、再度、高ストレス判定用にscoreを計算しなおす必要がある
#'   
#'   hsdata <- nbjsq |> 
#'     left_join(nbjsqscore, by="nbjsq") |> 
#'     mutate(score = case_when(
#'       good_nbjsq == 4 ~ 5- value,
#'       good_nbjsq == 1 ~ value
#'     )) |> 
#'     select(empid, nbjsq, score) |> 
#'     filter(between(nbjsq,1,55)) |> 
#'     left_join(areadat,by=c("nbjsq"="q")) |> 
#'     group_by(empid, area) |> 
#'     summarise(totalscore = sum(score)) |> 
#'     pivot_wider(id_cols = empid, names_from = area, values_from = totalscore) |> 
#'     mutate(isHS = case_when(
#'       B >= 77 ~ "HS",
#'       A + C >= 76 & B >= 63 ~ "HS",
#'       TRUE ~ "nonHS"
#'     )) |> 
#'     ungroup()
#'   
#'   hasjointhis <- base |> 
#'     left_join(hsdata, by="empid") |> 
#'     group_by(grp) |> 
#'     summarise(n = n(), isHS = sum(isHS == "HS"), percHS = isHS/n) |> 
#'     pivot_longer(cols = !grp) |> 
#'     rename(name1 = name) |> 
#'     mutate(name1 = case_when(
#'       name1 == "n" ~ "受検人数",
#'       name1 == "isHS" ~ "高ストレス者(人)",
#'       name1 == "percHS" ~ "高ストレス者(%)"
#'     ))
#'   
#'   hasjointhis_oa <- base |> 
#'     left_join(hsdata, by="empid") |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp) |> 
#'     summarise(n = n(), isHS = sum(isHS == "HS"), percHS = isHS/n) |> 
#'     pivot_longer(cols = !grp) |> 
#'     rename(name1 = name) |> 
#'     mutate(name1 = case_when(
#'       name1 == "n" ~ "受検人数",
#'       name1 == "isHS" ~ "高ストレス者(人)",
#'       name1 == "percHS" ~ "高ストレス者(%)"
#'     ))
#'   
#'   
#'   hs_hyou <- bind_rows(hasjointhis,hasjointhis_oa)
#'   
#'   
#'   #総合健康リスク計算（北里大学24年度データを利用）
#'   # 縦断解析
#'   # p/1-p
#'   # = exp (𝛽0 + 𝛽1 ∗ (仕事の量的負担) + 
#'   #          𝛽2∗ (仕事のコントロール) + 
#'   #          𝛽3∗ (上司の支援) + 
#'   #          𝛽4 ∗ (同僚の支援) + 
#'   #          𝛽5∗ (前年度高ストレス者割合) + 
#'   #          𝛽6∗ (年代平均) + 
#'   #          𝛽7 ∗ (女性比率）
#'   
#'   
#'   #総合健康リスクは平均ではなく、合計点の部署ごとの計算を行うため、それようの計算を行う
#'   #なお、ここでの計算にはすべて「逆転した点数の総和」を用いることに注意が必要
#'   
#'   sumscore <- nbjsq |> 
#'     mutate(score = 5- value) |> 
#'     filter(nbjsq %in% c(
#'       1, #vol
#'       2,
#'       3,
#'       8, #control
#'       9,
#'       10, 
#'       47,50,53, #boss,
#'       48,51,54 #fellow
#'     )) |> 　
#'     mutate(syakudogrp = factor(nbjsq, 
#'                                levels = c(1,2,3,8,9,10,47,50,53,48,51,54),
#'                                labels = c(rep("demand",3),rep("control",3),rep("boss_support",3),rep("fellow_support",3)))) |> 
#'     group_by(empid, syakudogrp) |> 
#'     filter(!is.na(score)) |> #ここでfilterをかけないとおかしい結果になる(回答していない人を割ってしまっている！)
#'     summarise(sumscore = sum(score)) |> ungroup()
#'   
#'   
#'   sc_averages_for_risk <- base |> 
#'     left_join(sumscore, by="empid") |> 
#'     group_by(grp, syakudogrp) |> 
#'     summarise(mean_score = mean(sumscore)) |> 
#'     ungroup() |> 
#'     pivot_wider(id_cols = grp, names_from = syakudogrp, values_from = mean_score)
#'   
#'   
#'   sc_averages_for_risk_overall <- base |> 
#'     left_join(sumscore, by="empid") |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp, syakudogrp) |> 
#'     summarise(mean_score = mean(sumscore)) |> 
#'     ungroup() |> 
#'     pivot_wider(id_cols = grp, names_from = syakudogrp, values_from = mean_score)
#'   
#'   input_data <- sc_averages_for_risk
#'   fixed_data <- read_csv("./riskcalc/fixed_data.csv")
#'   
#'   #' 総合健康リスク計算の関数
#'   #' 
#'   #' @param input_data ストレスチェック結果のデータフレーム（sc_averages_for_risk)
#'   #' @param fixed_data 業種別平均、係数データ（riskcalc/fixed_data.csvに収納)
#'   #' @param industry_name 対象業種名
#'   #' @return リスク結果計算を含むデータフレーム
#'   calculate_all_risks <- function(input_data, fixed_data, industry_name = "全体"){
#'     
#'     input_data <- input_data |> 
#'       mutate(across(where(is.numeric), ~ round(.x,3)))
#'     
#'     avg_vals <- fixed_data |> filter(industry == industry_name, type=="avg")
#'     coef_vals <- fixed_data |> filter(industry == industry_name, type=="coef")
#'     
#'     if(nrow(avg_vals) == 0 | nrow(coef_vals)==0) stop("指定された業種がfixed_dataに見当たりません")
#'     
#'     results <- input_data |> 
#'       mutate(
#'         #Vol-Control (RiskA)
#'         risk_A = floor(
#'           pmin(
#'             exp(
#'               ((demand - avg_vals$demand) * coef_vals$demand) + ((control - avg_vals$control) * coef_vals$control)
#'             ) * 100, 350)),
#'         
#'         #Support (RiskB)
#'         risk_B = floor(
#'           pmin(
#'             exp(
#'               ((boss_support - avg_vals$boss) * coef_vals$boss) + ((fellow_support - avg_vals$fellow) * coef_vals$fellow)
#'             ) * 100, 350)),
#'         
#'         #Total Risk
#'         total_risk = floor(risk_A * risk_B / 100),
#'         
#'         risk_A_old = floor(pmin(exp(((demand - 8.7) * 0.076) + (control - 8)*-0.089)*100, 350)),
#'         risk_B_old = floor(pmin(exp(((boss_support - 7.6) * -0.097) + (fellow_support - 8.1)*-0.097)*100, 350)),
#'         total_risk_old = floor(risk_A_old * risk_B_old / 100)
#'       )
#'     
#'     
#'     return(results)
#'     
#'     
#'   }
#'   
#'   total_risk_oa_hyou　 <- calculate_all_risks(sc_averages_for_risk_overall, fixed_data) |> 
#'     select(grp, total_risk)
#'   
#'   total_risk_data <- calculate_all_risks(sc_averages_for_risk, fixed_data) |> 
#'     select(grp, total_risk)
#'   
#'   total_risk_hyou <- bind_rows(total_risk_oa_hyou, total_risk_data) |> 
#'     mutate(name1 = "総合健康リスク") |> 
#'     select(grp,name1,value = total_risk)
#'   
#'   # 未受診者人数を算出-------------
#'   nas_hyou_grp <- nbjsq |> 
#'     group_by(empid) |> 
#'     summarise(nas = all(is.na(value))) |> 
#'     left_join(base, by=c("empid")) |> 
#'     group_by(grp) |> 
#'     summarise(nas = sum(nas))
#'   
#'   nas_hyou_oa <- nbjsq |> 
#'     group_by(empid) |> 
#'     summarise(nas = all(is.na(value))) |> 
#'     left_join(base, by=c("empid")) |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp) |> 
#'     summarise(nas = sum(nas))
#'   
#'   nas_hyou <- bind_rows(nas_hyou_oa, nas_hyou_grp) |> 
#'     mutate(name1 = "未受検者数(人)") |> 
#'     rename(value = nas) |> 
#'     relocate(grp,name1,value)
#'   
#'   
#'   #ここまでの結果を結合する
#'   fin <- bind_rows(
#'     hs_hyou,
#'     total_risk_hyou,
#'     harasment_hyou_data,
#'     hensati_hyou_data,
#'     nas_hyou
#'   )
#'   
#'   return(fin)
#'   
#' }
#' 
#' 
#' 
#' 
#' #単年度の使い捨てのツール
#' #ARMを前年度、今年度をNBJSQとするためのツール
#' #尚、今年度部分を流用して、前年度をBJSQにもできるようにプログラムは修正する
#' library(tidyverse)
#' 
#' convert_arm_to_nbjsq <- function(armdatapath, group_var){
#'   
#'   aton <- readxl::read_excel("設問との対応_ARMNBJSQ.xlsx")
#'   #単年度のARMの結果を読み込んで偏差値のデータに変更する関数
#'   arm <- read_csv(armdatapath) #"../SC2024/raw_202407290856.csv"
#'   ncalc1 <- readxl::read_excel("../scsetting/newBJSQ80.xlsx", sheet = "koumoku")
#'   ncalc2 <- readxl::read_excel("../scsetting/newBJSQ80.xlsx", sheet = "combscore")
#'   
#'   #group_var <- "組織名:第一階層"
#'   
#'   #まずatonでの変換表から、group_varを含んだ議事BJSQ結果を作成する
#'   #このとき、good_armとgood_nbjsqで「よい」とみなされる方向が逆な場合は、
#'   #good_nbjsqと合わせて作成する
#'   select_these <- aton |> 
#'     filter(!is.na(NBJSQ対応)) |> 
#'     pull(CSV列名)
#'   
#'   
#'   if(length(group_var) == 2){
#'     arm <- arm |> 
#'       mutate(grp = str_c(!!rlang::sym(group_var[1]),"_",!!rlang::sym(group_var[2])))
#'   }else if(length(group_var) == 3){
#'     arm <- arm |> 
#'       mutate(grp = str_c(!!rlang::sym(group_var[1]),"_",!!rlang::sym(group_var[2]),"_",!!rlang::sym(group_var[3])))
#'   }else if(length(group_var) == 4){
#'     arm <- arm |> 
#'       mutate(grp = str_c(!!rlang::sym(group_var[1]),"_",!!rlang::sym(group_var[2]),"_",!!rlang::sym(group_var[3]),"_",!!rlang::sym(group_var[4])))
#'   }else{
#'     arm <- arm |> 
#'       mutate(grp = !!rlang::sym(group_var))
#'   }
#'   
#'   arm2 <- arm |> 
#'     select(empid = `従業員番号`, sex = matches("性別"),grp, !!!rlang::syms(select_these))
#'   
#'   check_duplicate_answer <- arm2 |> select(empid) |> count(empid) |> filter(n > 1)
#'   
#'   if(nrow(check_duplicate_answer) > 0){
#'     print("ERROR! ADD LOGIC FOR REMOVE DUPLICATE")
#'   }
#'   
#'   base <- arm2 |> select(empid, grp, sex) |> 
#'     mutate(sex = factor(sex, levels = 1:2, labels = c("男性","女性")))
#'   
#'   #armをたてもちデータにする
#'   arm3 <- arm2 |> select(!c(grp,sex)) |> 
#'     pivot_longer(cols = !c(empid))
#'   
#'   
#'   #armをNBJSQの集計にするためにatonからのデータを結合する
#'   arm3_2 <- arm3 |> 
#'     left_join(
#'       aton |>select(csv = `CSV列名`, good_arm, nbjsq = NBJSQ対応, good_nbjsq),
#'       by = c("name" = "csv")
#'     )
#'   
#'   #ここで、valueが回答された値。good_armとgood_nbjsqが不一致の場合は、valueの値をひっくり返す
#'   arm4 <- arm3_2 |> 
#'     mutate(value = if_else(good_arm == good_nbjsq, value, 5 - value))
#'   
#'   #この結果を利用して疑似NBJSQのデータを作成、横持ちにする
#'   nbjsq <- arm4 |> 
#'     group_by(empid, nbjsq) |> 
#'     summarise(value = mean(value, na.rm=TRUE)) |> 
#'     ungroup()
#'   
#'   
#'   return(list(aton=aton, base = base, nbjsq = nbjsq, arm2 = arm2))
#' }
#' 
#' make_table_by_grp_from_nbjsq <- function(armdatas){
#'   
#'   aton <- armdatas$aton
#'   base <- armdatas$base #baseにグループを付ける
#'   nbjsq <- armdatas$nbjsq
#'   arm2 <- armdatas$arm2
#'   
#'   ncalc1 <- readxl::read_excel("../scsetting/newBJSQ80.xlsx", sheet = "koumoku")
#'   ncalc2 <- readxl::read_excel("../scsetting/newBJSQ80.xlsx", sheet = "combscore")
#'   #mbjsqの現在の値は尺度によって4が1点、であったり、4点であったりするので、一致させる
#'   nbjsqscore <- aton |> select(nbjsq = `NBJSQ対応`, good_nbjsq) |> distinct()
#'   
#'   nbjsq2 <- nbjsq |> 
#'     left_join(nbjsqscore, by="nbjsq") |> 
#'     mutate(score = case_when(
#'       good_nbjsq == 4 ~ value,
#'       good_nbjsq == 1 ~ 5 - value
#'     )) |> 
#'     select(empid, nbjsq, score)
#'   
#'   #尺度の計算を行うための設定データncalc1, ncalc2を利用して尺度計算を行う 
#'   syakudo <- nbjsq2 |>
#'     left_join(ncalc1, by=c("nbjsq"="q"))
#'   
#'   
#'   syakudo <- syakudo |> 
#'     group_by(empid, name1) |>
#'     summarise(syakudo_score = mean(score, na.rm=TRUE)) |> 
#'     ungroup()
#'   
#'   
#'   syakudo_grp <- syakudo |> 
#'     left_join(ncalc2,by=c("name1")) |> 
#'     filter(!is.na(name2)) |> 
#'     group_by(empid, name2) |> 
#'     summarise(syakudo_score = mean(syakudo_score, na.rm=TRUE)) |> 
#'     rename(name1 = name2)
#'   
#'   #尺度をもとに偏差値を計算する
#'   hensati_data <- read_csv("./table11.csv")
#'   
#'   hensati_data |> 
#'     filter(sheet == "全体", qtype == "NBJSQ") |> 
#'     pull(`尺度名`) |> 
#'     clipr::write_clip()
#'   
#'   syakudo_grp |> ungroup() |> 
#'     select(name1) |> distinct() |> pull(name1) |> clipr::write_clip()
#'   
#'   syakudo |> ungroup() |> 
#'     select(name1) |> distinct() |> pull(name1) |> clipr::write_clip()
#'   
#'   mapping_table <- tribble(
#'     ~table11, ~syakudoname,
#'     "仕事の負担" , "仕事の負担合計",
#'     "心理的な仕事の負担（量）" , "仕事の量的負担",
#'     "心理的な仕事の負担（質）" , "仕事の質的負担",
#'     "自覚的な身体的負担度" , "身体的負担度",
#'     "職場の対人関係でのストレス" , "職場での対人関係",
#'     "職場環境によるストレス" , "職場環境",
#'     "情緒的負担*" , "情緒的負担",
#'     "役割葛藤*" , "役割葛藤",
#'     "ワーク・セルフ・バランス（N）*" , "WSB（－）",
#'     "仕事の資源（作業レベル）" , "作業レベル資源合計",
#'     "仕事のコントロール度" , "仕事のコントロール",
#'     "技能の活用度" , "技能の活用",
#'     "仕事の適性度" , "仕事の適正",
#'     "働きがい（仕事の意義）" , "仕事の意義",
#'     "役割明確さ*" , "役割明確さ",
#'     "成長の機会*" , "成長の機会",
#'     "仕事の資源（部署レベル）†" , "部署レベル資源合計",
#'     "仕事の資源（部署レベル）‡" , "部署レベル資源合計-家族サポート含む",
#'     "上司からのサポート" , "上司の支援",
#'     "同僚からのサポート" , "同僚の支援",
#'     "家族・友人からのサポート" , "家族・友人の支援",
#'     "経済・地位報酬*" , "経済・地位報酬",
#'     "尊重報酬*" , "尊重報酬",
#'     "安定報酬*" , "安定報酬",
#'     "上司のリーダーシップ*" , "上司のリーダーシップ",
#'     "上司の公正な態度*" , "上司の公正な態度",
#'     "ほめてもらえる職場*" , "ほめてもらえる職場",
#'     "失敗を認める職場*" , "失敗を認める職場",
#'     "仕事の資源（事業場レベル）" , "事業場レベル資源",
#'     "経営層との信頼関係*" , "経営層との信頼関係",
#'     "変化への対応*" , "変化への対応",
#'     "個人の尊重*" , "個人の尊重",
#'     "公正な人事評価*" , "公正な人事評価",
#'     "多様な労働者への対応*" , "多様な労働者への対応",
#'     "キャリア形成*" , "キャリア形成",
#'     "ワーク・セルフ・バランス（P）*" , "WSB（＋）",
#'     "アウトカム（心理的ストレス反応）§" , "心理的ストレス反応合計",
#'     "活気" , "活気",
#'     "イライラ感" , "イライラ感",
#'     "疲労感" , "疲労感",
#'     "不安感" , "不安感",
#'     "抑うつ感" , "抑うつ感",
#'     "身体愁訴" , "身体愁訴",
#'     "仕事の満足度" , "仕事の満足度",
#'     "家庭の満足度" , "家庭の満足度",
#'     "職場のハラスメント*" , "職場のハラスメント",
#'     "職場の一体感*" , "ソーシャル・キャピタル",
#'     "ワーク・エンゲイジメント*" , "ワークエンゲージメント"
#'   )
#'   
#'   
#'   
#'   
#'   hensaticalcdata <- hensati_data |> 
#'     filter(sheet == "全体", qtype == "NBJSQ") |> 
#'     select(table11 = `尺度名`,mean =`平均値`,sd=`標準偏差`) |> 
#'     left_join(mapping_table,by="table11") |> 
#'     select(syakudoname, mean, sd)
#'   
#'   syakudo_scores <- bind_rows(syakudo_grp, syakudo)
#'   
#'   hensati_group_data <- base |> 
#'     left_join(syakudo_scores,by="empid") |> 
#'     filter(!is.na(syakudo_score)) |> 
#'     group_by(grp,name1) |> 
#'     summarise(score = mean(syakudo_score)) |> 
#'     ungroup() |> 
#'     left_join(hensaticalcdata, by=c("name1" = "syakudoname")) |> 
#'     mutate(hensati = 50 + 10*(score - mean)/sd) |> 
#'     select(grp, name1, hensati)
#'   
#'   hensati_overall_data　<- base |> 
#'     left_join(syakudo_scores,by="empid") |> 
#'     filter(!is.na(syakudo_score)) |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp,name1) |> 
#'     summarise(score = mean(syakudo_score)) |> 
#'     ungroup() |> 
#'     left_join(hensaticalcdata, by=c("name1" = "syakudoname")) |> 
#'     mutate(hensati = 50 + 10*(score - mean)/sd) |> 
#'     select(grp, name1, hensati)
#'   
#'   
#'   hensati_hyou_data <- bind_rows(hensati_overall_data,hensati_group_data) |> 
#'     rename(value = hensati)
#'   
#'   
#'   #ハラスメントの設問を入手
#'   haras <- aton |> 
#'     filter(str_detect(`CSV列名`,"ハラスメント")) |> 
#'     select(csv = `CSV列名`, cat = Cat3)
#'   
#'   haras_data <- arm2 |>  select(empid, matches("ハラスメント")) |> 
#'     pivot_longer(!empid) |> 
#'     left_join(haras,by=c("name"="csv")) |> 
#'     select(empid, name1 = cat, syakudo_score = value)
#'   
#'   harasment_group_data <- base |> 
#'     left_join(haras_data, by="empid") |> 
#'     group_by(grp, name1) |> 
#'     count(syakudo_score) |> 
#'     filter(!is.na(syakudo_score)) |> 
#'     pivot_wider(id_cols = c(grp,name1), names_from = syakudo_score, values_from = n, values_fill = 0) |> 
#'     ungroup() |> 
#'     mutate(present = (`3`+`4`)/(`1`+`2`+`3`+`4`)) |> 
#'     select(grp, name1, value = present)
#'   
#'   harasment_oa_data <- base |> 
#'     left_join(haras_data, by="empid") |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp, name1) |> 
#'     count(syakudo_score) |> 
#'     filter(!is.na(syakudo_score)) |> 
#'     pivot_wider(id_cols = c(grp,name1), names_from = syakudo_score, values_from = n, values_fill = 0) |> 
#'     ungroup() |> 
#'     mutate(present = (`3`+`4`)/(`1`+`2`+`3`+`4`)) |> 
#'     select(grp, name1, value = present)
#'   
#'   harasment_hyou_data <- bind_rows(harasment_oa_data, harasment_group_data)
#'   
#'   #resでアウトカム以降は全部計算可能
#'   #resから高ストレス者、総合健康リスクを計算する必要がある。
#'   
#'   #高ストレス者計算
#'   areadat <- tibble(
#'     area = c(rep("A",length(1:17)), rep("B",length(18:46)), rep("C",length(47:55))),
#'     q    = c(1:17                 , 18:46                 , 47:55)
#'   )
#'   
#'   #ここで、高ストレス者計算は悪いほど得点が高い。24年度公開の表は
#'   #良好程得点が高い形になっているため、再度、高ストレス判定用にscoreを計算しなおす必要がある
#'   
#'   hsdata <- nbjsq |> 
#'     left_join(nbjsqscore, by="nbjsq") |> 
#'     mutate(score = case_when(
#'       good_nbjsq == 4 ~ 5- value,
#'       good_nbjsq == 1 ~ value
#'     )) |> 
#'     select(empid, nbjsq, score) |> 
#'     filter(between(nbjsq,1,55)) |> 
#'     left_join(areadat,by=c("nbjsq"="q")) |> 
#'     group_by(empid, area) |> 
#'     summarise(totalscore = sum(score)) |> 
#'     pivot_wider(id_cols = empid, names_from = area, values_from = totalscore) |> 
#'     mutate(isHS = case_when(
#'       B >= 77 ~ "HS",
#'       A + C >= 76 & B >= 63 ~ "HS",
#'       TRUE ~ "nonHS"
#'     )) |> 
#'     ungroup()
#'   
#'   hasjointhis <- base |> 
#'     left_join(hsdata, by="empid") |> 
#'     group_by(grp) |> 
#'     summarise(n = n(), isHS = sum(isHS == "HS"), percHS = isHS/n) |> 
#'     pivot_longer(cols = !grp) |> 
#'     rename(name1 = name) |> 
#'     mutate(name1 = case_when(
#'       name1 == "n" ~ "受検人数",
#'       name1 == "isHS" ~ "高ストレス者(人)",
#'       name1 == "percHS" ~ "高ストレス者(%)"
#'     ))
#'   
#'   hasjointhis_oa <- base |> 
#'     left_join(hsdata, by="empid") |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp) |> 
#'     summarise(n = n(), isHS = sum(isHS == "HS"), percHS = isHS/n) |> 
#'     pivot_longer(cols = !grp) |> 
#'     rename(name1 = name) |> 
#'     mutate(name1 = case_when(
#'       name1 == "n" ~ "受検人数",
#'       name1 == "isHS" ~ "高ストレス者(人)",
#'       name1 == "percHS" ~ "高ストレス者(%)"
#'     ))
#'   
#'   
#'   hs_hyou <- bind_rows(hasjointhis,hasjointhis_oa)
#'   
#'   
#'   #総合健康リスク計算（北里大学24年度データを利用）
#'   # 縦断解析
#'   # p/1-p
#'   # = exp (𝛽0 + 𝛽1 ∗ (仕事の量的負担) + 
#'   #          𝛽2∗ (仕事のコントロール) + 
#'   #          𝛽3∗ (上司の支援) + 
#'   #          𝛽4 ∗ (同僚の支援) + 
#'   #          𝛽5∗ (前年度高ストレス者割合) + 
#'   #          𝛽6∗ (年代平均) + 
#'   #          𝛽7 ∗ (女性比率）
#'   
#'   
#'   #総合健康リスクは平均ではなく、合計点の部署ごとの計算を行うため、それようの計算を行う
#'   #なお、ここでの計算にはすべて「逆転した点数の総和」を用いることに注意が必要
#'   
#'   sumscore <- nbjsq |> 
#'     mutate(score = 5- value) |> 
#'     filter(nbjsq %in% c(
#'       1, #vol
#'       2,
#'       3,
#'       8, #control
#'       9,
#'       10, 
#'       47,50,53, #boss,
#'       48,51,54 #fellow
#'     )) |> 　
#'     mutate(syakudogrp = factor(nbjsq, 
#'                                levels = c(1,2,3,8,9,10,47,50,53,48,51,54),
#'                                labels = c(rep("demand",3),rep("control",3),rep("boss_support",3),rep("fellow_support",3)))) |> 
#'     group_by(empid, syakudogrp) |> 
#'     filter(!is.na(score)) |> #ここでfilterをかけないとおかしい結果になる(回答していない人を割ってしまっている！)
#'     summarise(sumscore = sum(score)) |> ungroup()
#'   
#'   
#'   sc_averages_for_risk <- base |> 
#'     left_join(sumscore, by="empid") |> 
#'     group_by(grp, syakudogrp) |> 
#'     summarise(mean_score = mean(sumscore)) |> 
#'     ungroup() |> 
#'     pivot_wider(id_cols = grp, names_from = syakudogrp, values_from = mean_score)
#'   
#'   
#'   sc_averages_for_risk_overall <- base |> 
#'     left_join(sumscore, by="empid") |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp, syakudogrp) |> 
#'     summarise(mean_score = mean(sumscore)) |> 
#'     ungroup() |> 
#'     pivot_wider(id_cols = grp, names_from = syakudogrp, values_from = mean_score)
#'   
#'   input_data <- sc_averages_for_risk
#'   fixed_data <- read_csv("./riskcalc/fixed_data.csv")
#'   
#'   #' 総合健康リスク計算の関数
#'   #' 
#'   #' @param input_data ストレスチェック結果のデータフレーム（sc_averages_for_risk)
#'   #' @param fixed_data 業種別平均、係数データ（riskcalc/fixed_data.csvに収納)
#'   #' @param industry_name 対象業種名
#'   #' @return リスク結果計算を含むデータフレーム
#'   calculate_all_risks <- function(input_data, fixed_data, industry_name = "全体"){
#'     
#'     input_data <- input_data |> 
#'       mutate(across(where(is.numeric), ~ round(.x,3)))
#'     
#'     avg_vals <- fixed_data |> filter(industry == industry_name, type=="avg")
#'     coef_vals <- fixed_data |> filter(industry == industry_name, type=="coef")
#'     
#'     if(nrow(avg_vals) == 0 | nrow(coef_vals)==0) stop("指定された業種がfixed_dataに見当たりません")
#'     
#'     results <- input_data |> 
#'       mutate(
#'         #Vol-Control (RiskA)
#'         risk_A = floor(
#'           pmin(
#'             exp(
#'               ((demand - avg_vals$demand) * coef_vals$demand) + ((control - avg_vals$control) * coef_vals$control)
#'             ) * 100, 350)),
#'         
#'         #Support (RiskB)
#'         risk_B = floor(
#'           pmin(
#'             exp(
#'               ((boss_support - avg_vals$boss) * coef_vals$boss) + ((fellow_support - avg_vals$fellow) * coef_vals$fellow)
#'             ) * 100, 350)),
#'         
#'         #Total Risk
#'         total_risk = floor(risk_A * risk_B / 100),
#'         
#'         risk_A_old = floor(pmin(exp(((demand - 8.7) * 0.076) + (control - 8)*-0.089)*100, 350)),
#'         risk_B_old = floor(pmin(exp(((boss_support - 7.6) * -0.097) + (fellow_support - 8.1)*-0.097)*100, 350)),
#'         total_risk_old = floor(risk_A_old * risk_B_old / 100)
#'       )
#'     
#'     
#'     return(results)
#'     
#'     
#'   }
#'   
#'   total_risk_oa_hyou　 <- calculate_all_risks(sc_averages_for_risk_overall, fixed_data) |> 
#'     select(grp, total_risk)
#'   
#'   total_risk_data <- calculate_all_risks(sc_averages_for_risk, fixed_data) |> 
#'     select(grp, total_risk)
#'   
#'   total_risk_hyou <- bind_rows(total_risk_oa_hyou, total_risk_data) |> 
#'     mutate(name1 = "総合健康リスク") |> 
#'     select(grp,name1,value = total_risk)
#'   
#'   # 未受診者人数を算出-------------
#'   nas_hyou_grp <- nbjsq |> 
#'     group_by(empid) |> 
#'     summarise(nas = all(is.na(value))) |> 
#'     left_join(base, by=c("empid")) |> 
#'     group_by(grp) |> 
#'     summarise(nas = sum(nas))
#'   
#'   nas_hyou_oa <- nbjsq |> 
#'     group_by(empid) |> 
#'     summarise(nas = all(is.na(value))) |> 
#'     left_join(base, by=c("empid")) |> 
#'     mutate(grp = "全体") |> 
#'     group_by(grp) |> 
#'     summarise(nas = sum(nas))
#'   
#'   nas_hyou <- bind_rows(nas_hyou_oa, nas_hyou_grp) |> 
#'     mutate(name1 = "未受検者数(人)") |> 
#'     rename(value = nas) |> 
#'     relocate(grp,name1,value)
#'   
#'   
#'   #ここまでの結果を結合する
#'   fin <- bind_rows(
#'     hs_hyou,
#'     total_risk_hyou,
#'     harasment_hyou_data,
#'     hensati_hyou_data,
#'     nas_hyou
#'   )
#'   
#'   return(fin)
#'   
#' }
#' 
#' 
#' make_table_setting<- function(){
#'   table_setting <- tribble(
#'     ~columngrp,~roworder, ~isround, ~type,
#'     "","grp",FALSE,"-",
#'     "","受検人数",FALSE,"人",
#'     "","未受検者数(人)",FALSE,"人",
#'     "","高ストレス者(人)",FALSE,"人",
#'     "","高ストレス者(%)",FALSE,"%",
#'     "","総合健康リスク",TRUE,"-",
#'     "アウトカム","ソーシャル・キャピタル",TRUE,"偏",
#'     "アウトカム","ワークエンゲージメント",TRUE,"偏",
#'     "アウトカム","心理的ストレス反応合計",TRUE,"偏",
#'     "アウトカム","身体愁訴",TRUE,"偏",
#'     "アウトカム","職場のハラスメント",TRUE,"偏",
#'     "負担・資源まとめ","仕事の負担合計",TRUE,"偏",
#'     "負担・資源まとめ","作業レベル資源合計",TRUE,"偏",
#'     "負担・資源まとめ","部署レベル資源合計",TRUE,"偏",
#'     "負担・資源まとめ","事業場レベル資源",TRUE,"偏",
#'     "仕事の負担","仕事の量的負担",TRUE,"偏",
#'     "仕事の負担","仕事の質的負担",TRUE,"偏",
#'     "仕事の負担","身体的負担度",TRUE,"偏",
#'     "仕事の負担","職場での対人関係",TRUE,"偏",
#'     "仕事の負担","職場環境",TRUE,"偏",
#'     "仕事の負担","情緒的負担",TRUE,"偏",
#'     "仕事の負担","役割葛藤",TRUE,"偏",
#'     "仕事の負担","WSB（－）",TRUE,"偏",
#'     "作業レベル資源","仕事のコントロール",TRUE,"偏",
#'     "作業レベル資源","仕事の適正",TRUE,"偏",
#'     "作業レベル資源","技能の活用",TRUE,"偏",
#'     "作業レベル資源","仕事の意義",TRUE,"偏",
#'     "作業レベル資源","役割明確さ",TRUE,"偏",
#'     "作業レベル資源","成長の機会",TRUE,"偏",
#'     "部署レベル資源","上司の支援",TRUE,"偏",
#'     "部署レベル資源","同僚の支援",TRUE,"偏",
#'     "部署レベル資源","経済・地位報酬",TRUE,"偏",
#'     "部署レベル資源","尊重報酬",TRUE,"偏",
#'     "部署レベル資源","安定報酬",TRUE,"偏",
#'     "部署レベル資源","上司のリーダーシップ",TRUE,"偏",
#'     "部署レベル資源","上司の公正な態度",TRUE,"偏",
#'     "部署レベル資源","ほめてもらえる職場",TRUE,"偏",
#'     "部署レベル資源","失敗を認める職場",TRUE,"偏",
#'     "事業場レベル資源","経営層との信頼関係",TRUE,"偏",
#'     "事業場レベル資源","変化への対応",TRUE,"偏",
#'     "事業場レベル資源","個人の尊重",TRUE,"偏",
#'     "事業場レベル資源","公正な人事評価",TRUE,"偏",
#'     "事業場レベル資源","多様な労働者への対応",TRUE,"偏",
#'     "事業場レベル資源","キャリア形成",TRUE,"偏",
#'     "事業場レベル資源","WSB（＋）",TRUE,"偏",
#'     "ハラスメント","上司からのハラスメント",FALSE,"%",
#'     "ハラスメント","同僚からのハラスメント",FALSE,"%",
#'     "ハラスメント","セクハラ",FALSE,"%",
#'     "ハラスメント","取引先からのハラスメント",FALSE,"%",
#'     "ハラスメント","環境型ハラスメント(上司)",FALSE,"%",
#'     "ハラスメント","環境型ハラスメント(同僚)",FALSE,"%",
#'     "ハラスメント","環境型ハラスメント(取引先・顧客)",FALSE,"%",
#'     "他","活気",TRUE,"偏",
#'     "他","イライラ感",TRUE,"偏",
#'     "他","不安感",TRUE,"偏",
#'     "他","抑うつ感",TRUE,"偏",
#'     "他","疲労感",TRUE,"偏",
#'     "他","仕事の満足度",TRUE,"偏",
#'     "他","家庭の満足度",TRUE,"偏",
#'     "他","家族・友人の支援"  ,TRUE,"偏"
#'   )
#' }
#' 
#' generate_hyou_from_fin <- function(fin, group_name){
#'   table_setting <- make_table_setting()
#'   
#'   numlessthan5 <- fin |> 
#'     filter(name1 == "受検人数") |> 
#'     filter(value < 5)
#'   
#'   #5人未満のgrpをNAに置き換える
#'   fin <- fin |> 
#'     mutate(remove_these = grp %in% numlessthan5$grp) |> 
#'     mutate(value = if_else(
#'       remove_these & name1 != "受検人数", 0, value 
#'     ))
#'   
#'   round_these <- table_setting |> filter(isround) |> pull(roworder)
#'   percent_these <- table_setting |> filter(type == "%") |> pull(roworder)
#'   
#'   
#'   hyou <- fin |> 
#'     mutate(roworder = if_else(grp == "全体", 0, 1)) |> 
#'     pivot_wider(id_cols = c(roworder,grp), names_from = name1, values_from = value) |> 
#'     arrange(roworder,grp) |> 
#'     relocate(!!!rlang::syms(table_setting$roworder)) |> 
#'     mutate(type = 1, .before = roworder) |> 
#'     mutate(across(round_these, round)) |> 
#'     mutate(across(percent_these, ~{100* . })) |> 
#'     #select(!c(type,roworder)) |> 
#'     rename(!!rlang::sym(group_name) := grp) #TODO：関数にするときにここはArgumentで置き換える
#'   
#'   return(hyou)
#' }
#' 
#' #group_setting <- c("組織名:第一階層","組織名:第二階層","組織名:第三階層","組織名:第四階層")
#' group_setting <- c("組織名:第一階層")
#' 
#' armdatas_current <- convert_arm_to_nbjsq("../SC2024/raw_202407290856.csv", group_setting)
#' fin_current <- make_table_by_grp_from_nbjsq(armdatas_current)
#' hyou_current <- generate_hyou_from_fin(fin_current,"grp")
#' 
#' armdatas_past <- convert_arm_to_nbjsq("../SC2023/raw_202407260910.csv", group_setting)
#' fin_past <- make_table_by_grp_from_nbjsq(armdatas_past)
#' hyou_past <- generate_hyou_from_fin(fin_past,"grp")
#' 
#' #単純に単年度の結果を見る表
#' make_table_single <- function(hyoufinal, desired_sheet_name, save_path){
#'   hyoufinal <- hyoufinal |> select(!c(type,roworder))
#'   table_setting <- make_table_setting()
#'   column_group <- table_setting$columngrp
#'   target_sheet <- desired_sheet_name
#'   
#'   library(openxlsx2)
#'   wb <- openxlsx2::wb_workbook()
#'   
#'   wb$add_worksheet(sheet = target_sheet)
#'   
#'   #1行目の書き込み
#'   wb$add_data(target_sheet,x=t(column_group),start_row=1,start_col = 1, col_names = FALSE)
#'   
#'   # 同じグループ名が連続している範囲をマージ
#'   start <- 1
#'   for (i in 2:length(column_group)) {
#'     if (column_group[i] != column_group[i - 1]) {
#'       if (column_group[start] != "") {
#'         wb$merge_cells(sheet = target_sheet, cols = start:(i - 1), rows = 1)
#'       }
#'       start <- i
#'     }
#'   }
#'   # 最後のグループもマージ
#'   if (column_group[start] != "") {
#'     wb$merge_cells(sheet = target_sheet, cols = start:length(column_group), rows = 1)
#'   }
#'   
#'   # 2行目に列名
#'   coln <- colnames(hyoufinal) %>% {str_remove(.,"(?<=環境型)ハラスメント")}
#'   wb$add_data(target_sheet, t(coln), start_row = 2, col_names = FALSE)
#'   
#'   # 3行目に type
#'   wb$add_data(target_sheet, t(table_setting$type), start_row = 3, col_names = FALSE)
#'   
#'   #1行目と2行目の色を設定
#'   wb$add_fill(sheet=target_sheet, dims = "L2", color=wb_color("#FCE4D6"))
#'   wb$add_fill(sheet=target_sheet, dims = "P1:W1", color=wb_color("#FCE4D6"))
#'   
#'   wb$add_fill(sheet=target_sheet, dims = "M2", color=wb_color("#FFF2CC"))
#'   wb$add_fill(sheet=target_sheet, dims = "X1:AC1", color=wb_color("#FFF2CC"))
#'   
#'   wb$add_fill(sheet=target_sheet, dims = "N2", color=wb_color("#E2EFDA"))
#'   wb$add_fill(sheet=target_sheet, dims = "AD1:AL1", color=wb_color("#E2EFDA"))
#'   
#'   wb$add_fill(sheet=target_sheet, dims = "O2", color=wb_color("#D9E1F2"))
#'   wb$add_fill(sheet=target_sheet, dims = "AM1:AS1", color=wb_color("#D9E1F2"))
#'   
#'   wb$add_fill(sheet=target_sheet, dims = "K2", color=wb_color("#D6DCE4"))
#'   wb$add_fill(sheet=target_sheet, dims = "AT1:AZ1", color=wb_color("#D6DCE4"))
#'   
#'   #2行目を1列目を除いて縦書き
#'   wb$add_cell_style(sheet=target_sheet, dims="B2:BH2", text_rotation=255)
#'   
#'   #3行目を水平中央ぞろいに
#'   wb$add_cell_style(sheet=target_sheet, dims = "B3:BH3", horizontal = "center" )
#'   
#'   # 4行目以降にデータ本体
#'   #欠損がN/Aと横幅とるので0にしてある
#'   wb$add_data(target_sheet, hyoufinal, start_row = 4, col_names = FALSE, apply_cell_style=FALSE)
#'   
#'   
#'   #罫線さきに縦線をいれておく
#'   upto <- nrow(hyoufinal)+3
#'   wb$add_border(sheet=target_sheet, dims=str_c("G1:G",upto)  , left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("L1:L",upto)  , left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("P1:P",upto)  , left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("X1:X",upto)  , left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("AD1:AD",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("AM1:AM",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("AT1:AT",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("BA1:BA",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("BI1:BI",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   
#'   # 条件付き書式を適用する関数
#'   apply_conditional_formatting <- function(wb, sheet, dims, type="hensati") { #type="percent"
#'     
#'     wb$add_dxfs_style(name = "danger", font_color = wb_color(hex="#FF0000"), bold = TRUE, bg_fill = wb_color("#FF9999"))
#'     wb$add_dxfs_style(name = "warn", bg_fill = wb_color("#FFCCCC"))
#'     wb$add_dxfs_style(name ="best" ,font_color = wb_color("#008000"), bold = TRUE, bg_fill = wb_color("#99FF99"))
#'     wb$add_dxfs_style(name ="better" , bg_fill = wb_color("#CCFFCC"))
#'     
#'     # 厳しい条件から順に適用
#'     if(type == "hensati"){
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = c(1,40),style = "danger",type = "between")
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = c(1,45),style = "warn",type = "between")
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = '>=55',style = "better")
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = '>=60',style = "best")
#'     }else if(type == "percent"){
#'       
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = '>=5',style = "warn")
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = '>=10',style = "danger")
#'       
#'     }
#'     
#'     return(wb)
#'   }
#'   
#'   # 4行目以降にスタイル適応
#'   maxval <- nrow(hyoufinal)+4
#'   for(row in 4:(nrow(hyoufinal)+4)){
#'     print(row)
#'     print(hyoufinal[[1]][row-3])
#'     if(row < maxval){
#'       print(str_c(row,"/",maxval))
#'       cell_ref1_hen <- paste0(int2col(7), row)
#'       cell_ref2_hen <- paste0(int2col(45), row)
#'       cell_ref_hen <- paste0(cell_ref1_hen,":",cell_ref2_hen)
#'       wb <- apply_conditional_formatting(wb, target_sheet, cell_ref_hen,"hensati")
#'       
#'       cell_ref1_perc <- paste0(int2col(46), row)
#'       cell_ref2_perc <- paste0(int2col(52), row)
#'       cell_ref_perc <- paste0(cell_ref1_perc,":",cell_ref2_perc)
#'       wb <- apply_conditional_formatting(wb, target_sheet, cell_ref_perc,"percent")
#'       
#'       cell_ref1_hen2 <- paste0(int2col(53), row)
#'       cell_ref2_hen2 <- paste0(int2col(60), row)
#'       cell_ref_hen2 <- paste0(cell_ref1_hen2,":",cell_ref2_hen2)
#'       wb <- apply_conditional_formatting(wb, target_sheet, cell_ref_hen2,"hensati")
#'       
#'       wb$add_border(sheet=target_sheet, dims=str_c("A",row,":F",row)  , left_border = NULL   , right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("G",row)           , left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("H",row,":K",row)  , left_border = NULL   , right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("L",row)           , left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("M",row,":O",row)  , left_border = NULL   , right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("P",row)           , left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("Q",row,":W",row)  , left_border = NULL   , right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("X",row)           , left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("Y",row,":AC",row) , left_border = NULL   , right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("AD",row)          , left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("AE",row,":AL",row), left_border = NULL   , right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("AM",row)          , left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("AN",row,":AS",row), left_border = NULL   , right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("AT",row)          , left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("AU",row,":AZ",row), left_border = NULL   , right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("BA",row)          , left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("BB",row,":BH",row), left_border = NULL   , right_border = NULL, top_border = "thin", bottom_border = NULL)
#'       wb$add_border(sheet=target_sheet, dims=str_c("BI",row)          , left_border = "thick", right_border = NULL, top_border = NULL  , bottom_border = NULL)
#'     }
#'     
#'     
#'     if(row == maxval){
#'       print("detect last row")
#'       wb$add_border(sheet=target_sheet, dims=str_c("A",row,":BH",row), left_border = NULL, right_border = NULL, top_border = "thick", bottom_border = NULL)
#'     }
#'   }
#'   
#'   #全体スタイルの適応
#'   wb$add_font(sheet = target_sheet, name = "BIZ UDPゴシック", dims = "A1:BI800")
#'   
#'   
#'   #列の幅を設定するG:BH
#'   wb$set_col_widths(sheet=target_sheet, cols = 1, widths = "auto")
#'   wb$set_col_widths(sheet=target_sheet, cols = 2, widths = 6.5)
#'   wb$set_col_widths(sheet=target_sheet, cols = 3:4, widths = 4.75)
#'   wb$set_col_widths(sheet=target_sheet, cols = 5:60, widths = 3.4)
#'   
#'   
#'   
#'   # 保存
#'   wb_save(wb, file = save_path, overwrite = TRUE)
#'   
#' }
#' 
#' make_table_single(hyou_current, "jig", "single_4kaisou.xlsx")
#' 
#' #過去差を縦に並べる表
#' make_diff_table_updown <- function(hyou_current, hyou_past,desired_sheet_name, save_path){
#'   grp_vec <- hyou_current$grp
#'   
#'   hyou <- bind_rows(
#'     hyou_current |> mutate(timing = 1),
#'     hyou_past |> mutate(timing = 2)  
#'   )
#'   
#'   hyou_diff <- hyou |> 
#'     arrange(roworder, timing) |> 
#'     group_by(roworder, grp) |> 
#'     summarise(across(everything(), ~first(.)-last(.))) |> 
#'     mutate(type = "2_diff") |> 
#'     mutate(grplabel = str_c("┗",grp,"(差)"), .before=1) |> 
#'     mutate(grp = factor(grp, levels = grp_vec)) |> 
#'     filter(!is.na(grp))
#'   
#'   hyoufinal <- hyou_current |> 
#'     mutate(type = "1_current") |>
#'     mutate(grplabel = str_c(grp,"-今年度"),.before=1) |> 
#'     mutate(grp = factor(grp,levels = grp_vec)) |> 
#'     bind_rows(hyou_diff) |> 
#'     arrange(roworder, grp, type) |> 
#'     relocate(grplabel) |> 
#'     select(!c(grp,roworder,type,timing)) |> 
#'     rename(`集団` = grplabel)
#'   
#'   table_setting <- make_table_setting()
#'   column_group <- table_setting$columngrp
#'   target_sheet <- desired_sheet_name
#'   
#'   
#'   
#'   library(openxlsx2)
#'   #wb <- wb_load("template.xlsx")
#'   wb <- openxlsx2::wb_workbook()
#'   
#'   wb$add_worksheet(sheet = target_sheet)
#'   
#'   #1行目の書き込み
#'   wb$add_data(target_sheet,x=t(column_group),start_row=1,start_col = 1, col_names = FALSE)
#'   
#'   # 同じグループ名が連続している範囲をマージ
#'   start <- 1
#'   for (i in 2:length(column_group)) {
#'     if (column_group[i] != column_group[i - 1]) {
#'       if (column_group[start] != "") {
#'         wb$merge_cells(sheet = target_sheet, cols = start:(i - 1), rows = 1)
#'       }
#'       start <- i
#'     }
#'   }
#'   # 最後のグループもマージ
#'   if (column_group[start] != "") {
#'     wb$merge_cells(sheet = target_sheet, cols = start:length(column_group), rows = 1)
#'   }
#'   
#'   # 2行目に列名
#'   coln <- colnames(hyoufinal) %>% {str_remove(.,"(?<=環境型)ハラスメント")}
#'   wb$add_data(target_sheet, t(coln), start_row = 2, col_names = FALSE)
#'   
#'   # 3行目に type
#'   wb$add_data(target_sheet, t(table_setting$type), start_row = 3, col_names = FALSE)
#'   
#'   
#'   
#'   
#'   #1行目と2行目の色を設定
#'   wb$add_fill(sheet=target_sheet, dims = "L2", color=wb_color("#FCE4D6"))
#'   wb$add_fill(sheet=target_sheet, dims = "P1:W1", color=wb_color("#FCE4D6"))
#'   
#'   wb$add_fill(sheet=target_sheet, dims = "M2", color=wb_color("#FFF2CC"))
#'   wb$add_fill(sheet=target_sheet, dims = "X1:AC1", color=wb_color("#FFF2CC"))
#'   
#'   wb$add_fill(sheet=target_sheet, dims = "N2", color=wb_color("#E2EFDA"))
#'   wb$add_fill(sheet=target_sheet, dims = "AD1:AL1", color=wb_color("#E2EFDA"))
#'   
#'   wb$add_fill(sheet=target_sheet, dims = "O2", color=wb_color("#D9E1F2"))
#'   wb$add_fill(sheet=target_sheet, dims = "AM1:AS1", color=wb_color("#D9E1F2"))
#'   
#'   wb$add_fill(sheet=target_sheet, dims = "K2", color=wb_color("#D6DCE4"))
#'   wb$add_fill(sheet=target_sheet, dims = "AT1:AZ1", color=wb_color("#D6DCE4"))
#'   
#'   #2行目を1列目を除いて縦書き
#'   wb$add_cell_style(sheet=target_sheet, dims="B2:BH2", text_rotation=255)
#'   
#'   #3行目を水平中央ぞろいに
#'   wb$add_cell_style(sheet=target_sheet, dims = "B3:BH3", horizontal = "center" )
#'   
#'   # 4行目以降にデータ本体
#'   #欠損がN/Aと横幅とるので0にしてある
#'   wb$add_data(target_sheet, hyoufinal, start_row = 4, col_names = FALSE, apply_cell_style=FALSE)
#'   
#'   
#'   #罫線さきに縦線をいれておく
#'   upto <- nrow(hyoufinal)+3
#'   wb$add_border(sheet=target_sheet, dims=str_c("G1:G",upto)  , left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("L1:L",upto)  , left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("P1:P",upto)  , left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("X1:X",upto)  , left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("AD1:AD",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("AM1:AM",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("AT1:AT",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("BA1:BA",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   wb$add_border(sheet=target_sheet, dims=str_c("BI1:BI",upto), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'   
#'   # 条件付き書式を適用する関数
#'   apply_conditional_formatting <- function(wb, sheet, dims, type="hensati") { #type="percent"
#'     
#'     wb$add_dxfs_style(name = "danger", font_color = wb_color(hex="#FF0000"), bold = TRUE, bg_fill = wb_color("#FF9999"))
#'     wb$add_dxfs_style(name = "warn", bg_fill = wb_color("#FFCCCC"))
#'     wb$add_dxfs_style(name ="best" ,font_color = wb_color("#008000"), bold = TRUE, bg_fill = wb_color("#99FF99"))
#'     wb$add_dxfs_style(name ="better" , bg_fill = wb_color("#CCFFCC"))
#'     
#'     # 厳しい条件から順に適用
#'     if(type == "hensati"){
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = c(1,40),style = "danger",type = "between")
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = c(1,45),style = "warn",type = "between")
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = '>=55',style = "better")
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = '>=60',style = "best")
#'     }else if(type == "percent"){
#'       
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = '>=5',style = "warn")
#'       wb$add_conditional_formatting(sheet = sheet,dims = dims,rule = '>=10',style = "danger")
#'       
#'     }
#'     
#'     return(wb)
#'   }
#'   
#'   # 4行目以降にスタイル適応
#'   maxval <- nrow(hyoufinal)+4
#'   for(row in 4:(nrow(hyoufinal)+4)){
#'     print(row)
#'     print(hyoufinal[[1]][row-3])
#'     if(row < maxval){
#'       if(str_detect(hyoufinal[[1]][row-3],"-今年度$")){
#'         print(str_c(row,"/",maxval))
#'         cell_ref1_hen <- paste0(int2col(7), row)
#'         cell_ref2_hen <- paste0(int2col(45), row)
#'         cell_ref_hen <- paste0(cell_ref1_hen,":",cell_ref2_hen)
#'         wb <- apply_conditional_formatting(wb, target_sheet, cell_ref_hen,"hensati")
#'         
#'         cell_ref1_perc <- paste0(int2col(46), row)
#'         cell_ref2_perc <- paste0(int2col(52), row)
#'         cell_ref_perc <- paste0(cell_ref1_perc,":",cell_ref2_perc)
#'         wb <- apply_conditional_formatting(wb, target_sheet, cell_ref_perc,"percent")
#'         
#'         cell_ref1_hen2 <- paste0(int2col(53), row)
#'         cell_ref2_hen2 <- paste0(int2col(60), row)
#'         cell_ref_hen2 <- paste0(cell_ref1_hen2,":",cell_ref2_hen2)
#'         wb <- apply_conditional_formatting(wb, target_sheet, cell_ref_hen2,"hensati")
#'         
#'         wb$add_border(sheet=target_sheet, dims=str_c("A",row,":F",row), left_border = NULL, right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("G",row), left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("H",row,":K",row), left_border = NULL, right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("L",row), left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("M",row,":O",row), left_border = NULL, right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("P",row), left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("Q",row,":W",row), left_border = NULL, right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("X",row), left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("Y",row,":AC",row), left_border = NULL, right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("AD",row), left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("AE",row,":AL",row), left_border = NULL, right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("AM",row), left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("AN",row,":AS",row), left_border = NULL, right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("AT",row), left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("AU",row,":AZ",row), left_border = NULL, right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("BA",row), left_border = "thick", right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("BB",row,":BH",row), left_border = NULL, right_border = NULL, top_border = "thin", bottom_border = "dotted")
#'         wb$add_border(sheet=target_sheet, dims=str_c("BI",row), left_border = "thick", right_border = NULL, top_border = NULL, bottom_border = NULL)
#'       }
#'     }
#'     
#'     
#'     if(row == maxval){
#'       print("detect last row")
#'       wb$add_border(sheet=target_sheet, dims=str_c("A",row,":BH",row), left_border = NULL, right_border = NULL, top_border = "thick", bottom_border = NULL)
#'     }
#'   }
#'   
#'   #全体スタイルの適応
#'   wb$add_font(sheet = target_sheet, name = "BIZ UDPゴシック", dims = "A1:BI60")
#'   
#'   
#'   #列の幅を設定するG:BH
#'   wb$set_col_widths(sheet=target_sheet, cols = 1, widths = "auto")
#'   wb$set_col_widths(sheet=target_sheet, cols = 2, widths = 6.5)
#'   wb$set_col_widths(sheet=target_sheet, cols = 3:4, widths = 4.75)
#'   wb$set_col_widths(sheet=target_sheet, cols = 5:60, widths = 3.4)
#'   
#'   
#'   
#'   # 保存
#'   wb_save(wb, file = save_path, overwrite = TRUE)
#'   
#' }
#' 
#' make_diff_table_updown(hyou_current, hyou_past, "jig1","difftable.xlsx")
#' 
#' make_main_diff_table <- function(hyou_current,hyou_past, tgt_sheet, save_path){
#'   #主要アウトカムとハラスメント詳細の左右前後比較
#'   target_cols <- c(
#'     "type",
#'     "roworder",
#'     "grp",
#'     "受検人数",
#'     "未受検者数(人)",
#'     "総合健康リスク",
#'     "高ストレス者(人)",
#'     "高ストレス者(%)",
#'     "ソーシャル・キャピタル",
#'     "ワークエンゲージメント",
#'     "心理的ストレス反応合計",
#'     "仕事の負担合計",
#'     "作業レベル資源合計",
#'     "部署レベル資源合計",
#'     "事業場レベル資源",
#'     "上司からのハラスメント",
#'     "同僚からのハラスメント",
#'     "セクハラ",
#'     "取引先からのハラスメント",
#'     "環境型ハラスメント(上司)",
#'     "環境型ハラスメント(同僚)",
#'     "環境型ハラスメント(取引先・顧客)"
#'   )
#'   
#'   
#'   
#'   tc <- target_cols[4:length(target_cols)]
#'   finalcolorder <- pmap(
#'     list(str_c(tc,"_Past"),str_c(tc,"_Now"),str_c(tc,"_diff")), ~c(..1,..2,..3)) |> 
#'     flatten() |> unlist()
#'   finalcolorder <- c("roworder","grp",finalcolorder)
#'   
#'   
#'   hyou <- bind_rows(
#'     hyou_current |> 
#'       select(!!!rlang::syms(target_cols)) |> 
#'       mutate(type = "Now"),
#'     hyou_past |> 
#'       select(!!!rlang::syms(target_cols)) |> 
#'       mutate(type = "Past")
#'   ) |> 
#'     pivot_longer(cols = !c(type, roworder,grp)) |> 
#'     pivot_wider(id_cols = c(roworder, grp, name), names_from = type, values_from = value) |> 
#'     mutate(diff = Now - Past) |> 
#'     pivot_wider(id_cols = c(roworder, grp), names_from = name, values_from = c(Now,Past,diff), names_glue = "{name}_{.value}") |> 
#'     relocate(!!!rlang::syms(finalcolorder)) |> 
#'     select(!roworder)
#'   
#'   
#'   hyouwidth <- ncol(hyou)
#'   hyouheight <- nrow(hyou)
#'   
#'   #1列目ハラスメントのみ
#'   #2列目列名
#'   #3列目前年、今回、差
#'   
#'   #colnames(hyou) |> clipr::write_clip()
#'   
#'   # ヘッダーの情報
#'   headertable <- tribble(
#'     ~r1, ~r2, ~r3,
#'     "統括","統括"                                ,"",
#'     "受検人数","受検人数"                      ,"昨年",
#'     "","受検人数"                       ,"今年",
#'     "","受検人数"                      ,"差",
#'     "未受検者数(人)","未受検者数(人)"                ,"昨年",
#'     "","未受検者数(人)"                 ,"今年",
#'     "","未受検者数(人)"                ,"差",
#'     "総合健康リスク","総合健康リスク"                ,"昨年",
#'     "","総合健康リスク"                 ,"今年",
#'     "","総合健康リスク"                ,"差",
#'     "高ストレス者(人)","高ストレス者(人)"              ,"昨年",
#'     "","高ストレス者(人)"               ,"今年",
#'     "","高ストレス者(人)"              ,"差",
#'     "高ストレス者(%)","高ストレス者(%)"               ,"昨年",
#'     "","高ストレス者(%)"                ,"今年",
#'     "","高ストレス者(%)"               ,"差",
#'     "ソーシャル・キャピタル","ソーシャル・キャピタル"        ,"昨年",
#'     "","ソーシャル・キャピタル"         ,"今年",
#'     "","ソーシャル・キャピタル"        ,"差",
#'     "ワークエンゲージメント","ワークエンゲージメント"        ,"昨年",
#'     "","ワークエンゲージメント"         ,"今年",
#'     "","ワークエンゲージメント"        ,"差",
#'     "心理的ストレス反応合計","心理的ストレス反応合計"        ,"昨年",
#'     "","心理的ストレス反応合計"         ,"今年",
#'     "","心理的ストレス反応合計"        ,"差",
#'     "仕事の負担合計","仕事の負担合計"                ,"昨年",
#'     "","仕事の負担合計"                 ,"今年",
#'     "","仕事の負担合計"                ,"差",
#'     "作業レベル資源合計","作業レベル資源合計"            ,"昨年",
#'     "","作業レベル資源合計"             ,"今年",
#'     "","作業レベル資源合計"            ,"差",
#'     "部署レベル資源合計","部署レベル資源合計"            ,"昨年",
#'     "","部署レベル資源合計"             ,"今年",
#'     "","部署レベル資源合計"            ,"差",
#'     "事業場レベル資源","事業場レベル資源"              ,"昨年",
#'     "","事業場レベル資源"               ,"今年",
#'     "","事業場レベル資源"              ,"差",
#'     "ハラスメント(%)","上司"              ,"昨年",
#'     "ハラスメント(%)","上司"               ,"今年",
#'     "ハラスメント(%)","上司"              ,"差",
#'     "ハラスメント(%)","同僚"              ,"昨年",
#'     "ハラスメント(%)","同僚"               ,"今年",
#'     "ハラスメント(%)","同僚"              ,"差",
#'     "ハラスメント(%)","セクハラ"          ,"昨年",
#'     "ハラスメント(%)","セクハラ"           ,"今年",
#'     "ハラスメント(%)","セクハラ"          ,"差",
#'     "ハラスメント(%)","取引先"            ,"昨年",
#'     "ハラスメント(%)","取引先"             ,"今年",
#'     "ハラスメント(%)","取引先"            ,"差",
#'     "環境型ハラスメント(%)","上司"        ,"昨年",
#'     "環境型ハラスメント(%)","上司"         ,"今年",
#'     "環境型ハラスメント(%)","上司"        ,"差",
#'     "環境型ハラスメント(%)","同僚"        ,"昨年",
#'     "環境型ハラスメント(%)","同僚"         ,"今年",
#'     "環境型ハラスメント(%)","同僚"        ,"差",
#'     "環境型ハラスメント(%)","取引先・顧客","昨年",
#'     "環境型ハラスメント(%)","取引先・顧客" ,"今年",
#'     "環境型ハラスメント(%)","取引先・顧客","差"
#'   ) #headertable
#'   
#'   target_sheet
#'   #wbを作成していく
#'   library(openxlsx2)
#'   
#'   wb <- openxlsx2::wb_workbook()
#'   wb$add_worksheet(sheet = target_sheet)
#'   
#'   #書き込み
#'   wb$add_data(target_sheet,x=t(headertable),start_row=1,start_col = 1, col_names = FALSE)
#'   wb$add_data(target_sheet,x=hyou,start_row=4,start_col = 1, col_names = FALSE)
#'   
#'   #1-2行目のマージ
#'   wb$merge_cells(sheet=target_sheet, dims="A1:A3")
#'   for(i in 1:12){
#'     print(i)
#'     fromAlpha <- int2col( 3*(i-1)+2 )
#'     toAlpha <-  int2col(3*(i-1)+2  + 2)
#'     tgtdim <- str_c(fromAlpha,"1:", toAlpha,2)
#'     wb$merge_cells(sheet=target_sheet, dims=tgtdim)  
#'   }
#'   for(i in 13:19){
#'     print(i)
#'     fromAlpha <- int2col( 3*(i-1)+2 )
#'     toAlpha <-  int2col(3*(i-1)+2  + 2)
#'     tgtdim <- str_c(fromAlpha,"2:", toAlpha,2)
#'     wb$merge_cells(sheet=target_sheet, dims=tgtdim)  
#'   }
#'   
#'   wb$merge_cells(sheet=target_sheet,dims="AL1:AW1")
#'   wb$merge_cells(sheet=target_sheet,dims="AX1:BF1")
#'   
#'   
#'   #2行目を1列目を除いて縦書き
#'   # wb$add_cell_style(sheet=target_sheet, dims="B1:AL1", text_rotation=255)
#'   
#'   
#'   wb$add_cell_style(sheet = target_sheet, dims="B1:AK2", wrap_text = TRUE)
#'   wb$add_cell_style(sheet = target_sheet, dims="AL2:BF2", wrap_text = TRUE)
#'   
#'   #3行目を水平中央ぞろいに
#'   wb$add_cell_style(sheet=target_sheet, dims = "B3:BF3", horizontal = "center" )
#'   
#'   
#'   #全体スタイルの適応
#'   wb$add_font(sheet = target_sheet, name = "BIZ UDPゴシック", dims = str_c("A1:BI",hyouheight+5))
#'   upto <- hyouheight+3
#'   
#'   #罫線を設定する
#'   wb$add_border(sheet=target_sheet, dims = str_c("B1:B",upto) , left_border = "thick")
#'   wb$add_border(sheet=target_sheet, dims = str_c("B1:B",upto) , left_border = "thick")
#'   
#'   for(i in 1:20){
#'     fromAlpha <- int2col( 3*(i-1)+2 )
#'     toAlpha <-  int2col(3*(i-1)+2  + 2)
#'     tgtdim <- str_c(fromAlpha,"1:", toAlpha,upto)
#'     print(tgtdim)
#'     wb$add_border(sheet=target_sheet, dims = tgtdim, left_border = "medium", inner_vgrid = "dashed", inner_hgrid = "dashed", bottom_border = "medium")
#'   }
#'   
#'   wb$add_border(sheet=target_sheet, dims=str_c("A1:A",upto), left_border = "medium", inner_hgrid = "dashed", bottom_border = "medium", top_border = "medium")
#'   
#'   
#'   #列の幅を設定するG:BH
#'   wb$set_col_widths(sheet=target_sheet, cols = 1, widths = "auto")
#'   wb$set_col_widths(sheet=target_sheet, cols = 2:3, widths = 6.5)
#'   wb$set_col_widths(sheet=target_sheet, cols = 4:60, widths = 5)
#'   
#'   
#'   #wb$set_row_heights(sheet=target_sheet, rows = 1, heights = 135)
#'   
#'   # 保存
#'   wb_save(wb, file = save_path, overwrite = TRUE)
#' }
#' 
#' make_main_diff_table(hyou_current, hyou_past, "diffside","difftableside.xlsx")
#' #関数作成途中
#' 
#' high_risk_bumon_excel_sheet_generator <- function(hyou_current, hyou_past, tgtpath){
#'   #ハイリスク部門比較表の作成
#'   find_highrisk_grp <- function(hyou){
#'     base_risk1 <- hyou |> 
#'       filter(`受検人数` >= 5 ) |> 
#'       filter(grp != "全体") |> 
#'       pivot_longer(cols = !grp) |> 
#'       mutate(
#'         risks = case_when(
#'           name == "上司からのハラスメント" & value >= 40 ~ "harassment",
#'           name == "同僚からのハラスメント" & value >= 40 ~ "harassment",
#'           name == "取引先からのハラスメント" & value >= 40 ~ "harassment",
#'           name == "環境型ハラスメント(上司)" & value >= 40 ~ "harassment",
#'           name == "環境型ハラスメント(同僚)" & value >= 40 ~ "harassment",
#'           name == "環境型ハラスメント(取引先・顧客)" & value >= 40 ~ "harassment",
#'           name == "心理的ストレス反応合計" & value < 45 ~ "stress_reaction",
#'           name == "仕事の負担合計" & value < 45 ~ "stress_cause",
#'           name == "作業レベル資源合計" & value < 45 ~ "stress_cause",
#'           name == "部署レベル資源合計" & value < 45 ~ "stress_cause",
#'           name == "事業場レベル資源" & value < 45 ~ "stress_cause",
#'           name == "ワークエンゲージメント" & value < 45 ~ "we",
#'           name == "総合健康リスク" & value >= 140 ~ "skrisk",
#'           name == "高ストレス者(%)" & value >= 40 ~ "hs",
#'           TRUE ~ NA #セクハラは別で作成。人数と数が必要
#'         )
#'       ) |> 
#'       filter(!is.na(risks)) |> 
#'       select(grp, risks)
#'     
#'     base_risk2 <- hyou |> 
#'       filter(`受検人数` > 5) |>
#'       filter(grp != "全体") |> 
#'       select(grp, n = `受検人数`, sexual_harassment = `セクハラ`) |> 
#'       mutate(n_sh = n * sexual_harassment / 100) |> 
#'       filter(sexual_harassment >= 40 | n_sh >= 2) |> 
#'       select(!n) |> 
#'       mutate(risks = "sexual_harassment") |> 
#'       select(grp, risks)
#'     
#'     base_risks <- bind_rows(base_risk1, base_risk2) |> distinct()
#'     
#'     finrisks <- base_risks |> 
#'       mutate(val = 1) |> 
#'       pivot_wider(id_cols = grp, names_from = risks, values_from = val, values_fill=0) 
#'     
#'     
#'     reqname <- c("harassment","stress_reaction","stress_cause","we","skrisk","hs","sexual_harassment")
#'     create_these <- reqname[!reqname %in% colnames(finrisks)]
#'     
#'     for(acolname in create_these){
#'       finrisks <- finrisks |> 
#'         mutate(!!rlang::sym(create_these) := 0)  
#'     }
#'     
#'     
#'     finrisks |> 
#'       mutate(
#'         .after = grp,
#'         risk1 = harassment == 1 & stress_reaction == 1 & stress_cause == 1,
#'         risk2 = harassment == 1,
#'         risk3 = stress_reaction == 1 & stress_cause == 1 & we == 1,
#'         risk4 = (skrisk == 1 | hs == 1) & stress_reaction == 1,
#'         risk5 = we == 1,
#'         risk6 = sexual_harassment == 1
#'       ) |> 
#'       select(grp, matches("risk\\d+")) |> 
#'       pivot_longer(cols = !grp) |> 
#'       filter(value)
#'   }
#'   
#'   risk_current <- find_highrisk_grp(hyou_current) |> mutate(timing = "current")
#'   risk_past <- find_highrisk_grp(hyou_past) |> mutate(timing = "past")
#'   
#'   comparison_table_data <- bind_rows(risk_current, risk_past) |> 
#'     group_nest(name) |> 
#'     mutate(comparison = map(data, ~{
#'       . |> 
#'         pivot_wider(id_cols = grp, names_from = timing, values_from = value, values_fill = FALSE) |> 
#'         mutate(type = case_when(
#'           past & current  ~ "継続",
#'           !past & current  ~ "新規",
#'           past & !current ~ "改善"
#'         ))    
#'     })) |> 
#'     select(name, comparison) |> 
#'     unnest(comparison)
#'   
#'   #ハイリスク部門総括単位数
#'   hyou_hr_toukatu <- comparison_table_data |> 
#'     mutate(grp1 = str_extract(grp,".+?(?=_)"), .before=1) |> 
#'     count(grp1,  type) |> 
#'     pivot_wider(id_cols = grp1, names_from = type, values_from = n, values_fill = 0) |> 
#'     mutate(`新規+継続` = `新規` + `継続`) |> 
#'     select(`統括名` = grp1, `新規+継続`, `継続`, `新規`, `改善`)
#'   ########################
#'   
#'   library(openxlsx2)
#'   wb <- openxlsx2::wb_workbook()
#'   ws1 <- "ハイリスク部門数(統括単位)"
#'   wb$add_worksheet(ws1)
#'   wb$add_data(ws1, hyou_hr_toukatu)
#'   
#'   dimarea <- str_c("A1:E",nrow(hyou_hr_toukatu)+1)
#'   wb$add_border(ws1, dims=dimarea, bottom_border = "thin", left_border = "thin", top_border = "thin", right_border = "thin", inner_hgrid = "thin", inner_vgrid = "thin")
#'   wb$add_fill(ws1,dims = "A1:E1", color = openxlsx2::wb_color("grey80"))
#'   wb$set_col_widths(ws1,cols=1,widths=22)
#'   
#'   #ハイリスク部門詳細  
#'   ws2 <- "ハイリスク基準と該当部門数"
#'   wb$add_worksheet(ws2)
#'   
#'   wb$add_data(ws2,x = "区分", dims="A1")
#'   wb$merge_cells(ws2, dims="A1:A2")
#'   wb$add_data(ws2,x = "ハイリスク部門基準", dims="B1")
#'   wb$merge_cells(ws2,dims=("B1:C2"))
#'   
#'   wb$add_data(ws2, x="部門数", dims="D1")
#'   wb$merge_cells(ws2,dims=("D1:F1"))
#'   wb$add_data(ws2, x="前年度", dims="D2")
#'   wb$add_data(ws2, x="今年度", dims="E2")
#'   wb$add_data(ws2, x="前年差", dims="F2")
#'   
#'   wb$add_data(ws2, x="継続", dims="G1")
#'   wb$merge_cells(ws2,dims=("G1:I1"))
#'   wb$add_data(ws2, x="部門数", dims="G2")
#'   wb$add_data(ws2, x="部門名", dims="H2")
#'   wb$add_data(ws2, x="部門名", dims="I2")
#'   
#'   wb$add_data(ws2, x="新規", dims="J1")
#'   wb$merge_cells(ws2,dims=("J1:L1"))
#'   wb$add_data(ws2, x="部門数", dims="J2")
#'   wb$add_data(ws2, x="部門名", dims="K2")
#'   wb$add_data(ws2, x="部門名", dims="L2")
#'   
#'   wb$add_data(ws2, x="改善", dims="M1")
#'   wb$merge_cells(ws2,dims=("M1:O1"))
#'   wb$add_data(ws2, x="部門数", dims="M2")
#'   wb$add_data(ws2, x="部門名", dims="N2")
#'   wb$add_data(ws2, x="部門名", dims="O2")
#'   
#'   
#'   explanation_table <- tribble(
#'     ~type,~title, ~expr,~roundnum,
#'     "risk1","ハラスメントリスク(高)","「ハラスメント」：40%以上\n＋「ストレス原因」：偏差値45未満\n＋「ストレス反応」：偏差値45未満","①",
#'     "risk2","ハラスメントリスク(中)","「ハラスメント」：40%以上","②",
#'     "risk3","身体不調リスク＋意欲低下","「ハラスメント」：「ストレス原因」：偏差値45未満\n＋「ストレス反応」：偏差値45未満\n＋「ワークエンゲージメント」：偏差値45未満","③",
#'     "risk4","身体不調リスク","「ストレス反応」：偏差値45未満かつ(「総合健康リスク」：140以上 あるいは 「高ストレス者割合」：40%以上)","④",
#'     "risk5","意欲低下","「ワークエンゲージメント」：偏差値45未満","⑤",
#'     "risk6","セクハラ","「セクハラ」：20%以上(2名以上)","⑥"
#'   )
#'   
#'   current_r <- 3
#'   for(arisk in str_c("risk",c(1:6))){
#'     print(arisk)
#'     
#'     exptxt <- explanation_table |> filter(type == arisk)
#'     
#'     risktgt <- comparison_table_data |> filter(name == arisk)
#'     past_num <- risktgt |> filter(past) |> nrow()
#'     current_num <- risktgt |> filter(current) |> nrow()
#'     new_grp_data <- risktgt |> filter(type == "新規")
#'     cont_grp_data <- risktgt |> filter(type == "継続")
#'     impr_grp_data <- risktgt |> filter(type == "改善")
#'     
#'     numrows <- max(nrow(new_grp_data), nrow(cont_grp_data), nrow(impr_grp_data))
#'     
#'     newgrps <- new_grp_data$grp
#'     if(length(newgrps) < numrows) newgrps <- c(newgrps, rep("", numrows-length(newgrps)))
#'     newgrps1 <- str_extract(newgrps,".+?(?=_)") |> replace_na("")
#'     newgrps2 <- str_extract(newgrps,"(?<=_).+") |> replace_na("") |> str_replace_all("_"," ")
#'     orig_newgrp_n <- length(newgrps)
#'     
#'     contgrps <- cont_grp_data$grp
#'     if(length(contgrps) < numrows) contgrps <- c(contgrps, rep("", numrows-length(contgrps)))
#'     contgrps1 <- str_extract(contgrps,".+?(?=_)") |> replace_na("")
#'     contgrps2 <- str_extract(contgrps,"(?<=_).+") |> replace_na("") |> str_replace_all("_"," ")
#'     orig_contgrp_n <- length(contgrps)
#'     
#'     imprgrps <- impr_grp_data$grp
#'     if(length(imprgrps) < numrows) imprgrps <- c(imprgrps, rep("", numrows-length(imprgrps)))
#'     imprgrps1 <- str_extract(imprgrps,".+?(?=_)") |> replace_na("")
#'     imprgrps2 <- str_extract(imprgrps,"(?<=_).+") |> replace_na("") |> str_replace_all("_"," ")
#'     orig_imprgrp_n <- length(imprgrps)
#'     
#'     if(numrows == 0){
#'       temprowdata <- tibble(
#'         `継続_N`　= "",
#'         `継続1`   = "",
#'         `継続2`   = "",
#'         `新規_N`  = "",
#'         `新規1`   = "",
#'         `新規2`   = "",
#'         `改善_N`  = "",
#'         `改善1`   = "",
#'         `改善2`   = ""
#'       )
#'     }else{
#'       temprowdata <- tibble(
#'         `継続_N`　= rep(orig_contgrp_n,numrows),
#'         `継続1` = contgrps1,
#'         `継続2` = contgrps2,
#'         `新規_N` = rep(orig_newgrp_n,numrows),
#'         `新規1` = newgrps1,
#'         `新規2` = newgrps2,
#'         `改善_N` = rep(orig_imprgrp_n,numrows),
#'         `改善1` = imprgrps1,
#'         `改善2` = imprgrps2
#'       )
#'     }
#'     
#'     
#'     
#'     
#'     wb$add_data(ws2,x=exptxt$roundnum, dims=str_c("A",current_r))
#'     wb$add_data(ws2,x=exptxt$title, dims=str_c("B",current_r))
#'     wb$add_data(ws2,x=exptxt$expr, dims=str_c("C",current_r))
#'     wb$add_cell_style(ws2,dims=str_c("C",current_r),wrap_text = TRUE)
#'     wb$add_data(ws2,x=past_num, dims=str_c("D", current_r))
#'     wb$add_data(ws2,x=current_num, dims=str_c("E", current_r))
#'     wb$add_data(ws2,x=current_num - past_num, dims=str_c("F", current_r))
#'     wb$add_data(ws2,x=temprowdata, start_col = 7, start_row = current_r, col_names = FALSE)
#'     
#'     end_r <- current_r + nrow(temprowdata) - 1
#'     #セルの結合
#'     for(tgtCol in c(LETTERS[1:7],"J","M")){
#'       wb$merge_cells(ws2, dims=str_c(tgtCol,current_r,":",tgtCol,end_r))
#'     }
#'     
#'     
#'     current_r <- current_r + nrow(temprowdata)
#'     
#'     
#'   }
#'   
#'   
#'   #列の幅と高さをAutoで設定する
#'   wb$set_col_widths(ws2, cols=2, widths = 30)
#'   wb$set_col_widths(ws2, cols=3, widths = 40)
#'   wb$set_col_widths(ws2, cols=c(8, 11,14), widths = 20)
#'   wb$set_col_widths(ws2, cols=c(8,9,11,12,14,15), widths = 50)
#'   
#'   #wb$set_row_heights(ws2, rows = 1:current_r, heights="auto")
#'   
#'   #色を指定する
#'   wb$add_fill(ws2,dims="A1:O2", color = wb_color(name="grey80"))
#'   
#'   #罫線
#'   wb$add_border(ws2, dims=str_c("A1:O",current_r), top_border = "thin", right_border = "thin", bottom_border = "thin", left_border = "thin", inner_hgrid = "thin", inner_vgrid = "thin")
#'   
#'   
#'   ######### ハイリスク部門詳細
#'   riskchangetable <- full_join(
#'     risk_current |> 
#'       group_by(grp) |> 
#'       summarise(risks_current = str_c(name,collapse="") |> str_remove_all("risk")),
#'     risk_past |> 
#'       group_by(grp) |> 
#'       summarise(risks_past = str_c(name,collapse="") |> str_remove_all("risk")),
#'     by="grp"
#'   ) |> 
#'     replace_na(list(risks_current = "-", risks_past = "-")) |> 
#'     mutate(ordergrp = case_when(
#'       risks_current != "-" & risks_past != "-" ~ 1,
#'       risks_current != "-" & risks_past == "-" ~ 2,
#'       risks_current == "-" & risks_past != "-" ~ 3
#'     )) |> 
#'     arrange(ordergrp, grp) |> 
#'     select(risks_past, risks_current, grp)
#'   
#'   
#'   
#'   #makemaindifftable から
#'   #主要アウトカムとハラスメント詳細の左右前後比較
#'   {target_cols <- c(
#'     "type",
#'     "roworder",
#'     "grp",
#'     "受検人数",
#'     "未受検者数(人)",
#'     "総合健康リスク",
#'     "高ストレス者(人)",
#'     "高ストレス者(%)",
#'     "ソーシャル・キャピタル",
#'     "ワークエンゲージメント",
#'     "心理的ストレス反応合計",
#'     "仕事の負担合計",
#'     "作業レベル資源合計",
#'     "部署レベル資源合計",
#'     "事業場レベル資源",
#'     "上司からのハラスメント",
#'     "同僚からのハラスメント",
#'     "セクハラ",
#'     "取引先からのハラスメント",
#'     "環境型ハラスメント(上司)",
#'     "環境型ハラスメント(同僚)",
#'     "環境型ハラスメント(取引先・顧客)"
#'   )}
#'   
#'   tc <- target_cols[4:length(target_cols)]
#'   finalcolorder <- pmap(
#'     list(str_c(tc,"_Past"),str_c(tc,"_Now"),str_c(tc,"_diff")), ~c(..1,..2,..3)) |> 
#'     flatten() |> unlist()
#'   finalcolorder <- c("roworder","grp",finalcolorder)
#'   
#'   hyou <- bind_rows(
#'     hyou_current |> 
#'       select(!!!rlang::syms(target_cols)) |> 
#'       mutate(type = "Now"),
#'     hyou_past |> 
#'       select(!!!rlang::syms(target_cols)) |> 
#'       mutate(type = "Past")
#'   ) |> 
#'     pivot_longer(cols = !c(type, roworder,grp)) |> 
#'     pivot_wider(id_cols = c(roworder, grp, name), names_from = type, values_from = value) |> 
#'     mutate(diff = Now - Past) |> 
#'     pivot_wider(id_cols = c(roworder, grp), names_from = name, values_from = c(Now,Past,diff), names_glue = "{name}_{.value}") |> 
#'     relocate(!!!rlang::syms(finalcolorder)) |> 
#'     select(!roworder)
#'   
#'   hyou <- riskchangetable |> 
#'     left_join(hyou, by="grp")
#'   
#'   
#'   
#'   hyouwidth <- ncol(hyou)
#'   hyouheight <- nrow(hyou)
#'   
#'   #1列目ハラスメントのみ
#'   #2列目列名
#'   #3列目前年、今回、差
#'   
#'   #colnames(hyou) |> clipr::write_clip()
#'   
#'   # ヘッダーの情報
#'   {headertable <- tribble(
#'     ~r1, ~r2, ~r3,
#'     "ハイリスク対象部門","ハイリスク対象部門","昨年",
#'     "","ハイリスク対象部門","今年",
#'     "統括"    ,"統括"                                ,"",
#'     "受検人数","受検人数"               ,"昨年",
#'     "","受検人数"                       ,"今年",
#'     "","受検人数"                      ,"差",
#'     "未受検者数(人)","未受検者数(人)"                ,"昨年",
#'     "","未受検者数(人)"                 ,"今年",
#'     "","未受検者数(人)"                ,"差",
#'     "総合健康リスク","総合健康リスク"                ,"昨年",
#'     "","総合健康リスク"                 ,"今年",
#'     "","総合健康リスク"                ,"差",
#'     "高ストレス者(人)","高ストレス者(人)"              ,"昨年",
#'     "","高ストレス者(人)"               ,"今年",
#'     "","高ストレス者(人)"              ,"差",
#'     "高ストレス者(%)","高ストレス者(%)"               ,"昨年",
#'     "","高ストレス者(%)"                ,"今年",
#'     "","高ストレス者(%)"               ,"差",
#'     "ソーシャル・キャピタル","ソーシャル・キャピタル"        ,"昨年",
#'     "","ソーシャル・キャピタル"         ,"今年",
#'     "","ソーシャル・キャピタル"        ,"差",
#'     "ワークエンゲージメント","ワークエンゲージメント"        ,"昨年",
#'     "","ワークエンゲージメント"         ,"今年",
#'     "","ワークエンゲージメント"        ,"差",
#'     "心理的ストレス反応合計","心理的ストレス反応合計"        ,"昨年",
#'     "","心理的ストレス反応合計"         ,"今年",
#'     "","心理的ストレス反応合計"        ,"差",
#'     "仕事の負担合計","仕事の負担合計"                ,"昨年",
#'     "","仕事の負担合計"                 ,"今年",
#'     "","仕事の負担合計"                ,"差",
#'     "作業レベル資源合計","作業レベル資源合計"            ,"昨年",
#'     "","作業レベル資源合計"             ,"今年",
#'     "","作業レベル資源合計"            ,"差",
#'     "部署レベル資源合計","部署レベル資源合計"            ,"昨年",
#'     "","部署レベル資源合計"             ,"今年",
#'     "","部署レベル資源合計"            ,"差",
#'     "事業場レベル資源","事業場レベル資源"              ,"昨年",
#'     "","事業場レベル資源"               ,"今年",
#'     "","事業場レベル資源"              ,"差",
#'     "ハラスメント(%)","上司"              ,"昨年",
#'     "ハラスメント(%)","上司"               ,"今年",
#'     "ハラスメント(%)","上司"              ,"差",
#'     "ハラスメント(%)","同僚"              ,"昨年",
#'     "ハラスメント(%)","同僚"               ,"今年",
#'     "ハラスメント(%)","同僚"              ,"差",
#'     "ハラスメント(%)","セクハラ"          ,"昨年",
#'     "ハラスメント(%)","セクハラ"           ,"今年",
#'     "ハラスメント(%)","セクハラ"          ,"差",
#'     "ハラスメント(%)","取引先"            ,"昨年",
#'     "ハラスメント(%)","取引先"             ,"今年",
#'     "ハラスメント(%)","取引先"            ,"差",
#'     "環境型ハラスメント(%)","上司"        ,"昨年",
#'     "環境型ハラスメント(%)","上司"         ,"今年",
#'     "環境型ハラスメント(%)","上司"        ,"差",
#'     "環境型ハラスメント(%)","同僚"        ,"昨年",
#'     "環境型ハラスメント(%)","同僚"         ,"今年",
#'     "環境型ハラスメント(%)","同僚"        ,"差",
#'     "環境型ハラスメント(%)","取引先・顧客","昨年",
#'     "環境型ハラスメント(%)","取引先・顧客" ,"今年",
#'     "環境型ハラスメント(%)","取引先・顧客","差"
#'   )} #headertable
#'   
#'   
#'   #wbを作成していく
#'   library(openxlsx2)
#'   
#'   ws3 <- "室・チーム別 ハイリスク部門詳細"
#'   wb$add_worksheet(sheet = ws3)
#'   
#'   #書き込み
#'   wb$add_data(ws3,x=t(headertable),start_row=1,start_col = 1, col_names = FALSE)
#'   wb$add_data(ws3,x=hyou,start_row=4,start_col = 1, col_names = FALSE)
#'   
#'   #1-2行目のマージ
#'   
#'   wb$merge_cells(sheet=ws3, dims="A1:B2")
#'   wb$merge_cells(sheet=ws3, dims="C1:C3")
#'   for(i in 1:12){
#'     fromAlpha <- int2col( 3*(i-1)+4 )
#'     toAlpha <-  int2col(3*(i-1)+2  + 4)
#'     tgtdim <- str_c(fromAlpha,"1:", toAlpha,2)
#'     print(tgtdim)
#'     wb$merge_cells(sheet=ws3, dims=tgtdim)  
#'   }
#'   for(i in 13:19){
#'     fromAlpha <- int2col( 3*(i-1)+4 )
#'     toAlpha <-  int2col(3*(i-1)+2  + 4)
#'     tgtdim <- str_c(fromAlpha,"2:", toAlpha,2)
#'     print(tgtdim)
#'     wb$merge_cells(sheet=ws3, dims=tgtdim)  
#'   }
#'   
#'   wb$merge_cells(sheet=ws3,dims="AN1:AY1")
#'   wb$merge_cells(sheet=ws3,dims="AZ1:BH1")
#'   
#'   
#'   wb$add_cell_style(sheet = ws3, dims="A1:AM2", wrap_text = TRUE)
#'   wb$add_cell_style(sheet = ws3, dims="AN2:BH2", wrap_text = TRUE)
#'   
#'   #3行目を水平中央ぞろいに
#'   wb$add_cell_style(sheet=ws3, dims = "D3:BH3", horizontal = "center" )
#'   
#'   
#'   #全体スタイルの適応
#'   wb$add_font(sheet = ws3, name = "BIZ UDPゴシック", dims = str_c("A1:BI",hyouheight+5))
#'   upto <- hyouheight+3
#'   
#'   #罫線を設定する
#'   wb$add_border(sheet=ws3, dims = str_c("A1:C",upto), left_border = "thick", top_border = "thick", right_border = "thin", bottom_border = "thick", inner_hgrid = "dashed", inner_vgrid = "thin")
#'   for(i in 1:20){
#'     fromAlpha <- int2col( 3*(i-1)+4 )
#'     toAlpha <-  int2col(3*(i-1)+2  + 4)
#'     tgtdim <- str_c(fromAlpha,"1:", toAlpha,upto)
#'     print(tgtdim)
#'     wb$add_border(sheet=ws3, dims = tgtdim, left_border = "medium", inner_vgrid = "dashed", inner_hgrid = "dashed", bottom_border = "medium")
#'   }
#'   
#'   wb$add_border(sheet=ws3, dims=str_c("A1:A",upto), left_border = "medium", inner_hgrid = "dashed", bottom_border = "medium", top_border = "medium")
#'   
#'   
#'   #列の幅を設定するG:BH
#'   wb$set_col_widths(sheet=ws3, cols = 1:3, widths = "auto")
#'   wb$set_col_widths(sheet=ws3, cols = 4:5, widths = 6.5)
#'   wb$set_col_widths(sheet=ws3, cols = 6:62, widths = 5)
#'   
#'   
#'   #wb$set_row_heights(sheet=target_sheet, rows = 1, heights = 135)
#'   # 保存
#'   wb_save(wb, file = tgtpath, overwrite = TRUE)
#'   
#'   
#'   
#' }
#' 
#' 
#' high_risk_bumon_excel_sheet_generator(hyou_current, hyou_past, "houkoku1.xlsx")