library(readr)
library(tidyr)
library(purrr)
library(dplyr)
library(stringr)

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
    select(tempid, name, qnum,value,is_reverse,score) 
  
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
  
  #高ストレスの有無を追加
  areadat <- tibble(
    area = c(rep("A",length(1:17)), rep("B",length(18:46)), rep("C",length(47:55))),
    q    = c(1:17                 , 18:46                 , 47:55)
  )
  
  #ここまででは、得点が高い＝良いとしてきたが、高ストレス者計算では
  #得点が高い＝悪いとする必要がある
  hs_scores <- tempd_score |> 
    filter(between(qnum,1,55)) |> 
    mutate(
      hsscore = 5- score
    ) |> 
    mutate(
      area = case_when(
        between(qnum,1 ,17) ~ "A",
        between(qnum,18,46) ~ "B",
        between(qnum,47,55) ~ "C"
      )
    ) |> 
    group_by(tempid,area) |> 
    summarise(hsscore = sum(hsscore, na.rm=FALSE), .groups="drop") |> 
    pivot_wider(id_cols = tempid, names_from = area, values_from = hsscore) |> 
    mutate(is_hs = case_when(
      B >= 77 ~ TRUE,
      A+C >= 76 & B>=63 ~ TRUE,
      TRUE ~ FALSE
    ))

  dfin <- d |> 
    left_join(hs_scores, by="tempid") |> 
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