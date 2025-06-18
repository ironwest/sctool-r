library(tidyverse)

# d <- read_csv("../demodata/processed_nbjsq_dummy_data1_alpha.csv")
# hensati_data <- read_csv("table11.csv")
# tgtsheet <- "全体"
# grp_vars <- c("dept1","dept2")
# nbjsq <- read_csv("nbjsq_question_text.csv")
# nbjsqlabs <- read_csv("nbjsq_label_hensati.csv")


#' @params ret either "hensati" or "precise"

calculate_hensati <- function(d, hensati_data, tgtsheet, grp_vars, nbjsq, nbjsqlabs, ret = "hensati"){

  #分類が性別と年代別のときは、こちらで利用する偏差値データを変更する
  #そうでないときは、基本的には全体シートを利用する
  if(tgtsheet != "全体"){
    
    if(tgtsheet == "age_kubun"){
      hensati_filt <- hensati_data |> 
        filter(sheet %in% c("10代","20代","30代","40代","50代","60代以上")) |> 
        filter(qtype == "NBJSQ") |> 
        select(sheet, syakudo_name = `尺度名`, mean_val = `平均値`, sd_val =`標準偏差`)  
    }else if(tgtsheet == "gender"){
      hensati_filt <- hensati_data |> 
        filter(sheet %in% c("男性","女性")) |> 
        filter(qtype == "NBJSQ") |> 
        select(sheet, syakudo_name = `尺度名`, mean_val = `平均値`, sd_val =`標準偏差`)  
    }
  }else{
    hensati_filt <- hensati_data |> 
      filter(sheet == tgtsheet) |> 
      filter(qtype == "NBJSQ") |> 
      select(sheet, syakudo_name = `尺度名`, mean_val = `平均値`, sd_val =`標準偏差`)  
  }
  
  tgtcols <- unique(c(nbjsq$syakudo_minor_eng, nbjsq$syakudo_major_eng)) 
  tgtcols <- tgtcols[!is.na(tgtcols)]
  tgtcols <- tgtcols[tgtcols != "outcome"]
  
  d2 <- d |> 
    group_by(across(all_of(grp_vars))) |> 
    summarise(across(all_of(tgtcols), ~mean(.))) |> 
    pivot_longer(cols = !all_of(grp_vars)) |> 
    left_join(nbjsqlabs, by=c("name" = "syakudo_english")) 
  
  if(tgtsheet != "全体"){
    d2 <- d2 |> rename(sheet = !!rlang::sym(grp_vars))
    d2 <- d2 |> 
      left_join(hensati_filt,by=c( "sheet","syakudo_hensati" = "syakudo_name")) |> 
      mutate(hensatigrp = sheet, .before=1) |> 
      rename(!!rlang::sym(grp_vars) := sheet)
  }else{
    d2 <- d2 |> 
      left_join(hensati_filt,by=c( "syakudo_hensati" = "syakudo_name"))  |> 
      mutate(hensatigrp = "全体", .before=1)
  }
  
  d2 <- d2 |> 
    mutate(hensati = 50 + 10*(value -mean_val)/sd_val)
  
  return(d2)
}

