# all high-level output options work with mock data

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: long 
    Code
      res <- get_hb_pop_est(output = opt)
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.
    Code
      print(dim(res))
    Output
      [1] 8 7
    Code
      print(names(res))
    Output
      [1] "year"       "hb2019"     "hb2019name" "sex"        "sex_name"  
      [6] "age"        "pop"       

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: age-and-sex 
    Code
      res <- get_hb_pop_est(output = opt)
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.
    Code
      print(dim(res))
    Output
      [1] 2 7
    Code
      print(names(res))
    Output
      [1] "year"         "hb2019"       "hb2019name"   "pop_male_0"   "pop_female_0"
      [6] "pop_male_1"   "pop_female_1"

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: age 
    Code
      res <- get_hb_pop_est(output = opt)
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.
    Code
      print(dim(res))
    Output
      [1] 4 6
    Code
      print(names(res))
    Output
      [1] "year"       "hb2019"     "hb2019name" "sex_name"   "pop_0"     
      [6] "pop_1"     

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: age-only 
    Code
      res <- get_hb_pop_est(output = opt)
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.
    Code
      print(dim(res))
    Output
      [1] 2 5
    Code
      print(names(res))
    Output
      [1] "year"       "hb2019"     "hb2019name" "pop_0"      "pop_1"     

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: sex 
    Code
      res <- get_hb_pop_est(output = opt)
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.
    Code
      print(dim(res))
    Output
      [1] 4 6
    Code
      print(names(res))
    Output
      [1] "year"       "hb2019"     "hb2019name" "age"        "pop_male"  
      [6] "pop_female"

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: sex-only 
    Code
      res <- get_hb_pop_est(output = opt)
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.
    Code
      print(dim(res))
    Output
      [1] 2 5
    Code
      print(names(res))
    Output
      [1] "year"       "hb2019"     "hb2019name" "pop_male"   "pop_female"

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: total 
    Code
      res <- get_hb_pop_est(output = opt)
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.
    Code
      print(dim(res))
    Output
      [1] 2 4
    Code
      print(names(res))
    Output
      [1] "year"       "hb2019"     "hb2019name" "pop"       

# all low-level output options work with split mock parquet files

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: long 
    Code
      res <- get_iz_pop_est(output = opt)
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.
    Code
      print(dim(res))
    Output
      [1] 12  6
    Code
      print(names(res))
    Output
      [1] "year"            "intzone2011"     "intzone2011name" "sex_name"       
      [5] "age"             "pop"            

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: age-and-sex 
    Code
      res <- get_iz_pop_est(output = opt)
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.
    Code
      print(dim(res))
    Output
      [1] 2 9
    Code
      print(names(res))
    Output
      [1] "year"            "intzone2011"     "intzone2011name" "pop_f_0"        
      [5] "pop_m_0"         "pop_f_1"         "pop_m_1"         "pop_f_90"       
      [9] "pop_m_90"       

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: age 
    Code
      res <- get_iz_pop_est(output = opt)
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.
    Code
      print(dim(res))
    Output
      [1] 4 7
    Code
      print(names(res))
    Output
      [1] "year"            "intzone2011"     "intzone2011name" "sex_name"       
      [5] "pop_0"           "pop_1"           "pop_90"         

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: age-only 
    Code
      res <- get_iz_pop_est(output = opt)
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.
    Code
      print(dim(res))
    Output
      [1] 2 6
    Code
      print(names(res))
    Output
      [1] "year"            "intzone2011"     "intzone2011name" "pop_0"          
      [5] "pop_1"           "pop_90"         

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: sex 
    Code
      res <- get_iz_pop_est(output = opt)
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.
    Code
      print(dim(res))
    Output
      [1] 6 6
    Code
      print(names(res))
    Output
      [1] "year"            "intzone2011"     "intzone2011name" "age"            
      [5] "pop_f"           "pop_m"          

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: sex-only 
    Code
      res <- get_iz_pop_est(output = opt)
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.
    Code
      print(dim(res))
    Output
      [1] 2 5
    Code
      print(names(res))
    Output
      [1] "year"            "intzone2011"     "intzone2011name" "pop_f"          
      [5] "pop_m"          

---

    Code
      cat("Output option:", opt, "\n")
    Output
      Output option: total 
    Code
      res <- get_iz_pop_est(output = opt)
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.
    Code
      print(dim(res))
    Output
      [1] 2 4
    Code
      print(names(res))
    Output
      [1] "year"            "intzone2011"     "intzone2011name" "pop"            

# high-level and low-level population totals consistency on mock data

    Code
      hb_long <- get_hb_pop_est(output = "long")
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.
    Code
      hb_sex_only <- get_hb_pop_est(output = "sex-only")
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.

---

    Code
      iz_long <- get_iz_pop_est(output = "long")
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.
    Code
      iz_age_only <- get_iz_pop_est(output = "age-only")
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.

# year filtering works on mock data

    Code
      hb_2020 <- get_hb_pop_est(min_year = 2020L, max_year = 2020L)
    Message
      v Using the latest available geography: "HB2019".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2019"`.
    Code
      iz_2021 <- get_iz_pop_est(min_year = 2021L, max_year = 2021L)
    Message
      v Using the latest available geography: "IntZone2011".
      i If you require older boundaries or for reproducibility purposes specify `version` explicitly: `version = "2011"`.

