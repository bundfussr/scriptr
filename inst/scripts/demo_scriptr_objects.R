add_chunk("load_pkgs", {
  library(metacore)
  library(metatools)
  library(pharmaversesdtm)
  library(admiral)
  library(xportr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
})

add_chunk(
  "read_adsl_input",
  description = "Read in input SDTM data",
  {
    data("dm")
    data("ex")
  }
)

add_chunk_template(
  "read_specs",
  description = "Read in metacore object",
  {
    load(metacore_example("pilot_ADaM.rda"))
    metacore <- metacore %>%
      select_dataset(!!dataset_name)
  })

add_chunk(
  "add_adsl_predecessors",
  {
    adsl_preds <- build_from_derived(metacore,
                                     ds_list = list("dm" = dm),
                                     predecessor_only = FALSE, keep = TRUE)
  }
)

add_call(
  "derive_agegr1",
  create_cat_var(metacore, ref_var = AGE,
                 grp_var = AGEGR1, num_grp_var = AGEGR1N)
)

add_call(
  "derive_racen",
  create_var_from_codelist(metacore = metacore,
                           input_var = RACE,
                           out_var = RACEN)
)

add_call(
  "remove_screening",
  description = "Removing screen failures from ARM and TRT01P to match the define and FDA guidance",
  mutate(ARM = if_else(ARM == "Screen Failure", NA_character_, ARM),
         TRT01P = if_else(TRT01P == "Screen Failure", NA_character_, TRT01P)
  )
)

add_call(
  "derive_exstdtm",
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  )
)

add_call(
  "derive_exendtm",
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )
)

add_call(
  "derive_trtsdtm",
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & nchar(EXSTDTC) >= 10,
    new_vars = exprs(TRTSDTM = EXSTDTM),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )
)

add_call(
  "derive_trtedtm",
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & nchar(EXENDTC) >= 10,
    new_vars = exprs(TRTEDTM = EXENDTM),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )
)

add_call(
  "derive_trtdt",
  description = "Convert Datetime variables to date",
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))
)

add_call(
  "derive_trtdurd",
  derive_var_trtdurd()
)

add_call(
  "derive_saffl",
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))
  )
)

add_call(
  "drop_unspec_vars",
  description = "This will drop any columns that aren't specified in the metacore object",
  drop_unspec_vars(metacore)
)

add_chunk_template(
  "prepare_export",
  {
  glue_sym("{dataset_name}_raw") %>%
    check_variables(metacore) %>%
    check_ct_data(metacore, na_acceptable = TRUE) %>%
    order_cols(metacore) %>%
    sort_by_key(metacore) %>%
    xportr_type(metacore, domain = glue_char("{str_to_upper(dataset_name)}")) %>%
    xportr_length(metacore) %>%
    xportr_label(metacore) %>%
    xportr_df_label(metacore)
  }
)
