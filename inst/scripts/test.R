add_call(
  id = "trtsdtm",
  call = derive_vars_merged(
    dataset_add = ex,
    by_vars = exprs(USUBJID),
    order = exprs(EXSDTM = convert_dtc_to_dtm(EXSTDTC)),
    mode = "first",
    new_vars = exprs(TRTSDTM = EXSDTM)
  )
)

add_call(
  id = "trtdurd",
  call = derive_var_trtdurd()
)

add_call_template(
  "inc_var",
  mutate(VAR = VAR + !!step, glue_sym("{domain}STRESC") := glue_char("Found in {domain}")),
  defaults = list(step = 1, domain = "VS")
)

add_chunk("paths", {
  "# Set up paths"
  config <- yaml::read_yaml("config_workflow.yml")

  sdtm_path <- if_else(
    config[["study_config"]][["datacut"]][["enable"]],
    config[["study_config"]][["datacut"]][["directory"]],
    config[["paths"]][["sdtmv"]][["data"]]
  )
})

add_chunk_template(
  "apply_metadata",
  {
  metacore <- admiralroche::read_dap_m3(dataset = glue_char("{str_to_upper(dataset)}"))

  "# Check DAP M3"
  check_dap_m3(dataset = glue_char("{str_to_upper(dataset)}"))

  glue_sym("{dataset}") <- glue_sym("{dataset}_prefinal") %>%
    drop_unspec_vars(metacore = metacore) %>% # Drop unspecified variables from specs
    check_variables(metacore = metacore) %>% # Check all variables specified are present and no more
    check_ct_data(metacore = metacore) %>% # Checks all variables with CT only contain values within the CT
    order_cols(metacore = metacore) %>% # Orders the columns according to the spec
    sort_by_key(metacore = metacore) %>% # Sorts the rows by the sort keys
    xportr_type(metadata = metacore, verbose = "warn") %>% # Coerce variable type to match spec
    xportr_label(metadata = metacore) %>% # Assigns variable label from metacore specifications
    xportr_df_label(metadata = metacore, domain = glue_char("{str_to_upper(dataset)}")) # Assigns dataset label from metacore specifications
  }
)

set_scriptr_sources("scriptr")

execute({
  insert_chunks(exprs(paths))

  library(syntheticsdtm)
  # Read in data
  data(dm)
  data(ex)

  "# Derive treatment start/end"
  dm %>% derive_vars_merged(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    mode = "first",
    new_vars = exprs(TRTSDTM, convert_dtc_to_dtm(EXSTDTC))
  ) %>% derive_vars_merged(
    dataset_add = ex,
    by_vars = exprs(STUDYID, USUBJID),
    mode = "last",
    new_vars = exprs(TRTEDTM, convert_dtc_to_dtm(EXENDTC))
  )

  insert_calls(dm, ids = exprs(trtsdtm, trtdurd, inc_var(step = 3), chg))

  insert_chunks(exprs(apply_metadata(dataset = "adae")))
}, file = file("./test_out.R", "w"))
