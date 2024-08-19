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

add_call_template("inc_var", mutate(VAR = VAR + !!step), defaults = list(step = 1))

set_scriptr_sources("scriptr")

execute({
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

  insert_calls(dm, ids = exprs(trtsdtm, trtdurd, inc_var(3)))
}, file = file("./test_out.R", "w"))
