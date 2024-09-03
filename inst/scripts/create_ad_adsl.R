library(rlang)
library(stringr)

execute({
  # Initialisation and prepare DM data
  insert_chunks(exprs(
    load_pkgs,
    read_adsl_input,
    read_specs(dataset_name = "ADSL"),
    add_adsl_predecessors
  ))

  # Derivations
  insert_calls(
    input = adsl_preds,
    ids = exprs(derive_agegr1, derive_racen, remove_screening),
    output = adsl_ct
  )

  insert_calls(
    input = ex,
    ids = exprs(derive_exstdtm, derive_exendtm),
    output = ex_ext
  )

  insert_calls(
    input = adsl_ct,
    ids = exprs(
      derive_trtsdtm, derive_trtedtm, derive_trtdt, derive_trtdurd,
      derive_saffl, drop_unspec_vars
    ),
    output = adsl_raw
  )
  ""
  "# Set data cut off date"
  adsl_raw <- adsl_raw %>% mutate(DCUTDT = ymd("2014-10-01"))

  insert_calls(
    input = adsl_raw,
    ids = exprs(derive_aerepedt(time_window = 28))
  )

  # Export final dataset
  insert_chunks(ids = exprs(prepare_export(dataset_name = "adsl")))
},
file = "inst/scripts_generated/ad_adsl.R"
)
