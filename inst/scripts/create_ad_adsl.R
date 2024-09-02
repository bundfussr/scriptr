library(rlang)
library(stringr)

execute({
  insert_chunks(exprs(
    load_pkgs,
    read_adsl_input,
    read_specs(dataset_name = "ADSL"),
    add_adsl_predecessors
  ))

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

  insert_chunks(ids = exprs(prepare_export(dataset_name = "adsl")))
},
file = "inst/scripts_generated/ad_adsl.R"
)
