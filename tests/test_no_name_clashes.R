# test_no_name_clashes.R
# run_daily.R sources every R/ file into one environment, so two files
# defining the same function name means the later one silently replaces the
# earlier (this broke run #67: two different region_days()). Each test file
# only loads its own module, so only a check like this catches it.
#   Rscript tests/test_no_name_clashes.R

files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
defs <- do.call(rbind, lapply(files, function(f) {
  exprs <- parse(f, keep.source = FALSE)
  nm <- vapply(exprs, function(e) {
    if (is.call(e) && (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
        is.name(e[[2]]) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
      as.character(e[[2]])
    } else NA_character_
  }, "")
  nm <- nm[!is.na(nm)]
  if (length(nm)) data.frame(name = nm, file = basename(f)) else NULL
}))
dup <- defs[defs$name %in% defs$name[duplicated(defs$name)], ]
if (nrow(dup)) {
  print(dup[order(dup$name), ], row.names = FALSE)
  stop("Function defined in more than one R/ file (the later source() wins silently).")
}
cat("No function name clashes across", length(files), "R/ files.\n")
