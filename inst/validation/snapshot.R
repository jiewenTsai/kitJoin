# Report snapshot of kitJoin: one fixed scenario through the steps whose files users download.
# Data: the three example files in inst/extdata (missing codes 7777 and 9999), and simulated wide
# item data for the kits response file. Every file is written with the app's writers and its MD5
# compared with results/snapshot_md5.txt.
# Run: source(system.file("validation", "snapshot.R", package = "kitJoin"))
#      (KIT_SNAPSHOT_WRITE=1 rewrites results/snapshot_md5.txt in the source folder of the working directory)
out <- Sys.getenv("KIT_VALIDATION_OUT", file.path(tempdir(), "kit_validation"))
out <- file.path(out, "kitJoin")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
ns <- asNamespace("kitJoin")
names_t <- paste0("example_t", 1:3, ".csv")
raw <- lapply(names_t, function(f) ns$read_data_file(system.file("extdata", f, package = "kitJoin"), f))
raw <- lapply(raw, function(x) if (is.data.frame(x)) x else x$data)

# 1 missing codes -> NA (ID excluded)
clean <- lapply(raw, function(d) ns$replace_na_codes(d, c(7777, 9999), exclude = "release_id")$data)
# 2 merge the waves (full join on release_id; sex is the same in every wave)
joined <- ns$join_waves(clean, names_t, c("t1", "t2", "t3"), "release_id", method = "full_join",
                        constant_cols = "sex")$result
# 3 attrition of t1 at t2
attr_t2 <- ns$build_attrition(clean[[1]], clean[[2]], "release_id")$result
# 4 stack the waves, then long -> wide on lang_score
stacked <- ns$stack_waves(clean, names_t, c("t1", "t2", "t3"))
wide <- ns$reshape_wider(stacked$result, ns$wider_args(id_cols = "release_id", names_from = stacked$wave_col,
                                                     values_from = "lang_score", names_prefix = "lang_",
                                                     names_sort = TRUE))$result
# 5 kits response file from simulated wide item data (two cohorts, three waves)
sim_wide <- function(ids, wave, wave_m, seed) {
  set.seed(seed)
  n <- length(ids)
  sc <- function() sample(1:5, n, replace = TRUE)
  data.frame(release_id = ids, wave = wave, wave_m = wave_m, age_m = wave_m + 0.2,
             sex = rep(c("boy", "girl"), length.out = n), lana01 = sc(), lana02 = sc(), bodyaf01 = sc())
}
kb_dfs <- list(rbind(sim_wide(paste0("K", 1:6), "m03g01", 3, 1), sim_wide(paste0("K", 1:5), "m06g02", 6, 2)),
               sim_wide(paste0("J", 1:4), "m36g01", 36, 3))
map <- ns$kits_guess_map(unique(unlist(lapply(kb_dfs, names))))
cand <- ns$kits_candidate_cols(kb_dfs, ns$kits_mapped_cols(map))
set <- ns$kits_settings_apply(ns$kits_item_settings(kb_dfs, cand$numeric),
                              list(type = "table", data = data.frame(item_id = c("lana01", "lana02", "bodyaf01"),
                                                                     domain = c("lan", "lan", "motor"))))
set <- ns$kits_bulk_shift(set, set$column, -1)   # scores 1-5 -> 0-4
spec <- ns$kits_spec(lapply(c("M03", "M36"), function(x) list(type = "text", value = x)), map,
                     ns$kits_settings_items(set), ns$kits_settings_undecided(set))
kb <- suppressMessages(ns$kits_build(kb_dfs, c("m03.csv", "m36.csv"), spec))
stopifnot(length(kb$problems) == 0)

files <- file.path(out, c("missing_t2.csv", "joined.csv", "attrition_t2.csv", "stacked.csv", "wide.csv",
                          "kits_long.csv"))
ns$write_kit_csv(clean[[2]], files[1])
ns$write_kit_csv(joined, files[2])
ns$write_kit_csv(attr_t2, files[3])
ns$write_kit_csv(stacked$result, files[4])
ns$write_kit_csv(wide, files[5])
ns$write_kits_long_csv(prep = kb$prep, file = files[6])

now <- data.frame(file = basename(files), lines = vapply(files, function(f) length(readLines(f, warn = FALSE)), 1L),
                  md5 = unname(tools::md5sum(files)))
if (nzchar(Sys.getenv("KIT_SNAPSHOT_WRITE"))) {
  utils::write.table(now, file.path("inst", "validation", "results", "snapshot_md5.txt"), row.names = FALSE,
                     quote = FALSE)
}
ref <- utils::read.table(system.file("validation", "results", "snapshot_md5.txt", package = "kitJoin"),
                         header = TRUE, stringsAsFactors = FALSE)
cmp <- merge(ref, now, by = "file", suffixes = c("_ref", "_now"))
cmp$same <- cmp$md5_ref == cmp$md5_now
print(cmp[c("file", "lines_now", "same")], row.names = FALSE)
cat(if (nrow(cmp) == nrow(now) && all(cmp$same)) "Snapshot unchanged.\n" else "Snapshot CHANGED: say why in NEWS.\n")
