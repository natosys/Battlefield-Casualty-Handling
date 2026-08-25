# Unit coverage of the console's helpers, which sit outside server() and so
# need no reactive context to exercise.
#
# The compositional split helpers and the deferred-rendering default fill are
# covered most closely: both sit on the path between what the Configure panel
# holds and what a run is actually given, so an error in either changes the
# simulation's inputs without changing anything a reader would see.

test_that("a two-handle split slider expands to three shares summing to one", {
  values <- inject_split(list(pri_split = c(0.55, 0.85)),
                         "pri_split", c("pri_one", "pri_two", "pri_three"))
  expect_equal(values$pri_one,   0.55)
  expect_equal(values$pri_two,   0.30)
  expect_equal(values$pri_three, 0.15)
  expect_equal(values$pri_one + values$pri_two + values$pri_three, 1)
})

test_that("a split slider at either extreme still sums to one", {
  all_first <- inject_split(list(s = c(1, 1)), "s", c("a", "b", "c"))
  expect_equal(unlist(all_first[c("a", "b", "c")], use.names = FALSE), c(1, 0, 0))

  all_last <- inject_split(list(s = c(0, 0)), "s", c("a", "b", "c"))
  expect_equal(unlist(all_last[c("a", "b", "c")], use.names = FALSE), c(0, 0, 1))
})

test_that("a missing or malformed split slider leaves the values untouched", {
  expect_identical(inject_split(list(), "s", c("a", "b", "c")), list())
  one_handle <- list(s = 0.5)
  expect_identical(inject_split(one_handle, "s", c("a", "b", "c")), one_handle)
})

test_that("every compositional split the panel offers is expanded", {
  slots <- seq_len(MASS_CASUALTY_SCHEDULE_SLOTS)
  values <- inject_all_splits(c(
    list(pri_split = c(0.5, 0.8), dnbi_split = c(0.2, 0.7), mc_pri_split = c(0.7, 0.9)),
    setNames(rep(list(c(0.6, 0.8)), length(slots)),
             sprintf("mc_event_pri_split_%d", slots))
  ))
  expect_equal(values$pri_one, 0.5)
  expect_equal(values$dnbi_bf_pct, 0.2)
  expect_equal(values$mc_pri_one, 0.7)
  for (i in slots) {
    parts <- unlist(values[sprintf(c("mc_sched_pri_one_%d", "mc_sched_pri_two_%d",
                                     "mc_sched_pri_three_%d"), i)], use.names = FALSE)
    expect_equal(sum(parts), 1)
  }
})

test_that("an unopened panel's fields fall back to their scenario defaults", {
  json     <- test_startup_json()
  defaults <- registry_defaults(PARAM_REGISTRY, json)
  filled   <- fill_missing_defaults(list(), PARAM_REGISTRY, json)
  expect_setequal(names(filled), names(defaults))
  expect_identical(filled, defaults)
})

test_that("a field the user has edited is not overwritten by its default", {
  json  <- test_startup_json()
  edited <- fill_missing_defaults(list(pop_combat = 1234), PARAM_REGISTRY, json)
  expect_equal(edited$pop_combat, 1234)
})

test_that("triangular triples are detected only where all three fields exist", {
  expect_true(length(TRI_TRIPLES) > 0)
  ids <- vapply(PARAM_REGISTRY, function(f) f$id, character(1))
  for (tt in TRI_TRIPLES) {
    expect_true(all(c(tt$min_id, tt$mode_id, tt$max_id) %in% ids))
    expect_true(nzchar(tt$label))
    # The mode field's own label carries the "Most Likely (Mode)" suffix; the
    # triple's shared label is that label with the suffix removed.
    expect_false(grepl("Most Likely", tt$label))
  }

  # A mode field with no matching min/max is not a triple.
  lone <- detect_tri_triples(list(list(id = "solo_mode", label = "Solo")))
  expect_length(lone, 0)
})

test_that("the fork-count cap is at least one and never exceeds the core count", {
  for (days in c(1, 30, 90)) {
    cores <- detect_safe_cores(n_days = days)
    expect_gte(cores, 1)
    expect_lte(cores, parallel::detectCores())
  }
})

test_that("a longer run is never given more concurrent forks than a shorter one", {
  expect_lte(detect_safe_cores(n_days = 90), detect_safe_cores(n_days = 1))
})

test_that("each configuration preview renders a plot rather than erroring", {
  expect_s3_class(render_gen_curve(10, 4, "lognormal"), "ggplot")
  expect_s3_class(render_gen_curve(10, 4, "exponential"), "ggplot")
  expect_s3_class(render_tri_curve(10, 20, 40), "ggplot")
  expect_s3_class(render_dow_curve(0.001, 0.02, 0.5, 240), "ggplot")
})

test_that("a degenerate triangular preview does not error", {
  expect_s3_class(render_tri_curve(10, 10, 10), "ggplot")
})

test_that("the force structure diagram scales with the team and bed counts", {
  r2b_beds <- c(resus = 2, hold = 4)
  r2e_beds <- c(resus = 4, hold = 20)
  small <- force_bed_table(r2b_teams = 1, r2b_beds = r2b_beds,
                           r2e_teams = 1, r2e_beds = r2e_beds)
  large <- force_bed_table(r2b_teams = 3, r2b_beds = r2b_beds,
                           r2e_teams = 2, r2e_beds = r2e_beds)
  expect_s3_class(small, "shiny.tag")
  # One row per bed type, and the aggregate cells move with the team counts.
  expect_true(grepl("resus", as.character(small), fixed = TRUE))
  expect_false(identical(as.character(small), as.character(large)))

  expect_s3_class(force_node_graph(1, 2, 3), c("shiny.tag", "shiny.tag.list", "html"))
  expect_s3_class(force_structure_diagram(1, 1, r2b_beds, 1, r2e_beds),
                  c("shiny.tag", "shiny.tag.list", "html"))
})

test_that("the field label carries the overridden marker only for an overridden path", {
  field <- Filter(function(f) !is.null(f$path), PARAM_REGISTRY)[[1]]
  plain      <- as.character(field_label(field, character(0)))
  overridden <- as.character(field_label(field, field$path))
  expect_false(identical(plain, overridden))
})
