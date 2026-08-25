# testServer coverage of the console's reactive state machine.
#
# These tests exist to be run before and after the decomposition of `server`
# into per-tab modules, so that a refactor which changes what the app does
# fails here rather than in a reviewer's browser. They assert observable
# reactive state only (the reactiveVals the run/screening state machines
# advance through, and the reactives derived from the loaded configuration),
# never rendered markup, which is the Playwright suite's half of the split.
#
# Nothing here starts a simulation: the asynchronous run paths are entered
# only along their rejection branches, where the configuration fails
# validation and no future is created.

test_that("the run state machine starts idle with nothing computed", {
  shiny::testServer(server, {
    expect_identical(run_state(), "idle")
    expect_identical(run_mode(), "quick")
    expect_null(run_error())
    expect_identical(progress_pct(), 0)
    expect_null(mon_data())
    expect_null(analysis_results())
    expect_null(pending_future())
  })
})

test_that("every screening state machine starts idle with nothing computed", {
  shiny::testServer(server, {
    expect_identical(morris_state(), "idle")
    expect_identical(sobol_state(), "idle")
    expect_identical(transport_sweep_state(), "idle")
    expect_null(morris_results())
    expect_null(sobol_results())
    expect_null(transport_sweep_results())
    expect_identical(morris_progress_done(), 0)
    expect_identical(sobol_progress_done(), 0)
    expect_identical(transport_sweep_progress_done(), 0)
  })
})

test_that("the scenario selector offers the profiles the configuration defines", {
  shiny::testServer(server, {
    choices <- scenario_choices()
    expect_true(is.character(choices))
    expect_identical(unname(choices)[1], "default")
    expect_setequal(unname(choices),
                    c("default", names(test_startup_json()$scenarios)))
    # Every option carries a label, and none of them is the bare identifier
    # padded out by the fallback in scenario_choices().
    expect_true(all(nzchar(names(choices))))
  })
})

test_that("selecting a profile overlays its parameters and leaves the base pristine", {
  shiny::testServer(server, {
    base <- raw_env_data()
    session$setInputs(scenario_select = "high_intensity")
    expect_identical(current_scenario(), "high_intensity")

    overlaid <- scenario_json()
    expect_false(identical(overlaid, base))
    # resolve_scenario() overlays values only, so the structural blocks are
    # untouched and the base reactiveVal itself never moves.
    expect_identical(raw_env_data(), base)
    expect_identical(names(overlaid$elms), names(base$elms))

    paths <- scenario_overridden_paths()
    expect_true(length(paths) > 0)
    expect_true(all(grepl("\\.", paths)))
  })
})

test_that("returning to the default profile restores the base configuration", {
  shiny::testServer(server, {
    base <- raw_env_data()
    session$setInputs(scenario_select = "high_intensity")
    expect_true(length(scenario_overridden_paths()) > 0)

    session$setInputs(scenario_select = "default")
    expect_identical(scenario_json(), base)
    expect_identical(scenario_overridden_paths(), character(0))
  })
})

test_that("the resolved profile drives the derived configuration reactives", {
  shiny::testServer(server, {
    session$setInputs(scenario_select = "default")
    default_shape <- dow_shape()
    expect_setequal(names(default_shape), c("p1", "p2"))
    expect_setequal(names(default_shape$p1), c("p_base", "k", "t_mid"))

    families <- gen_distributions()
    expect_identical(names(families), GEN_STREAM_ACTYS)
    expect_true(all(nzchar(families)))
  })
})

test_that("scheduled mass casualty rows are added and removed within their bounds", {
  shiny::testServer(server, {
    start <- mc_event_count()
    expect_gte(start, 1L)

    session$setInputs(mc_event_add = 1)
    expect_equal(mc_event_count(), min(start + 1L, MASS_CASUALTY_SCHEDULE_SLOTS))

    session$setInputs(mc_event_remove = 1)
    expect_identical(mc_event_count(), start)
  })
})

test_that("the scheduled mass casualty row count cannot fall below one", {
  shiny::testServer(server, {
    for (i in seq_len(MASS_CASUALTY_SCHEDULE_SLOTS + 1L)) {
      session$setInputs(mc_event_remove = i)
    }
    expect_identical(mc_event_count(), 1L)
  })
})

test_that("the scheduled mass casualty row count cannot exceed the slot count", {
  shiny::testServer(server, {
    for (i in seq_len(MASS_CASUALTY_SCHEDULE_SLOTS + 1L)) {
      session$setInputs(mc_event_add = i)
    }
    expect_equal(mc_event_count(), MASS_CASUALTY_SCHEDULE_SLOTS)
  })
})

test_that("loading a valid configuration replaces the base configuration", {
  shiny::testServer(server, {
    edited <- test_startup_json()
    edited$scenarios <- edited$scenarios[1]
    session$setInputs(upload_json = test_upload(edited))
    expect_setequal(unname(scenario_choices()),
                    c("default", names(edited$scenarios)))
  })
})

test_that("loading a malformed configuration leaves the loaded one in place", {
  shiny::testServer(server, {
    before <- raw_env_data()
    session$setInputs(upload_json = test_upload("{ not json", raw = TRUE))
    expect_identical(raw_env_data(), before)

    # Well-formed JSON that is not a configuration is rejected by
    # validate_env_data_json() at the same boundary, with the same outcome.
    session$setInputs(upload_json = test_upload(list(elms = list())))
    expect_identical(raw_env_data(), before)
  })
})

test_that("an invalid configuration stops Quick Run before a run starts", {
  shiny::testServer(server, {
    session$setInputs(pop_combat = 0, n_days = 2, seed = "42", run_quick = 1)
    expect_identical(run_state(), "idle")
    expect_null(pending_future())
    expect_null(analysis_results())
  })
})

test_that("an invalid configuration stops Full Analysis before a run starts", {
  shiny::testServer(server, {
    session$setInputs(pop_combat = 0, n_days = 2, n_reps = 2, run_full = 1)
    expect_identical(run_state(), "idle")
    expect_null(pending_future())
    expect_identical(rep_progress_done(), 0)
  })
})

test_that("the Analyse tab's derived reactives stay silent until a run completes", {
  shiny::testServer(server, {
    expect_error(tab_plot(), class = "shiny.silent.error")
    expect_error(utilisation_panel_heights(), class = "shiny.silent.error")
  })
})

test_that("the calibration table is available before any screening runs", {
  shiny::testServer(server, {
    df <- calibration_df()
    expect_s3_class(df, "data.frame")
    expect_gt(nrow(df), 0)
  })
})
