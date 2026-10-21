##############################################
## tests/testthat/test-server-state.R       ##
## testServer coverage of reactive state    ##
##############################################

# testServer coverage of the console's reactive state machine.
#
# These tests exist to be run before and after the decomposition of `server`
# into per-tab modules, so that a refactor which changes what the app does
# fails here rather than in a reviewer's browser. They assert observable
# reactive state only (the reactives derived from the loaded configuration,
# and the absence of any run state), never rendered markup, which is the
# Playwright suite's half of the split. The console runs nothing, so nothing
# here starts a simulation.

test_that("the console holds no state for any run", {
  shiny::testServer(server, {
    for (gone in c("run_state", "run_error", "progress_pct", "mon_data", "analysis_results",
                   "pending_future", "run_mode", "morris_state", "sobol_state",
                   "transport_sweep_state", "rep_progress_done")) {
      expect_false(exists(gone, inherits = FALSE), info = gone)
    }
  })
})

test_that("the scenario selector offers the profiles the configuration defines", {
  shiny::testServer(server, {
    choices <- scenario_choices()
    expect_true(is.character(choices))
    expect_identical(unname(choices)[1], "default")
    # An overlay that overrides nothing is an alias of the default and is
    # not offered, so each option runs a distinct configuration.
    scenarios <- test_startup_json()$scenarios
    overlaid <- names(scenarios)[vapply(scenarios, function(sc) length(sc$vars) > 0, logical(1))]
    expect_setequal(unname(choices), c("default", overlaid))
    expect_identical(
      names(choices),
      c("Moderate Intensity", "High Intensity", "High Intensity, Modern Care")
    )
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

test_that("scheduled casualty surge rows are added and removed within their bounds", {
  shiny::testServer(server, {
    start <- mc_event_count()
    expect_gte(start, 1L)

    session$setInputs(mc_event_add = 1)
    expect_equal(mc_event_count(), min(start + 1L, CASUALTY_SURGE_SCHEDULE_SLOTS))

    session$setInputs(mc_event_remove = 1)
    expect_identical(mc_event_count(), start)
  })
})

test_that("the scheduled casualty surge row count cannot fall below one", {
  shiny::testServer(server, {
    for (i in seq_len(CASUALTY_SURGE_SCHEDULE_SLOTS + 1L)) {
      session$setInputs(mc_event_remove = i)
    }
    expect_identical(mc_event_count(), 1L)
  })
})

test_that("the scheduled casualty surge row count cannot exceed the slot count", {
  shiny::testServer(server, {
    for (i in seq_len(CASUALTY_SURGE_SCHEDULE_SLOTS + 1L)) {
      session$setInputs(mc_event_add = i)
    }
    expect_equal(mc_event_count(), CASUALTY_SURGE_SCHEDULE_SLOTS)
  })
})

test_that("loading a valid configuration replaces the base configuration", {
  shiny::testServer(server, {
    edited <- test_startup_json()
    edited$scenarios <- edited$scenarios["high_intensity"]
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

test_that("the displayed command follows the file name and seed boxes", {
  shiny::testServer(server, {
    session$setInputs(config_filename = "plan_a", seed = "7")
    expect_match(output$full_protocol_command_ui$html,
                 "Rscript run.R --config plan_a.json --seed 7 --iterations 30 --days 360",
                 fixed = TRUE)
  })
})

test_that("Save Configuration downloads under the name the command refers to", {
  shiny::testServer(server, {
    session$setInputs(config_filename = "plan_a")
    path <- output$download_json
    expect_match(basename(path), "plan_a\\.json$")
    saved <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    expect_true("vars" %in% names(saved))
  })
})
