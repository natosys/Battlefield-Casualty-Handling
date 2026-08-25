// Browser-level verification of the Shiny console.
//
// The suite drives a real app over HTTP, so it is indifferent to how server()
// is structured internally: it is the half of the verification that survives
// the decomposition of that function into per-tab modules unchanged. Its
// counterpart, shiny::testServer() coverage of the reactive state machine,
// lives in tests/testthat and needs no browser.
//
// Assertions target behaviour rather than appearance. A plot is checked for
// existing and being non-empty, never compared pixel by pixel: a snapshot
// suite over ggplot output would fail on a font substitution and tell a
// reader nothing about whether the app works. The tradeoff is recorded in
// docs/Continuous_Integration.md — a layout regression that breaks no
// assertion passes here.
//
// The browser is whatever PLAYWRIGHT_BROWSERS_PATH already provides. Nothing
// here downloads one.

const { defineConfig, devices } = require('@playwright/test');

const PORT = Number(process.env.BCH_APP_PORT || 7654);
const BASE_URL = `http://127.0.0.1:${PORT}`;

module.exports = defineConfig({
  testDir: './tests/playwright',
  // Shiny serves one session per browser context and the app holds global
  // simulation state, so the specs run one at a time against one app.
  fullyParallel: false,
  workers: 1,
  forbidOnly: !!process.env.CI,
  retries: 0,
  // A Quick Run is a real simulation, not a stub; it dominates the suite's
  // runtime and needs a timeout measured against it rather than against a
  // page load.
  timeout: 10 * 60 * 1000,
  expect: { timeout: 30 * 1000 },
  reporter: process.env.CI ? [['list'], ['github']] : [['list']],
  use: {
    // A locator that no longer matches should fail in seconds, not consume
    // the whole test timeout the Quick Run needs.
    actionTimeout: 30 * 1000,
    baseURL: BASE_URL,
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
  },
  projects: [
    { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
  ],
  webServer: {
    // tests/playwright/serve.R rather than an inline `R -e` expression; its
    // banner records why, and the reason is that a Quick Run fails under the
    // latter.
    command: `Rscript tests/playwright/serve.R ${PORT}`,
    url: BASE_URL,
    reuseExistingServer: !process.env.CI,
    // The app sources every R/ module and builds the parameter registry
    // before it listens, which takes appreciably longer than a static server
    // and several minutes on a modest runner.
    timeout: 10 * 60 * 1000,
    stdout: 'pipe',
    stderr: 'pipe',
  },
});
