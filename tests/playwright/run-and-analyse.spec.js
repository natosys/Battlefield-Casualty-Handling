// Quick Run through to the Analyse tab.
//
// Serial, and against one page, because a Quick Run's results live in the
// Shiny session that produced them: a fresh browser context would be a fresh
// session with nothing to analyse. The run itself is a real simulation, not a
// stub, so it is kept short — long enough to produce casualties through every
// echelon, short enough that the suite stays inside a per-pull-request gate.

const { test, expect } = require('@playwright/test');
const {
  openTab, activePanel, tabControl, waitForShinyIdle, expectPlotRendered,
  expectNoOutputErrors,
} = require('./helpers');

// Days simulated by the Quick Run these tests analyse. Five days at the
// shipped casualty rates reaches R1, R2B and R2E and produces strategic
// evacuation decisions, which is what the tab assertions below need.
const RUN_DAYS = 5;
const RUN_SEED = '42';

// Every Analyse tab, with one plot output from each that must render. The
// list is what "each Analyse tab renders" means in the issue this suite was
// written for; a tab added to the app without being added here is not covered.
const ANALYSE_TABS = [
  ['Casualty Flow', 'plot_casualty_flow'],
  ['Queue Depths', 'plot_r1_queues'],
  ['Bed & Resource Utilisation', null],
  ['Transport', null],
  ['Waiting Times', 'plot_waiting_times'],
  ['Return to Duty & DOW', null],
  ['Force Regeneration', null],
  ['Strategic AME', null],
  ['Mass Casualty Events', null],
  ['Sensitivity Calibration', null],
];

test.describe.configure({ mode: 'serial' });

test.describe('Quick Run and Analyse', () => {
  let page;

  test.beforeAll(async ({ browser }) => {
    page = await browser.newPage();
    await page.goto('/');
  });

  test.afterAll(async () => {
    await page.close();
  });

  test('a Quick Run completes', async () => {
    await openTab(page, 'Run');
    await waitForShinyIdle(page);

    await page.locator('#n_days').fill(String(RUN_DAYS));
    await page.locator('#seed').fill(RUN_SEED);
    await page.locator('#n_days').blur();
    await waitForShinyIdle(page);

    await expect(page.locator('#run_status')).toContainText('No run yet');
    await page.locator('#run_quick').click();

    await expect(page.locator('#run_status .alert-success'))
      .toContainText('Run complete', { timeout: 9 * 60 * 1000 });
    await expectNoOutputErrors(page);
  });

  test('the Analyse tab is populated by the completed run', async () => {
    await openTab(page, 'Analyse');
    await waitForShinyIdle(page);

    // The placeholder the tab shows before a run is withdrawn, and the result
    // tabs are in its place. The KPI summary cards are deliberately not
    // asserted here: the app renders them for Full Analysis alone, since a
    // single replication has no confidence interval to report.
    await expect(page.locator('.alert-info', { hasText: 'No results yet' })).toHaveCount(0);
    const analyse = activePanel(page);
    for (const [tabName] of ANALYSE_TABS) {
      await expect(tabControl(analyse, tabName)).toBeVisible();
    }
    await expectNoOutputErrors(page);
  });

  for (const [tabName, plotId] of ANALYSE_TABS) {
    test(`the ${tabName} tab renders`, async () => {
      await openTab(page, 'Analyse');
      const analyse = activePanel(page);
      await tabControl(analyse, tabName).click();
      await waitForShinyIdle(page);

      // Something must be on the tab: either the plot it is named for, or
      // the explanatory alert the app renders when a run produced no data of
      // that kind. Both are correct; a blank tab is not.
      const panel = analyse.locator('.tab-pane.active').first();
      expect((await panel.innerText()).length).toBeGreaterThan(0);

      if (plotId !== null) {
        await expectPlotRendered(page, plotId);
      }
      await expectNoOutputErrors(page);
    });
  }
});
