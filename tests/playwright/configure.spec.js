// The Configure panel's parameter round trip.
//
// Every field a planner edits reaches a run through the same path: the input
// binds, the paired exact-value box syncs to it, and the value is read back
// into the configuration the run is built from. These tests walk that path in
// the browser, which is the part shiny::testServer() coverage cannot reach.

const { test, expect } = require('@playwright/test');
const { openTab, waitForShinyIdle, openAccordionPanel, expectNoOutputErrors } = require('./helpers');

test.describe('Configure', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await openTab(page, 'Configure');
    await waitForShinyIdle(page);
  });

  test('a numeric parameter edit round-trips through the panel', async ({ page }) => {
    await openAccordionPanel(page, 'Force Size');

    const field = page.locator('#pop_combat');
    await expect(field).toBeVisible();
    const original = await field.inputValue();

    await field.fill('1234');
    await field.blur();
    await waitForShinyIdle(page);
    await expect(field).toHaveValue('1234');

    // The edit survives leaving the panel and coming back, which is what
    // makes it a round trip rather than a value sitting in the DOM.
    await openTab(page, 'Run');
    await openTab(page, 'Configure');
    await waitForShinyIdle(page);
    await expect(page.locator('#pop_combat')).toHaveValue('1234');

    await page.locator('#pop_combat').fill(original);
    await page.locator('#pop_combat').blur();
    await waitForShinyIdle(page);
    await expectNoOutputErrors(page);
  });

  test('a slider and its exact-value box stay in step', async ({ page }) => {
    await openTab(page, 'Run');
    await waitForShinyIdle(page);

    // n_reps is the one slider outside the accordion, so it needs no panel
    // opened; the two-way sync it exercises is the same one every screened
    // parameter in the Configure panel is wired with.
    const box = page.locator('#n_reps_txt');
    await expect(box).toBeVisible();
    await box.fill('250');
    await box.blur();
    await waitForShinyIdle(page);

    const slider = page.locator('#n_reps').first();
    await expect(slider).toHaveValue(/250/);
    await expectNoOutputErrors(page);
  });

  test('choosing a casualty intensity profile reports what it overrides', async ({ page }) => {
    const selector = page.locator('#scenario_select');
    await expect(selector).toBeVisible();

    const options = await selector.locator('option').allTextContents();
    expect(options.length).toBeGreaterThan(1);

    await selector.selectOption({ index: 1 });
    await waitForShinyIdle(page);
    await expect(page.locator('#scenario_scope_note .alert')).toBeVisible();

    // Returning to the base configuration withdraws the note, which is what
    // says the overlay was removed rather than compounded.
    await selector.selectOption('default');
    await waitForShinyIdle(page);
    await expect(page.locator('#scenario_scope_note .alert')).toHaveCount(0);
    await expectNoOutputErrors(page);
  });

  test('the structural diagrams render alongside the fields they describe', async ({ page }) => {
    await openAccordionPanel(page, 'Health System Architecture');
    await expect(page.locator('#force_design_diagram table')).toBeVisible();
    await expectNoOutputErrors(page);
  });
});
