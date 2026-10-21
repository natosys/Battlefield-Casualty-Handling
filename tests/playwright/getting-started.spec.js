// The Getting Started tab, which is the app's own documentation. It is the cheapest possible check
// that the app started, served its assets and rendered its navbar.

const { test, expect } = require('@playwright/test');
const { openTab, activePanel, expectNoOutputErrors } = require('./helpers');

test.describe('Getting Started', () => {
  test('loads with the user guide rendered', async ({ page }) => {
    await page.goto('/');
    await openTab(page, 'Getting Started');

    // includeMarkdown() renders docs/Getting_Started.md into the panel, so a
    // heading and a body of real length are what a successful render looks
    // like; an empty panel means the file was not found at the app's own
    // working directory.
    const panel = activePanel(page);
    await expect(panel.locator('h1, h2').first()).toBeVisible();
    expect((await panel.innerText()).length).toBeGreaterThan(500);
    await expectNoOutputErrors(page);
  });

  test('offers every top-level tab and no Analyse tab', async ({ page }) => {
    await page.goto('/');
    for (const name of ['Getting Started', 'Configure', 'Run']) {
      await expect(page.locator('.navbar').getByRole('tab', { name, exact: true })).toBeVisible();
    }
    await expect(page.locator('.navbar').getByRole('tab', { name: 'Analyse', exact: true }))
      .toHaveCount(0);
  });

});
