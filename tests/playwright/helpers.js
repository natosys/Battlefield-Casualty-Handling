// Shared helpers for the console's browser suite.
//
// Shiny renders every panel from the server, so almost every assertion here
// has to wait for a round trip rather than for a page load. The waits below
// are expressed against what the user would look at (a control being enabled,
// a diagram having rendered) rather than against Shiny's internal busy flag,
// which flickers between the several messages one interaction produces.

const { expect } = require('@playwright/test');

/**
 * Open one of the console's top-level tabs and wait for it to be shown.
 *
 * page_navbar() renders its panels as a tablist, so the lookup is scoped to the
 * navbar rather than matching by role across the whole page.
 *
 * @param {import('@playwright/test').Page} page
 * @param {string} name Tab label as it appears in the navbar.
 */
async function openTab(page, name) {
  await page.locator('.navbar').getByRole('tab', { name, exact: true }).click();
  await expect(activePanel(page)).toBeVisible();
}

/**
 * The top-level tab panel currently on screen.
 *
 * @param {import('@playwright/test').Page} page
 * @returns {import('@playwright/test').Locator}
 */
function activePanel(page) {
  return page.locator('.tab-content > .tab-pane.active').first();
}

/**
 * Wait until Shiny has finished the round trip an interaction started.
 *
 * Shiny sets html.shiny-busy for the duration of a recalculation. A single
 * interaction can produce several, so this waits for the class to be absent
 * and to stay absent briefly, rather than for one transition.
 *
 * @param {import('@playwright/test').Page} page
 * @param {number} timeout Milliseconds to allow.
 */
async function waitForShinyIdle(page, timeout = 60000) {
  await page.waitForFunction(
    () => !document.documentElement.classList.contains('shiny-busy'),
    null,
    { timeout },
  );
  await page.waitForTimeout(250);
  await page.waitForFunction(
    () => !document.documentElement.classList.contains('shiny-busy'),
    null,
    { timeout },
  );
}

/**
 * Open a Configure accordion panel by its heading, if it is not already open.
 *
 * @param {import('@playwright/test').Page} page
 * @param {string} heading Panel heading text.
 */
async function openAccordionPanel(page, heading) {
  const button = page.locator('.accordion-button', { hasText: heading }).first();
  await button.scrollIntoViewIfNeeded();
  if ((await button.getAttribute('aria-expanded')) !== 'true') {
    await button.click();
  }
  await waitForShinyIdle(page);
}

/**
 * Assert that nothing on the page is reporting an R error.
 *
 * Shiny renders a failed output as .shiny-output-error, with the message in
 * the element when the app is not in sanitised mode. A tab that "renders"
 * only such elements is a tab that is broken, so every tab assertion checks
 * this alongside its own expectations.
 *
 * @param {import('@playwright/test').Page} page
 */
async function expectNoOutputErrors(page) {
  await expect(page.locator('.shiny-output-error:visible')).toHaveCount(0);
}

module.exports = {
  openTab,
  activePanel,
  waitForShinyIdle,
  openAccordionPanel,
  expectNoOutputErrors,
};
