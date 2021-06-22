export function setup(app) {
  gtag('consent', 'update', {
    ad_storage: 'denied',
    analytics_storage: loadCookieConsentChoice() ? 'granted' : 'denied',
  });
  gtag('js', new Date());
  gtag('config', __ANALYTICS_ID__, { anonymize_ip: true });

  app.ports.setPageConfig.subscribe(setGtagPagePath);
  app.ports.setCookieConsent.subscribe(setCookieConsent);
}

// Set the current path for better analytics
function setGtagPagePath(pagePath) {
  gtag('config', __ANALYTICS_ID__, { page_path: pagePath });
}

// Set consent for analytics
function setCookieConsent(granted) {
  storeChoice(granted);
  gtag('consent', 'update', { ad_storage: 'denied', analytics_storage: granted ? 'granted' : 'denied' });
}

// Save the choice in a cookie
function storeChoice(granted) {
  document.cookie = 'cookie-consent=' + granted;
}

// Look for a cookie storing the previous choice of the user.
// Returns undefined if no choice was stored, otherwise a boolean for whether the
// user gave consent.
export function loadCookieConsentChoice() {
  if (!document.cookie.includes('cookie-consent=')) {
    return undefined;
  }
  return document.cookie.includes('cookie-consent=true');
}
