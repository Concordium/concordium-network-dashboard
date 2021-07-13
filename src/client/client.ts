import { Elm } from './elm/Main';
import { setup as clipboardSetup } from './Clipboard';
import { setup as storageSetup } from './Storage';
import { loadCookieConsentChoice, setup as analyticsSetup } from './Analytics';

window.loadApp = (isProduction) => {
  const { clientWidth, clientHeight } = document.documentElement;
  const app = Elm.Main.init({
    flags: {
      window: {
        width: clientWidth,
        height: clientHeight,
      },
      isProduction,
      version: __VERSION__,
      showCookieConsentBanner: loadCookieConsentChoice() === undefined,
    },
  });

  storageSetup(app);
  clipboardSetup(app);
  analyticsSetup(app);
};
