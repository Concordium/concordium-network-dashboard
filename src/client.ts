import { Elm } from './elm/Main.elm';
import { setup as clipboardSetup } from './Clipboard';
import { setup as storageSetup } from './Storage';
import { loadCookieConsentChoice, setup as analyticsSetup } from './Analytics';

const { clientWidth, clientHeight } = document.documentElement;
const app = Elm.Main.init({
  flags: {
    window: {
      width: clientWidth,
      height: clientHeight,
    },
    isProduction: __PRODUCTION__,
    version: __VERSION__,
    minVersionIncludedInStats: __MIN_VERSION_INCLUDED_IN_STATS__,
    showCookieConsentBanner: loadCookieConsentChoice() === undefined,
  },
});

storageSetup(app);
clipboardSetup(app);
analyticsSetup(app);
