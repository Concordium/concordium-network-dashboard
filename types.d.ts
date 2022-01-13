declare const __VERSION__: string;
declare const __STATS_VERSION__: string;
declare const __ANALYTICS_ID__: string;
declare const __PRODUCTION__: boolean;
// Defined globally in index.ejs
declare function gtag<A extends any[]>(...a: A): void;
declare module '*.elm' {
  export const Elm: any;
}
