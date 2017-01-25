<!-- README.md is generated from README.Rmd. Please edit that file -->
<!--html_preserve-->
<style type="text/css">svg#quanteda-logo{
    display: block;
    clear: both;
} </style>
<!--/html_preserve-->
<!--html_preserve-->
<svg id="quanteda-logo" width="300px" height="100px" role="img" aria-label="quanteda: quantitative analysis of textual data" id="Layer_1" data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1200 390">
<defs>
<style>.cls-1{fill:#0070c0;}</style>
</defs>
<title>
quanteda\_phrase
</title>
<path class="cls-1" d="M180,326H148.07l4.51-63.17H124q-10.54,0-19.11-.3a43.37,43.37,0,0,1-14.6-2.71,38.51,38.51,0,0,1-16.71-11.74Q66.8,240,66.8,225.8q0-5.11.3-9.33t.9-9.33l5.72-68q1.5-15.35,10.38-25.28a39.66,39.66,0,0,1,22.42-12.64,65.3,65.3,0,0,1,13.24-1.35q8.13-.15,18.06-.15,10.53,0,19,.3a43.18,43.18,0,0,1,14.45,2.71A38.5,38.5,0,0,1,188,114.43q6.77,8.13,6.77,22.27,0,5.12-.3,9.33t-.9,9.33ZM161.92,153q.3-2.71.45-4.82t.15-4.51q0-10.23-9-12.94a26.64,26.64,0,0,0-6.47-1.05q-3.46-.15-10.38-.15H127.9a55.85,55.85,0,0,0-7.83.6,15.45,15.45,0,0,0-10.54,5A18.82,18.82,0,0,0,105,146.34l-5.42,63.81q-.3,2.11-.45,4.67c-.1,1.71-.15,3.16-.15,4.36q0,9.93,8.43,12.64a33.9,33.9,0,0,0,8.43,1.05q5.42.16,9.93.15h7.83a55.17,55.17,0,0,0,7.83-.6A16.08,16.08,0,0,0,152,227.76a16.8,16.8,0,0,0,4.52-10.38Z"/><path class="cls-1" d="M313,99.69h31.91L334.09,223.4q-1.51,15.35-10.38,25.28a39.68,39.68,0,0,1-22.42,12.64,63.85,63.85,0,0,1-13.09,1.35q-8,.16-17.91.15-10.54,0-19.11-.3a43.37,43.37,0,0,1-14.6-2.71,38.52,38.52,0,0,1-16.71-11.74q-6.77-8.13-6.77-22.27,0-5.11.3-9.33t.9-9.33l9.33-107.46h32.21l-9.93,110.47q-.3,2.11-.45,4.67c-.1,1.71-.15,3.16-.15,4.36a14.81,14.81,0,0,0,2.41,8.43,10.26,10.26,0,0,0,6.62,4.51,55.48,55.48,0,0,0,8.13.75q4.81.16,8.43.15h8.28a60.22,60.22,0,0,0,8.58-.6,16.08,16.08,0,0,0,10.54-4.67,16.79,16.79,0,0,0,4.51-10.38Z"/><path class="cls-1" d="M421.56,99.69q10.53,0,19,.3A43.19,43.19,0,0,1,455,102.7a38.51,38.51,0,0,1,16.71,11.74q6.77,8.13,6.77,22.27,0,5.12-.3,9.33t-.9,9.33l-6,68q-1.51,15.35-10.38,25.28a39.68,39.68,0,0,1-22.42,12.64,63.86,63.86,0,0,1-13.09,1.35q-8,.16-17.91.15-10.54,0-19.11-.3a43.37,43.37,0,0,1-14.6-2.71A38.51,38.51,0,0,1,357,248.08q-6.77-8.13-6.77-22.27,0-8.13.6-15.2t1.2-14q1.5-15.35,10.54-25.28a40.4,40.4,0,0,1,22.58-12.64,63.74,63.74,0,0,1,11-1.2q7.07-.3,23.93-.3H436.3a10.7,10.7,0,0,0,6.17-2q2.86-2,3.46-6.77.3-2.1.45-3.76c.1-1.1.15-2.15.15-3.16q0-8.42-8.43-10.84a30.91,30.91,0,0,0-7.67-1.05q-4.67-.15-9.18-.15H377.31L380,99.69ZM442.63,187H406.36a56.66,56.66,0,0,0-8,.6,15.16,15.16,0,0,0-10.54,5.12,19.52,19.52,0,0,0-4.52,11.44,134.76,134.76,0,0,0-.9,14.75q0,10.24,9,12.94a30,30,0,0,0,7.53,1.05q4.51.16,10.23.15h8.58a51.12,51.12,0,0,0,7.07-.6,16.08,16.08,0,0,0,10.53-4.67,16.79,16.79,0,0,0,4.51-10.38Z"/><path class="cls-1" d="M590.45,153.87q.3-2.71.45-5.27t.15-5q0-10.23-9-12.94a26.63,26.63,0,0,0-6.47-1.05q-3.46-.15-10.38-.15h-8.73a55.83,55.83,0,0,0-7.83.6q-13.55,1.51-15.05,17.16l-9.93,115.58H491.72l10.54-122.51q1.5-16.55,10.38-26.49a39.66,39.66,0,0,1,22.43-12.64,65.28,65.28,0,0,1,13.24-1.35q8.13-.15,18.06-.15,10.53,0,19,.3a43.2,43.2,0,0,1,14.45,2.71,38.51,38.51,0,0,1,16.71,11.74q6.77,8.13,6.77,22.27,0,5.12-.3,10.38t-.9,10.38l-9.33,105.35H580.81Z"/><path class="cls-1" d="M657.19,52H689.1l-5,47.69h43.34l-2.71,29.5h-43l-7.22,81q-.3,2.11-.45,4.67c-.1,1.71-.15,3.16-.15,4.36q0,9.93,8.43,12.64a21,21,0,0,0,6,.75q4.51.16,9.93.3t10.23.15h7.22L713,262.83H700.79q-7.08,0-14-.3t-12.79-.9a34.62,34.62,0,0,1-8.88-1.81,38.51,38.51,0,0,1-16.71-11.74q-6.77-8.13-6.77-22.27,0-5.11.3-9.33t.9-9.33Z"/><path class="cls-1" d="M788.44,262.83q-10.54,0-19-.3A42.93,42.93,0,0,1,755,259.82a38.51,38.51,0,0,1-16.71-11.74q-6.77-8.13-6.77-22.27,0-5.11.3-9.33t.9-9.33l6-68q1.5-15.35,10.38-25.28a39.66,39.66,0,0,1,22.42-12.64,64.45,64.45,0,0,1,13.09-1.35q8-.15,17.91-.15,10.53,0,19.11.3a43.61,43.61,0,0,1,14.6,2.71A38.51,38.51,0,0,1,853,114.43q6.77,8.13,6.77,22.27,0,5.12-.75,13.24t-1.65,16q-1.51,15.35-10.38,25.28a39.67,39.67,0,0,1-22.42,12.64,21.34,21.34,0,0,1-4.67.75q-2.56.16-6.47.45t-9.63.3H765l-.6,5.12q-.3,3.32-.45,6.17c-.1,1.91-.15,3.36-.15,4.36q0,8.43,8.43,10.84a30.62,30.62,0,0,0,7.68,1.05q4.66.16,9.18.15h45.45l-2.71,29.8Zm-21.07-87.29h36a56.05,56.05,0,0,0,8-.6,15.16,15.16,0,0,0,10.54-5.12,19.57,19.57,0,0,0,4.51-11.44q.6-4.21.9-8t.3-6.77q0-10.23-9-12.94a30.28,30.28,0,0,0-7.52-1.05q-4.52-.15-10.23-.15H792.2a51.74,51.74,0,0,0-7.07.6,16.08,16.08,0,0,0-10.54,4.67,16.75,16.75,0,0,0-4.52,10.38Z"/><path class="cls-1" d="M990.74,223.4q-1.51,15.35-10.38,25.28a39.68,39.68,0,0,1-22.43,12.64,63.83,63.83,0,0,1-13.09,1.35q-8,.16-17.91.15-10.54,0-19.11-.3a43.37,43.37,0,0,1-14.6-2.71,38.52,38.52,0,0,1-16.71-11.74q-6.77-8.13-6.77-22.27,0-5.11.3-9.33t.9-9.33l5.72-68q1.5-15.35,10.38-25.28a39.66,39.66,0,0,1,22.42-12.64,65.28,65.28,0,0,1,13.24-1.35q8.13-.15,18.06-.15h28.9l4.21-48.16h31.91ZM964.85,153q.3-2.71.45-4.82t.15-4.51q0-10.23-9-12.94a26.64,26.64,0,0,0-6.47-1.05q-3.46-.15-10.38-.15h-8.73a55.84,55.84,0,0,0-7.83.6,15.45,15.45,0,0,0-10.54,5A18.81,18.81,0,0,0,908,146.34l-5.42,63.81q-.3,2.11-.45,4.51c-.1,1.61-.15,3-.15,4.21q0,10.24,9,12.94a30,30,0,0,0,7.53,1.05q4.51.16,10.23.15h8.58a51.11,51.11,0,0,0,7.07-.6,16.08,16.08,0,0,0,10.53-4.67,16.8,16.8,0,0,0,4.52-10.38Z"/><path class="cls-1" d="M1080.07,99.69q10.53,0,19,.3a43.19,43.19,0,0,1,14.45,2.71,38.51,38.51,0,0,1,16.71,11.74q6.77,8.13,6.77,22.27,0,5.12-.3,9.33t-.9,9.33l-6,68q-1.51,15.35-10.38,25.28a39.68,39.68,0,0,1-22.42,12.64,63.86,63.86,0,0,1-13.09,1.35q-8,.16-17.91.15-10.54,0-19.11-.3a43.36,43.36,0,0,1-14.6-2.71,38.52,38.52,0,0,1-16.71-11.74q-6.77-8.13-6.77-22.27,0-8.13.6-15.2t1.2-14q1.5-15.35,10.53-25.28a40.4,40.4,0,0,1,22.58-12.64,63.74,63.74,0,0,1,11-1.2q7.07-.3,23.93-.3h16.25a10.7,10.7,0,0,0,6.17-2q2.86-2,3.46-6.77.3-2.1.45-3.76t.15-3.16q0-8.42-8.43-10.84a30.92,30.92,0,0,0-7.68-1.05q-4.67-.15-9.18-.15h-43.95l2.71-29.8ZM1101.14,187h-36.27a56.67,56.67,0,0,0-8,.6,15.16,15.16,0,0,0-10.54,5.12,19.52,19.52,0,0,0-4.51,11.44,134.54,134.54,0,0,0-.9,14.75q0,10.24,9,12.94a30,30,0,0,0,7.53,1.05q4.51.16,10.23.15h8.58a51.11,51.11,0,0,0,7.07-.6,16.08,16.08,0,0,0,10.54-4.67,16.79,16.79,0,0,0,4.51-10.38Z"/><path class="cls-1" d="M230.88,318.54a6.86,6.86,0,0,1-1.29,4.34,5.65,5.65,0,0,1-3.36,2.1,12.25,12.25,0,0,1-1.34.22l1.23,6.38H221.7l-.84-6.22h-4.12q-1.09,0-2.07-.06t-1.82-.14c-.56-.06-1-.12-1.4-.2a5.64,5.64,0,0,1-3.36-2.1,6.85,6.85,0,0,1-1.29-4.34V292.78a6.86,6.86,0,0,1,1.29-4.34,5.65,5.65,0,0,1,3.36-2.1,22.14,22.14,0,0,1,3.22-.31q2.1-.08,4.17-.08T223,286a22.19,22.19,0,0,1,3.22.31,5.66,5.66,0,0,1,3.36,2.1,6.86,6.86,0,0,1,1.29,4.34Zm-12.1,3,2.46,0q1.34,0,2.3-.14a3.8,3.8,0,0,0,2.35-1.15,4.19,4.19,0,0,0,.73-2.72V293.78a4.19,4.19,0,0,0-.73-2.72,3.81,3.81,0,0,0-2.35-1.15q-1-.11-2.3-.14l-2.46,0-2.38,0q-1.32,0-2.27.14a3.8,3.8,0,0,0-2.35,1.15,4.18,4.18,0,0,0-.73,2.72v23.74a4.18,4.18,0,0,0,.73,2.72,3.79,3.79,0,0,0,2.35,1.15q1,.11,2.27.14Z"/><path class="cls-1" d="M261.88,295v15.18q0,3.14,0,5.43t-.14,3.42a7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11,11,0,0,1-2.88.53q-1.54.08-3.56.08t-3.56-.08a10.94,10.94,0,0,1-2.88-.53,6.62,6.62,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.11-1.12-.14-3.42t0-5.43V295h4v17.7q0,1.23,0,2.27t.06,1.85q0,.81,0,1.15a3.53,3.53,0,0,0,2.63,3.53,6.1,6.1,0,0,0,1.74.25q1.06,0,2.46,0t2.46,0a6.11,6.11,0,0,0,1.74-.25,3.53,3.53,0,0,0,2.63-3.53c0-.22,0-.6,0-1.12s0-1.14.06-1.85,0-1.47,0-2.3V295Z"/><path class="cls-1" d="M281.17,295q2,0,3.56.08a11,11,0,0,1,2.88.53,6.65,6.65,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75q.06.56.08,1.6t.06,2.27q0,1.23,0,2.55v4.87q0,1.32,0,2.55t-.06,2.24q0,1-.08,1.57a7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11,11,0,0,1-2.91.53q-1.57.08-3.58.08t-3.58-.08a11,11,0,0,1-2.91-.53,6.62,6.62,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75,10.61,10.61,0,0,1-.11-1.51v-3a12,12,0,0,1,.11-1.6,7,7,0,0,1,1.46-3.75,6.64,6.64,0,0,1,2.91-2,11.13,11.13,0,0,1,2.94-.53q1.6-.08,3.61-.08h4.2a3.32,3.32,0,0,0,1.93-.56,2.49,2.49,0,0,0,.81-2.18v-1.29a3.54,3.54,0,0,0-.67-2.35,4.07,4.07,0,0,0-2-1.18,6.25,6.25,0,0,0-1.76-.25q-1.09,0-2.49,0H274V295Zm0,15.18q-1.4,0-2.52,0a6.35,6.35,0,0,0-1.79.25,3.48,3.48,0,0,0-1.9,1.26,4.39,4.39,0,0,0-.73,2.21,8,8,0,0,0-.06,1v2.07a8.5,8.5,0,0,0,.06,1,4.4,4.4,0,0,0,.7,2.18,3.39,3.39,0,0,0,1.88,1.23,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0t2.49,0a6.22,6.22,0,0,0,1.76-.25,3.48,3.48,0,0,0,2.63-3.3q.17-1.85.17-4.26v-3.7Z"/><path class="cls-1" d="M318.77,325.31v-17.7q0-1.23,0-2.27t-.06-1.85q0-.81,0-1.15a3.53,3.53,0,0,0-2.63-3.53,6.16,6.16,0,0,0-1.74-.25q-1.06,0-2.46,0t-2.46,0a6.15,6.15,0,0,0-1.74.25,3.53,3.53,0,0,0-2.63,3.53q0,.34,0,1.15t-.06,1.85q0,1,0,2.3v17.67h-4V310.14q0-3.14,0-5.43t.14-3.42a7,7,0,0,1,1.46-3.75,6.63,6.63,0,0,1,2.91-2,11,11,0,0,1,2.88-.53q1.54-.08,3.56-.08t3.56.08a11,11,0,0,1,2.88.53,6.64,6.64,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75q.11,1.12.14,3.42t0,5.43v15.18Z"/><path class="cls-1" d="M342.69,325.31q-2,0-3.56-.08a11,11,0,0,1-2.88-.53,6.63,6.63,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.11-1.12-.14-3.42t0-5.43V288.8h4V295h8.62v3.64h-8.62v14q0,1.23,0,2.27t.06,1.85q0,.81,0,1.15a3.53,3.53,0,0,0,2.63,3.53,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0h1.68v3.64Z"/><path class="cls-1" d="M356.59,291h-4v-3.86h4Zm0,34.33h-4V295h4Z"/><path class="cls-1" d="M376.69,325.31q-2,0-3.56-.08a11,11,0,0,1-2.88-.53,6.63,6.63,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.11-1.12-.14-3.42t0-5.43V288.8h4V295h8.62v3.64h-8.62v14q0,1.23,0,2.27t.06,1.85q0,.81,0,1.15a3.53,3.53,0,0,0,2.63,3.53,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0h1.68v3.64Z"/><path class="cls-1" d="M396.54,295q2,0,3.56.08a11,11,0,0,1,2.88.53,6.65,6.65,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75q.06.56.08,1.6t.06,2.27q0,1.23,0,2.55v4.87q0,1.32,0,2.55t-.06,2.24q0,1-.08,1.57a7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11,11,0,0,1-2.91.53q-1.57.08-3.58.08t-3.58-.08a11,11,0,0,1-2.91-.53,6.62,6.62,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75,10.61,10.61,0,0,1-.11-1.51v-3a12,12,0,0,1,.11-1.6,7,7,0,0,1,1.46-3.75,6.64,6.64,0,0,1,2.91-2,11.13,11.13,0,0,1,2.94-.53q1.6-.08,3.61-.08h4.2a3.32,3.32,0,0,0,1.93-.56,2.49,2.49,0,0,0,.81-2.18v-1.29a3.54,3.54,0,0,0-.67-2.35,4.07,4.07,0,0,0-2-1.18,6.25,6.25,0,0,0-1.76-.25q-1.09,0-2.49,0h-7.22V295Zm0,15.18q-1.4,0-2.52,0a6.35,6.35,0,0,0-1.79.25,3.48,3.48,0,0,0-1.9,1.26,4.39,4.39,0,0,0-.73,2.21,8,8,0,0,0-.06,1v2.07a8.5,8.5,0,0,0,.06,1,4.4,4.4,0,0,0,.7,2.18,3.39,3.39,0,0,0,1.88,1.23,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0t2.49,0a6.22,6.22,0,0,0,1.76-.25,3.48,3.48,0,0,0,2.63-3.3q.17-1.85.17-4.26v-3.7Z"/><path class="cls-1" d="M427.07,325.31q-2,0-3.56-.08a11,11,0,0,1-2.88-.53,6.63,6.63,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.11-1.12-.14-3.42t0-5.43V288.8h4V295h8.62v3.64h-8.62v14q0,1.23,0,2.27t.06,1.85q0,.81,0,1.15a3.53,3.53,0,0,0,2.63,3.53,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0h1.68v3.64Z"/><path class="cls-1" d="M441,291h-4v-3.86h4Zm0,34.33h-4V295h4Z"/><path class="cls-1" d="M459.61,325.54a5.67,5.67,0,0,1-4-1.32,7.77,7.77,0,0,1-2-4.34L449,295h4.14l4.31,24.19a4.58,4.58,0,0,0,.87,2.27,3,3,0,0,0,2.21.64h.39a2.85,2.85,0,0,0,2.18-.64,5.15,5.15,0,0,0,.9-2.27L468.79,295h4.09l-5.26,24.92a8.2,8.2,0,0,1-2,4.34,5.57,5.57,0,0,1-4,1.32Z"/><path class="cls-1" d="M498.57,325.31h-7.73q-2,0-3.56-.08a11,11,0,0,1-2.88-.53,6.63,6.63,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.06-.56-.08-1.6t-.06-2.27q0-1.23,0-2.55v-4.87q0-1.32,0-2.55t.06-2.24q0-1,.08-1.57a7,7,0,0,1,1.46-3.75,6.64,6.64,0,0,1,2.91-2,11.07,11.07,0,0,1,2.91-.53q1.57-.08,3.58-.08t3.58.08a11.06,11.06,0,0,1,2.91.53,6.64,6.64,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75,10.77,10.77,0,0,1,.11,1.51v3a12.18,12.18,0,0,1-.11,1.6,7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11.07,11.07,0,0,1-2.94.53q-1.6.08-3.61.08h-6.94v4.09a3.36,3.36,0,0,0,2.63,3.53,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0h7.78Zm-7.73-15.18q1.4,0,2.52,0a6.28,6.28,0,0,0,1.79-.25,3.47,3.47,0,0,0,1.9-1.26,4.39,4.39,0,0,0,.73-2.21,8.16,8.16,0,0,0,.06-1v-2.07a8.71,8.71,0,0,0-.06-1,4.4,4.4,0,0,0-.7-2.18,3.4,3.4,0,0,0-1.88-1.23,6.29,6.29,0,0,0-1.76-.25q-1.09,0-2.49,0t-2.49,0a6.29,6.29,0,0,0-1.76.25,3.48,3.48,0,0,0-2.63,3.3q-.17,1.85-.17,4.26v3.7Z"/><path class="cls-1" d="M546.17,325.31v-15.4H530.6v15.4h-4.26V292.78a6.86,6.86,0,0,1,1.29-4.34,5.65,5.65,0,0,1,3.36-2.1,22.16,22.16,0,0,1,3.22-.31q2.1-.08,4.17-.08t4.17.08a22.17,22.17,0,0,1,3.22.31,5.65,5.65,0,0,1,3.36,2.1,6.86,6.86,0,0,1,1.29,4.34v32.54ZM530.6,306.16h15.57V293.78a4.19,4.19,0,0,0-.73-2.72,3.8,3.8,0,0,0-2.35-1.15q-1-.11-2.3-.14l-2.46,0-2.38,0q-1.32,0-2.27.14a3.8,3.8,0,0,0-2.35,1.15,4.18,4.18,0,0,0-.73,2.72Z"/><path class="cls-1" d="M577.65,325.31v-17.7q0-1.23,0-2.27t-.06-1.85q0-.81,0-1.15a3.53,3.53,0,0,0-2.63-3.53,6.16,6.16,0,0,0-1.74-.25q-1.06,0-2.46,0t-2.46,0a6.15,6.15,0,0,0-1.74.25,3.53,3.53,0,0,0-2.63,3.53q0,.34,0,1.15t-.06,1.85q0,1,0,2.3v17.67h-4V310.14q0-3.14,0-5.43t.14-3.42a7,7,0,0,1,1.46-3.75,6.63,6.63,0,0,1,2.91-2,11,11,0,0,1,2.88-.53q1.54-.08,3.56-.08t3.56.08a11,11,0,0,1,2.88.53,6.64,6.64,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75q.11,1.12.14,3.42t0,5.43v15.18Z"/><path class="cls-1" d="M601,295q2,0,3.56.08a11,11,0,0,1,2.88.53,6.65,6.65,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75q.06.56.08,1.6t.06,2.27q0,1.23,0,2.55v4.87q0,1.32,0,2.55t-.06,2.24q0,1-.08,1.57a7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11,11,0,0,1-2.91.53q-1.57.08-3.58.08t-3.58-.08a11,11,0,0,1-2.91-.53,6.62,6.62,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75,10.61,10.61,0,0,1-.11-1.51v-3a12,12,0,0,1,.11-1.6,7,7,0,0,1,1.46-3.75,6.64,6.64,0,0,1,2.91-2,11.13,11.13,0,0,1,2.94-.53q1.6-.08,3.61-.08h4.2a3.32,3.32,0,0,0,1.93-.56,2.49,2.49,0,0,0,.81-2.18v-1.29a3.54,3.54,0,0,0-.67-2.35,4.07,4.07,0,0,0-2-1.18,6.25,6.25,0,0,0-1.76-.25q-1.09,0-2.49,0h-7.22V295Zm0,15.18q-1.4,0-2.52,0a6.35,6.35,0,0,0-1.79.25,3.48,3.48,0,0,0-1.9,1.26,4.39,4.39,0,0,0-.73,2.21,8,8,0,0,0-.06,1v2.07a8.5,8.5,0,0,0,.06,1,4.4,4.4,0,0,0,.7,2.18,3.39,3.39,0,0,0,1.88,1.23,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0t2.49,0a6.22,6.22,0,0,0,1.76-.25,3.48,3.48,0,0,0,2.63-3.3q.17-1.85.17-4.26v-3.7Z"/><path class="cls-1" d="M620.69,286h4v30.94a8,8,0,0,0,.06,1,3.76,3.76,0,0,0,2.74,3.53,5.94,5.94,0,0,0,1.2.22q.64.06,1.37.06v3.7q-1.46,0-2.63-.08a7.44,7.44,0,0,1-2.18-.48,6.59,6.59,0,0,1-3-2.07,6.91,6.91,0,0,1-1.43-3.86q-.17-1.85-.17-3.64Z"/><path class="cls-1" d="M648.51,331.19q1.4,0,2.49,0a6.28,6.28,0,0,0,1.76-.25,3.51,3.51,0,0,0,2.63-3.53l.06-2.07h-6.94q-2,0-3.56-.08a10.94,10.94,0,0,1-2.88-.53,6.62,6.62,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.11-1.12-.14-3.42t0-5.43V295h4V312.6q0,1.23,0,2.3t.06,1.85q0,.78,0,1.12a3.53,3.53,0,0,0,2.63,3.53,6.1,6.1,0,0,0,1.74.25q1.06,0,2.46,0t2.46,0a6.11,6.11,0,0,0,1.74-.25,3.53,3.53,0,0,0,2.63-3.53q.06-.5.08-1.82t0-3V295h4V322.9q0,1.23,0,2.83t-.14,2.72a7,7,0,0,1-1.46,3.75,6.65,6.65,0,0,1-2.91,2,11,11,0,0,1-2.88.53q-1.54.08-3.56.08h-6.94v-3.64Z"/><path class="cls-1" d="M677,321.62l1.9,0a17.47,17.47,0,0,0,1.85-.14,3.13,3.13,0,0,0,2.63-1.57A6.66,6.66,0,0,0,684,317v-1.12a4.05,4.05,0,0,0-1-3.05,4.71,4.71,0,0,0-3.28-.92h-4.2q-5.82,0-7.06-4.65a9.06,9.06,0,0,1-.25-1.54q-.08-.92-.08-1.82t.06-1.82a13.05,13.05,0,0,1,.17-1.48,6,6,0,0,1,3-4.4,12.49,12.49,0,0,1,5.82-1.2h8.74v3.64h-8a17.29,17.29,0,0,0-2.27.11A3.63,3.63,0,0,0,673,300a4.71,4.71,0,0,0-.81,2.88v1.46a5.27,5.27,0,0,0,.48,2.32,2.53,2.53,0,0,0,1.76,1.32,8,8,0,0,0,1.54.2l1.6,0h3q3.92,0,5.68,1.93a7.39,7.39,0,0,1,1.76,5.18v2.41a8.41,8.41,0,0,1-.87,4.09,6.31,6.31,0,0,1-2.44,2.46,7.09,7.09,0,0,1-2.83.84,35.3,35.3,0,0,1-3.56.17h-9.52v-3.7Z"/><path class="cls-1" d="M700.45,291h-4v-3.86h4Zm0,34.33h-4V295h4Z"/><path class="cls-1" d="M718,321.62l1.9,0a17.47,17.47,0,0,0,1.85-.14,3.13,3.13,0,0,0,2.63-1.57A6.66,6.66,0,0,0,725,317v-1.12a4.05,4.05,0,0,0-1-3.05,4.71,4.71,0,0,0-3.28-.92h-4.2q-5.82,0-7.06-4.65a9.06,9.06,0,0,1-.25-1.54q-.08-.92-.08-1.82t.06-1.82a13.05,13.05,0,0,1,.17-1.48,6,6,0,0,1,3-4.4,12.49,12.49,0,0,1,5.82-1.2h8.74v3.64h-8a17.29,17.29,0,0,0-2.27.11,3.63,3.63,0,0,0-2.6,1.32,4.71,4.71,0,0,0-.81,2.88v1.46a5.27,5.27,0,0,0,.48,2.32,2.53,2.53,0,0,0,1.76,1.32,8,8,0,0,0,1.54.2l1.6,0h3q3.92,0,5.68,1.93a7.39,7.39,0,0,1,1.76,5.18v2.41a8.41,8.41,0,0,1-.87,4.09,6.31,6.31,0,0,1-2.44,2.46,7.09,7.09,0,0,1-2.83.84,35.3,35.3,0,0,1-3.56.17h-9.52v-3.7Z"/><path class="cls-1" d="M763.59,325.37q-2,0-3.58-.08a11,11,0,0,1-2.91-.53,6.63,6.63,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.11-1.12-.14-3.44t0-5.4q0-3.14,0-5.43t.14-3.42a7,7,0,0,1,1.46-3.75,6.64,6.64,0,0,1,2.91-2A11.07,11.07,0,0,1,760,295q1.57-.08,3.58-.08t3.58.08a11.06,11.06,0,0,1,2.91.53,6.64,6.64,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75q.11,1.12.14,3.42t0,5.43q0,3.08,0,5.4t-.14,3.44a7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11,11,0,0,1-2.91.53Q765.61,325.37,763.59,325.37Zm0-26.82q-1.4,0-2.49,0a6.27,6.27,0,0,0-1.76.25,3.53,3.53,0,0,0-2.63,3.53q0,.34,0,1.15t-.06,1.85q0,1,0,2.3v5q0,1.23,0,2.3t.06,1.85q0,.78,0,1.12a3.53,3.53,0,0,0,2.63,3.53,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0t2.49,0a6.21,6.21,0,0,0,1.76-.25,3.53,3.53,0,0,0,2.63-3.53q0-.34,0-1.15t.06-1.85q0-1,0-2.27v-5q0-1.23,0-2.27t-.06-1.85q0-.81,0-1.15a3.53,3.53,0,0,0-2.63-3.53,6.26,6.26,0,0,0-1.76-.25Q765,298.54,763.59,298.54Z"/><path class="cls-1" d="M797.48,305.38h-9.91v19.94h-4V301.23q0-3.14,0-5.43t.14-3.42a7,7,0,0,1,1.46-3.75,6.63,6.63,0,0,1,2.91-2,11,11,0,0,1,2.88-.53q1.54-.08,3.56-.08h3v3.64h-2.91q-1.4,0-2.49,0a6.29,6.29,0,0,0-1.76.25,3.53,3.53,0,0,0-2.63,3.53q-.06.73-.08,2t0,2.66v3.58h9.91Z"/><path class="cls-1" d="M818.32,286h22.06v3.81h-8.9v35.5h-4.26v-35.5h-8.9Z"/><path class="cls-1" d="M861.82,325.31h-7.73q-2,0-3.56-.08a11,11,0,0,1-2.88-.53,6.63,6.63,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.06-.56-.08-1.6t-.06-2.27q0-1.23,0-2.55v-4.87q0-1.32,0-2.55t.06-2.24q0-1,.08-1.57a7,7,0,0,1,1.46-3.75,6.64,6.64,0,0,1,2.91-2,11.07,11.07,0,0,1,2.91-.53q1.57-.08,3.58-.08t3.58.08a11.06,11.06,0,0,1,2.91.53,6.64,6.64,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75,10.77,10.77,0,0,1,.11,1.51v3a12.18,12.18,0,0,1-.11,1.6,7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11.07,11.07,0,0,1-2.94.53q-1.6.08-3.61.08h-6.94v4.09a3.36,3.36,0,0,0,2.63,3.53,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0h7.78Zm-7.73-15.18q1.4,0,2.52,0a6.28,6.28,0,0,0,1.79-.25,3.47,3.47,0,0,0,1.9-1.26,4.39,4.39,0,0,0,.73-2.21,8.16,8.16,0,0,0,.06-1v-2.07a8.71,8.71,0,0,0-.06-1,4.4,4.4,0,0,0-.7-2.18,3.4,3.4,0,0,0-1.88-1.23,6.29,6.29,0,0,0-1.76-.25q-1.09,0-2.49,0t-2.49,0a6.29,6.29,0,0,0-1.76.25,3.48,3.48,0,0,0-2.63,3.3q-.17,1.85-.17,4.26v3.7Z"/><path class="cls-1" d="M875.4,313.89a11.64,11.64,0,0,1,1.37-2.52,3.71,3.71,0,0,1,2.21-1.46v-.11a3.42,3.42,0,0,1-2.24-1.54,13.22,13.22,0,0,1-1.46-3.05L871.76,295h4.37l3.3,10.19a6.33,6.33,0,0,0,1,2,2.34,2.34,0,0,0,2,.78h.34a2.26,2.26,0,0,0,1.93-.78,7.84,7.84,0,0,0,1-2L889.06,295h4.26l-3.53,10.25a13.44,13.44,0,0,1-1.43,3,3.58,3.58,0,0,1-2.27,1.57v.11a3.94,3.94,0,0,1,2.21,1.46,10.4,10.4,0,0,1,1.43,2.52l4.48,11.42h-4.48l-4-10.75a5.26,5.26,0,0,0-1.18-2,2.51,2.51,0,0,0-1.85-.73h-.34a2.6,2.6,0,0,0-1.88.73,5.05,5.05,0,0,0-1.2,2l-4.09,10.75h-4.37Z"/><path class="cls-1" d="M912.33,325.31q-2,0-3.56-.08a11,11,0,0,1-2.88-.53,6.63,6.63,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.11-1.12-.14-3.42t0-5.43V288.8h4V295H914v3.64h-8.62v14q0,1.23,0,2.27t.06,1.85q0,.81,0,1.15a3.53,3.53,0,0,0,2.63,3.53,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0h1.68v3.64Z"/><path class="cls-1" d="M944,295v15.18q0,3.14,0,5.43t-.14,3.42a7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11,11,0,0,1-2.88.53q-1.54.08-3.56.08t-3.56-.08a10.94,10.94,0,0,1-2.88-.53,6.62,6.62,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.11-1.12-.14-3.42t0-5.43V295h4v17.7q0,1.23,0,2.27t.06,1.85q0,.81,0,1.15a3.53,3.53,0,0,0,2.63,3.53,6.1,6.1,0,0,0,1.74.25q1.06,0,2.46,0t2.46,0a6.11,6.11,0,0,0,1.74-.25,3.53,3.53,0,0,0,2.63-3.53c0-.22,0-.6,0-1.12s0-1.14.06-1.85,0-1.47,0-2.3V295Z"/><path class="cls-1" d="M963.31,295q2,0,3.56.08a11,11,0,0,1,2.88.53,6.65,6.65,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75q.06.56.08,1.6t.06,2.27q0,1.23,0,2.55v4.87q0,1.32,0,2.55t-.06,2.24q0,1-.08,1.57a7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11,11,0,0,1-2.91.53q-1.57.08-3.58.08t-3.58-.08a11,11,0,0,1-2.91-.53,6.62,6.62,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75,10.61,10.61,0,0,1-.11-1.51v-3a12,12,0,0,1,.11-1.6,7,7,0,0,1,1.46-3.75,6.64,6.64,0,0,1,2.91-2,11.13,11.13,0,0,1,2.94-.53q1.6-.08,3.61-.08h4.2a3.32,3.32,0,0,0,1.93-.56,2.49,2.49,0,0,0,.81-2.18v-1.29a3.54,3.54,0,0,0-.67-2.35,4.07,4.07,0,0,0-2-1.18,6.25,6.25,0,0,0-1.76-.25q-1.09,0-2.49,0h-7.22V295Zm0,15.18q-1.4,0-2.52,0a6.35,6.35,0,0,0-1.79.25,3.48,3.48,0,0,0-1.9,1.26,4.39,4.39,0,0,0-.73,2.21,8,8,0,0,0-.06,1v2.07a8.5,8.5,0,0,0,.06,1,4.4,4.4,0,0,0,.7,2.18,3.39,3.39,0,0,0,1.88,1.23,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0t2.49,0a6.22,6.22,0,0,0,1.76-.25,3.48,3.48,0,0,0,2.63-3.3q.17-1.85.17-4.26v-3.7Z"/><path class="cls-1" d="M983,286h4v30.94a8,8,0,0,0,.06,1,3.76,3.76,0,0,0,2.74,3.53,5.94,5.94,0,0,0,1.2.22q.64.06,1.37.06v3.7q-1.46,0-2.63-.08a7.44,7.44,0,0,1-2.18-.48,6.59,6.59,0,0,1-3-2.07,6.91,6.91,0,0,1-1.43-3.86q-.17-1.85-.17-3.64Z"/><path class="cls-1" d="M1015.76,325.31V292.78a6.86,6.86,0,0,1,1.29-4.34,5.65,5.65,0,0,1,3.36-2.1,22.16,22.16,0,0,1,3.22-.31q2.1-.08,4.17-.08t4.17.08a22.17,22.17,0,0,1,3.22.31,5.65,5.65,0,0,1,3.36,2.1,6.86,6.86,0,0,1,1.29,4.34v25.7a6.86,6.86,0,0,1-1.29,4.34,5.64,5.64,0,0,1-3.36,2.1c-.37.08-.84.14-1.4.2s-1.17.1-1.82.14-1.34.06-2.07.06h-14.14Zm4.26-3.81h7.73l2.46,0q1.34,0,2.3-.14a3.79,3.79,0,0,0,2.35-1.15,4.18,4.18,0,0,0,.73-2.72V293.78a4.19,4.19,0,0,0-.73-2.72,3.8,3.8,0,0,0-2.35-1.15q-1-.11-2.3-.14l-2.46,0-2.38,0q-1.32,0-2.27.14a3.8,3.8,0,0,0-2.35,1.15,4.18,4.18,0,0,0-.73,2.72Z"/><path class="cls-1" d="M1059.12,295q2,0,3.56.08a11,11,0,0,1,2.88.53,6.65,6.65,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75q.06.56.08,1.6t.06,2.27q0,1.23,0,2.55v4.87q0,1.32,0,2.55t-.06,2.24q0,1-.08,1.57a7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11,11,0,0,1-2.91.53q-1.57.08-3.58.08t-3.58-.08a11,11,0,0,1-2.91-.53,6.62,6.62,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75,10.61,10.61,0,0,1-.11-1.51v-3a12,12,0,0,1,.11-1.6,7,7,0,0,1,1.46-3.75,6.64,6.64,0,0,1,2.91-2,11.13,11.13,0,0,1,2.94-.53q1.6-.08,3.61-.08h4.2a3.32,3.32,0,0,0,1.93-.56,2.49,2.49,0,0,0,.81-2.18v-1.29a3.54,3.54,0,0,0-.67-2.35,4.07,4.07,0,0,0-2-1.18,6.25,6.25,0,0,0-1.76-.25q-1.09,0-2.49,0H1052V295Zm0,15.18q-1.4,0-2.52,0a6.35,6.35,0,0,0-1.79.25,3.48,3.48,0,0,0-1.9,1.26,4.39,4.39,0,0,0-.73,2.21,8,8,0,0,0-.06,1v2.07a8.5,8.5,0,0,0,.06,1,4.4,4.4,0,0,0,.7,2.18,3.39,3.39,0,0,0,1.88,1.23,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0t2.49,0a6.22,6.22,0,0,0,1.76-.25,3.48,3.48,0,0,0,2.63-3.3q.17-1.85.17-4.26v-3.7Z"/><path class="cls-1" d="M1089.65,325.31q-2,0-3.56-.08a11,11,0,0,1-2.88-.53,6.63,6.63,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75q-.11-1.12-.14-3.42t0-5.43V288.8h4V295h8.62v3.64h-8.62v14q0,1.23,0,2.27t.06,1.85q0,.81,0,1.15a3.53,3.53,0,0,0,2.63,3.53,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0h1.68v3.64Z"/><path class="cls-1" d="M1109.5,295q2,0,3.56.08a11,11,0,0,1,2.88.53,6.65,6.65,0,0,1,2.91,2,7,7,0,0,1,1.46,3.75q.06.56.08,1.6t.06,2.27q0,1.23,0,2.55v4.87q0,1.32,0,2.55t-.06,2.24q0,1-.08,1.57a7,7,0,0,1-1.46,3.75,6.63,6.63,0,0,1-2.91,2,11,11,0,0,1-2.91.53q-1.57.08-3.58.08t-3.58-.08a11,11,0,0,1-2.91-.53,6.62,6.62,0,0,1-2.91-2,7,7,0,0,1-1.46-3.75,10.61,10.61,0,0,1-.11-1.51v-3a12,12,0,0,1,.11-1.6,7,7,0,0,1,1.46-3.75,6.64,6.64,0,0,1,2.91-2,11.13,11.13,0,0,1,2.94-.53q1.6-.08,3.61-.08h4.2a3.32,3.32,0,0,0,1.93-.56,2.49,2.49,0,0,0,.81-2.18v-1.29a3.54,3.54,0,0,0-.67-2.35,4.07,4.07,0,0,0-2-1.18,6.25,6.25,0,0,0-1.76-.25q-1.09,0-2.49,0h-7.22V295Zm0,15.18q-1.4,0-2.52,0a6.35,6.35,0,0,0-1.79.25,3.48,3.48,0,0,0-1.9,1.26,4.39,4.39,0,0,0-.73,2.21,8,8,0,0,0-.06,1v2.07a8.5,8.5,0,0,0,.06,1,4.4,4.4,0,0,0,.7,2.18,3.39,3.39,0,0,0,1.88,1.23,6.22,6.22,0,0,0,1.76.25q1.09,0,2.49,0t2.49,0a6.22,6.22,0,0,0,1.76-.25,3.48,3.48,0,0,0,2.63-3.3q.17-1.85.17-4.26v-3.7Z"/>
</svg>
<!--/html_preserve-->
[![CRAN Version](https://www.r-pkg.org/badges/version/quanteda)](https://CRAN.R-project.org/package=quanteda) [![Downloads](https://cranlogs.r-pkg.org/badges/quanteda)](https://CRAN.R-project.org/package=quanteda) [![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quanteda?color=orange)](https://CRAN.R-project.org/package=quanteda) [![Travis-CI Build Status](https://travis-ci.org/kbenoit/quanteda.svg?branch=master)](https://travis-ci.org/kbenoit/quanteda) [![Build status](https://ci.appveyor.com/api/projects/status/e3tf2h1ff0nlv249/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/quanteda/branch/master) [![codecov.io](https://codecov.io/github/kbenoit/quanteda/coverage.svg?branch=master)](https://codecov.io/gh/kbenoit/quanteda/branch/master)

**quanteda** v0.9.9 under development
-------------------------------------

This version of the package is a transitional release prior to v1.0. It includes some major API changes (see below), but with the most of the older functions retained and deprecated. v0.9.9 also implements many enhancements and performance improvements. See [Quanteda Structure and Design](https://kbenoit.github.io/quanteda/articles/development-plans.html) for details.

About the package
-----------------

An R package for managing and analyzing text, created by [Kenneth Benoit](kbenoit@lse.ac.uk) in collaboration with a team of core [contributors](https://github.com/kbenoit/quanteda/graphs/contributors): [Paul Nulty](https://github.com/pnulty), [Adam Obeng](https://github.com/adamobeng), [Kohei Watanabe](https://github.com/koheiw), [Haiyan Wang](https://github.com/HaiyanLW), [Ben Lauderdale](https://github.com/lauderdale), and [Will Lowe](https://github.com/conjugateprior).
Supported by the European Research Council grant ERC-2011-StG 283794-QUANTESS.

For more details, see the [**package website**](https://kbenoit.github.io/quanteda/).

### Leave feedback

If you like **quanteda**, please consider leaving [feedback or a testimonial here](https://github.com/kbenoit/quanteda/issues/461).

Features
--------

### Powerful text analytics

**Generalized, flexible corpus management.**
**quanteda** provides a comprehensive workflow and ecosystem for the management, processing, and analysis of texts. Documents and associated document- and collection-level metadata are easily loaded and stored as a *corpus* object, although most of **quanteda**'s operations work on simple character objects as well. A corpus is designed to efficiently store all of the texts in a collection, as well as meta-data for documents and for the collection as a whole. This makes it easy to perform natural language processing on the texts in a corpus simply and quickly, such as tokenizing, stemming, or forming ngrams. **quanteda**'s functions for tokenizing texts and forming multiple tokenized documents into a *document-feature matrix* are both extremely fast and extremely simple to use. **quanteda** can segment texts easily by words, paragraphs, sentences, or even user-supplied delimiters and tags.

**Works nicely with UTF-8**.
Built on the text processing functions in the **stringi** package, which is in turn built on C++ implementation of the [ICU](http://www.icu-project.org/) libraries for Unicode text handling, **quanteda** pays special attention to fast and correct implementation of Unicode and the handling of text in any character set, following conversion internally to UTF-8.

**Built for efficiency and speed**.
All of the functions in **quanteda** are built for maximum performance and scale while still being as R-based as possible. The package makes use of three efficient architectural elements:
the **stringi** package for text processing, the **Matrix** package for sparse matrix objects, and the **data.table** package for indexing large documents efficiently. If you can fit it into memory, **quanteda** will handle it quickly. (And eventually, we will make it possible to process objects even larger than available memory.)

**Super-fast conversion of texts into a document-feature matrix**.
**quanteda** is principally designed to allow users a fast and convenient method to go from a corpus of texts to a selected matrix of documents by features, after defining and selecting the documents and features. The package makes it easy to redefine documents, for instance by splitting them into sentences or paragraphs, or by tags, as well as to group them into larger documents by document variables, or to subset them based on logical conditions or combinations of document variables. A special variation of the "dfm", a *feature co-occurrence matrix*, is also implemented, for direct use with embedding and representational models such as [**text2vec**](https://github.com/dselivanov/text2vec).

**Extensive feature selection capabilities**.
The package also implements common NLP feature selection functions, such as removing stopwords and stemming in numerous languages, selecting words found in dictionaries, treating words as equivalent based on a user-defined "thesaurus", and trimming and weighting features based on document frequency, feature frequency, and related measures such as *tf-idf*.

**Qualitative exploratory tools**.
Easily search and save *keywords in context*, for instance, or identify keywords. Like all of **quanteda**'s pattern matching functions, users have the option of simple "[glob](https://en.wikipedia.org/wiki/Glob_(programming))" expressions, regular expressions, or fixed pattern matches.

**Dictionary-based analysis**.
**quanteda** allows fast and flexible implementation of dictionary methods, including the import and conversion of foreign dictionary formats such as those from Provalis's WordStat, the Linguistic Inquiry and Word Count (LIWC), Lexicoder, and Yoshioder.

**Text analytic methods**.
Once constructed, a *dfm* can be easily analyzed using either **quanteda**'s built-in tools for scaling document positions (for the "wordfish" and "Wordscores" models, direct use with the [**ca**](https://CRAN.R-project.org/package=ca) package for correspondence analysis), predictive models using Naive Bayes multinomial and Bernoulli classifiers, computing distance or similarity matrixes of features or documents, or computing readability or lexical diversity indexes.

In addition, **quanteda** a document-feature matrix is easily used with or converted for a number of other text analytic tools, such as:

-   *topic models* (including converters for direct use with the [**topicmodels**](https://CRAN.R-project.org/package=topicmodels), [**LDA**](https://CRAN.R-project.org/package=lda), and [**stm**](http://www.structuraltopicmodel.com) packages);

-   machine learning through a variety of other packages that take matrix or matrix-like inputs.

**Planned features.** Coming soon to **quanteda** are:

-   *Bootstrapping methods* for texts that makes it easy to resample texts from pre-defined units, to facilitate computation of confidence intervals on textual statistics using techniques of non-parametric bootstrapping, but applied to the original texts as data.

-   *Additional predictive and analytic methods* by expanding the `textstat_` and `textmodel_` functions. Current textmodel types include correspondence analysis, "Wordscores", "Wordfish", and Naive Bayes; current textstat statistics are readability, lexical diversity, similarity, and distance.

-   *Expanded settings* for all objects, that will propogate through downstream objects.

-   *Object histories*, that will propogate through downstream objects, to enhance analytic reproducibility and transparency.

How to Install
--------------

1.  From [CRAN](https://CRAN.R-project.org/package=quanteda): Use your GUI's R package installer, or execute:

    ``` r
    install.packages("quanteda") 
    ```

2.  From [GitHub](https://github.com/kbenoit/quanteda), using:

    ``` r
    # devtools packaged required to install quanteda from Github 
    devtools::install_github("kbenoit/quanteda") 
    ```

    Because this compiles some C++ source code, you will need a compiler installed. If you are using a Windows platform, this means you will need also to install the [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/) software available from CRAN. If you are using OS X, you will need to to install XCode, available for free from the App Store, or if you prefer a lighter footprint set of tools, [just the Xcode command line tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/), using the command `xcode-select --install` from the Terminal.

3.  Additional recommended packages:

    The following packages work well with **quanteda** and we recommend that you also install them:

    -   [**readtext**](https://github.com/kbenoit/readtext): For reading text data into R.

        ``` r
        devtools::install_github("kbenoit/readtext")
        ```

    -   [**quantedaData**](https://github.com/kbenoit/quantedaData): Additional textual data for use with **quanteda**.

        ``` r
        r devtools::install_github("kbenoit/quantedaData")
        ```

    -   [**spacyr**](https://github.com/kbenoit/spacyr): NLP using the [spaCy](http://spacy.io) library.

Getting Started
---------------

See the [package website](http://kbenoit.github.io/quanteda), which includes the [Getting Started Vignette](https://kbenoit.github.io/quanteda/articles/quickstart.html).

Demonstration
-------------

``` r
library(quanteda)
## quanteda version 0.9.9.15
## 
## Attaching package: 'quanteda'
## The following object is masked from 'package:utils':
## 
##     View
## The following object is masked from 'package:base':
## 
##     sample

# create a corpus from the immigration texts from UK party platforms
uk2010immigCorpus <- 
    corpus(data_char_ukimmig2010,
           docvars = data.frame(party = names(data_char_ukimmig2010)),
           metacorpus = list(notes = "Immigration-related sections of 2010 UK party manifestos"))
uk2010immigCorpus
## Corpus consisting of 9 documents and 1 docvar.
summary(uk2010immigCorpus)
## Corpus consisting of 9 documents.
## 
##          Text Types Tokens Sentences        party
##           BNP  1126   3330        88          BNP
##     Coalition   144    268         4    Coalition
##  Conservative   252    503        15 Conservative
##        Greens   325    687        21       Greens
##        Labour   296    703        29       Labour
##        LibDem   257    499        14       LibDem
##            PC    80    118         5           PC
##           SNP    90    136         4          SNP
##          UKIP   346    739        27         UKIP
## 
## Source:  /home/kohei/packages/quanteda/* on x86_64 by kohei
## Created: Wed Jan 25 07:06:08 2017
## Notes:   Immigration-related sections of 2010 UK party manifestos

# key words in context for "deport", 3 words of context
kwic(uk2010immigCorpus, "deport", 3)
##                                                                    
## [BNP, 159]         The BNP will | deport | all foreigners convicted
## [BNP, 1970]                . 2. | Deport | all illegal immigrants  
## [BNP, 1976] immigrants We shall | deport | all illegal immigrants  
## [BNP, 2621]  Criminals We shall | deport | all criminal entrants

# create a dfm, removing stopwords
mydfm <- dfm(uk2010immigCorpus, remove = c("will", stopwords("english")),
             removePunct = TRUE)
mydfm
## Document-feature matrix of: 9 documents, 1,547 features (83.8% sparse).

topfeatures(mydfm, 20)  # 20 top words
## immigration     british      people      asylum     britain          uk 
##          66          37          35          29          28          27 
##      system  population     country         new  immigrants      ensure 
##          27          21          20          19          17          17 
##       shall citizenship      social    national         bnp     illegal 
##          17          16          14          14          13          13 
##        work     percent 
##          13          12

# plot a word cloud
textplot_wordcloud(mydfm, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
```

![](images/quanteda_example-1.png)

Contributing
------------

Contributions in the form of feedback, comments, code, and bug reports are most welcome. How to contribute:

-   Fork the source code, modify, and issue a [pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/) through the [project GitHub page](https://github.com/kbenoit/quanteda). See our [Contributor Code of Conduct](https://github.com/kbenoit/quanteda/blob/master/CONDUCT.md).

-   Issues, bug reports, and wish lists: [File a GitHub issue](https://github.com/kbenoit/quanteda/issues).

-   Usage questions: Submit a question on the [**quanteda** channel on StackOverflow](http://stackoverflow.com/questions/tagged/quanteda).

-   Contact [the maintainer](kbenoit@lse.ac.uk) by email.
