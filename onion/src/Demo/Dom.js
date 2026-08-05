// FFI for Demo.Dom — the only FFI in the demo package.

export const setSvgContent = (id) => (svgMarkup) => () => {
  const el = document.getElementById(id);
  if (el) el.innerHTML = svgMarkup;
};

export const setCellsSvg = (id) => (svgArray) => () => {
  const el = document.getElementById(id);
  if (el) el.innerHTML = svgArray.join('');
};

export const setCode = (id) => (text) => () => {
  const el = document.getElementById(id);
  if (el) el.textContent = text;
};

export const elementExists = (id) => () => !!document.getElementById(id);

// Tick from a TIMER, not requestAnimationFrame.
//
// rAF fires zero callbacks in a hidden or covered tab — measured — so a
// rAF-driven reveal does not slow down, it hangs forever, with no error and
// nothing in the console. A timer throttles in the background but always
// finishes. Anything destined for a one-pass screen capture has to survive
// the recorder's own window covering the tab, so this is the safe default.
//
// The delta passed to the handler is the FIXED nominal step, not the measured
// wall-clock gap: the same tick sequence then produces the same frames on
// every run, which is what makes a capture reproducible.
export const everyTick = (stepMs) => (handler) => () => {
  const id = setInterval(() => handler(stepMs)(), stepMs);
  return () => { clearInterval(id); };
};

export const onClick = (id) => (handler) => () => {
  const el = document.getElementById(id);
  if (el) el.addEventListener('click', () => handler());
};
