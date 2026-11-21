// The page's clock, for the film: in film mode (?film) nothing runs by
// itself and the renderer steps the page frame by frame through
// window.macondo.step(n). Elsewhere requestAnimationFrame drives it.
export const filmMode = () => new URLSearchParams(window.location.search).has("film");

export const exposeStep = (step) => (cues) => () => {
  // CSS transitions run on the wall clock, which the film does not keep:
  // a film frame shows exactly what the frame clock says, so they are off.
  document.body.classList.add("film");
  window.macondo = { step: (n) => step(n)(), cues, ready: true };
};

export const reload = () => window.location.reload();

export const setOpacity = (selector) => (value) => () => {
  for (const element of document.querySelectorAll(selector)) element.style.opacity = String(value);
};
