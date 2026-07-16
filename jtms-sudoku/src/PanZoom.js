export function attachPanZoomImpl(selector) {
  return () => {
    const svg = document.querySelector(selector);
    if (!svg || svg.dataset.panzoom) return;
    svg.dataset.panzoom = "1";
    const vb = () => svg.viewBox.baseVal;
    svg.addEventListener('wheel', (e) => {
      e.preventDefault();
      const b = vb();
      const f = e.deltaY > 0 ? 1.15 : 1 / 1.15;
      const rect = svg.getBoundingClientRect();
      const mx = b.x + (e.clientX - rect.left) / rect.width * b.width;
      const my = b.y + (e.clientY - rect.top) / rect.height * b.height;
      b.x = mx - (mx - b.x) * f;
      b.y = my - (my - b.y) * f;
      b.width *= f;
      b.height *= f;
    }, { passive: false });
    let drag = null;
    svg.addEventListener('pointerdown', (e) => {
      drag = { x: e.clientX, y: e.clientY };
      svg.setPointerCapture(e.pointerId);
    });
    svg.addEventListener('pointermove', (e) => {
      if (!drag) return;
      const b = vb();
      const rect = svg.getBoundingClientRect();
      b.x -= (e.clientX - drag.x) * b.width / rect.width;
      b.y -= (e.clientY - drag.y) * b.height / rect.height;
      drag = { x: e.clientX, y: e.clientY };
    });
    svg.addEventListener('pointerup', () => { drag = null; });
    svg.addEventListener('pointerleave', () => { drag = null; });
  };
}
