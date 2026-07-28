export function attachPanZoomImpl(selector) {
  return () => {
    const svg = document.querySelector(selector);
    if (!svg || svg.dataset.panzoom) return;
    svg.dataset.panzoom = "1";
    const vb = () => svg.viewBox.baseVal;
    svg.addEventListener('wheel', (e) => {
      e.preventDefault();
      const b = vb();
      const f = Math.exp(e.deltaY * 0.0012);
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
      // no capture yet: capturing on pointerdown retargets the derived
      // click to the svg, so child onClick handlers never fire
      drag = { x: e.clientX, y: e.clientY, id: e.pointerId, moved: false };
    });
    svg.addEventListener('pointermove', (e) => {
      if (!drag) return;
      if (!drag.moved) {
        // click tolerance: only becomes a pan after real movement
        if (Math.abs(e.clientX - drag.x) + Math.abs(e.clientY - drag.y) < 3) return;
        drag.moved = true;
        svg.setPointerCapture(drag.id);
      }
      const b = vb();
      const rect = svg.getBoundingClientRect();
      b.x -= (e.clientX - drag.x) * b.width / rect.width;
      b.y -= (e.clientY - drag.y) * b.height / rect.height;
      drag = { x: e.clientX, y: e.clientY, id: drag.id, moved: true };
    });
    svg.addEventListener('pointerup', () => { drag = null; });
    svg.addEventListener('pointerleave', () => { drag = null; });
  };
}
