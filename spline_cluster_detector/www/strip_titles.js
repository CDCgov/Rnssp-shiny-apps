(function () {
  function stripTitlesInPanels() {
    const roots = document.querySelectorAll('.tab-content, .bslib-sidebar-layout, .card, main');
    roots.forEach(root => {
      root.querySelectorAll('[title]').forEach(el => {
        el.removeAttribute('title');
        el.removeAttribute('data-bs-original-title');
        el.removeAttribute('data-bs-title');
      });
    });
  }

  function run() { stripTitlesInPanels(); }

  document.addEventListener('DOMContentLoaded', run);
  document.addEventListener('shown.bs.tab', run);

  const obs = new MutationObserver(() => run());
  obs.observe(document.documentElement, { childList: true, subtree: true });
})();
