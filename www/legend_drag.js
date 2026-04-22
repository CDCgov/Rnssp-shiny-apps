// Function make leaflet legends Draggable

window.makeLeafletLegendDraggable = function(el) {
  var legend = el.querySelector('.info.legend');
  if (!legend) return;

  // Avoid re-initializing if the widget re-renders
  if (legend.__draggableLegendEnabled) return;
  legend.__draggableLegendEnabled = true;

  legend.style.cursor = 'move';
  legend.style.pointerEvents = 'auto';

  // Prevent map interactions while dragging the legend
  L.DomEvent.disableClickPropagation(legend);
  L.DomEvent.disableScrollPropagation(legend);
  L.DomEvent.on(legend, 'mousedown touchstart dblclick', L.DomEvent.stopPropagation);

  // Enable dragging
  var draggable = new L.Draggable(legend);
  draggable.enable();
};

