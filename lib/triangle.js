(function() {
  'use strict';
  window.onload = function() {
    var target = document.querySelector('.hero');
    var greyHues = [
      '#969696',
      '#525252',
      '#525252',
      '#969696'
    ];

    var t = new Trianglify({
      cellsize: 80,
      x_gradient: greyHues,
      fillOpacity: 0.6
    });
    var pattern = t.generate(target.clientWidth, target.clientHeight);
    target.style['background-image'] = pattern.dataUrl;
  }
})();
