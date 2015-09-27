function animateSvg(el) {
  var l;
  switch(el.tagName.toLowerCase()) {
    case 'circle':
      l = Math.PI * 2 * (+el.getAttribute('r'));
      break;
    case 'path':
      l = el.getTotalLength();
      break;
  }

  el.style.transition = el.style.WebkitTransition = 'none';

  el.style.strokeDasharray = l + ' ' + l;
  el.style.strokeDashoffset = l;

  el.getBoundingClientRect();
  el.style.transition = el.style.WebkitTransition = 'stroke-dashoffset 2s ease-in-out';

  el.style.strokeDashoffset = 0;
}

const circle = document.getElementById('logo--circle');
animateSvg(circle);

const eye = document.getElementById('logo--eye');
// animateSvg(eye);

const body1 = document.getElementById('logo--body1');
animateSvg(body1);

const body2 = document.getElementById('logo--body2');
animateSvg(body2);
// body2.style.strokeDashoffset += '-';

