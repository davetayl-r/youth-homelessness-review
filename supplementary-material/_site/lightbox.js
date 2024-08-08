document.addEventListener('DOMContentLoaded', (event) => {
  function setupLightbox(buttonId, lightboxId) {
    var lightbox = document.getElementById(lightboxId);
    var btn = document.getElementById(buttonId);
    var span = lightbox.getElementsByClassName("close")[0];

    btn.onclick = function() {
      lightbox.style.display = "block";
    }

    span.onclick = function() {
      lightbox.style.display = "none";
    }

    window.onclick = function(event) {
      if (event.target == lightbox) {
        lightbox.style.display = "none";
      }
    }
  }

  // Setup each of the four lightboxes
  setupLightbox("openLightbox1", "lightbox1");

  setupLightbox("openLightbox2", "lightbox2");
  
  setupLightbox("openLightbox3", "lightbox3");
  
  setupLightbox("openLightbox4", "lightbox4");
});