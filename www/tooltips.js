// Tooltips aktivieren für alle Info-Icons
$(document).ready(function() {
  // Aktiviere Bootstrap Tooltips
  $('[data-toggle="tooltip"]').tooltip({
    container: 'body',
    trigger: 'hover',
    html: true
  });

  // Reaktiviere nach Shiny Updates
  $(document).on('shiny:value', function(event) {
    setTimeout(function() {
      $('[data-toggle="tooltip"]').tooltip({
        container: 'body',
        trigger: 'hover',
        html: true
      });
    }, 100);
  });
});

// === i18n Language Switch Redirect ===
Shiny.addCustomMessageHandler('redirect', function(url) {
  window.location.href = url;
});

// === Language Selector Change Handler ===
$(document).ready(function() {
  // Aktuelle Sprache aus URL ermitteln
  var urlParams = new URLSearchParams(window.location.search);
  var currentLang = urlParams.get('lang') || 'de';

  // Change-Handler nur bei echter Benutzeraktion
  $(document).on('change', '#selected_language', function() {
    var selectedLang = $(this).val();
    // Nur redirect wenn Sprache sich wirklich ändert
    if (selectedLang !== currentLang) {
      // Cache-Busting: Timestamp hinzufügen um Shiny-Caching zu umgehen
      var timestamp = new Date().getTime();
      var newUrl = window.location.pathname + '?lang=' + selectedLang + '&_t=' + timestamp;
      window.location.href = newUrl;
    }
  });
});
