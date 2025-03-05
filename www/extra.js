$(document).on('click', '.btn-talk-more-info, .btn-talk-more-info- info', function(ev) {
  Shiny.setInputValue('talk_more_info', ev.target.closest('.btn').dataset.value, {priority: 'event'})
})

$().ready(function() {
  document.querySelector('.navbar-brand').classList.add('text-monospace')
})

$(document).on('shiny:sessioninitialized', function() {
  Shiny.setInputValue('browser_tz', Intl.DateTimeFormat().resolvedOptions().timeZone)
})
