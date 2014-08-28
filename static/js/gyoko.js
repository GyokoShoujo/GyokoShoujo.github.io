$(document).ready(function() {
    $(".nav-go").click(function(evt) {
        evt.preventDefault();
        window.location = $(".nav-select", $(this).parent()).val();
    });
});
