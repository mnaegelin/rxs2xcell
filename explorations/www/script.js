/*
$(document).ready(function() \{\
  $('.scroll-left').click(function() \{\
    $('.nav-underline').animate(\{ scrollLeft: '-=200' \}, 300);\
  \});\
  \
  $('.scroll-right').click(function() \{\
    $('.nav-underline').animate(\{ scrollLeft: '+=200' \}, 300);\
  \});\
\});\
}
*/

$(document).ready(function() {
  $(".btn-toggle-nav").on("click", "a", function() {
    // remove "active" class from all elements:
    $(".btn-toggle-nav a").removeClass("active");

    // add "active" class to the clicked element:
    $(this).addClass("active");
  });
});
