function init(productsApiPath, userName) {
  $(document).ready(function() {
    initialState = {
      productsApiPath: productsApiPath,
      user: userName
    };

    const app = Elm.Main.fullscreen(initialState);

    // disable bootstrap transition animations
    $.support.transition = false;
  });
}
