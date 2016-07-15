function init(env, productsApiPath, userName) {
  $(document).ready(function() {
    initialState = {
      environment: env,
      productsApiPath: productsApiPath,
      user: userName
    };

    const app = Elm.Main.fullscreen(initialState);

    // disable bootstrap transition animations
    $.support.transition = false;
  });
}
