function init(userName) {
  $(document).ready(function() {
    initialState = {
      apiPath: "",
      user: userName
    };

    const app = Elm.Main.fullscreen(initialState);

    // disable bootstrap transition animations
    $.support.transition = false;
  });
}
