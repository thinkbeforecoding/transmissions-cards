class MyHandler extends Paged.Handler {
    constructor(chunker, polisher, caller) {
      super(chunker, polisher, caller);
    }

    afterPreview(pages) {
        var url = location.href;
        if (url.includes('#'))
          location.href = url
    }
  }
  Paged.registerHandlers(MyHandler);