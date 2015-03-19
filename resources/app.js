function concat (arrs) {
  var catted = [];
  var j = 0;
  for (var i = 0; i < arrs.length; i++) {
    var arr = arrs[i];
    for (var k = 0; k < arr.length; k++, j++) {
      catted[j] = arr[k];
    }
  }
  return catted;
}

function group (size, arr) {
  var grouped = [];
  for (var i = 0; i < arr.length; i += size) {
    grouped.push(arr.slice(i, i+size));
  }
  return grouped;
}

function pack (img) {
  var c = concat(img);
  var words = [];
  for (var i = 0; i+8 <= c.length; i += 8) {
    words.push(c[i]*128 + c[i+1]*64 + c[i+2]*32 + c[i+3]*16 +
               c[i+4]*8 + c[i+5]*4 + c[i+6]*2 + c[i+7]*1);
  }
  return words;
}

function unpack (img, dims) {
  return group(dims.cols, concat(img.map(function (byte) {
    return [ !! (byte & 128)
           , !! (byte & 64)
           , !! (byte & 32)
           , !! (byte & 16)
           , !! (byte & 8)
           , !! (byte & 4)
           , !! (byte & 2)
           , !! (byte & 1)
           ];
  })));
}

socket = io();

socket.on('flipboard', function (obj) {
  initialize(obj.dims, obj.image);
});

function initialize (dims, initialImg) {
  function addButton (container, label, onclick) {
    $(document.createTextNode(' ')).appendTo(container);
    return $('<input type="button" />')
      .attr({ value: label })
      .on('click', function (e) { e.preventDefault(); onclick(e); })
      .appendTo(container);
  }

  var undoStack = [];
  var redoStack = [];

  function undoRedoHelper (fromStack, toStack) {
    if (fromStack.length === 0) { return; }
    toStack.push(currImg);
    currImg = fromStack.pop();
    imgChanged();
    updateUndoRedoButtonState();
  }

  function undo () { undoRedoHelper(undoStack, redoStack); }
  function redo () { undoRedoHelper(redoStack, undoStack); }

  var isMouseDown = false;
  function mouseDown (e) { isMouseDown = true; operation(paint)(e); e.preventDefault(); }
  function mouseUp ()    { isMouseDown = false; }

  function cloneImg (img) {
    var c = [];
    for (var y = 0; y < dims.rows; y++) {
      c[y] = [];
      for (var x = 0; x < dims.cols; x++) {
        c[y][x] = img[y][x];
      }
    }
    return c;
  }

  function operation (fn, mergeState) {
    return function () {
      if (!mergeState) {
        var clone = cloneImg(currImg);
        undoStack.push(clone);
        redoStack = [];
      }
      fn.apply(this, arguments);
      updateUndoRedoButtonState();
      imgChanged();
    };
  }

  function paint (e) {
    var off = canvas.offset();
    var x = Math.floor((e.pageX - off.left) / d);
    var y = Math.floor((e.pageY - off.top) / d);
    currImg[y][x] = currColor;
  }

  function mouseMove (e) {
    if (!isMouseDown) { return; }
    operation(paint, true)(e);
    e.preventDefault();
  }

  function clear (color) {
    return operation(function () {
      for (var y = 0; y < dims.rows; y++) {
        for (var x = 0; x < dims.cols; x++) {
          currImg[y][x] = color;
        }
      }
      currColor = !color;
    });
  }

  var invert = operation(function () {
    for (var y = 0; y < dims.rows; y++) {
      for (var x = 0; x < dims.cols; x++) {
        currImg[y][x] = !currImg[y][x];
      }
    }
  });

  function imgChanged () {
    displayImg(currImg);
    queueSendImg();
  }

  function displayImg (img) {
    var ctx = canvas.get(0).getContext('2d');
    for (var y = 0; y < dims.rows; y++) {
      for (var x = 0; x < dims.cols; x++) {
        var color = img[y][x] ? '#fff' : '#000';
        ctx.fillStyle = color;
        ctx.fillRect(x*d, y*d, d, d);
      }
    }
  }

  var sendQueued;
  function queueSendImg () {
    if (sendQueued) { return; }
    setTimeout(function () {
      sendQueued = false;
      socket.emit('new image', pack(currImg));
    }, 100);
    sendQueued = true;
  }

  var d = 12;
  var canvas = $('<canvas />')
    .attr({ width: dims.cols * d, height: dims.rows * d })
    .appendTo($('#wrapper'))
    .on('mousedown', mouseDown)
    .on('mousemove', mouseMove);
  $(document.body).on('mouseup', mouseUp);

  var buttons = $('<div class="buttons" />').appendTo($('#wrapper'));

  var toolbox = $('<div class="toolbox" />').appendTo(buttons);
  var currColor = true;
  addButton(toolbox, "white", function () { currColor = true; });
  addButton(toolbox, "black", function () { currColor = false; });
  addButton(toolbox, "clear white", clear(true));
  addButton(toolbox, "clear black", clear(false));
  addButton(toolbox, "invert", invert);

  var undoContainer = $('<div class="undo" />').appendTo(buttons);
  var undoButton = addButton(undoContainer, "undo", undo);
  var redoButton = addButton(undoContainer, "redo", redo);
  updateUndoRedoButtonState();

  function setEnabled (button, state) {
    $(button).each(function() {
      this.disabled = !state;
    });
  }

  function updateUndoRedoButtonState () {
    setEnabled(undoButton, undoStack.length > 0);
    setEnabled(redoButton, redoStack.length > 0);
  }

  socket.on('new image', function (img) {
    displayImg(unpack(img, dims));
  });

  var currImg = unpack(initialImg, dims);
  displayImg(currImg);
}

