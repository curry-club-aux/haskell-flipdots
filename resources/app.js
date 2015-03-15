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
  var d = 12;
  var canvas = $('<canvas />')
    .attr({ width: dims.cols * d, height: dims.rows * d })
    .appendTo($('#wrapper'))
    .on('mousedown', mouseDown)
    .on('mousemove', mouseMove);
  $(document.body).on('mouseup', mouseUp);

  var currColor = true;
  var buttons = $('<div class="buttons" />').appendTo($('#wrapper'));
  function addButton (label, onclick) {
    $(document.createTextNode(' ')).appendTo(buttons);
    return $('<input type="button" />')
      .attr({ value: label })
      .on('click', function (e) { e.preventDefault(); onclick(e); })
      .appendTo(buttons);
  }
  addButton("white", function () { currColor = true; });
  addButton("black", function () { currColor = false; });
  addButton("clear white", function () { clear(true); currColor = false; });
  addButton("clear black", function () { clear(false); currColor = true; });
  addButton("invert", invert);

  socket.on('new image', function (img) {
    displayImg(unpack(img, dims));
  });

  var isMouseDown = false;
  function mouseDown (e) { isMouseDown = true; paint(e); e.preventDefault(); }
  function mouseUp ()    { isMouseDown = false; }

  function paint (e) {
    var off = canvas.offset();
    var x = Math.floor((e.pageX - off.left) / d);
    var y = Math.floor((e.pageY - off.top) / d);
    currImg[y][x] = currColor;
    imgChanged();
  }

  function mouseMove (e) {
    if (!isMouseDown) { return; }
    paint(e);
    e.preventDefault();
  }

  function clear (color) {
    for (var y = 0; y < dims.rows; y++) {
      for (var x = 0; x < dims.cols; x++) {
        currImg[y][x] = color;
      }
    }
    imgChanged();
  }

  function invert () {
    for (var y = 0; y < dims.rows; y++) {
      for (var x = 0; x < dims.cols; x++) {
        currImg[y][x] = !currImg[y][x];
      }
    }
    imgChanged();
  }

  function imgChanged () {
    displayImg(currImg);
    queueSendImg();
  }

  var ctx = canvas.get(0).getContext('2d');
  function displayImg (img) {
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

  var currImg = unpack(initialImg, dims);
  displayImg(currImg);
}

