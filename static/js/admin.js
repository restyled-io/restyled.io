/* Global Modal
 *
 * Currently geared towards logging output.
 *
 * Usage:
 *
 *   var modal = new Modal();
 *
 *   modal.show();
 *   modal.append("some text\n");
 *   modal.append("some more text\n");
 *   modal.clear();
 *   modal.hide();
 *
 */
function Modal() {
  this.modal = document.getElementById("modal")
  this.close = this.modal.getElementsByClassName("close")[0];
  this.inner = this.modal.getElementsByClassName("inner")[0];

  // Set up close handlers
  var self = this;

  window.addEventListener("click", function(event) {
    if (event.target == self.modal) {
      self.hide();
    }
  });

  this.close.addEventListener("click", self.hide);
}

Modal.prototype.hide = function() { modal.style.display = "none"; }
Modal.prototype.show = function() { modal.style.display = "block"; }
Modal.prototype.clear = function() { this.inner.innerHTML = ""; }
Modal.prototype.append = function(html) {
  this.show(); // Just in case
  this.inner.innerHTML += html;
}

/* Requesting a WebSockets response through a form
 *
 * Usage:
 *
 *   <!-- HTML -->
 *   <form id="..." action={WebSockets URL}>
 *
 *   // JavaScript
 *   form = document.getElementById("...");
 *   FormWebSockets.attach(form, options);
 *
 * Options:
 *
 *   - onconnect: function called on connect.
 *   - onmessage: function called with each received message.
 *
 */
function FormWebSockets(form, options) {
  this.form = form;
  this.onconnect = options.onconnect;
  this.onmessage = options.onmessage;
}

FormWebSockets.prototype.connect = function() {
  var self = this;
  var query = new URLSearchParams(new FormData(this.form));
  var streamUrl = this.form
    .getAttribute("action")
    .replace(/http(s?):/, "ws$1:");
  var socket = new WebSocket(`${streamUrl}?${query}`);

  this.onconnect();

  socket.onerror = function(data) {
    console.error("websockets error", data);
  };

  socket.onmessage = function(data) {
    self.onmessage(data.data);
    socket.send("acknowledged");
  };
}

FormWebSockets.attach = function(form, options) {
  form.addEventListener("submit", function(event) {
    event.preventDefault();
    new FormWebSockets(form, options).connect();
  });
}

// Always set up any .form-websockets to the Global Modal
document.addEventListener("DOMContentLoaded", function() {
  var modal = new Modal();
  var forms = document.getElementsByClassName("form-websockets");

  for (var i = 0; i < forms.length; i++) {
    FormWebSockets.attach(forms[i], {
      onconnect: function() { modal.clear() },
      onmessage: function(output) { modal.append(`${output}\n`); }
    });
  }
});
