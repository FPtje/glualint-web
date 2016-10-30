/** @constructor */

var editor;
var editorElement;
function createCodeMirror(initValue, mode, theme) {
  editor = CodeMirror(function(elt) { editorElement = elt; }, {value: initValue, mode: mode, lineNumbers: true, theme: theme, autofocus: true});
  editor.setCursor(1, 0)
  setTimeout(function() {
    document.getElementById("content").appendChild(editorElement);
    editor.refresh();
    editor.focus();
  });
}

function getEditor() { return editor; };

function createOnEvent(tp, f) {editor.on(tp, function() { f(); })};
