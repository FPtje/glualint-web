/** @constructor */

var editor;
var editorElement;
function createCodeMirror(initValue, mode, theme) {
  editor = CodeMirror(function(elt) { editorElement = elt; }, {value: initValue, mode: mode, lineNumbers: true, theme: theme, autofocus: true});
  setTimeout(function() { document.body.appendChild(editorElement); editor.refresh() });
}

function getEditor() { return editor; };

function createOnEvent(tp, f) {editor.on(tp, function() { f(); })};
