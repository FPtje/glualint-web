// Linting messages
var messages = [];


var editor;
var editorElement;
function createCodeMirror(initValue, mode, theme) {
  editor = CodeMirror(function(elt) { editorElement = elt; }, {value: initValue, mode: mode, lint: true, lineNumbers: true, theme: theme, gutters: ["CodeMirror-lint-markers"], autofocus: true});
  editor.setCursor(1, 0)
  setTimeout(function() {
    document.getElementById("content").appendChild(editorElement);
    editor.refresh();
    editor.focus();

    CodeMirror.registerHelper("lint", "lua", function() {
      return messages;
    });
  });
}

function getEditor() { return editor; };

function createOnEvent(tp, f) {editor.on(tp, function() { f(); })};


function resetMessages()
{
  messages = [];
};

function addLintMessage(lineStart, columnStart, lineEnd, columnEnd, severity, msg)
{
  messages.push({
    from: CodeMirror.Pos(lineStart, columnStart),
    to: CodeMirror.Pos(lineEnd, columnEnd),
    severity: severity,
    message: msg
  });
}

function cmRefresh()
{
  editor.refresh();
}

function cmSelectRegion(lineStart, columnStart, lineEnd, columnEnd)
{
  editor.setSelection(
    { line: lineStart
    , ch: columnStart
    }
  , { line: lineEnd
    , ch: columnEnd
    }
  );

  coords = editor.charCoords({line: lineStart, ch: columnStart})
  window.scrollTo(0, coords.top)

  editor.focus();
}
