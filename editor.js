// Linting messages
var messages = [];

function createCodeMirror(element, initValue, mode, theme) {
  var editor = CodeMirror
      (element
      , { value: initValue
        , mode: mode
        , lint: true
        , lineNumbers: true
        , theme: theme
        , gutters: ["CodeMirror-lint-markers"]
        , autofocus: true
        }
      );

  editor.setCursor(1, 0)
  editor.refresh();
  editor.focus();
  editor.execCommand('selectAll');

  CodeMirror.registerHelper("lint", "lua", function() {
    return messages;
  });

  return editor;
}

function resetMessages(editor)
{
  messages = [];
  editor.refresh();
};

function addLintMessage(editor, lineStart, columnStart, lineEnd, columnEnd, severity, msg)
{
  messages.push({
    from: CodeMirror.Pos(lineStart, columnStart),
    to: CodeMirror.Pos(lineEnd, columnEnd),
    severity: severity,
    message: msg
  });
}

function cmSelectRegion(editor, lineStart, columnStart, lineEnd, columnEnd)
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
