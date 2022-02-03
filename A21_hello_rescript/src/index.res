type qwin
@new external createQMainWindow: unit => qwin = "QMainWindow"
@send external show: (qwin) => unit = "show"

type qlabel
@new external createQLabel: qwin => qlabel = "QLabel"
@send external setText: (qlabel, string) => unit = "setText"
@send external setInlineStyle: (qlabel, string) => unit = "setInlineStyle"
@send external adjustSize: (qlabel) => unit = "adjustSize"

%%raw(`const {QLabel, QMainWindow } = require("@nodegui/nodegui")`)

let win = createQMainWindow()

let label = createQLabel(win)
label->setText("Hello ReScript & NodeGui")
label->setInlineStyle("font-size: 30px; color: green;");
label->adjustSize

win->show

%%raw(`global.win = win`)