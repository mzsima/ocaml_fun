// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


const {QLabel, QMainWindow } = require("@nodegui/nodegui")
;

var win = new QMainWindow();

var label = new QLabel(win);

label.setText("Hello ReScript & NodeGui");

label.setInlineStyle("font-size: 30px; color: green;");

label.adjustSize();

win.show();

global.win = win
;

exports.win = win;
exports.label = label;
/*  Not a pure module */