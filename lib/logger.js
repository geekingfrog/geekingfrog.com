'use strict';

var logPath = require('path').join(__dirname, '../logs');
var fs = require('fs');
if(!fs.existsSync(logPath)) { fs.mkdirSync(logPath); console.log('creating directory'); }

var log4js = require('log4js');
log4js.loadAppender('file');
log4js.addAppender(log4js.appenders.file(logPath+'/server.log'));

module.exports = log4js.getLogger();

