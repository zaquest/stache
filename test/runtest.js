var requirejs = require('requirejs');
requirejs.config({ nodeRequire: require });
var escapeHTML = require('escape-html');
requirejs.define('escapeHTML', [], function () { return escapeHTML; });
var test = requirejs('test.js');

var data = {
  'id': 10,
  'url': 'http://arru',
  'nm': 'there is the name',
  'img': 'http://imgur.com',
  'sz': '100Кб',
  'ad': 'после дождичка в четверг',
  'u': 5,
  'd': 6,
  'od': 'длинное описание',
  'list': [
    {
      'title': 'first',
      'body': 'fuck',
      'test': true,
      'test2': true
    },
    {
      'title': 'second',
      'body': 'азазаз',
      'test': false,
      'test2': true
    },
    {
      'title': 'third',
      'body': 'азазаз',
      'test': false,
      'test2': false
    }
  ]
};

console.log(test(data));
