'use strict';

import css from './app/bulma.css'

var Elm = require('./src/Mastermind.elm');
var mountNode = document.getElementById('elm-mastermind');

// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Mastermind.embed(mountNode);