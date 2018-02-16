#!/usr/bin/env bash
stack exec alex ./src/Lexer/Lexer.x;
stack exec -- happy --outfile ./src/Parser/Parser.hs ./src/Parser/Parser.y;
stack ghci
