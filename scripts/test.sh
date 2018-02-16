#!/usr/bin/env bash
rm ./src/Lexer/Lexer.hs
rm ./src/Parser/Parser.hs
stack exec alex ./src/Lexer/Lexer.x;
stack exec -- happy --outfile ./src/Parser/Parser.hs ./src/Parser/Parser.y;
stack test
