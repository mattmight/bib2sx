#lang racket


; bib2sx: A Racket script for converting a bibtex .bib file to S-Expressions

; Copyright (C) 2015 Matthew Might

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; ISSUES:

; + Does not support @comment or @preamble

; TODO:

; + Add --parse-names to extract sructure from author, editor, etc.
; + Add support for @comment and @preamble


; Output bibtex AST grammar:

; <bibtex-ast> ::= (<item> ...)

; <item> ::= (<item-type> <id> <attr> ...)

; <attr> ::= (<name> <expr> ...)  ;
;         |  (<name> . <string>)  ; if --flatten

; <expr> ::= <string>
;         |  '<expr>
;         |  '(<expr> ...)
;         |  <id>

; <name> ::= <id>

; <item-type> ::= inproceedings | article | ...



(require "bibtex/main.rkt")

(require xml)


;; Handling command line options:


; <config>
(define bibtex-input-port (current-input-port))
(define bibtex-output-format 'ast)
(define inline? #f)
(define flatten? #f)
(define lex-only? #f)
; </config>


(define (parse-options! args)
  (match args
    ['()
     (void)]
    
    [(cons "--drracket" rest)
     (set! bibtex-input-port (open-input-file "mattmight.bib"))
     (parse-options! rest)]
    
    ; just lexically analyze; don't parse:
    [(cons "--lex" rest)
     (set! lex-only? #t)
     (parse-options! rest)]
    
    ; flatten values into a single string:
    [(cons "--flatten" rest)
     (set! flatten? #t)
     (parse-options! rest)]
    
    ; inline all @string declarations:
    [(cons "--inline" rest)
     (set! inline? #t)
     (parse-options! rest)]
    
    ; convert to a bibtex .bib file:
    [(cons "--bib" rest)
     (set! bibtex-output-format 'bib)
     (parse-options! rest)]
    
    ; convert to JSON:
    [(cons "--json" rest)
     (set! bibtex-output-format 'json)
     (parse-options! rest)]
    
    ; convert to JSON:
    [(cons "--xml" rest)
     (set! bibtex-output-format 'xml)
     (parse-options! rest)]
        
    
    ; provide a filename to parse:
    [(cons filename rest)
     (set! bibtex-input-port (open-input-file filename))
     (parse-options! rest)]
    
    ))




; for testing in DrRacket:
;(current-command-line-arguments #("--inline" "--flatten" "test/test.bib"))

(parse-options! (vector->list (current-command-line-arguments)))
    

; for debugging, allow looking at the lexical analyzers output:
(when lex-only?
  (define tokens (bibtex-lexer bibtex-input-port))
  (pretty-write (stream->list tokens))
  (exit))


(define token-generator (generate-token-generator bibtex-input-port))

(define bibtex-ast (bibtex-parse token-generator))

(when inline?
  (set! bibtex-ast (bibtex-inline-strings bibtex-ast)))

(when flatten?
  (set! bibtex-ast (bibtex-flatten-strings bibtex-ast)))

(match bibtex-output-format
  ['ast     (pretty-write bibtex-ast)]
  ['bib     (for ([item bibtex-ast])
              (display (bibtex-item->bibstring item))
              (newline) (newline))]
  ['xml     (let ([xml (bibtex-ast->xml bibtex-ast)])
              (display-xml/content xml))]
  ['json    (display (bibtex-ast->json bibtex-ast))])
  
    
