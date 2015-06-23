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

; + Add --tokenize flag to tokenize into strings meaningful to BibTeX/LaTeX:
;     $, \<cmd>, <whitespace>, <word>
; + Add flag to choose name fields (currently author, editor)
; + Add support for @comment and @preamble


; Output bibtex AST grammar:

; <bibtex-ast> ::= (<item> ...)

; <item> ::= (<item-type> <id> <attr> ...)

; <attr> ::= (<id> <expr> ...)  ;
;         |  (<id> . <string>)  ; if --flatten
;         |  (<id> <name> ...)  ; if --parse-names

; <expr> ::= <string>
;         |  '<expr>
;         |  '(<expr> ...)
;         |  <id>

; <name> ::= (name (first <expr> ...) ; if --parse-names
;                  (von <expr> ...)
;                  (last <expr> ...) 
;                  (jr <expr>))

; <item-type> ::= inproceedings | article | ...



(require "bibtex/main.rkt")

(require xml)


;; Handling command line options:


; <config>
(define bibtex-input-port (current-input-port))
(define bibtex-output-format 'sx)
(define bibtex-input-format 'bib)
(define inline? #f)
(define flatten? #f)
(define texenize? #f)
(define lex-only? #f)
(define parse-names? #f)
; </config>


(define (parse-options! args)
  (match args
    ['()
     (void)]
    
    [(cons "--drracket" rest)
     (set! bibtex-input-port (open-input-file "test/test.bib"))
     (parse-options! rest)]
    
    ; choose the input format:
    [`("--in" ,format . ,rest)
     (set! bibtex-input-format (string->symbol format))
     (parse-options! rest)]
    
    ; choose the output format:
    [`("--out" ,format . ,rest)
     (set! bibtex-output-format (string->symbol format))
     (parse-options! rest)]
    
    ; just lexically analyze; don't parse:
    [(cons "--lex" rest)
     (set! lex-only? #t)
     (parse-options! rest)]
    
    ; flatten values into a single string:
    [(cons "--flatten" rest)
     (set! flatten? #t)
     (parse-options! rest)]
    
    ; parse names into a vector of vectors:
    [(cons "--parse-names" rest)
     (set! parse-names? #t)
     (parse-options! rest)]
    
    ; inline all @string declarations:
    [(cons "--inline" rest)
     (set! inline? #t)
     (parse-options! rest)]

    ; tokenize strings into units meaningful to TeX:
    [(cons "--texenize" rest)
     ; useful if you want to render to a different output
     ; format such as HTML, and you need ot interpret TeX
     (set! texenize? #t)
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



(define bibtex-ast 
  (match bibtex-input-format
    ['bib   
     (define token-generator (generate-token-generator bibtex-input-port))
     (bibtex-parse token-generator)]
    
    ['sx   
     (read bibtex-input-port)]
    
    [else 
     (error (format "unrecognized input format: ~a" bibtex-input-format))]))

(when inline?
  (set! bibtex-ast (bibtex-inline-strings bibtex-ast)))

(when parse-names?
  (set! bibtex-ast (map bibtex-parse-names bibtex-ast)))

(when flatten?
  (set! bibtex-ast (bibtex-flatten-strings bibtex-ast)))

(when texenize?
  (set! bibtex-ast (map bibtex-texenize-item bibtex-ast)))

(match bibtex-output-format
  ['sx      (pretty-write bibtex-ast)]
  ['bib     (for ([item bibtex-ast])
              (display (bibtex-item->bibstring item))
              (newline) (newline))]
  ['xml     (let ([xml (bibtex-ast->xml bibtex-ast)])
              (display-xml/content xml))]
  ['json    (display (bibtex-ast->json bibtex-ast))]
  [else     (error (format "unrecognized output format: ~a" 
                           bibtex-output-format))])
  
    
