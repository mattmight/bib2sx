(module 
  bibtex
  racket
  
  (provide (all-defined-out))
  
  (require parser-tools/lex)
  (require (prefix-in : parser-tools/lex-sre))
  
  (require parser-tools/yacc)
  
  
  (require xml)
  (require xml/xexpr)
  
  ;; Lexical analysis of BibTeX
  
  ; Most of the complexity in analyzing BibTeX has bene
  ; pushed into the lexer.  The lexer may recognize the same
  ; token different depending on the context.
  
  ; The lexer tracks whether it is inside quotes (") and
  ; how many layers of {}-nesting it is within.
  
  ; At two layers of {}-nesting, most tokens are treated as
  ; strings and whitespace becomes a string as well.
  
  ; At one layer of {}-nesting and between quotes ("),
  ; most tokens are strings and whitespace becomes a string
  ; as well.
  
  
  ; Token types:
  (define-empty-tokens PUNCT (@ |{| |}| |"| |#| |,| =))
  (define-empty-tokens EOF (EOF))
  
  (define-tokens EXPR (ID STRING SPACE))
  
  
  
  (define-lex-abbrev bibtex-id 
    (:+ (char-complement (char-set " \t\r\n{}@#=,\\\""))))
  
  (define-lex-abbrev bibtex-comment
    (:: (or #\c #\C) (or #\o #\O) (or #\m #\M) (or #\m #\M) (or #\e #\E) (or #\n #\N) (or #\t #\T)))
  
  (define-lex-abbrev bibtex-preamble
    (:: (or #\p #\P) (or #\r #\R) (or #\e #\E) (or #\a #\A) (or #\m #\M) (or #\b #\B) (or #\l #\L) (or #\e #\E)))
  
  
  (define (bibtex-lexer port [nesting 0] [in-quotes? #f])
    
    ; helpers to recursively call the lexer with defaults:
    (define (lex port) 
      (bibtex-lexer port nesting in-quotes?))
    
    (define (lex+1 port) 
      ; increase {}-nesting
      (bibtex-lexer port (+ nesting 1) in-quotes?))
    
    (define (lex-1 port) 
      ; increase {}-nesting
      (bibtex-lexer port (- nesting 1) in-quotes?))
    
    (define (lex-quotes port)
      ; toggle inside quotes
      (bibtex-lexer port nesting (not in-quotes?)))
    
    (define (not-quotable?) 
      ; iff not inside a string context
      (and (not in-quotes?) (< nesting 2)))
    
    {(lexer
      [(eof)
       empty-stream]
      
      [(:+ whitespace)
       (if (not-quotable?)
           (lex input-port)
           (stream-cons (token-SPACE lexeme)
                        (lex input-port)))]
      
      ["#"
       (stream-cons (if (not-quotable?) 
                        (token-#) (token-STRING lexeme))
                    (lex input-port))]
      
      ["@"
       (stream-cons (if (not-quotable?) 
                        (token-@) (token-STRING lexeme))
                    (lex input-port))]
      
      ["="
       (stream-cons (if (not-quotable?) 
                        (token-=) (token-STRING lexeme))
                    (lex input-port))]
      
      [","
       (stream-cons (if (not-quotable?) 
                        (|token-,|) (token-STRING lexeme))
                    (lex input-port))]
      
      [#\"
       (cond
         [in-quotes?        
          ;=>
          ; pretend we're closing a {}-string
          (stream-cons (|token-}|)
                       (lex-quotes input-port))]
         
         [(and (not in-quotes?) (= nesting 1))
          ;=>
          ; pretend we're opening a {}-string
          (stream-cons (|token-{|)
                       (lex-quotes input-port))]
         
         [(and (not in-quotes?) (>= nesting 2))
          ;=>
          (stream-cons (token-STRING lexeme)
                       (lex input-port))])]
      
      ["\\"
       (stream-cons (token-STRING "\\")
                    (lex input-port))]
      
      ["\\{"
       (stream-cons (token-STRING "{")
                    (lex input-port))]
      
      ["\\}"
       (stream-cons (token-STRING "}")
                    (lex input-port))]
      
      [(:: "{" (:* whitespace))
       (begin
         (stream-cons (|token-{|)
                      (if (and (<= nesting 1) (not in-quotes?))
                          (lex+1 input-port)
                          (if (= (string-length lexeme) 1)
                              (lex+1 input-port)
                              (stream-cons (token-SPACE (substring lexeme 1))
                                           (lex+1 input-port))))))]
      
      [(:: (:* whitespace) "}")
       (begin
         (stream-cons (|token-}|)
                      (if (and (<= nesting 2) (not in-quotes?))
                          (lex-1 input-port)
                          (if (= (string-length lexeme) 1)
                              (lex-1 input-port)
                              (stream-cons (token-SPACE (substring 
                                                         lexeme 0 
                                                         (- (string-length lexeme) 1)))
                                           (lex-1 input-port))))))]
      
      [(:+ numeric)
       (stream-cons (token-STRING lexeme)
                    (lex input-port))]
      
      [bibtex-id
       (stream-cons (if (not-quotable?)
                        (token-ID (string->symbol lexeme)) 
                        (token-STRING lexeme))
                    (lex input-port))])
     
     port})
  
  
  ; generator-token-generator : port -> (-> token)
  (define (generate-token-generator port)
    (define tokens (bibtex-lexer port))
    (λ ()
      (if (stream-empty? tokens)
          (token-EOF)
          (let
              ([tok (stream-first tokens)])
            (set! tokens (stream-rest tokens))
            tok))))
  
  
  
  ;; Parsing BibTeX
  
  ; flatten-top-level-quotes : expr* -> expr*
  (define (flatten-top-level-quotes exprs)
    ; removes the top level {}-quotes because these
    ; should not influence formatting.
    (match exprs
      ['()
       '()]
      
      [(cons `(quote ,values) rest)
       (append values (flatten-top-level-quotes rest))]
      
      [(cons hd tl)
       (cons hd (flatten-top-level-quotes tl))]))
  
  ; bibtex-simplify-quotes : expr* -> expr*
  (define (bibtex-simplify-quotes exprs)
    ; concatenates and simplifies where possible
    (match exprs
      ['()
       '()]

      [`((quote ,(? string? substring)) . ,tl)
       `((quote ,substring) . ,(bibtex-simplify-quotes tl))]
       
      [`((quote ,sub-exprs) . ,tl)
       (define reduced (bibtex-simplify-quotes sub-exprs))
       (when (and (list? reduced) (= (length reduced) 1))
         (set! reduced (car reduced)))
       (cons `(quote ,reduced)
             (bibtex-simplify-quotes tl))]
      
      [`(,(and a (? string?)) ,(and b (? string?)) . ,rest)
       (bibtex-simplify-quotes (cons (string-append a b) rest))]
      
      [(cons hd tl)
       (cons hd (bibtex-simplify-quotes tl))]))
  
  ; flatten+simplify : expr* -> expr*
  (define (flatten+simplify exprs)
    (bibtex-simplify-quotes (flatten-top-level-quotes exprs)))
  
  ; helpers:
  (define (symbol-downcase s)
    (string->symbol (string-downcase (symbol->string s))))
  
  ; bibtex-parse : (-> token) -> bibtex-ast
  (define bibtex-parse
    (parser
     [grammar 
      (itemlist [{item itemlist}  (cons $1 $2)]
                [{}               '()]
                
                [{ID   itemlist}  $2]
                [{|,|  itemlist}  $2]
                [{|#|  itemlist}  $2])
      
      (item [{|@| ID |{| taglist |}|} 
             ; =>
             (cons (symbol-downcase $2) $4)])
      
      (tag [{ID}           $1]
           [{ID = expr}    (cons (symbol-downcase $1)
                                 (flatten+simplify $3))])
      
      (expr [{atom |#| expr}       (cons $1 $3)]
            [{atom}                (list $1)])
      
      (atom  [{ID}                  (symbol-downcase $1)]
             [{STRING}              $1]
             [{SPACE}               $1]
             [{ |{| atomlist |}| }  (list 'quote $2)])
      
      (atomlist [{atom atomlist}    (cons $1 $2)]
                [{}                 '()])
      
      (taglist [{tag |,| taglist}   (cons $1 $3)]
               [{tag}               (list $1)]
               [{}                 '()])]
     
     
     [tokens PUNCT EOF EXPR]
     
     [start itemlist]
     
     [end EOF]
     
     [error (lambda (tok-ok? tok-name tok-value)
              (error (format "parsing error: ~a ~a ~a"
                             tok-ok? tok-name tok-value)))]))

  
  ;; Name parsing

  (define (string-starts-with str prefix)
    (define (prefixed? chars pre)
      (match* {chars pre}
        [{(cons a atl) (cons b btl)}
         (and (equal? a b)
              (prefixed? atl btl))]

        [{_  '()}        #t]))

    (prefixed? (string->list str) (string->list prefix)))
         
  
  (define (string-tokenize str seps [include-seps? #f])

    (define (is-sep? char)
      (set-member? seps char))
        
    (define (chars->tokens chars [rev-chars '()])
      (match chars
        ['()
         ;=>
         (if (> (length rev-chars) 0)
             (list (list->string (reverse rev-chars)))
             (list))]
        
        [(cons (and hd (? is-sep?)) rest)
         ;=>
         (append
          (if (> (length rev-chars) 0)
              (list (list->string (reverse rev-chars)))
              (list))
          (if include-seps?
              (cons (list->string (list hd))
                    (chars->tokens rest))
              (chars->tokens rest)))]

        [(cons hd rest)
         ;=>
         (chars->tokens rest (cons hd rev-chars))]))

    (chars->tokens (string->list str)))
  
  (define-lex-abbrev blank-lines
    (:: (:* (char-set " \tr")) "\n" (:* (char-set "  \t\r")) "\n" (:* whitespace)))

  (define-lex-abbrev end-of-line
    (:: (:* (char-set " \tr")) "\n" (:* (char-set "  \t\r"))))
    
  (define (bibtex-texenize exprs #:collapse-whitespace [collapse? #t])

    ; first condense:
    (define condensed-exprs (bibtex-simplify-quotes exprs))

    ; then lex:
    (define tex-lexer
      (lexer
       [(:: "\\" (:+ alphabetic))
        ;=>
        (cons lexeme (tex-lexer input-port))]

       [(:: "\\" any-char)
        ;=>
        (cons lexeme (tex-lexer input-port))]

       [end-of-line
        ;=>
        (if collapse?
            (cons " " (tex-lexer input-port))
            (cons lexeme (tex-lexer input-port)))]

       [blank-lines
        ;=>
        (if collapse?
            (cons "\n\n" (tex-lexer input-port))
            (cons lexeme (tex-lexer input-port)))]

       [(:* (char-set " \t"))
        ;=>
        (if collapse?
            (cons " " (tex-lexer input-port))
            (cons lexeme (tex-lexer input-port)))]

       [any-char
        ;=>
        (cons lexeme (tex-lexer input-port))]

       [(eof)
        ;=>
        '()]))

    (define (texinize expr)
      (match expr
        [(? string?)
         (tex-lexer (open-input-string expr))]

        [(? symbol?)
         (error (format "BibTeX symbols [~v] cannot be TeXinized; inline first" expr))]

        [`(quote ,(? string? str))
         (list `(quote ,(texinize str)))]

        [`(quote (quote ,inner-exprs))
         (list `(quote ,(texinize `(quote ,inner-exprs))))]

        [`(quote ,(? list? exprs))
         (list `(quote ,(apply append (map texinize exprs))))]))
    
    (apply append (map texinize condensed-exprs)))
       

  (define (bibtex-whitespace? expr)
    (and (string? expr)
         (andmap char-whitespace? (string->list expr))))
  
  (define (bibtex-not-whitespace? expr)
    (not (bibtex-whitespace? expr)))
  

  (define (bibtex-lower-case? expr)
    (or (and (list? expr)
             (match expr
               [`(quote ())        #t]
               
               [`(quote (quote ,expr))
                (bibtex-lower-case? `(quote ,expr))]
               
               [`(quote ,(or (cons (and s (? string?)) _)
                             (and s (? string?))))
                (match (string->list s)
                  [(cons #\\ _) #t]
                  [else #f])]))
        
        (and (string? expr) 
             (match (string->list expr)
               ['() #f]
               ['(#\,) #f]
               [`(#\\ ,char . ,_)
                (char-lower-case? char)]
               [(cons char _) 
                (char-lower-case? char)]))))
  
  (define (bibtex-not-lowercase? expr)
      (not (bibtex-lower-case? expr)))
  
  (define (bibtex-upper-case? expr)
    (or (and (list? expr)
             (match expr
               [`(quote ())        #t]
                 
               [`(quote (quote ,expr))
                (bibtex-upper-case? `(quote ,expr))]
               
               [`(quote ,(or (cons (and s (? string?)) _)
                             (and s (? string?))))
                (match (string->list s)
                  [(cons #\\ _) #f]
                  [else #t])]))
        
        (and (string? expr) 
             (match (string->list expr)
               ['() #f]
               ['(#\,) #f]
               [`(#\\ ,char . ,_)
                (char-upper-case? char)]
               [(cons char _) 
                (char-upper-case? char)]))))
  
  
  (define (bibtex-tokens->words tokens)
    ; return a list of words, where a list of words is a set of exprs
    (match tokens
      [(cons (? bibtex-whitespace?) _)
       (define-values (white rest) (splitf-at tokens bibtex-whitespace?))
       (bibtex-tokens->words rest)]

      [(cons non-white _)
       (define-values (word rest) (splitf-at tokens bibtex-not-whitespace?))
       (cons word (bibtex-tokens->words rest))]

      ['()
       '()]))
            
      
  (define (bibtex-split-tokens tokens pred?)
    ; creates lists of tokens separated by `pred?`
    
    (define (not-pred? x) (not (pred? x)))
    
    (match tokens
      ['()
       '()]
        
      [(list (? not-pred?) ...)
       (list tokens)]
      
      [(list (and segment (? not-pred?)) ... (? pred?) rest ...)
       (cons segment (bibtex-split-tokens rest pred?))]))


  ;  bibtex-trim trims whitespace at both ends
  (define (bibtex-trim exprs)
    
    (define (trim-left lst)
      (match lst
        [(list (or "" ''() (? bibtex-whitespace?)) ... term ...)
         term]))
    
    (trim-left (reverse (trim-left (reverse exprs)))))

  
  (define (bibtex-parse-name exprs)
    
    (define tokens (bibtex-texenize exprs))

    (define (comma? str)
      (equal? str ","))

    (define (name? word)
      (and (not (null? word)) (bibtex-upper-case? (car word))))

    (define (von? word)
      (and (not (null? word)) (bibtex-lower-case? (car word))))

    ; $ : word list -> token list
    (define ($ words)
      (apply append (add-between words '(" "))))

    (define (parse-von-last tokens)
      (match (bibtex-tokens->words tokens)
        
        ; Last | last
        [(list word)
         (list '() word)]

        ; last ...
        [(list (? von?) ...)
         (list '() tokens)]

        ; von ... Last ...
        [(list (? von? vons) ... (? name? lasts) ..1)
         (list ($ vons) ($ lasts))]

        ; von ... stuff ... last
        [(list (? von? von-start) vons-middle ... (? von? last))
         (list ($ `(,von-start ,@vons-middle)) last)]

        ; von stuff ... von Last ...
        [(list (? name? firsts) ...
               (? von? von-start)
               vons-middle ...
               (? von? von-end)
               (? name? lasts) ..1)
         (list ($ `(,von-start ,@vons-middle ,von-end)) ($ lasts))]))


    (define (parse-first-von-last tokens)
      (match (bibtex-tokens->words tokens)

        ; Last | last
        [(list word)
         (list '() '() word '())]

        ; last ...
        [(list (? von?) ...)
         (list '() '() tokens '())]

        ; First ... Last | First ... last
        [(list (? name? firsts) ... last)
         (list ($ firsts) '() last '())]

        ; First ... von ... Last ...
        [(list (? name? firsts) ... (? von? vons) ... (? name? lasts) ..1)
         (list ($ firsts) ($ vons) ($ lasts) '())]

        ; First ... von ... stuff ... last
        [(list (? name? firsts) ... (? von? von-start) vons-middle ... (? von? last))
         (list ($ firsts) ($ `(,von-start ,@vons-middle)) last '())]

        ; First ... von stuff ... von Last ...
        [(list (? name? firsts) ...
               (? von? von-start)
               vons-middle ...
               (? von? von-end)
               (? name? lasts) ..1)
         (list ($ firsts) ($ `(,von-start ,@vons-middle ,von-end)) ($ lasts) '())]))
    
    
    (define parsed 
      (match (bibtex-split-tokens tokens comma?)
        
        [(list first-von-last) (parse-first-von-last first-von-last)]
        
        [(list von-last first) 
         `(,first ,@(parse-von-last von-last) ())]
        
        [(list von-last jr first)
         `(,first ,@(parse-von-last von-last) ,jr)]
        
        [else tokens]))
    
    (match (map bibtex-trim parsed)
      [`(,first ,von ,last ,jr)
       `(name
         (first . ,first)
         (von . ,von)
         (last . ,last)
         (jr . ,jr))]))
       

  
  (define (bibtex-parse-names item [name-fields (set 'author 'editor)])

    (define (split-ands toks [rev-cur '()] [rev-all '()])
      (match toks
        ['()
         (reverse (cons (reverse rev-cur) rev-all))]

        [(or `("a" "n" "d" . ,rest)
             (cons "and" rest))
         (split-ands rest '() (cons (reverse rev-cur) rev-all))]

        [(cons hd tl)
         (split-ands tl (cons hd rev-cur) rev-all)]))
    
    (match item
      [`(,item-type 
         ,key
         (,names . ,exprs) ...)
       ;=>
       `(,item-type
         ,key
         ,@(for/list ([n names]
                      [e exprs])
             (if (set-member? name-fields n)
                 (let ()
                   (define (is-and? expr) (equal? expr "and"))
                   (define toks (bibtex-texenize e))
                   (define all-names (split-ands toks)) 
                   (cons n (map bibtex-parse-name all-names)))
                 (cons n e))))]))
            
  
  ;; BibTeX formatting
  
  (define bibtex-default-strings
    #hasheq((jan . ("January"))
            (feb . ("February"))
            (mar . ("March"))
            (apr . ("April"))
            (may . ("May"))
            (jun . ("June"))
            (jul . ("July"))
            (aug . ("August"))
            (sep . ("September"))
            (oct . ("October"))
            (nov . ("November"))
            (dec . ("December"))))
  
  
  ;; Inlining @string values into entries
  
  ; bibtex-inline-strings : bibtex-ast -> bibtex-ast
  (define (bibtex-inline-strings
           items 
           [env bibtex-default-strings])
    
    (define ((replace env) expr)
      (if (symbol? expr)
          (hash-ref env expr (λ () (list "")))
          (list expr)))
    
    (define (inline exprs [env env])
      (apply append (map (replace env) exprs)))
    
    (define (extend* env names exprs)
      (for/fold 
       ([env env])
       ([n names]
        [e exprs])
        (hash-set env n (inline e env))))
    
    (match items
      
      ['()
       '()]
      
      ; TODO: Add handling for @preamble and @comment
      
      ; Pick up more bindings:
      [(cons `(string (,names . ,exprs) ...) rest)
       ;=>
       (bibtex-inline-strings rest (extend* env names exprs))]
      
      [(cons `(,item-type ,key (,names . ,exprs) ...) rest)
       ;=>
       (cons `(,item-type ,key ,@(map cons names (map inline exprs)))
             (bibtex-inline-strings rest env))]
      
      ))
  
  
  ;; Compiling back to .bib
  
  (define (bibtex-exprs->bibstring exprs)
    
    (define (name->bibstring name)
      (match name
        [`(name (first . ,first) (von . ,von) (last . ,last) (jr))
         (string-append (bibtex-exprs->bibstring von)
                        (if (null? von) "" " ")
                        (bibtex-exprs->bibstring last)
                        ", "
                        (bibtex-exprs->bibstring first))]
        
        
        [`(name (first . ,first) (von . ,von) (last . ,last) (jr . ,jr))
         (string-append (bibtex-exprs->bibstring von)
                        (if (null? von) "" " ")
                        (bibtex-exprs->bibstring last)
                        ", "
                        (bibtex-exprs->bibstring jr)
                        ", "
                        (bibtex-exprs->bibstring first))]
        [else (error (format "unrecognized name format: ~v" name))]))
    
    (define (expr->string-list expr)
      (match expr
        [(? string?)         (list (escape expr))]
        [(? symbol?)         (list "} # " (symbol->string expr) " # {")]
        
        [`(quote (quote . ,s))
         `("{" ,@(expr->string-list `(quote . ,s)) "}")]
        
        [`(quote ,(and s (? string?)))
         ; =>
         (list "{" s "}")]
        
        [`(quote (,s ...))
         ; =>
         `("{" ,@(apply append (map expr->string-list s)) "}")]
        
        [else (error (format "unrecognize expr: ~v" expr))]))
    
    (define (escape str)
      (set! str (string-replace str "{" "\\{"))
      (set! str (string-replace str "}" "\\}"))
      str)
    
    (match exprs
      ; list of names:
      [(cons `(name . ,_) _)
       (string-join (map name->bibstring exprs) " and ")]
              
      ; already flattened:
      [(? string?)
       exprs]
      
      [(list (and sym (? symbol?)))
       (string-append "} # " (symbol->string sym) " # {")]
      
      [else
       ;=>
       (apply string-append (apply append (map expr->string-list exprs)))]))
  
  
  
  (define (bibtex-item->bibstring item)
    (match item
      [`(,item-type ,key (,names . ,exprs) ...)
       (string-append
        "@" (symbol->string item-type) "{" (symbol->string key) ",\n"
        (apply string-append (for/list ([n names]
                                        [e exprs])
                               (match e
                                 [(list (and sym (? symbol?)))
                                  (format "  ~a = ~a,\n" n sym)]
                                 [else
                                  (format "  ~a = { ~a },\n" n (bibtex-exprs->bibstring e))])))
        "}\n")]))
  
  
  ;; Flattening:
  
  (define (bibtex-flatten-strings items)
    
    (define (flatten-item item)
      (match item
        [`(,item-type ,key (,names . ,exprs) ...)
         `(,item-type ,key
                      ,@(for/list ([n names]
                                   [e exprs])
                          (cons n (bibtex-exprs->bibstring e))))]))
    
    (map flatten-item items))
  
  
  
  ;; Converting to XML:
  
  (define (bibtex-ast->xml items)
    
    (define (exprs->xexpr exprs)
      (if (string? exprs)
          `(([value ,exprs]))
          (for/list ([e exprs])
            (match e
              [(? symbol?)
               (symbol->string e)]
              
              [(? string?)             e]
              
              [`(name . ,_)
               e]
               
              
              [`(quote ,(? string?))   e]
              
              [`(quote (quote . ,body))
               `(quote ,@(exprs->xexpr 
                          (list `(quote . ,body))))]
              
              [`(quote (,exprs ...))
               `(quote ,@(exprs->xexpr exprs))]))))
    
    (define (item->xexpr item)
      (match item
        [`(,item-type ,key (,names . ,exprs) ...)
         `(,item-type (bibtex-key ,(symbol->string key))
                      ,@(for/list ([n names]
                                   [e exprs])
                          `(,n ,@(exprs->xexpr e))))]))
    
    (xexpr->xml `(bibtex ,@(map item->xexpr items))))
  
  
  
  
  
  
  ;; Converting to JSON:
  
  (define (bibtex-ast->json items)
    (string-join (map bibtex-item->json items) ",\n"
                 #:before-first "[\n"
                 #:after-last "\n]\n"))
  
  (define (bibtex-item->json item)
    
    (define (escape str)
      (when (symbol? str)
        (set! str (symbol->string str)))
      (set! str (string-replace str "\\" "\\\\"))
      (set! str (string-replace str "\r" "\\r"))
      (set! str (string-replace str "\n" "\\n"))
      (set! str (string-replace str "\t" "\\t"))
      (set! str (string-replace str "\"" "\\\""))
      (set! str (string-replace str "\'" "\\\'"))
      (string-append "\"" str "\""))
    
    (define (expr->json expr)
      (match expr
        [(? symbol?)
         (escape (car (hash-ref bibtex-default-strings expr (λ () (list "")))))]
        
        [`(name (first . ,first) (von . ,von) (last . ,last) (jr . ,jr))
         (string-append "{first: [" (string-join (map expr->json first) ",") "],"
                        "von: [" (string-join (map expr->json von) ",") "],"
                        "last: [" (string-join (map expr->json last) ",") "],"
                        "jr: [" (string-join (map expr->json jr) ",") "]}")]
        
        [`(quote (,exprs ...))
         (string-append "[" (string-join (map expr->json exprs) ", ") "]")]
        
        [`(quote ,expr)
         (string-append "[" (expr->json expr) "]")]
        
        [(? string?)
         (escape expr)]
        
        [else 
         (error (format "no rule for expr->json: ~a" expr))]))
    
    (match item
      [`(,item-type ,key (,names . ,exprs) ...)
       
       
       (define entries
         (for/list ([n names] [e exprs])
           (format "~a: ~a" 
                   (escape n) 
                   (if (string? e)
                       (escape e)
                       (string-append "[" (string-join (map expr->json e) ",") "]")))))
       
       (string-append
        "{" 
        (string-join entries ",\n " #:before-first " " #:after-last ",\n ")
        "\"bibtexKey\": " (escape key)
        " }")]))
  
  
  )
