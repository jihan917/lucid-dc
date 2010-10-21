"vim script for HTML abbreviations/snippet expansion
"(C) Copyright 2010, Ji Han (jihan917<at>yahoo<dot>com)
"free to distribute under the GPL license
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function IMapHtml ()

    "xhtml 1.0 strict
    inoremap <buffer> html<Tab> <?xml version="1.0" encoding="UTF-8"?><CR><!DOCTYPE html <CR>PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"<CR>"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"><CR><CR><html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"><CR><head><CR><title> {{TITLE}} </title><CR><meta http-equiv="content-type" content="text/html; charset=utf-8" /><CR></head><CR><CR><body><CR><CR></body><CR></html><CR><Up><Up><Up>

    "xhtml 1.0 transitional
    inoremap <buffer> Html<Tab> <?xml version="1.0" encoding="UTF-8"?><CR><!DOCTYPE html <CR>PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"<CR>"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"><CR><CR><html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"><CR><head><CR><title> {{TITLE}} </title><CR><meta http-equiv="content-type" content="text/html; charset=utf-8" /><CR></head><CR><CR><body><CR><CR></body><CR></html><CR><Up><Up><Up>

    "external javascript
    inoremap <buffer> js<Tab> <script type="text/javascript" src="{{SRC}}" charset="UTF-8"></script>

    "inline javascript
    inoremap <buffer> script<Tab> <script type="text/javascript">//<![CDATA[<CR><CR>//]]></script><CR><Up><Up>

    inoremap <buffer> meta<Tab> <meta name="{{NAME}}" content="{{CONTENT}}" />
    inoremap <buffer> metahdr<Tab> <meta http-equiv="{{HTTP-EQUIV}}" content="{{CONTENT}}" />
    inoremap <buffer> link<Tab> <link href="{{HREF}}" rel="{{REL}}" type="{{TYPE}}" />

    inoremap <buffer> div<Tab> <div><CR><CR></div><Up>
    inoremap <buffer> span<Tab> <span>  </span><C-o>8h

    inoremap <buffer> h1<Tab> <h1>  </h1><Esc>6ha
    inoremap <buffer> h2<Tab> <h2>  </h2><Esc>6ha
    inoremap <buffer> h3<Tab> <h3>  </h3><Esc>6ha
    inoremap <buffer> h4<Tab> <h4>  </h4><Esc>6ha
    inoremap <buffer> h5<Tab> <h5>  </h5><Esc>6ha
    inoremap <buffer> h6<Tab> <h6>  </h6><Esc>6ha
    inoremap <buffer> hr<Tab> <hr />

    inoremap <buffer> p<Tab> <p><CR><CR></p><Up>
    inoremap <buffer> br<Tab> <br />

    inoremap <buffer> ul<Tab> <ul><CR><CR></ul><Up>
    inoremap <buffer> ol<Tab> <ol><CR><CR></ol><Up>
    inoremap <buffer> li<Tab> <li>  </li><Esc>6ha

    inoremap <buffer> a<Tab> <a href="{{HREF}}">  </a><Esc>5ha
    inoremap <buffer> img<Tab> <img src="{{SRC}}" alt="{{ALT}}" />

    inoremap <buffer> form<Tab> <form method="{{METHOD}}" id="{{ID}}" action="{{ACTION}}"><CR><CR></form><Up>
    inoremap <buffer> input<Tab> <input type="{{TYPE}}" name="{{NAME}}" value="{{VALUE}}" />

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "common escapes

    "space or non-breaking space
    inoremap <buffer> <Space><Tab> &nbsp;

    "ampersand
    inoremap <buffer> &<Tab> &amp;

    "less than and greater than
    inoremap <buffer> <<Tab> &lt;
    inoremap <buffer> ><Tab> &gt;

    "em-dash and en-dash
    inoremap <buffer> ---<Tab> &mdash;
    inoremap <buffer> --<Tab> &ndash;

    "paragraph and section
    inoremap <buffer> pp<Tab> &para;
    inoremap <buffer> ss<Tab> &sect;

endfunction

autocmd FileType html,xhtml call IMapHtml()

