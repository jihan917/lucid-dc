"vim script for C/C++ code snippet expansion
"(C) Copyright 2010, Ji Han (jihan917<at>yahoo<dot>com)
"free to distribute under the GPL license
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function IMapCpp ()

    inoremap <buffer> inc<Tab> #include <><Left>
    inoremap <buffer> inclu<Tab> #include ""<Left>
    inoremap <buffer> incc<Tab> #include <cassert><CR>#include <cctype><CR>#include <cstdint><CR>#include <cstdio><CR>#include <cstdlib><CR>#include <cstring><CR>#include <cmath><CR>#include <ctime><CR>
    inoremap <buffer> inccc<Tab> #include <algorithm><CR>#include <bitset><CR>#include <deque><CR>#include <fstream><CR>#include <functional><CR>#include <iomanip><CR>#include <iostream><CR>#include <iterator><CR>#include <list><CR>#include <map><CR>#include <numeric><CR>#include <queue><CR>#include <set><CR>#include <sstream><CR>#include <stack><CR>#include <string><CR>#include <unordered_map><CR>#include <unordered_set><CR>#include <utility><CR>#include <valarray><CR>#include <vector><CR>

    inoremap <buffer> ifdef<Tab> #if defined()<CR>#endif<CR><Up><Up><End><Left>
    inoremap <buffer> once<Tab> <C-o>mm<C-o>gg#if !defined(<C-R>=toupper(tr(expand("%:t"), '.', '_')) . '_INCLUDED'<CR>)<CR>#define <C-R>=toupper(tr(expand("%:t"), '.', '_')) . '_INCLUDED__'<CR><CR><CR><C-o>G<C-o>A<CR>#endif /* !defined(<C-R>=toupper(tr(expand("%:t"), '.', '_')) . '_INCLUDED__'<CR>) */<CR><C-o>`m

    inoremap <buffer> main<Tab> int main(int argc, char **argv)<CR>{<CR><CR>return 0;<CR>}<CR><Up><Up><Up><Tab>

    inoremap <buffer> for<Tab> for (<initializer>; <condition>; <iterator>)<CR>{<CR><CR>}<CR><C-o>4k<End><C-o>3T<
    inoremap <buffer> foreach<Tab> std::for_each(<container>.begin(), <container>.end(), <functor>);<C-o>3T<
    inoremap <buffer> while<Tab> while (<condition>)<CR>{<CR><statements><CR>}<CR><C-o>4k<End><C-o>T<
    inoremap <buffer> do<Tab> do {<CR><statements><CR>} while (<condition>);<CR><C-o>k<C-o>T<
    inoremap <buffer> if<Tab> if (<condition>)<CR>{<CR><statements><CR>}<C-o>3k<End><C-o>T<
    inoremap <buffer> else<Tab> else (<condition>)<CR>{<CR><statements><CR>}<CR><C-o>4k<End><C-o>T<
    inoremap <buffer> switch<Tab> switch (<expression>)<CR>{<CR>case <constant-expression>: <statements> break;<CR>case <constant-expression>: <statements> break;<CR>case <constant-expression>: <statements> break;<CR>default: <statements><CR>}<CR><C-o>7k<End><C-o>T<

    inoremap <buffer> ns<Tab> namespace
    inoremap <buffer> inl<Tab> inline
    inoremap <buffer> tmpl<Tab> template <typename ><Left>
    inoremap <buffer> class<Tab> class <CR>{<CR><CR>};<CR><Up><Up><Up><Up><End>
    inoremap <buffer> struct<Tab> struct <CR>{<CR><CR>};<CR><Up><Up><Up><Up><End>

    inoremap <buffer> defpi<Tab> const double Pi = 3.141592653589793;
    inoremap <buffer> defe<Tab> const double E = 2.718281828459045;

    inoremap <buffer> defmin<Tab> template<typename T> inline const T min(const T& a, const T&b) { return (b < a) ? b : a; }<CR>template<typename T> inline const T min(const T& a, const T& b, const T& c) { return min(min(a, b), c); }<CR>template<typename T> inline const T min(const T& a, const T& b, const T& c, const T& d) { return min(min(min(a, b), c), d); }<CR>
    inoremap <buffer> defmax<Tab> template<typename T> inline const T max(const T& a, const T&b) { return (a < b) ? b : a; }<CR>template<typename T> inline const T max(const T& a, const T& b, const T& c) { return max(max(a, b), c); }<CR>template<typename T> inline const T max(const T& a, const T& b, const T& c, const T& d) { return max(max(max(a, b), c), d); }<CR>

endfunction

autocmd FileType c,cpp call IMapCpp()
