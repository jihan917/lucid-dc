"vim script for C/C++ code abbreviation/snippet expansion
"(C) Copyright 2010, Ji Han (jihan917<at>yahoo<dot>com)
"free to distribute under the GPL license
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function AbbrevC ()

    inoremap <buffer> incs<Tab> #include <><Left>
    inoremap <buffer> incu<Tab> #include ""<Left>
    inoremap <buffer> ic<Tab> #include <cassert><CR>#include <cctype><CR>#include <cstdint><CR>#include <cstdio><CR>#include <cstdlib><CR>#include <cstring><CR>#include <cmath><CR>#include <ctime><CR>
    inoremap <buffer> icc<Tab> #include <algorithm><CR>#include <bitset><CR>#include <deque><CR>#include <fstream><CR>#include <functional><CR>#include <iomanip><CR>#include <iostream><CR>#include <iterator><CR>#include <list><CR>#include <map><CR>#include <numeric><CR>#include <queue><CR>#include <set><CR>#include <sstream><CR>#include <stack><CR>#include <string><CR>#include <unordered_map><CR>#include <unordered_set><CR>#include <utility><CR>#include <valarray><CR>#include <vector><CR>

    inoremap <buffer> ifdef<Tab> #if defined()<CR>#endif<CR><Up><Up><End><Left>
    inoremap <buffer> once<Tab> #ifndef <C-R>=toupper(tr(expand("%:t"), '.', '_'))<CR><CR>#define <C-R>=toupper(tr(expand("%:t"), '.', '_'))<CR><CR><CR>#endif /* <C-R>=toupper(tr(expand("%:t"), '.', '_'))<CR> */<CR><Up><Up>

    inoremap <buffer> main<Tab> int main(int argc, char **argv)<CR>{<CR><CR>return 0;<CR>}<CR><Up><Up><Up><Tab>

    inoremap <buffer> for<Tab> for (;;)<CR>{<CR><CR>}<CR><Up><Up><Up><Up><End><Left><Left><Left>
    inoremap <buffer> foreach<Tab> std::for_each(v.begin(), v.end(), );<Left><Left>
    inoremap <buffer> while<Tab> while ()<CR>{<CR><CR>}<CR><Up><Up><Up><Up><End><Left>
    inoremap <buffer> do<Tab> do ()<CR>{<CR><CR>} while ();<CR><Up><Up><Up><Up><End><Left>
    inoremap <buffer> if<Tab> if ()<CR>{<CR><CR>}<Up><Up><Up><End><Left>
    inoremap <buffer> else<Tab> else ()<CR>{<CR><CR>}<CR><Up><Up><Up><Up><End><Left>
    inoremap <buffer> switch<Tab> switch ()<CR>{<CR><CR>}<Up><Up><Up><End><Left>

    inoremap <buffer> ns<Tab> namespace
    inoremap <buffer> inl<Tab> inline
    inoremap <buffer> tmpl<Tab> template <typename ><Left>
    inoremap <buffer> class<Tab> class <CR>{<CR><CR>};<CR><Up><Up><Up><Up><End>
    inoremap <buffer> struct<Tab> struct <CR>{<CR><CR>};<CR><Up><Up><Up><Up><End>

    inoremap <buffer> defpi<Tab> const double Pi = 3.141592653589793;
    inoremap <buffer> defe<Tab> const double E = 2.718281828459045;

    inoremap <buffer> defmin2<Tab> template<typename T> inline const T min(const T& a, const T&b) { return (b < a) ? b : a; }
    inoremap <buffer> defmin3<Tab> template<typename T> inline const T min(const T& a, const T& b, const T& c) { return min(min(a, b), c); }
    inoremap <buffer> defmin4<Tab> template<typename T> inline const T min(const T& a, const T& b, const T& c, const T& d) { return min(min(min(a, b), c), d); }

    inoremap <buffer> defmax2<Tab> template<typename T> inline const T max(const T& a, const T&b) { return (a < b) ? b : a; }
    inoremap <buffer> defmax3<Tab> template<typename T> inline const T max(const T& a, const T& b, const T& c) { return max(max(a, b), c); }
    inoremap <buffer> defmax4<Tab> template<typename T> inline const T max(const T& a, const T& b, const T& c, const T& d) { return max(max(max(a, b), c), d); }

endfunction

autocmd FileType c,cpp call AbbrevC()

