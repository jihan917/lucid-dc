"vim script for C/C++ code abbreviation/snippet expansion
"(C) Copyright 2010, Ji Han (jihan917<at>yahoo<dot>com)
"free to distribute under the GPL license
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function AbbrevC ()
    if !exists('g:abc_mapped')
        "avoid mapping the keys each time a C/C++ file is opened.
        "this block might get executed more than once by multiple buffers
        "opened almost at the same time, which is ok for our purpose.
        "don't panic (if you're concerned with race condition).
        let g:abc_mapped=1

        inoremap incs<Tab> #include <><Left>
        inoremap incu<Tab> #include ""<Left>
        inoremap ic<Tab> #include <cassert><CR>#include <cctype><CR>#include <cstdint><CR>#include <cstdio><CR>#include <cstdlib><CR>#include <cstring><CR>#include <cmath><CR>#include <ctime><CR>
        inoremap icc<Tab> #include <algorithm><CR>#include <bitset><CR>#include <deque><CR>#include <fstream><CR>#include <functional><CR>#include <iomanip><CR>#include <iostream><CR>#include <iterator><CR>#include <list><CR>#include <map><CR>#include <numeric><CR>#include <queue><CR>#include <set><CR>#include <sstream><CR>#include <stack><CR>#include <string><CR>#include <unordered_map><CR>#include <unordered_set><CR>#include <utility><CR>#include <valarray><CR>#include <vector><CR>

        inoremap main<Tab> int main(int argc, char **argv)<CR>{<CR><CR>return 0;<CR>}<CR><Up><Up><Up><Tab>

        inoremap for<Tab> for (;;)<CR>{<CR><CR>}<CR><Up><Up><Up><Up><End><Left><Left><Left>
        inoremap foreach<Tab> std::for_each(v.begin(), v.end(), );<Left><Left>
        inoremap while<Tab> while ()<CR>{<CR><CR>}<CR><Up><Up><Up><Up><End><Left>
        inoremap do<Tab> do ()<CR>{<CR><CR>} while ();<CR><Up><Up><Up><Up><End><Left>
        inoremap if<Tab> if ()<CR>{<CR><CR>}<Up><Up><Up><End><Left>
        inoremap else<Tab> else ()<CR>{<CR><CR>}<CR><Up><Up><Up><Up><End><Left>
        inoremap switch<Tab> switch ()<CR>{<CR><CR>}<Up><Up><Up><End><Left>

        inoremap ns<Tab> namespace
        inoremap inl<Tab> inline
        inoremap tmpl<Tab> template <typename ><Left>
        inoremap class<Tab> class <CR>{<CR><CR>};<CR><Up><Up><Up><Up><End>
        inoremap struct<Tab> struct <CR>{<CR><CR>};<CR><Up><Up><Up><Up><End>

        inoremap defpi<Tab> const double Pi = 3.141592653589793;
        inoremap defe<Tab> const double E = 2.718281828459045;

        inoremap defmin2<Tab> template<typename T> inline const T min(const T& a, const T&b) { return (b < a) ? b : a; }
        inoremap defmin3<Tab> template<typename T> inline const T min(const T& a, const T& b, const T& c) { return min(min(a, b), c); }
        inoremap defmin4<Tab> template<typename T> inline const T min(const T& a, const T& b, const T& c, const T& d) { return min(min(min(a, b), c), d); }

        inoremap defmax2<Tab> template<typename T> inline const T max(const T& a, const T&b) { return (a < b) ? b : a; }
        inoremap defmax3<Tab> template<typename T> inline const T max(const T& a, const T& b, const T& c) { return max(max(a, b), c); }
        inoremap defmax4<Tab> template<typename T> inline const T max(const T& a, const T& b, const T& c, const T& d) { return max(max(max(a, b), c), d); }

    endif
endfunction

autocmd FileType c,cpp call AbbrevC()

